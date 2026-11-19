#' Read an ATS Acoustic Receiver File
#'
#' Reads standard ATS receiver logs and clock-corrected logs containing an
#' Adjusted_Date_Time field. When that field is present, it takes precedence
#' over the receiver's original DateTime for every detection in the file.
#' Additional ATS columns are located by header name rather than by position.
#'
#' @param path Directory containing the ATS file.
#' @param file ATS filename.
#' @param timezone Time zone of the recorded clock readings. Defaults to fixed
#'   PST (UTC-8), without daylight saving time.
#' @return Standardized ATS detections.
#' @export
read_ats <- function(path, file, timezone = "Etc/GMT+8") {
  # Receiver logging restarts can append full file headers separated by
  # long blocks of NUL bytes. read.delim() does not reliably process these
  # blocks; readLines(skipNul = TRUE) preserves all subsequent log sections.
  # The parser below identifies the header applying to each detection row.
  lines <- readLines(file.path(path, file), warn = FALSE, skipNul = TRUE)
  raw <- data.frame(lines = lines, stringsAsFactors = FALSE)

  # ATS logs can contain repeated headers after a logging interruption.
  header_rows <- which(grepl("TagCode", lines, fixed = TRUE) &
                         grepl("DateTime", lines, fixed = TRUE))
  header_names <- lapply(
    strsplit(lines[header_rows], ",", fixed = TRUE),
    function(x) gsub("[^a-z0-9]", "", tolower(trimws(x)))
  )
  has_adjusted <- vapply(
    header_names,
    function(x) "adjusteddatetime" %in% x,
    logical(1)
  )

  if (any(has_adjusted)) {
    # Never silently combine corrected and uncorrected clock readings within
    # one file. Require consistent adjusted headers across logging restarts.
    if (any(!has_adjusted)) {
      stop("ATS file contains a mixture of adjusted and unadjusted headers; ",
           "cannot safely select detection timestamps", call. = FALSE)
    }

    required <- c("adjusteddatetime", "tagcode", "tilt", "vbatt",
                  "temp", "sigstr", "bitperiod", "threshold")
    for (h in header_names) {
      missing <- setdiff(required, h)
      if (length(missing)) {
        stop("Adjusted ATS header missing fields: ",
             paste(missing, collapse = ", "), call. = FALSE)
      }
    }

    candidate_rows <- which(grepl("G72", lines, fixed = TRUE))
    block <- findInterval(candidate_rows, header_rows)
    candidate_rows <- candidate_rows[block > 0L]
    block <- block[block > 0L]

    records <- vector("list", length(header_rows))
    for (i in seq_along(header_rows)) {
      selected <- candidate_rows[block == i]
      if (!length(selected)) next
      parts <- strsplit(lines[selected], ",", fixed = TRUE)
      h <- header_names[[i]]

      field <- function(name) {
        position <- match(name, h)
        vapply(parts, function(x) {
          if (length(x) < position) NA_character_ else {
            gsub('^"|"$', "", trimws(x[position]))
          }
        }, character(1))
      }
      tags <- field("tagcode")
      keep <- !is.na(tags) & grepl("^G72", tags)
      if (!any(keep)) next
      records[[i]] <- data.frame(
        DateTime = field("adjusteddatetime")[keep],
        TagCode = tags[keep],
        Tilt = field("tilt")[keep],
        VBatt = field("vbatt")[keep],
        Temp = field("temp")[keep],
        SigStr = field("sigstr")[keep],
        Bit_Period = field("bitperiod")[keep],
        Threshold = field("threshold")[keep],
        stringsAsFactors = FALSE
      )
    }

    records <- Filter(Negate(is.null), records)
    if (length(records)) {
      ATS <- do.call(rbind, records)
    } else {
      ATS <- data.frame(
        DateTime = character(), TagCode = character(),
        Tilt = character(), VBatt = character(), Temp = character(),
        SigStr = character(), Bit_Period = character(),
        Threshold = character(), stringsAsFactors = FALSE
      )
    }

    # Do not silently fall back to the drifting DateTime if an adjusted value
    # is missing or malformed. Keep subsecond precision when parsing.
    ATS$DateTime_Local <- suppressWarnings(
      lubridate::mdy_hms(ATS$DateTime, tz = timezone, quiet = TRUE)
    )
    iso <- which(is.na(ATS$DateTime_Local) & !is.na(ATS$DateTime))
    if (length(iso)) {
      ATS$DateTime_Local[iso] <- suppressWarnings(
        lubridate::ymd_hms(ATS$DateTime[iso], tz = timezone, quiet = TRUE)
      )
    }
    invalid <- is.na(ATS$DateTime_Local)
    if (any(invalid)) {
      warning(sum(invalid), " ATS detections have missing or invalid ",
              "Adjusted_Date_Time values; returned with NA timestamps ",
              "for explicit quality control (no fallback to DateTime)",
              call. = FALSE)
    }
  } else {
    # Original ATS log layout (13 commas per detection) remains unchanged.
    raw$G72 <- grepl("G72", raw$lines)
    raw$commas <- lengths(regmatches(raw$lines, gregexpr(",", raw$lines)))
    raw$valid <- raw$G72 & raw$commas == 13L
    ATS <- raw[raw$valid, , drop = FALSE]
    ATS <- tidyr::separate(
      data = ATS, col = lines, sep = ",",
      into = c(NA, NA, NA, NA, "DateTime", "TagCode", "Tilt",
               "VBatt", "Temp", "Pressure", "SigStr", "Bit_Period",
               "Threshold")
    )
    ATS <- ATS[grepl("\\.", ATS$DateTime), , drop = FALSE]
    ATS$DateTime_Local <- lubridate::mdy_hms(ATS$DateTime, tz = timezone)
  }

  # Preserve the existing standardized ATS output and frequency calculation.
  ATS$Tag_Hex <- toupper(substr(trimws(ATS$TagCode), 5L, 8L))
  ATS$Tag_Decimal <- broman::hex2dec(ATS$Tag_Hex)
  ATS$Tilt <- suppressWarnings(as.numeric(ATS$Tilt))
  ATS$Volt <- suppressWarnings(as.numeric(ATS$VBatt))
  ATS$Temp <- suppressWarnings(as.numeric(ATS$Temp))
  ATS$SigStr <- suppressWarnings(as.numeric(ATS$SigStr))
  bits <- strsplit(trimws(ATS$Bit_Period), "\\s+")
  b1 <- suppressWarnings(as.numeric(vapply(bits, function(x) {
    if (length(x)) x[1L] else NA_character_
  }, character(1))))
  fraction <- vapply(bits, function(x) {
    if (length(x) >= 2L) x[2L] else NA_character_
  }, character(1))
  b3 <- suppressWarnings(as.numeric(sub("/.*$", "", fraction)))
  ATS$Freq <- ifelse(is.na(b3), -999, 100000 / (b1 + b3 / 31))
  ATS$Thres <- ifelse(is.na(b3), -999,
                       suppressWarnings(as.numeric(ATS$Threshold)))

  receiver <- strsplit(gsub("^SR", "", file), "_", fixed = TRUE)[[1L]][1L]
  receiver <- sub("\\..*$", "", receiver)
  ATS$ReceiverSN <- rep(receiver, nrow(ATS))
  ATS$Make <- rep("ATS", nrow(ATS))
  dplyr::select(ATS, ReceiverSN, Make, DateTime_Local, Tag_Decimal,
                Tag_Hex, Tilt, Volt, Temp, SigStr, Freq, Thres)
}
