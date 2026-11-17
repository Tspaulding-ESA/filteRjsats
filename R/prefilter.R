#' Prefilter JSATS Detections
#'
#' Remove unresolved timestamps, apparent multipath (<=0.3 s), and detections
#' without another tag/receiver observation inside the broad initial time
#' window (120 seconds for fish, 192 seconds for reference tags). Both members
#' of a qualifying pair are retained; isolated endpoints never pass by default.
#'
#' @param jsats_file Standardized detections from read_jsats() or format_detects().
#' @param reference_tags Character vector of reference tag identifiers.
#' @return Standardized detections with first-stage temporal evidence.
#' @export
prefilter <- function(jsats_file, reference_tags) {
  required <- c("ReceiverSN", "Tag_Hex", "DateTime_Local")
  missing <- setdiff(required, names(jsats_file))
  if (length(missing)) {
    stop("Missing required detection columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (nrow(jsats_file) == 0L) return(jsats_file)

  temp <- dplyr::ungroup(jsats_file)
  temp$Tag_Hex <- toupper(trimws(as.character(temp$Tag_Hex)))
  reference_tags <- toupper(trimws(as.character(reference_tags)))
  temp <- dplyr::filter(
    temp, !is.na(ReceiverSN), as.character(ReceiverSN) != "",
    !is.na(Tag_Hex), Tag_Hex != "",
    !is.na(DateTime_Local)
  )
  if (!inherits(temp$DateTime_Local, "POSIXt")) {
    stop("DateTime_Local must be POSIXct/POSIXlt", call. = FALSE)
  }
  temp <- dplyr::arrange(temp, ReceiverSN, Tag_Hex, DateTime_Local)

  # Compare each candidate against the LAST RETAINED hit, not the preceding
  # raw hit, which may itself be multipath. Otherwise a chain such as
  # 0.0, 0.2, 0.4 s incorrectly loses the 0.4-s detection.
  # Process separately by receiver and tag, in chronological order.
  times <- as.numeric(temp$DateTime_Local)
  receivers <- as.character(temp$ReceiverSN)
  tags <- temp$Tag_Hex
  retained <- logical(nrow(temp))
  last_retained_time <- -Inf
  for (i in seq_len(nrow(temp))) {
    if (i == 1L || receivers[i] != receivers[i - 1L] ||
        tags[i] != tags[i - 1L]) {
      last_retained_time <- -Inf
    }
    if (times[i] - last_retained_time > 0.3) {
      retained[i] <- TRUE
      last_retained_time <- times[i]
    }
  }
  temp <- temp[retained, , drop = FALSE]
  temp <- dplyr::arrange(temp, ReceiverSN, Tag_Hex, DateTime_Local)
  temp <- dplyr::group_by(temp, ReceiverSN, Tag_Hex)
  temp <- dplyr::mutate(
    temp,
    time_diff_lag = as.numeric(difftime(
      DateTime_Local, dplyr::lag(DateTime_Local), units = "secs"
    )),
    RefTag = Tag_Hex %in% reference_tags,
    max_interval = dplyr::if_else(RefTag, 192, 120),
    pair_pass = dplyr::coalesce(
      is.finite(dplyr::lead(time_diff_lag)) &
        dplyr::lead(time_diff_lag) > 0.3 &
        dplyr::lead(time_diff_lag) < max_interval,
      FALSE
    ),
    CheckMBP = pair_pass |
      dplyr::lag(pair_pass, default = FALSE)
  )
  temp <- dplyr::filter(temp, CheckMBP == TRUE)
  temp <- dplyr::ungroup(temp)
  temp <- dplyr::select(temp, -time_diff_lag,
                         -RefTag, -max_interval, -pair_pass)
  dplyr::arrange(temp, ReceiverSN, Tag_Hex, DateTime_Local)
}
