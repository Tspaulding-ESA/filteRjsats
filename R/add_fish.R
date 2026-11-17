#' Join fish metadata to prefiltered detections
#'
#' Match by normalized hexadecimal tag code, then use the actual release date
#' and twice the reported tag life to determine whether a match is plausible.
#' There is NO independent 200-day release-date restriction. Multiple plausible
#' fish records with a reused tag code remain separate pending reconciliation.
#'
#' @param prefilter_file Prefiltered detection data frame.
#' @param fish Fish metadata, including tag ID, release date, and tag life.
#' @return Detections linked to a fish within its modeled active period.
#' @export
add_fish <- function(prefilter_file, fish) {
  if (!"Tag_Hex" %in% names(prefilter_file)) {
    stop("prefilter_file must have Tag_Hex", call. = FALSE)
  }
  if (!"tag_id_hex" %in% names(fish)) {
    if ("Tag_Hex" %in% names(fish)) {
      fish$tag_id_hex <- fish$Tag_Hex
    } else {
      stop("fish must have tag_id_hex or Tag_Hex", call. = FALSE)
    }
  }
  required_fish <- c("fish_release_date", "tag_life")
  missing <- setdiff(required_fish, names(fish))
  if (length(missing)) {
    stop("Missing fish metadata: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  # get_tagged_fish() can supply BOTH tag_id_hex and Tag_Hex;
  # retain the left-hand canonical Tag_Hex when joining.
  if ("Tag_Hex" %in% names(fish)) fish$Tag_Hex <- NULL
  file <- prefilter_file
  file$Tag_Hex <- toupper(trimws(as.character(file$Tag_Hex)))
  fish$tag_id_hex <- toupper(trimws(as.character(fish$tag_id_hex)))
  file$TagInFile <- !is.na(file$Tag_Hex) &
    file$Tag_Hex %in% fish$tag_id_hex

  message("Detections in file: ", nrow(file))
  file <- dplyr::filter(file, TagInFile == TRUE)
  file <- dplyr::left_join(file, fish, by = c("Tag_Hex" = "tag_id_hex"))

  # The metadata generator may represent tag life as lubridate::Period;
  # formatted user data usually provide a number of days.
  if (inherits(file$tag_life, "Period")) {
    life_days <- as.numeric(lubridate::as.duration(file$tag_life)) / 86400
  } else {
    life_days <- suppressWarnings(as.numeric(file$tag_life))
  }

  file$CheckDT <- !is.na(file$DateTime_Local) &
    !is.na(file$fish_release_date) &
    file$DateTime_Local > file$fish_release_date
  file$CheckBattLife <- is.finite(life_days) & life_days > 0 &
    !is.na(file$DateTime_Local) &
    !is.na(file$fish_release_date) &
    file$DateTime_Local <= (file$fish_release_date +
                            lubridate::days(2 * life_days))

  # Unknown release time or life is NOT evidence of a valid fish match.
  file <- dplyr::filter(file, CheckDT == TRUE, CheckBattLife == TRUE)
  message("Valid fish-tag IDs: ", length(unique(file$Tag_Hex)))
  message("Detections after fish metadata checks: ", nrow(file))
  file
}
