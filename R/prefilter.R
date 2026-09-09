#' Apply the "prefilter" to a Detection Dataframe
#'
#' This function takes a detection dataframe output from read_jsats or from
#' format_detects and filters out multipath signals (time between detections
#' < 0.3 seconds) and spurious signals which do not occur within a specified
#' time frame of the last detection. For beacon tags, this time frame is 192
#' seconds, and for fish, this time is 120 seconds. Following this, the
#' dataframes are standardized so that all detection dataframes from any
#' technology type are identical and superfluous fields are removed.
#'
#' @param jsats_file A dataframe which is the output from read_jsats() or
#' format_detects()
#' @param reference_tags A vector of potential reference (beacon) tag IDs
#' @returns A standardized detection dataframe with multipath detects removed
#' @export
prefilter <- function(jsats_file, reference_tags){

  temp <- jsats_file

  # Arrange and group by tag
  temp <- dplyr::arrange(
    .data = temp,
    Tag_Hex,
    DateTime_Local
  )

  temp <- dplyr::group_by(
    .data = temp,
    Tag_Hex
  )

  # Count detections per tag
  det_count <- dplyr::summarise(
    .data = temp,
    det_count = dplyr::n(),
    .groups = "drop"
  )

  temp <- dplyr::left_join(
    temp,
    det_count,
    by = "Tag_Hex"
  )

  # Require at least two detections
  temp <- temp[temp$det_count > 1, ]

  # Re-establish grouping after join/subset
  temp <- dplyr::group_by(
    .data = temp,
    Tag_Hex
  )

  temp <- dplyr::arrange(
    .data = temp,
    Tag_Hex,
    DateTime_Local
  )

  # Calculate time difference within each tag
  temp <- dplyr::mutate(
    .data = temp,

    time_diff_lag = difftime(
      DateTime_Local,
      dplyr::lag(DateTime_Local),
      units = "secs"
    ),

    multipath = ifelse(
      time_diff_lag > lubridate::seconds(0.3),
      FALSE,
      TRUE
    ),

    # For the first detection of a tag:
    # if the next detection is not multipath, retain the first detection;
    # otherwise flag the first detection as part of the multipath sequence.
    multipath = ifelse(
      is.na(time_diff_lag) &
        dplyr::lead(multipath, default = FALSE) == FALSE,
      FALSE,
      ifelse(
        is.na(time_diff_lag),
        TRUE,
        multipath
      )
    )
  )

  # Remove multipath
  temp <- temp[temp$multipath == FALSE, ]

  # Re-group after filtering and recalculate time difference
  temp <- dplyr::group_by(
    .data = temp,
    Tag_Hex
  )

  temp <- dplyr::arrange(
    .data = temp,
    Tag_Hex,
    DateTime_Local
  )

  temp <- dplyr::mutate(
    .data = temp,

    time_diff_lag = difftime(
      DateTime_Local,
      dplyr::lag(DateTime_Local),
      units = "secs"
    ),

    RefTag = ifelse(
      Tag_Hex %in% reference_tags,
      TRUE,
      FALSE
    ),

    # Beacon tags: two detections within 192 sec
    # Fish tags: two detections within 120 sec
    CheckMBP = ifelse(
      RefTag == TRUE,
      time_diff_lag < lubridate::seconds(3 * 64),
      time_diff_lag < lubridate::seconds(12 * 10)
    ),

    # First detection takes validity from its following interval
    CheckMBP = ifelse(
      is.na(time_diff_lag) &
        dplyr::lead(CheckMBP, default = FALSE) == FALSE,
      FALSE,
      ifelse(
        is.na(time_diff_lag),
        TRUE,
        CheckMBP
      )
    ),

    # If invalid relative to the previous detection but the next
    # detection occurs within 120 sec, retain the current detection
    CheckMBP = ifelse(
      CheckMBP == FALSE &
        !is.na(dplyr::lead(time_diff_lag)) &
        dplyr::lead(time_diff_lag) < lubridate::seconds(12 * 10),
      TRUE,
      CheckMBP
    )
  )

  temp <- temp[temp$CheckMBP == TRUE, ]

  # Remove old detection count and recalculate after filtering
  temp <- dplyr::select(
    .data = temp,
    -det_count
  )

  temp <- dplyr::group_by(
    .data = temp,
    Tag_Hex
  )

  det_count <- dplyr::summarise(
    .data = temp,
    det_count = dplyr::n(),
    .groups = "drop"
  )

  temp <- dplyr::left_join(
    temp,
    det_count,
    by = "Tag_Hex"
  )

  # Require at least two surviving detections
  temp <- temp[temp$det_count > 1, ]

  temp <- dplyr::ungroup(temp)

  temp <- dplyr::select(
    .data = temp,
    -time_diff_lag,
    -multipath,
    -RefTag,
    -det_count
  )

  temp <- dplyr::arrange(
    .data = temp,
    ReceiverSN,
    Tag_Hex,
    DateTime_Local
  )

  temp <- temp[!is.na(temp$DateTime_Local), ]

  prefilter_file <- temp

  prefilter_file
}
