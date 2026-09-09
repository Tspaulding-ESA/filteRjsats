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

  # Arrange and group detections by tag
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
  temp <- dplyr::filter(
    .data = temp,
    det_count > 1
  )

  # Re-establish grouping after join/filter
  temp <- dplyr::group_by(
    .data = temp,
    Tag_Hex
  )

  temp <- dplyr::arrange(
    .data = temp,
    Tag_Hex,
    DateTime_Local
  )

  # Calculate time difference WITHIN tag
  temp <- dplyr::mutate(
    .data = temp,

    time_diff_lag = difftime(
      DateTime_Local,
      dplyr::lag(DateTime_Local),
      units = "secs"
    ),

    # First detection cannot be evaluated yet.
    # Otherwise <= 0.3 sec is multipath.
    multipath = ifelse(
      is.na(time_diff_lag),
      NA,
      time_diff_lag <= lubridate::seconds(0.3)
    ),

    # For the first detection, use the status of the following
    # interval. If there is no following interval, pass.
    multipath = ifelse(
      is.na(time_diff_lag),
      dplyr::coalesce(
        dplyr::lead(multipath),
        FALSE
      ),
      multipath
    )
  )

  # Remove multipath detections
  temp <- dplyr::filter(
    .data = temp,
    multipath == FALSE
  )

  # Recalculate time difference after multipath removal
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

    RefTag = Tag_Hex %in% reference_tags,

    # Beacon: < 192 sec
    # Fish:   < 120 sec
    CheckMBP = ifelse(
      RefTag,
      time_diff_lag < lubridate::seconds(3 * 64),
      time_diff_lag < lubridate::seconds(12 * 10)
    ),

    # First detection adopts the next interval's validity.
    # If there is no next interval, the unevaluable check passes.
    CheckMBP = ifelse(
      is.na(time_diff_lag),
      dplyr::coalesce(
        dplyr::lead(CheckMBP),
        TRUE
      ),
      CheckMBP
    ),

    # A detection that failed relative to the preceding hit can
    # be rescued if the FOLLOWING hit occurs within 120 sec.
    # Missing next hit does NOT rescue a known failure.
    CheckMBP = ifelse(
      CheckMBP == FALSE &
        !is.na(dplyr::lead(time_diff_lag)) &
        dplyr::coalesce(
          dplyr::lead(time_diff_lag) <
            lubridate::seconds(12 * 10),
          FALSE
        ),
      TRUE,
      CheckMBP
    ),

    # Any remaining unevaluable check passes
    CheckMBP = dplyr::coalesce(
      CheckMBP,
      TRUE
    )
  )

  temp <- dplyr::filter(
    .data = temp,
    CheckMBP == TRUE
  )

  # Recalculate surviving detection count
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

  # Prefilter still requires at least two surviving detections
  temp <- dplyr::filter(
    .data = temp,
    det_count > 1
  )

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

  temp <- dplyr::filter(
    .data = temp,
    !is.na(DateTime_Local)
  )

  prefilter_file <- temp

  prefilter_file
}
