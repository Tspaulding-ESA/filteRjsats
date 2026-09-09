#' Four Hit Filter for Lotek Detections
#'
#' This function takes a Lotek detection dataframe generated from the add_fish()
#' function and filters it a second time to remove any remaining multipath
#' detections, and then check the remaining detections by comparing the time
#' between detections, for a rolling window of 4 detections to ensure it is less
#' 16.6x the stated pulse rate interval. It additionally checks that all
#' detections within the window occur within 20% of the pulse rate interval of
#' the other detections and that the standard deviation of pulse rate intervals
#' is less than 0.025. Called by second_filter().
#'
#' @param fish_file a dataframe of detections retrieved from add_fish()
#' @returns A dataframe which has been filtered to remove false positives
#' @export
#' @examples
#' # Apply the Lotek filter to a prefiltered dataset with fish and tag
#' # attributes
#' second_filter_lotek(filter_fish_detects)
second_filter_lotek <- function(fish_file){

  filtered <- fish_file

  filtered <- dplyr::group_by(
    .data = filtered,
    Tag_Hex
  )

  filtered <- dplyr::arrange(
    .data = filtered,
    Tag_Hex,
    DateTime_Local
  )

  # Initial detection intervals
  filtered <- dplyr::mutate(
    .data = filtered,

    td = difftime(
      DateTime_Local,
      dplyr::lag(DateTime_Local),
      units = "secs"
    )
  )

  # Count detections
  det_count <- dplyr::summarise(
    .data = filtered,
    det_count = dplyr::n(),
    .groups = "drop"
  )

  filtered <- dplyr::left_join(
    filtered,
    det_count,
    by = "Tag_Hex"
  )

  filtered <- dplyr::group_by(
    .data = filtered,
    Tag_Hex
  )

  # Remaining multipath
  filtered <- dplyr::mutate(
    .data = filtered,

    multipath = ifelse(
      td > 0.3 | is.na(td),
      FALSE,
      TRUE
    ),

    multipath = ifelse(
      multipath == TRUE &
        dplyr::lead(
          multipath == FALSE,
          default = FALSE
        ),
      FALSE,
      multipath
    )
  )

  # Four-hit requirement
  filtered <- filtered[
    filtered$det_count > 3 &
      filtered$multipath == FALSE,
  ]

  # Recalculate intervals
  filtered <- dplyr::group_by(
    .data = filtered,
    Tag_Hex
  )

  filtered <- dplyr::arrange(
    .data = filtered,
    Tag_Hex,
    DateTime_Local
  )

  filtered <- dplyr::mutate(
    .data = filtered,

    td = difftime(
      DateTime_Local,
      dplyr::lag(DateTime_Local),
      units = "secs"
    ),

    tdiff = as.numeric(td) /
      round(
        as.numeric(td) /
          as.numeric(tag_pulse_rate_interval_nominal)
      ),

    tdiff = ifelse(
      is.infinite(tdiff),
      NA_real_,
      tdiff
    ),

    tdiff = round(tdiff * 100) / 100,

    tdiff = ifelse(
      is.na(tdiff),
      dplyr::lead(tdiff),
      tdiff
    ),

    # Time across a four-detection window
    cs = difftime(
      dplyr::lead(DateTime_Local, 3),
      DateTime_Local,
      units = "secs"
    ),

    pr = as.numeric(
      tag_pulse_rate_interval_nominal
    )
  )

  # Four detections must occur within 16.6 nominal PRIs
  filtered <- filtered[
    filtered$cs < filtered$pr * 16.6 |
      is.na(filtered$cs),
  ]

  # Re-group because following rolling checks must remain within tag
  filtered <- dplyr::group_by(
    .data = filtered,
    Tag_Hex
  )

  filtered <- dplyr::arrange(
    .data = filtered,
    Tag_Hex,
    DateTime_Local
  )

  filtered <- dplyr::mutate(
    .data = filtered,

    v1 = abs(
      pr - tdiff
    ) < pr * 0.20,

    v2 = abs(
      pr - dplyr::lead(tdiff)
    ) < pr * 0.20,

    v3 = abs(
      pr - dplyr::lead(tdiff, 2)
    ) < pr * 0.20,

    # rolling_sd_3 now receives only the current Tag_Hex group
    sd_roll_check =
      rolling_sd_3(as.numeric(pr)) < 0.025,

    sd_roll_check = ifelse(
      is.na(sd_roll_check),
      TRUE,
      sd_roll_check
    )
  )

  filtered <- filtered[
    filtered$v1 == TRUE &
      filtered$v2 == TRUE &
      filtered$v3 == TRUE &
      filtered$sd_roll_check == TRUE,
  ]

  # Recalculate surviving detection count
  filtered <- dplyr::select(
    .data = filtered,
    -det_count
  )

  filtered <- dplyr::group_by(
    .data = filtered,
    Tag_Hex
  )

  det_count <- dplyr::summarise(
    .data = filtered,
    det_count = dplyr::n(),
    .groups = "drop"
  )

  filtered <- dplyr::left_join(
    filtered,
    det_count,
    by = "Tag_Hex"
  )

  filtered <- dplyr::ungroup(filtered)

  filtered
}
