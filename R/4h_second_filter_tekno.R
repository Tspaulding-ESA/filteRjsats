#' Four Hit Filter for Tekno Detections
#'
#' This function takes an Tekno detection dataframe generated from the
#' add_fish() function and filters it a second time to remove any remaining
#' multipath detections, and then check the remaining detections by comparing the time
#' between detections, for a rolling window of 4 detections to ensure it is less
#' 16.6x the stated pulse rate interval. It additionally checks that all detections have a frequency
#' between 390 and 445 kHz and that the frequency of all detections are within
#' 55kHz of each other. Called by second_filter_4h().
#'
#'
#' @param fish_file a dataframe of detections retrieved from add_fish()
#' @returns A dataframe which has been filtered to remove false positives
#' @export
#' @examples
#' # Apply a four-hit Teknologics filter to a prefiltered dataset with fish and tag
#' # attributes
#' second_filter_tekno_4h(filter_fish_detects)
#' # No detections are valid
second_filter_tekno_4h <- function(fish_file){

  filtered <- fish_file

  filtered$pr_nom <- as.numeric(
    filtered$tag_pulse_rate_interval_nominal
  )

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

  # Detection count
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

  # Multipath
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

  # Recalculate td after multipath removal
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
    )
  )

  # Remove short PRIs
  filtered <- filtered[
    filtered$td > 0.8 * filtered$pr_nom |
      is.na(filtered$td),
  ]

  # Recalculate after removal
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

    # Four-hit time window
    cs = difftime(
      dplyr::lead(DateTime_Local, 3),
      DateTime_Local,
      units = "secs"
    ),

    pr = as.numeric(
      tag_pulse_rate_interval_nominal
    )
  )

  filtered <- filtered[
    filtered$cs < filtered$pr_nom * 16.6 |
      is.na(filtered$cs),
  ]

  # Re-group before rolling calculations
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
      pr_nom - tdiff
    ) < pr_nom * 0.20,

    v2 = abs(
      pr_nom - dplyr::lead(tdiff)
    ) < pr_nom * 0.20,

    v3 = abs(
      pr_nom - dplyr::lead(tdiff, 2)
    ) < pr_nom * 0.20,

    sd_roll_check =
      rolling_sd_3(as.numeric(pr_nom)) < 0.025,

    sd_roll_check = ifelse(
      is.na(sd_roll_check),
      TRUE,
      sd_roll_check
    ),

    FreqDiff = abs(
      Freq -
        dplyr::lead(Freq)
    ),

    freq_check = ifelse(
      FreqDiff < 55 |
        is.na(FreqDiff),
      TRUE,
      FALSE
    ),

    pr_check = ifelse(
      td < pr_nom * 4 |
        is.na(td),
      TRUE,
      FALSE
    ),

    pr_check = ifelse(
      pr_check == FALSE &
        !is.na(dplyr::lead(td)) &
        dplyr::lead(td) < pr_nom * 4,
      TRUE,
      pr_check
    )
  )

  filtered <- filtered[
    filtered$pr_check == TRUE &
      filtered$freq_check == TRUE &
      filtered$v1 == TRUE &
      filtered$v2 == TRUE &
      filtered$v3 == TRUE &
      filtered$sd_roll_check == TRUE,
  ]

  # Final Tekno PRI and frequency criteria
  filtered <- filtered[
    abs(
      as.numeric(filtered$pr_nom) -
        as.numeric(filtered$tdiff)
    ) <
      (as.numeric(filtered$pr_nom) * 0.1) &
      filtered$Freq > 390.00 &
      filtered$Freq < 445.00,
  ]

  # Surviving detection count
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
