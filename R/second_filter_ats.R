#' Two Hit Filter for ATS Detections
#'
#' This function takes an ATS detection dataframe generated from the
#' add_fish() function and filters it a second time to remove any remaining
#' multipath detections, and then checks the remaining detections by comparing
#' the time between each detection to ensure it is less than 4x the stated
#' pulse rate interval. It additionally checks that all detections have a
#' frequency between 416.3 and 418.75 kHz and that the frequency of adjacent
#' detections is within 0.505 kHz.
#'
#' @param fish_file a dataframe of detections retrieved from add_fish()
#' @returns A dataframe which has been filtered to remove false positives
#' @export
#'
second_filter_ats <- function(fish_file){

  filtered <- fish_file

  # Convert nominal pulse rate interval to numeric
  filtered$pr_nom <- as.numeric(
    filtered$tag_pulse_rate_interval_nominal
  )

  # Group detections by tag and arrange chronologically
  filtered <- dplyr::group_by(
    .data = filtered,
    Tag_Hex
  )

  filtered <- dplyr::arrange(
    .data = filtered,
    Tag_Hex,
    DateTime_Local
  )

  # Calculate time difference between successive detections WITHIN each tag
  filtered <- dplyr::mutate(
    .data = filtered,
    td = difftime(
      DateTime_Local,
      dplyr::lag(DateTime_Local),
      units = "secs"
    )
  )

  # Count detections for each tag
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

  # Re-establish grouping after join
  filtered <- dplyr::group_by(
    .data = filtered,
    Tag_Hex
  )

  # Identify remaining multipath detections
  filtered <- dplyr::mutate(
    .data = filtered,

    multipath = ifelse(
      td > 0.3 | is.na(td),
      FALSE,
      TRUE
    ),

    # If a short-interval detection is followed by a non-multipath
    # detection, retain the current detection
    multipath = ifelse(
      multipath == TRUE &
        dplyr::lead(multipath == FALSE),
      FALSE,
      multipath
    )
  )

  # Two-hit filter:
  # remove tags represented by fewer than 2 detections
  # and remove remaining multipath detections
  filtered <- filtered[
    filtered$det_count > 1 &
      filtered$multipath == FALSE,
  ]

  # Re-group and re-arrange after filtering
  filtered <- dplyr::group_by(
    .data = filtered,
    Tag_Hex
  )

  filtered <- dplyr::arrange(
    .data = filtered,
    Tag_Hex,
    DateTime_Local
  )

  # Recalculate time difference after removal of multipath detections
  filtered <- dplyr::mutate(
    .data = filtered,
    td = difftime(
      DateTime_Local,
      dplyr::lag(DateTime_Local),
      units = "secs"
    )
  )

  # Remove detections occurring substantially sooner than the nominal PRI
  filtered <- filtered[
    filtered$td > 0.8 * filtered$pr_nom |
      is.na(filtered$td),
  ]

  # Re-group and re-arrange after filtering
  filtered <- dplyr::group_by(
    .data = filtered,
    Tag_Hex
  )

  filtered <- dplyr::arrange(
    .data = filtered,
    Tag_Hex,
    DateTime_Local
  )

  # Recalculate time difference after removal of short PRI detections
  filtered <- dplyr::mutate(
    .data = filtered,
    td = difftime(
      DateTime_Local,
      dplyr::lag(DateTime_Local),
      units = "secs"
    )
  )

  # Calculate estimated PRI accounting for missed transmissions
  filtered <- dplyr::mutate(
    .data = filtered,

    tdiff = as.numeric(td) /
      round(
        as.numeric(td) /
          as.numeric(pr_nom)
      ),

    tdiff = ifelse(
      is.infinite(tdiff),
      NA,
      tdiff
    ),

    tdiff = round(
      tdiff * 100
    ) / 100,

    # First detection within each tag has no preceding interval.
    # Use the following interval for evaluation.
    tdiff = ifelse(
      is.na(tdiff),
      dplyr::lead(tdiff),
      tdiff
    )
  )

  # Calculate frequency difference WITHIN each tag
  filtered <- dplyr::mutate(
    .data = filtered,

    FreqDiff = abs(
      Freq -
        dplyr::lead(Freq)
    ),

    freq_check = ifelse(
      FreqDiff < 0.505 |
        is.na(FreqDiff),
      TRUE,
      FALSE
    )
  )

  # Check whether detections occur within four nominal PRIs
  filtered <- dplyr::mutate(
    .data = filtered,

    pr_check = ifelse(
      td < pr_nom * 4 |
        is.na(td),
      TRUE,
      FALSE
    ),

    # If the preceding interval fails but the following interval
    # is within four PRIs, retain the detection
    pr_check = ifelse(
      pr_check == FALSE &
        dplyr::lead(td) < pr_nom * 4,
      TRUE,
      pr_check
    )
  )

  # Apply PRI spacing and frequency consistency checks
  filtered <- filtered[
    filtered$pr_check == TRUE &
      filtered$freq_check == TRUE,
  ]

  # Apply final nominal PRI and ATS frequency-range checks
  filtered <- filtered[
    abs(
      as.numeric(filtered$pr_nom) -
        as.numeric(filtered$tdiff)
    ) <
      (as.numeric(filtered$pr_nom) * 0.1) &
      filtered$Freq > 416.30 &
      filtered$Freq < 418.75,
  ]

  # Remove old detection count
  filtered <- dplyr::select(
    .data = filtered,
    -det_count
  )

  # Recalculate the number of surviving detections per tag
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

  # Return an ungrouped dataframe
  filtered <- dplyr::ungroup(filtered)

  filtered
}
