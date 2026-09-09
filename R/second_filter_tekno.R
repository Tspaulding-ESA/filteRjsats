#' Two Hit Filter for Teknologic Detections
#'
#' This function takes a Teknologic detection dataframe generated from the
#' add_fish() function and filters it a second time to remove any remaining
#' multipath detections, and then check the remaining detections by comparing
#' the time between each detection to ensure it is less 4x the stated pulse rate
#' interval. It additionally checks that all detections have a frequency
#' between 390 and 445 kHz and that the frequency of all detections are within
#' 55kHz of each other. Called by second_filter_2h4h().
#'
#'
#' @param fish_file a dataframe of detections retrieved from add_fish()
#' @return A dataframe which has been filtered to remove false positives
#' @export
#' @examples
#' # Apply the Teknologics filter to a prefiltered dataset with fish and tag
#' # attributes
#' second_filter_tekno(filter_fish_detects)
second_filter_tekno <- function(fish_file){

  filtered <- fish_file

  filtered$pr_nom <- as.numeric(
    filtered$tag_pulse_rate_interval_nominal
  )

  # Group and arrange detections
  filtered <- dplyr::group_by(
    .data = filtered,
    Tag_Hex
  )

  filtered <- dplyr::arrange(
    .data = filtered,
    Tag_Hex,
    DateTime_Local
  )

  # Calculate detection interval within tag
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

  # Identify remaining multipath
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

  # This is a TWO-HIT filter, so require >= 2 detections
  filtered <- filtered[
    filtered$det_count > 1 &
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

  # Remove detections substantially shorter than expected PRI
  filtered <- filtered[
    filtered$td > 0.8 * filtered$pr_nom |
      is.na(filtered$td),
  ]

  # Recalculate td after short-PRI removal
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

    # Estimate underlying PRI when transmissions were missed
    tdiff = as.numeric(td) /
      round(
        as.numeric(td) /
          as.numeric(pr_nom)
      ),

    tdiff = ifelse(
      is.infinite(tdiff),
      NA_real_,
      tdiff
    ),

    tdiff = round(tdiff * 100) / 100,

    # Assign following interval to first detection of tag
    tdiff = ifelse(
      is.na(tdiff),
      dplyr::lead(tdiff),
      tdiff
    ),

    # Frequency comparison within tag
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

    # Detection must be within 4 nominal PRIs
    pr_check = ifelse(
      td < pr_nom * 4 |
        is.na(td),
      TRUE,
      FALSE
    ),

    # Retain if following interval meets criterion
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
      filtered$freq_check == TRUE,
  ]

  # Final PRI and frequency checks
  filtered <- filtered[
    abs(
      as.numeric(filtered$pr_nom) -
        as.numeric(filtered$tdiff)
    ) <
      (as.numeric(filtered$pr_nom) * 0.1) &
      filtered$Freq > 390.00 &
      filtered$Freq < 445.00,
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
