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

  filtered$pr_nom <- as.numeric(
    filtered$tag_pulse_rate_interval_nominal
  )

  # Group and arrange
  filtered <- dplyr::group_by(
    .data = filtered,
    Tag_Hex
  )

  filtered <- dplyr::arrange(
    .data = filtered,
    Tag_Hex,
    DateTime_Local
  )

  # Initial time difference within tag
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

  # Identify remaining multipath
  filtered <- dplyr::mutate(
    .data = filtered,

    multipath = !is.na(td) & td <= 0.3,

    # Rescue a short interval only if the following detection
    # is explicitly non-multipath.
    # No following detection does not rescue a known failure.
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

  # TWO-HIT requirement
  filtered <- dplyr::filter(
    .data = filtered,
    det_count > 1,
    multipath == FALSE
  )

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

  # Remove short PRIs.
  # NA td is the first detection and therefore passes.
  filtered <- dplyr::filter(
    .data = filtered,
    dplyr::coalesce(
      td > 0.8 * pr_nom,
      TRUE
    )
  )

  # Recalculate td after short-PRI filtering
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

    # Estimated PRI allowing for missed transmissions
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

    tdiff = round(tdiff, 2),

    # First detection uses the following interval
    tdiff = ifelse(
      is.na(tdiff),
      dplyr::lead(tdiff),
      tdiff
    ),

    # Frequency difference within tag
    FreqDiff = abs(
      Freq -
        dplyr::lead(Freq)
    ),

    # End-of-sequence NA passes
    freq_check = dplyr::coalesce(
      FreqDiff < 0.505,
      TRUE
    ),

    # Primary PRI check.
    # First detection has td = NA and passes.
    pr_check = dplyr::coalesce(
      td < pr_nom * 4,
      TRUE
    ),

    # A known failed PRI can be rescued by a known-valid
    # following interval. Missing following interval does
    # not rescue a known failure.
    pr_check = ifelse(
      pr_check == FALSE &
        !is.na(dplyr::lead(td)) &
        dplyr::coalesce(
          dplyr::lead(td) < pr_nom * 4,
          FALSE
        ),
      TRUE,
      pr_check
    )
  )

  filtered <- dplyr::filter(
    .data = filtered,
    pr_check == TRUE,
    freq_check == TRUE
  )

  # Final checks.
  # An unavailable calculated PRI check passes.
  filtered <- dplyr::mutate(
    .data = filtered,

    tdiff_check = dplyr::coalesce(
      abs(pr_nom - tdiff) <
        (pr_nom * 0.1),
      TRUE
    ),

    freq_range_check = dplyr::coalesce(
      Freq > 416.30 &
        Freq < 418.75,
      TRUE
    )
  )

  filtered <- dplyr::filter(
    .data = filtered,
    tdiff_check == TRUE,
    freq_range_check == TRUE
  )

  # Remove temporary checks and old count
  filtered <- dplyr::select(
    .data = filtered,
    -det_count,
    -tdiff_check,
    -freq_range_check
  )

  # Recalculate surviving detection count
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
