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

  filtered <- dplyr::mutate(
    .data = filtered,

    multipath = !is.na(td) & td <= 0.3,

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

  # Recalculate td
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

  # Short PRI filter; unavailable comparison passes
  filtered <- dplyr::filter(
    .data = filtered,
    dplyr::coalesce(
      td > 0.8 * pr_nom,
      TRUE
    )
  )

  # Recalculate after short-PRI filtering
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
          as.numeric(pr_nom)
      ),

    tdiff = ifelse(
      is.infinite(tdiff),
      NA_real_,
      tdiff
    ),

    tdiff = round(tdiff, 2),

    tdiff = ifelse(
      is.na(tdiff),
      dplyr::lead(tdiff),
      tdiff
    ),

    FreqDiff = abs(
      Freq -
        dplyr::lead(Freq)
    ),

    freq_check = dplyr::coalesce(
      FreqDiff < 55,
      TRUE
    ),

    pr_check = dplyr::coalesce(
      td < pr_nom * 4,
      TRUE
    ),

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

  filtered <- dplyr::mutate(
    .data = filtered,

    tdiff_check = dplyr::coalesce(
      abs(pr_nom - tdiff) <
        (pr_nom * 0.1),
      TRUE
    ),

    freq_range_check = dplyr::coalesce(
      Freq > 390.00 &
        Freq < 445.00,
      TRUE
    )
  )

  filtered <- dplyr::filter(
    .data = filtered,
    tdiff_check == TRUE,
    freq_range_check == TRUE
  )

  filtered <- dplyr::select(
    .data = filtered,
    -det_count,
    -tdiff_check,
    -freq_range_check
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
