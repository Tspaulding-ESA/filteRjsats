#' Four Hit Filter for ATS Detections
#'
#' This function takes an ATS detection dataframe generated from the
#' add_fish() function and filters it a second time to remove any remaining
#' multipath detections, and then check the remaining detections by comparing the time
#' between detections, for a rolling window of 4 detections to ensure it is less
#' 16.6x the stated pulse rate interval. It additionally checks that all
#' detections within the window occur within 20% of the pulse rate interval of
#' the other detections and that the standard deviation of pulse rate intervals
#' is less than 0.025. It additionally checks that all detections have a frequency
#' between 416.3 and 418.75 kHz and that the frequency of all detections are within
#' 0.505kHz of each other. Called by second_filter().
#'
#'
#' @param fish_file a dataframe of detections retrieved from add_fish()
#' @returns A dataframe which has been filtered to remove false positives
#' @export
#' @examples
#' # Apply a four-hit ATS filter to a prefiltered dataset with fish and tag
#' # attributes
#' second_filter_ats_4h(filter_fish_detects)
#' # No detections are valid
second_filter_ats_4h <- function(fish_file){

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

  filtered <- dplyr::filter(
    .data = filtered,
    det_count > 3,
    multipath == FALSE
  )

  # Recalculate after multipath removal
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

  filtered <- dplyr::filter(
    .data = filtered,
    dplyr::coalesce(
      td > 0.8 * pr_nom,
      TRUE
    )
  )

  # Recalculate after short-PRI removal
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

    cs = difftime(
      dplyr::lead(DateTime_Local, 3),
      DateTime_Local,
      units = "secs"
    ),

    pr = as.numeric(
      tag_pulse_rate_interval_nominal
    ),

    cs_check = dplyr::coalesce(
      cs < pr_nom * 16.6,
      TRUE
    )
  )

  filtered <- dplyr::filter(
    .data = filtered,
    cs_check == TRUE
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

    v1 = dplyr::coalesce(
      abs(pr_nom - tdiff) <
        pr_nom * 0.20,
      TRUE
    ),

    v2 = dplyr::coalesce(
      abs(
        pr_nom -
          dplyr::lead(tdiff)
      ) < pr_nom * 0.20,
      TRUE
    ),

    v3 = dplyr::coalesce(
      abs(
        pr_nom -
          dplyr::lead(tdiff, 2)
      ) < pr_nom * 0.20,
      TRUE
    ),

    sd_roll_check = dplyr::coalesce(
      rolling_sd_3(
        as.numeric(pr_nom)
      ) < 0.025,
      TRUE
    ),

    FreqDiff = abs(
      Freq -
        dplyr::lead(Freq)
    ),

    freq_check = dplyr::coalesce(
      FreqDiff < 0.505,
      TRUE
    ),

    pr_check = dplyr::coalesce(
      td < pr_nom * 4,
      TRUE
    ),

    # Missing following interval does not rescue a known failure
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
    freq_check == TRUE,
    v1 == TRUE,
    v2 == TRUE,
    v3 == TRUE,
    sd_roll_check == TRUE
  )

  filtered <- dplyr::mutate(
    .data = filtered,

    tdiff_check = dplyr::coalesce(
      abs(pr_nom - tdiff) <
        pr_nom * 0.1,
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

  filtered <- dplyr::select(
    .data = filtered,
    -det_count,
    -cs_check,
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
