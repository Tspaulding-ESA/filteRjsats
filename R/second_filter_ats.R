#' Two-hit ATS detection filter
#'
#' Require a complete, supported two-hit pair. The two observed frequencies
#' must each fall in 416-419 kHz and differ by less than 0.505 kHz. Timing
#' must meet the nominal PRI consistency and four-PRI spacing criteria.
#' @param fish_file A detection data frame from add_fish().
#' @return Detections supported by a qualifying pair.
#' @export
second_filter_ats <- function(fish_file) {
  filter_detection_sequences(
    fish_file, hits = 2L,
    frequency_bounds = c(416, 419),
    frequency_tolerance = 0.505,
    frequency_inclusive = TRUE
  )
}
