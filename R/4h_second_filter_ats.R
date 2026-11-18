#' Four-hit ATS detection filter
#'
#' Retain only detections belonging to at least one complete four-hit window.
#' Frequency for every hit must lie in 416-419 kHz; consecutive frequencies
#' must differ by less than 0.505 kHz. Required PRI checks and the rolling
#' variability test are evaluated within the same window.
#' @param fish_file A detection data frame from add_fish().
#' @return Detections supported by a qualifying four-hit window.
#' @param .include_rejected Internal: return every input row with rejection reasons.
#' @export
second_filter_ats_4h <- function(fish_file, .include_rejected = FALSE) {
  filter_detection_sequences(
    fish_file, hits = 4L,
    frequency_bounds = c(416, 419),
    frequency_tolerance = 0.505,
    frequency_inclusive = TRUE,
    require_short_window_edges = TRUE,
    include_rejected = .include_rejected
  )
}
