#' Four-hit Teknologic detection filter
#'
#' Require a complete four-hit window meeting timing, normalized PRI,
#' variability, and technology-specific frequency criteria.
#' @param fish_file A detection data frame from add_fish().
#' @return Detections supported by a qualifying four-hit window.
#' @export
second_filter_tekno_4h <- function(fish_file) {
  filter_detection_sequences(
    fish_file, hits = 4L,
    frequency_bounds = c(390, 445),
    frequency_tolerance = 55,
    require_short_window_edges = TRUE
  )
}
