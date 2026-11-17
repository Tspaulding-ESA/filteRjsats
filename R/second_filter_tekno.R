#' Two-hit Teknologic detection filter
#'
#' Retain detections supported by a complete qualifying pair, with the
#' technology-specific 390-445 kHz frequency range and 55 kHz tolerance.
#' @param fish_file A detection data frame from add_fish().
#' @return Detections supported by a qualifying pair.
#' @export
second_filter_tekno <- function(fish_file) {
  filter_detection_sequences(
    fish_file, hits = 2L,
    frequency_bounds = c(390, 445),
    frequency_tolerance = 55
  )
}
