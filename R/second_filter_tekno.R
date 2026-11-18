#' Two-hit Teknologic detection filter
#'
#' Retain detections supported by a complete qualifying pair, with the
#' technology-specific 390-445 kHz frequency range and 55 kHz tolerance.
#' @param fish_file A detection data frame from add_fish().
#' @return Detections supported by a qualifying pair.
#' @param .include_rejected Internal: return every input row with rejection reasons.
#' @export
second_filter_tekno <- function(fish_file, .include_rejected = FALSE) {
  filter_detection_sequences(
    fish_file, hits = 2L,
    frequency_bounds = c(390, 445),
    frequency_tolerance = 55,
    include_rejected = .include_rejected
  )
}
