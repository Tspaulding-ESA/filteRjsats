#' Four-hit Lotek detection filter
#'
#' A detection requires a complete four-hit window with cumulative duration
#' below 16.6x nominal PRI, three PRI-normalized intervals within 20%, and
#' sample SD of those intervals below 0.025 seconds. An incomplete window
#' never passes by default.
#' @param fish_file A detection data frame from add_fish().
#' @return Detections supported by a qualifying four-hit window.
#' @export
second_filter_lotek <- function(fish_file) {
  filter_detection_sequences(fish_file, hits = 4L)
}
