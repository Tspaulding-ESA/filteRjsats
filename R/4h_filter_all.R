#' Four-hit detection filtering by receiver technology
#' @param fish_file Detection data from add_fish().
#' @return Every input detection with spurious TRUE unless it has complete
#'   qualifying four-hit sequence evidence. Rejected records have one
#'   primary reject_code (multipath, freq, or pri); accepted records have NA.
#' @export
second_filter_4h <- function(fish_file) {
  run_second_filter(fish_file, four_hit = TRUE)
}
