#' Two- or four-hit detection filtering by receiver technology
#' @param fish_file Detection data from add_fish().
#' @return Every input detection with spurious TRUE unless it has complete
#'   qualifying sequence evidence; accepted records have spurious FALSE.
#'   Rejected rows have one primary reject_code: multipath, freq, or pri.
#'   Accepted rows have reject_code = NA_character_.
#' @export
second_filter_2h4h <- function(fish_file) {
  run_second_filter(fish_file, four_hit = FALSE)
}
