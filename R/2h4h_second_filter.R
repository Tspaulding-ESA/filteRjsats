#' Two- or four-hit detection filtering by receiver technology
#' @param fish_file Detection data from add_fish().
#' @return Every input detection with spurious TRUE unless it has complete
#'   qualifying sequence evidence; accepted records have spurious FALSE.
#' @export
second_filter_2h4h <- function(fish_file) {
  run_second_filter(fish_file, four_hit = FALSE)
}
