#' Four Hit Filter for Any Detection File
#'
#' This function takes any acoustic receiver detection dataframe generated from
#' the add_fish() function and filters it a second time to remove any remaining
#' multipath detections. Then applies an algorithm to assess and filter all
#' remaining detections based on a four hit filter.
#'
#'
#' @param fish_file a dataframe of detections retrieved from add_fish()
#' @returns A dataframe which has been filtered to flag false positives aka
#' spurious detections. These are retained in the dataframe in a new logical
#' column `spurious` with the value of TRUE. Real detections have a FALSE value
#' in the spurious column.
#' @export
#' @examples
#' # Apply a 4 hit filter based on the technology
#' second_filter_4h(filter_fish_detects)
second_filter_4h <- function(fish_file){
  if (nrow(fish_file) > 0) {
    make = fish_file$Make[1]
    final_file = NULL
    if (make == "Lotek") final_file <- second_filter_lotek(fish_file)
    if (make == "Tekno") final_file <- second_filter_tekno(fish_file)
    if (make == "ATS")   final_file <- second_filter_ats(fish_file)
    if (is.null(final_file)) errorCondition("Receiver make should be one of Lotek, Tekno, or ATS")

    message(paste0("Number of Valid Tag IDs: ", length(unique(final_file$Tag_Hex))))
    message(paste0("Number of Valid Detections: ", length(final_file$DateTime_Local)))

    spur <- compare_detects(fish_file, final_file)

    message(paste0("Number of Tags with Spurious Detections: ", length(unique(spur$Tag_Hex))))
    message(paste0("Tags with Only Spurious Detections: "), spur$Tag_Hex[which(!(unique(spur$Tag_Hex) %in% (unique(final_file$Tag_Hex))))])
    message(paste0("Number of Spurious Detections: ", length(spur$DateTime_Local)))

    spur$spurious <- TRUE
    final_file$spurious <- FALSE

    final_file <- bind_rows(final_file, spur)
    final_file <- dplyr::arrange(final_file, Tag_Hex, DateTime_Local)

    final_file
  } else {
    message("No Valid Detections in File, Skipping File")
    NULL
  }
}
