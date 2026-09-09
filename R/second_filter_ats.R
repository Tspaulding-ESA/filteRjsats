#' Two Hit Filter for ATS Detections
#'
#' This function takes an ATS detection dataframe generated from the
#' add_fish() function and filters it a second time to remove any remaining
#' multipath detections, and then check the remaining detections by comparing
#' the time between each detection to ensure it is less 4x the stated pulse rate
#' interval. It additionally checks that all detections have a frequency
#' between 416.3 and 418.75 kHz and that the frequency of all detections are within
#' 0.505kHz of each other. Called by second_filter_2h4h().
#'
#'
#' @param fish_file a dataframe of detections retrieved from add_fish()
#' @returns A dataframe which has been filtered to remove false positives
#' @export
#' @examples
#' # Apply the ATS filter to a prefiltered dataset with fish and tag
#' # attributes
#' second_filter_ats(filter_fish_detects)
second_filter_ats <- function(fish_file){
  filtered <- fish_file
  filtered$pr_nom <- as.numeric(filtered$tag_pulse_rate_interval_nominal)
  filtered <- dplyr::group_by(.data = filtered, Tag_Hex)
  filtered <- dplyr::group_by(.data = filtered, Tag_Hex)
  filtered <- dplyr::arrange(.data = filtered, Tag_Hex, DateTime_Local)
  filtered$td = difftime(filtered$DateTime_Local,
                         dplyr::lag(filtered$DateTime_Local), #compare the time difference between each hit
                         units = "secs")
  det_count <- dplyr::summarise(filtered, det_count = dplyr::n())
  filtered <- dplyr::left_join(filtered, det_count, by = "Tag_Hex")
  filtered$multipath <- ifelse(filtered$td > 0.3 | is.na(filtered$td), FALSE, TRUE) #identify any remaining multipath
  filtered$multipath <- ifelse(filtered$multipath == TRUE &
                                 dplyr::lead(filtered$multipath == FALSE),
                               FALSE,
                               filtered$multipath)
  filtered <- filtered[filtered$det_count >=2 & filtered$multipath == FALSE,] #first filter out any final multipath and any tags with <3 detects
  filtered$td = difftime(filtered$DateTime_Local,
                         dplyr::lag(filtered$DateTime_Local), # recalculate time diff
                         units = "secs")
  filtered <- filtered[filtered$td > 0.8*filtered$pr_nom|is.na(filtered$td),] # Remove short PRIs as false positives
  filtered$td = difftime(filtered$DateTime_Local,
                         dplyr::lag(filtered$DateTime_Local), # recalculate time diff
                         units = "secs")
  # Calculating for every two hits
  filtered$tdiff = as.numeric(filtered$td)/round(as.numeric(filtered$td)/as.numeric(filtered$pr_nom))
  filtered$tdiff = ifelse(is.infinite(filtered$tdiff),NA,filtered$tdiff)
  filtered$tdiff = round(filtered$tdiff*100)/100
  filtered$tdiff = ifelse(is.na(filtered$tdiff),
                          dplyr::lead(filtered$tdiff),
                          filtered$tdiff)
  filtered$FreqDiff = abs(filtered$Freq - dplyr::lead(filtered$Freq))
  filtered$freq_check = ifelse(filtered$FreqDiff < 0.505|
                                 is.na(filtered$FreqDiff),
                               TRUE,
                               FALSE)
  filtered$pr_check = ifelse(filtered$td < filtered$pr_nom*4 |
                               is.na(filtered$td),
                             TRUE,
                             FALSE)
  filtered$pr_check = ifelse(filtered$pr_check == FALSE &
                               dplyr::lead(filtered$td) < filtered$pr_nom*4,
                             TRUE,
                             filtered$pr_check)
  filtered <- filtered[filtered$pr_check == TRUE &
                         filtered$freq_check == TRUE,]
  filtered <- filtered[abs(as.numeric(filtered$pr_nom)-as.numeric(filtered$tdiff)) < (as.numeric(filtered$pr_nom)*0.1) &
                         filtered$Freq > 416.30 &
                         filtered$Freq < 418.75,]
  filtered <- dplyr::select(filtered, -c(det_count))
  det_count <- dplyr::summarise(filtered, det_count = dplyr::n())
  filtered <- dplyr::left_join(filtered, det_count, by = "Tag_Hex")
  filtered
}
