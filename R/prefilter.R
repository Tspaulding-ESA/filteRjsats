#' Apply the "prefilter" to a Detection Dataframe
#'
#' This function takes a detection dataframe output from read_jsats or from
#' format_detects and filters out multipath signals (time between detections
#' < 0.3 seconds) and spurious signals which do not occur within a specified
#' time frame of the last detection. For beacon tags, this time frame is 192
#' seconds, and for fish, this time is 120 seconds. Following this, the
#' dataframes are standardized so that all detection dataframes from any
#' technology type are identical and superfluous fields are removed.
#'
#' @param jsats_file A dataframe which is the output from read_jstats() or
#' format_detects()
#' @param reference_tags A vector of potential reference (beacon) tag IDs
#' @returns A standardized detection dataframe with multipath detects removed
#' @export
#' @examples
#' # Filter a raw detection dataset
#' prefilter(raw_ats, reftags)
prefilter <- function(jsats_file, reference_tags){
  temp <- jsats_file
  temp <- dplyr::arrange(.data = temp,Tag_Hex)
  temp <- dplyr::group_by(.data = temp, Tag_Hex)
  temp <- dplyr::arrange(.data = temp, Tag_Hex, DateTime_Local)
  det_count <- dplyr::summarise(temp, det_count = dplyr::n())
  temp <- dplyr::left_join(temp, det_count, by = "Tag_Hex")
  temp <- temp[temp$det_count > 1,]
  temp$time_diff_lag = difftime(temp$DateTime_Local, dplyr::lag(temp$DateTime_Local),
                                units = "secs")
  temp$multipath = ifelse(temp$time_diff_lag > lubridate::seconds(0.3)|
                            temp$Tag_Hex != dplyr::lag(temp$Tag_Hex), FALSE, TRUE)
  temp$multipath = ifelse(is.na(temp$time_diff_lag) &
                            dplyr::lead(temp$multipath) == FALSE,
                          FALSE,
                          ifelse(is.na(temp$time_diff_lag),
                                 TRUE,
                                 temp$multipath))
  temp <- temp[temp$multipath == FALSE,] # filter out Multipath
  temp$time_diff_lag = difftime(temp$DateTime_Local, dplyr::lag(temp$DateTime_Local),
                                units = "secs")
  temp$time_diff_lag = ifelse(temp$Tag_Hex != dplyr::lag(temp$Tag_Hex),NA,temp$time_diff_lag)
  temp$RefTag = ifelse(temp$Tag_Hex %in% reference_tags, TRUE, FALSE) #Is it a ref tag?
  temp$CheckMBP = ifelse(temp$RefTag == TRUE, # If a ref tag,
                         (temp$time_diff_lag < lubridate::seconds(3*64)), # 2 hits in 3*Max PRI
                         (temp$time_diff_lag < lubridate::seconds(12*10))) # 2 hits in 12*Max PRI
  temp$CheckMBP = ifelse(is.na(temp$time_diff_lag) & dplyr::lead(temp$CheckMBP) == FALSE, #First Detection is FALSE if 2nd is FALSE
                         FALSE,
                         ifelse(is.na(temp$time_diff_lag),
                                TRUE,
                                temp$CheckMBP)) #Otherwise it's valid
  temp$CheckMBP = ifelse(temp$CheckMBP == FALSE & !is.na(dplyr::lead(temp$time_diff_lag)) &
                           dplyr::lead(temp$time_diff_lag) < lubridate::seconds(12*10), #If invalid based on last detection, but following detection is <120s
                         TRUE, #Valid
                         temp$CheckMBP) #Return Previous Assignment
  temp <- temp[temp$CheckMBP == TRUE,]
  temp <- dplyr::select(.data =  temp,
                        -c(`det_count`))
  det_count <- dplyr::summarise(temp, det_count = dplyr::n())
  temp <- dplyr::left_join(temp, `det_count`, by = "Tag_Hex")
  temp <- temp[temp$det_count > 1,]
  temp <- dplyr::ungroup(temp)
  temp <- dplyr::select(.data =  temp,
                        -c(time_diff_lag,multipath,RefTag,`det_count`))
  temp <- dplyr::arrange(.data = temp, `ReceiverSN`, `Tag_Hex`, `DateTime_Local`)
  temp <- temp[!is.na(temp$DateTime_Local),]
  prefilter_file <- temp
  prefilter_file
}
