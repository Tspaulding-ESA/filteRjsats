#' Get A Dataframe of All Receiver Data from CalFishTrack
#'
#' This function searches the California Fish Tracking ERDAPP Database to create
#' a dataframe ofacoustic receiver metadata. Desired fields can be set to select
#' only certain fields, object rcvr_fields is the default. This data is used to add
#' receiver metadata to detection data in the add_fish function.
#'
#' @param fields a vector of important field names to be imported from the
#' ERDDAP dataset
#' @returns A dataframe of receiver metadata which can be joined to detection data
#' @export
#' @examples
#' # Retrieve the default set of fields from Cal Fish Track needed for package
#' tout <- getOption("timeout")
#' options(timeout = 4)
#' try(get_rcvr_data())
#' options(timeout = tout)
get_rcvr_data <- function(fields = rcvr_fields) {
  rerddap::cache_delete_all()

  # Try to get dataset info
  info <- tryCatch({
    rerddap::info('FED_JSATS_receivers',
                  url = 'https://oceanview.pfeg.noaa.gov/erddap')
  }, error = function(e) {
    message("⚠️ Unable to access ERDDAP server or retrieve receiver dataset info: ", e$message)
    return(NULL)
  })

  if (is.null(info)) return(NULL)

  # Safely resolve fields
  if (!is.null(fields)) {
    fields <- tryCatch({
      info$variables$variable_name[fields]
    }, error = function(e) {
      message("⚠️ Problem resolving selected fields: ", e$message)
      return(NULL)
    })
  } else {
    fields <- NULL
  }

  # Try to download receiver data
  unique_receivers <- tryCatch({
    rerddap::tabledap('FED_JSATS_receivers',
                      url = "https://oceanview.pfeg.noaa.gov/erddap/",
                      fields = fields)
  }, error = function(e) {
    message("⚠️ Failed to download receiver data from ERDDAP: ", e$message)
    return(NULL)
  })

  if (is.null(unique_receivers)) return(data.frame())

  # Begin processing
  unique_receiver <- dplyr::distinct(.data = unique_receivers)

  to_numeric <- function(x) suppressWarnings(as.numeric(x))
  unique_receiver$latitude <- to_numeric(unique_receiver$latitude)
  unique_receiver$longitude <- to_numeric(unique_receiver$longitude)
  unique_receiver$receiver_river_km <- to_numeric(unique_receiver$receiver_river_km)
  unique_receiver$receiver_general_river_km <- to_numeric(unique_receiver$receiver_general_river_km)

  unique_receiver$receiver_start <- lubridate::mdy_hms(unique_receiver$receiver_start)
  unique_receiver$receiver_end <- lubridate::mdy_hms(unique_receiver$receiver_end)
  unique_receiver$receiver_serial_number <- as.character(unique_receiver$receiver_serial_number)

  unique_receiver <- dplyr::rename(.data = unique_receiver,
                                   "ReceiverSN" = receiver_serial_number)

  # Regional location assignment
  unique_receiver$regional_location <- dplyr::case_when(
    unique_receiver$receiver_general_location == "Benicia_east" ~ "Benicia_East",
    unique_receiver$receiver_general_location == "Benicia_west" ~ "Benicia_West",
    unique_receiver$receiver_general_location %in% c("CVP_Trash_Rack_1", "CVP_UpStream_TrashRack", "CVP_Trash_Rack_US", "CVP_Trash_Rack_DS") ~ "CVP_Trash_Rack",
    unique_receiver$receiver_general_location %in% c("GC_D") ~ "Grant_Line_Ctrl_DS",
    unique_receiver$receiver_general_location %in% c("GC_U") ~ "Grant_Line_Ctrl_US",
    unique_receiver$receiver_general_location %in% c("Grant_Line_DS") ~ "Grant_Line_DS",
    unique_receiver$receiver_general_location %in% c("Grant_Line_US") ~ "Grant_Line_US",
    unique_receiver$receiver_general_location %in% c("Old_River_Tracy_D") ~ "Old_River_Tracy_DS",
    unique_receiver$receiver_general_location %in% c("Old_River_Tracy_U1", "Old_River_Tracy_U2") ~ "Old_River_Tracy_US",
    unique_receiver$receiver_location %in% c("CC_RGD_1_J", "CC_RGD_2_J", "CC_RGD1_J", "CC_RGD2_J") ~ "SWP_radial_gates_DS",
    unique_receiver$receiver_location %in% c("CC_RGU_1_J", "CC_RGU_2_J", "CC_RGU1_J", "CC_RGU2_J") ~ "SWP_radial_gates_DS",
    unique_receiver$receiver_general_location == "CVP_Tank" ~ "CVP_Tank",
    unique_receiver$receiver_general_location == "Holland_Cut_Quimby" ~ "Holland_Cut_Quimby",
    unique_receiver$receiver_general_location == "I80-50_Br" ~ "I80-50_Br",
    unique_receiver$receiver_general_location == "MiddleRiver" ~ "MiddleRiver",
    unique_receiver$receiver_general_location == "MidR_OR_B_2021" ~ "Middle_River_at_Old_River",
    unique_receiver$receiver_general_location == "Old River" ~ "Old_River",
    unique_receiver$receiver_general_location == "Old_River_Quimby" ~ "Old_River_Quimby",
    unique_receiver$receiver_general_location == "Sac_BlwGeorgiana" ~ "Sac_BlwGeorgiana",
    unique_receiver$receiver_general_location == "Sac_BlwGeorgiana2" ~ "Sac_BlwGeorgiana2",
    unique_receiver$receiver_general_location %in% c("SWP_intake", "Clifton_Court_SWP") ~ "SWP_intake",
    unique_receiver$receiver_general_location == "SWP_radial_gates_DS" ~ "SWP_radial_gates_DS",
    unique_receiver$receiver_general_location %in% c("SWP_radial_gates_US", "Clifton_Court_RadGates", "SWP_radial_gates") ~ "SWP_radial_gates_US",
    unique_receiver$receiver_general_location == "TowerBridge" ~ "TowerBridge",
    unique_receiver$receiver_general_location == "Durhamferry" ~ "Durham_Ferry",
    unique_receiver$receiver_general_location == "HOR" ~ "SJ_Head_of_Old_River",
    unique_receiver$receiver_general_location == "MidR_hwy4_DS" ~ "Middle_River_Hwy4_US",
    unique_receiver$receiver_general_location == "MidR_hwy4_US" ~ "Middle_River_Hwy4_DS",
    unique_receiver$receiver_general_location == "OR_HOR_DS" ~ "OR_Head_of_Old_River_DS",
    unique_receiver$receiver_general_location == "OR_HOR_US" ~ "OR_Head_of_Old_River_US",
    unique_receiver$receiver_general_location == "OR_hwy4_US" ~ "Old_River_Hwy4_US",
    unique_receiver$receiver_general_location == "OR_hwy4_DS" ~ "Old_River_Hwy4_DS",
    unique_receiver$receiver_general_location == "ORMR" ~ "Old_River_at_Middle_River",
    unique_receiver$receiver_general_location %in% c("RT_MiddleRiver", "MiddleRiver") ~ "MiddleRiver_RR_Bridge",
    unique_receiver$receiver_general_location == "RT_OldRiver" ~ "OldRiver_RR_Bridge",
    unique_receiver$receiver_general_location == "SJ_BCA" ~ "SJ_BCA",
    unique_receiver$receiver_general_location == "Mossdale" ~ "Mossdale",
    unique_receiver$receiver_general_location %in% c("SJ_HOR_DS", "SJ_HOR_US") ~ "SR_Head_of_Old_River"
  )

  data.frame(unique_receiver)
}
