#' Get A List of Fish Related Fields from ERDAPP
#'
#' This function searches the California Fish Tracking ERDAPP Database to create
#' a list of all potential fields related to fish metrics. Used to identify
#' important fish data fields to add to detection data in the add_fish function.
#'
#' @returns A vector of potential fish fields which the user may review to
#' retrieve specific field indices
#' @export
#' @examples
#' fish_fields <- get_fish_fields
get_fish_fields <- function() {
  info <- tryCatch({
    rerddap::info('FED_JSATS_taggedfish',
                  url = 'https://oceanview.pfeg.noaa.gov/erddap')
  }, error = function(e) {
    message("⚠️ Unable to access ERDDAP server or retrieve dataset information. Error message: ", e$message)
    return(NULL)
  })

  if (is.null(info)) return(invisible(NULL))

  info$variables
}
