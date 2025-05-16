#' Get A List of Receiver Related Fields from ERDAPP
#'
#' This function searches the California Fish Tracking ERDAPP Database to create
#' a list of all potential fields related to acoustic receiver metadata.
#' Used to identify important metadata fields to include when adding receiver
#' data in the join_rcvr_data function.
#'
#' @returns A vector of potential receiver metadata fields which the user may review
#' @export
#' @examples
#' # View a list of available receiver fields
#' get_rcvr_fields()
get_rcvr_fields <- function() {
  info <- tryCatch({
    rerddap::info('FED_JSATS_receivers',
                  url = 'https://oceanview.pfeg.noaa.gov/erddap')
  }, error = function(e) {
    message("⚠️ Unable to access ERDDAP or retrieve receiver fields. Error: ", e$message)
    return(NULL)
  })

  if (is.null(info)) return(invisible(NULL))

  info$variables
}
