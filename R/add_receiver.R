#' Attach Receiver Metadata Using Nonoverlapping Deployment Intervals
#'
#' Match each detection to the deployment of the same receiver serial number
#' that was active at the detection time. Intervals are [receiver_start,
#' receiver_end); a missing receiver_end means the deployment is ongoing.
#' All clock readings are interpreted as fixed PST (UTC-8; NO daylight saving).
#' Overlapping intervals are errors. Unmatched detections are retained once,
#' with missing deployment metadata rather than an invented location.
#'
#' @param final_file Pre-join detections with ReceiverSN and DateTime_Local.
#'   Times must be POSIXct/POSIXlt representing recorded PST clock readings.
#' @param rcvr_data Deployment metadata with ReceiverSN, receiver_start,
#'   receiver_end (POSIXct/POSIXlt), and optional metadata columns. Missing
#'   receiver_end denotes a receiver still deployed and recording.
#' @return All input detection rows, deployment metadata (NA if unmatched),
#'   receiver_match_status (matched/unmatched), and receiver_match_count (1/0).
#'   DateTime_Local and deployment dates are normalized to fixed PST, retaining
#'   their displayed clock readings. Spurious/reject_code are unchanged.
#' @export
join_rcvr_data <- function(final_file, rcvr_data) {
  if (!all(c("ReceiverSN", "DateTime_Local") %in% names(final_file))) {
    stop("final_file requires ReceiverSN and DateTime_Local", call. = FALSE)
  }
  if (!all(c("ReceiverSN", "receiver_start", "receiver_end") %in%
           names(rcvr_data))) {
    stop("rcvr_data requires ReceiverSN, receiver_start, receiver_end",
         call. = FALSE)
  }
  if (!inherits(final_file$DateTime_Local, "POSIXt") ||
      !inherits(rcvr_data$receiver_start, "POSIXt") ||
      !inherits(rcvr_data$receiver_end, "POSIXt")) {
    stop("Detection and deployment timestamps must be POSIXct/POSIXlt",
         call. = FALSE)
  }
  if (any(c("receiver_match_status", "receiver_match_count") %in%
          names(final_file))) {
    stop("Input has already been assigned receiver deployment metadata",
         call. = FALSE)
  }

  deployments <- unique(dplyr::ungroup(rcvr_data))
  metadata_names <- setdiff(names(deployments),
                            c("ReceiverSN",
                              if ("Make" %in% names(final_file)) "Make"))
  shared <- intersect(names(final_file), metadata_names)
  if (length(shared)) {
    stop("Input already contains receiver metadata: ",
         paste(shared, collapse = ", "),
         ". Start with the original pre-join detection table.", call. = FALSE)
  }

  # force_tz() reinterprets recorded local clock time as permanent UTC-8.
  # with_tz() would *shift the displayed clock* and is not appropriate when
  # the original recordings did not observe daylight saving time.
  pst <- "Etc/GMT+8"
  detected_at <- lubridate::force_tz(as.POSIXct(final_file$DateTime_Local),
                                      tzone = pst)
  deployments$receiver_start <- lubridate::force_tz(
    as.POSIXct(deployments$receiver_start), tzone = pst)
  deployments$receiver_end <- lubridate::force_tz(
    as.POSIXct(deployments$receiver_end), tzone = pst)

  serial <- trimws(as.character(deployments$ReceiverSN))
  start <- as.numeric(deployments$receiver_start)
  end <- as.numeric(deployments$receiver_end)
  if (anyNA(serial) || any(!nzchar(serial)) || any(!is.finite(start))) {
    stop("Receiver metadata has missing serial numbers or start times",
         call. = FALSE)
  }
  if (any(!is.na(end) & (!is.finite(end) | end <= start))) {
    stop("Receiver metadata has invalid end times", call. = FALSE)
  }
  if ("dep_id" %in% names(deployments) &&
      anyDuplicated(deployments$dep_id[!is.na(deployments$dep_id)])) {
    stop("Receiver metadata has duplicated dep_id values", call. = FALSE)
  }
  # Missing end times are intentional open-ended deployments, not zero length.
  end_limit <- end
  end_limit[is.na(end_limit)] <- Inf

  # Validate every receiver's full deployment history before assigning rows.
  # An open-ended deployment cannot precede a redeployment of that serial.
  for (idx in split(seq_along(serial), serial)) {
    idx <- idx[order(start[idx])]
    if (length(idx) < 2L) next
    for (j in seq.int(2L, length(idx))) {
      previous <- idx[j - 1L]
      current <- idx[j]
      if (start[current] < end_limit[previous]) {
        stop("Overlapping deployments for receiver ", serial[current],
             if ("dep_id" %in% names(deployments))
               paste0(" (", deployments$dep_id[previous], " and ",
                      deployments$dep_id[current], ")") else "",
             call. = FALSE)
      }
    }
  }

  n <- nrow(final_file)
  detected_serial <- trimws(as.character(final_file$ReceiverSN))
  detected_numeric <- as.numeric(detected_at)
  chosen <- rep(NA_integer_, n)
  eligible <- which(!is.na(detected_serial) & nzchar(detected_serial) &
                      is.finite(detected_numeric))
  det_groups <- split(eligible, detected_serial[eligible])
  dep_groups <- split(seq_along(serial), serial)

  for (rec in intersect(names(det_groups), names(dep_groups))) {
    rows <- det_groups[[rec]]
    for (j in dep_groups[[rec]]) {
      matches <- rows[detected_numeric[rows] >= start[j] &
                        detected_numeric[rows] < end_limit[j]]
      if (!length(matches)) next
      if (any(!is.na(chosen[matches]))) {
        stop("Multiple matching deployments for receiver ", rec,
             call. = FALSE)
      }
      chosen[matches] <- j
    }
  }

  metadata <- deployments[chosen, metadata_names, drop = FALSE]
  rownames(metadata) <- NULL
  result <- dplyr::bind_cols(dplyr::ungroup(final_file), metadata)
  result$DateTime_Local <- detected_at
  result$receiver_match_status <- ifelse(is.na(chosen), "unmatched", "matched")
  result$receiver_match_count <- as.integer(!is.na(chosen))
  if (anyNA(chosen)) {
    warning(sum(is.na(chosen)),
            " detections did not match a deployment; retained with NA ",
            "receiver metadata", call. = FALSE)
  }
  if (nrow(result) != n) {
    stop("Receiver metadata join changed detection row count", call. = FALSE)
  }
  result
}
