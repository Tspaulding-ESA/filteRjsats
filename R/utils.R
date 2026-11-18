#' Compare two detection data frames
#'
#' Report each input row missing from a filtered data frame, preserving
#' duplicate multiplicity. Prefer a stable input-row ID supplied by the
#' filter dispatcher; fall back to per-key occurrence matching.
#' @param a1 Original detections.
#' @param a2 Accepted detections.
#' @return Removed rows from a1.
#' @export
compare_detects <- function(a1, a2) {
  if (nrow(a1) == 0L) return(a1)
  if (".detection_row_id" %in% names(a1) &&
      ".detection_row_id" %in% names(a2)) {
    return(a1[!(a1$.detection_row_id %in% a2$.detection_row_id), ,
              drop = FALSE])
  }

  keys <- intersect(c("ReceiverSN", "Tag_Hex", "DateTime_Local",
                      "fish_id", "fish_rel_group", "fish_release_date"),
                    intersect(names(a1), names(a2)))
  if (!all(c("Tag_Hex", "DateTime_Local") %in% keys)) {
    stop("Cannot compare detections: required identification fields missing",
         call. = FALSE)
  }
  left <- dplyr::ungroup(a1)
  right <- dplyr::ungroup(a2)
  left$.source_row <- seq_len(nrow(left))
  left <- dplyr::group_by(left, dplyr::across(dplyr::all_of(keys)))
  left <- dplyr::mutate(left, .instance = dplyr::row_number())
  right <- dplyr::group_by(right, dplyr::across(dplyr::all_of(keys)))
  right <- dplyr::mutate(right, .instance = dplyr::row_number())
  unmatched <- dplyr::anti_join(
    dplyr::ungroup(left),
    dplyr::select(dplyr::ungroup(right), dplyr::all_of(keys), .instance),
    by = c(keys, ".instance")
  )
  a1[sort(unmatched$.source_row), , drop = FALSE]
}

#' Rolling standard deviation of three consecutive observations
#' @param x Numeric series.
#' @return Sample SD of each observation and the two following values.
#' @export
rolling_sd_3 <- function(x) {
  avg <- (x + dplyr::lead(x) + dplyr::lead(x, 2L)) / 3
  ss <- (x - avg)^2 + (dplyr::lead(x) - avg)^2 +
    (dplyr::lead(x, 2L) - avg)^2
  sqrt(ss / 2)
}

# Internal dispatcher. An input row always appears exactly once in the result.
#' @keywords internal
run_second_filter <- function(fish_file, four_hit = FALSE) {
  if (nrow(fish_file) == 0L) {
    message("No detections to evaluate")
    out <- dplyr::ungroup(fish_file)
    out$spurious <- logical(0)
    out$reject_code <- character(0)
    return(out)
  }
  fish_file <- dplyr::ungroup(fish_file)
  makes <- unique(as.character(fish_file$Make))
  if (length(makes) != 1L || is.na(makes) ||
      !(makes %in% c("ATS", "Tekno", "Lotek"))) {
    stop("Input must contain one receiver technology: ATS, Tekno, or Lotek",
         call. = FALSE)
  }
  if (!".detection_row_id" %in% names(fish_file)) {
    fish_file$.detection_row_id <- seq_len(nrow(fish_file))
  }
  if (anyNA(fish_file$.detection_row_id) ||
      anyDuplicated(fish_file$.detection_row_id)) {
    stop(".detection_row_id must be unique and non-missing", call. = FALSE)
  }

  out <- switch(
    makes,
    ATS = if (four_hit) second_filter_ats_4h(fish_file, .include_rejected = TRUE)
          else second_filter_ats(fish_file, .include_rejected = TRUE),
    Tekno = if (four_hit) second_filter_tekno_4h(fish_file, .include_rejected = TRUE)
            else second_filter_tekno(fish_file, .include_rejected = TRUE),
    Lotek = second_filter_lotek(fish_file, .include_rejected = TRUE)
  )
  out <- dplyr::arrange(out, ReceiverSN, Tag_Hex,
                        DateTime_Local, .detection_row_id)
  if (nrow(out) != nrow(fish_file) ||
      anyDuplicated(out$.detection_row_id) ||
      anyNA(out$spurious) ||
      any(out$spurious & is.na(out$reject_code)) ||
      any(!out$spurious & !is.na(out$reject_code))) {
    stop("Filtering lost rows or produced inconsistent rejection codes",
         call. = FALSE)
  }
  message("Valid tags: ", length(unique(out$Tag_Hex[!out$spurious])),
          "; valid detections: ", sum(!out$spurious),
          "; spurious detections: ", sum(out$spurious))
  out
}

#' @name Set_GVs
utils::globalVariables(c(
  'B2', 'BitPer', 'DateTime_Local', 'Freq', 'Make',
  'ReceiverSN', 'RefTag', 'SigStr', 'Tag_Decimal',
  'Tag_Hex', 'Temp', 'Thres', 'Tilt', 'Volt',
  'fish_release_date', 'lines', 'multipath',
  'rcvr_fields', 'read.csv', 'read.delim',
  'receiver_beacon_id_hex', 'receiver_retrieve',
  'receiver_serial_number', 'receiver_start',
  'setTxtProgressBar', 'tag_life',
  'tag_pulse_rate_interval_nominal', 'time_diff_lag',
  'txtProgressBar', '%>%', 'receiver_end',
  'td', 'tdiff', 'pr_nom', 'pulse_multiple',
  'freq_in_range', 'FreqDiff', 'freq_pair_ok',
  'pr_check', 'tdiff_check', 'pri_metadata_check',
  'window_pass', 'sequence_pass', 'cs', 'cs_check',
  'v1', 'v2', 'v3', 'sd_roll_check',
  'freq_window_check', 'det_count', 'max_interval',
  'pair_pass', 'CheckMBP', 'TagInFile', 'CheckDT',
  'reject_code', 'timing_support',
  'CheckBattLife', '.instance', '.source_row',
  '.sequence_order', '.detection_row_id'
))
