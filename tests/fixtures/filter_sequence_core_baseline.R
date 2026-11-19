# Internal, technology-neutral JSATS sequence validator.
# A detection passes only if it participates in at least one COMPLETE,
# qualifying two- or four-hit sequence on the same receiver and tag.
# Intervening nonqualifying hits do not break a genuinely qualifying sequence.
# When include_rejected = TRUE, assign a single primary reject_code per row:
# multipath > invalid individual frequency > frequency-inconsistent supported
# timing sequence > missing/failed PRI or incomplete sequence evidence.
#' @keywords internal
filter_detection_sequences_baseline <- function(fish_file, hits,
                                       frequency_bounds = NULL,
                                       frequency_tolerance = NULL,
                                       frequency_inclusive = FALSE,
                                       require_short_window_edges = FALSE,
                                       include_rejected = FALSE) {
  if (!hits %in% c(2L, 4L)) {
    stop("hits must be 2 or 4", call. = FALSE)
  }
  required <- c("ReceiverSN", "Tag_Hex", "DateTime_Local",
                "tag_pulse_rate_interval_nominal")
  if (!is.null(frequency_bounds)) required <- c(required, "Freq")
  missing <- setdiff(required, names(fish_file))
  if (length(missing)) {
    stop("Missing required detection columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (!inherits(fish_file$DateTime_Local, "POSIXt")) {
    stop("DateTime_Local must be POSIXct/POSIXlt (not character)", call. = FALSE)
  }
  if (!is.null(frequency_bounds) &&
      (length(frequency_bounds) != 2L || is.null(frequency_tolerance))) {
    stop("Frequency bounds and tolerance must both be supplied", call. = FALSE)
  }
  if (nrow(fish_file) == 0L) {
    if (!include_rejected) return(fish_file)
    out <- dplyr::ungroup(fish_file)
    out$spurious <- logical(0)
    out$reject_code <- character(0)
    return(out)
  }

  out <- dplyr::ungroup(fish_file)
  out$pr_nom <- suppressWarnings(as.numeric(out$tag_pulse_rate_interval_nominal))
  if (!is.null(frequency_bounds)) {
    out$Freq <- suppressWarnings(as.numeric(out$Freq))
  }
  out$.sequence_order <- seq_len(nrow(out))
  out <- dplyr::arrange(out, ReceiverSN, Tag_Hex, DateTime_Local, .sequence_order)
  n <- nrow(out)
  out$td <- rep(NA_real_, n)
  out$tdiff <- rep(NA_real_, n)
  out$multipath <- rep(FALSE, n)
  out$window_pass <- rep(FALSE, n)
  out$sequence_pass <- rep(FALSE, n)
  # A complete, PRI-valid sequence, even if its frequency checks fail.
  # This is diagnostic only; it cannot cause a detection to be accepted.
  out$timing_support <- rep(FALSE, n)
  if (!is.null(frequency_bounds)) {
    out$FreqDiff <- rep(NA_real_, n)
  }

  # Use observed, ordered detections. Group boundaries include receiver AND tag.
  # Only one observation of a <=0.3-s multipath cluster can contribute to
  # sequence evidence; the remaining copies stay spurious in the dispatcher.
  rows <- which(!is.na(out$ReceiverSN) & as.character(out$ReceiverSN) != "" &
                  !is.na(out$Tag_Hex) & as.character(out$Tag_Hex) != "" &
                  !is.na(out$DateTime_Local))
  if (length(rows)) {
    groups <- split(rows, interaction(out$ReceiverSN[rows], out$Tag_Hex[rows],
                                      drop = TRUE, lex.order = TRUE))
    for (idx in groups) {
      times_all <- as.numeric(out$DateTime_Local[idx])
      # De-duplicate against the last retained observation, not the last raw
      # observation (which could itself be multipath).
      retained <- rep(FALSE, length(idx))
      last_time <- -Inf
      for (q in seq_along(idx)) {
        if (times_all[q] - last_time <= 0.3) {
          out$multipath[idx[q]] <- TRUE
        } else {
          retained[q] <- TRUE
          last_time <- times_all[q]
        }
      }
      idx <- idx[retained]
      m <- length(idx)
      if (m == 0L) next
      times <- as.numeric(out$DateTime_Local[idx])
      pri <- out$pr_nom[idx]
      delta <- c(NA_real_, diff(times))
      out$td[idx] <- delta
      multiple <- round(delta / pri)
      good_multiple <- is.finite(pri) & pri > 0 &
        is.finite(multiple) & multiple >= 1
      normalized <- rep(NA_real_, m)
      normalized[good_multiple] <-
        delta[good_multiple] / multiple[good_multiple]
      out$tdiff[idx] <- normalized
      if (!is.null(frequency_bounds)) {
        freq <- out$Freq[idx]
        out$FreqDiff[idx] <- c(abs(diff(freq)), NA_real_)
        in_range <- if (frequency_inclusive) {
          freq >= frequency_bounds[1] & freq <= frequency_bounds[2]
        } else {
          freq > frequency_bounds[1] & freq < frequency_bounds[2]
        }
        freq_ok <- is.finite(freq) & in_range
      } else {
        freq <- rep(NA_real_, m)
        freq_ok <- rep(TRUE, m)
      }
      if (m < hits) next

      # The same interval/PRI and frequency equations apply to both technologies
      # and both hit-count modes; only tolerances and window sizes differ.
      # Separate timing and frequency evidence so rejection codes describe
      # why an otherwise qualifying temporal sequence was not accepted.
      edge_timing_ok <- function(a, b, nominal, tolerance) {
        gap <- times[b] - times[a]
        if (!is.finite(gap) || gap <= 0.3 ||
            !is.finite(pri[b]) || pri[b] != nominal) return(FALSE)
        pulse_count <- round(gap / nominal)
        if (!is.finite(pulse_count) || pulse_count < 1 ||
            abs(gap / pulse_count - nominal) >= nominal * tolerance) {
          return(FALSE)
        }
        TRUE
      }
      edge_freq_ok <- function(a, b) {
        if (!freq_ok[a] || !freq_ok[b]) return(FALSE)
        if (!is.null(frequency_bounds) &&
            !(abs(freq[b] - freq[a]) < frequency_tolerance)) return(FALSE)
        TRUE
      }

      for (i in seq_len(m - hits + 1L)) {
        nominal <- pri[i]
        if (!is.finite(nominal) || nominal <= 0) next
        if (hits == 2L) {
          for (j in seq.int(i + 1L, m)) {
            gap <- times[j] - times[i]
            if (gap >= 4 * nominal) break
            if (gap <= 0.8 * nominal ||
                !edge_timing_ok(i, j, nominal, 0.10)) next
            pair <- idx[c(i, j)]
            out$timing_support[pair] <- TRUE
            if (!edge_freq_ok(i, j)) next
            out$window_pass[idx[i]] <- TRUE
            out$sequence_pass[pair] <- TRUE
          }
        } else {
          for (j in seq.int(i + 1L, m - 2L)) {
            gap1 <- times[j] - times[i]
            if (gap1 >= 16.6 * nominal) break
            if (require_short_window_edges && gap1 >= 4 * nominal) break
            if (!edge_timing_ok(i, j, nominal, 0.20)) next
            for (k in seq.int(j + 1L, m - 1L)) {
              if (times[k] - times[i] >= 16.6 * nominal) break
              if (!edge_timing_ok(j, k, nominal, 0.20)) next
              for (l in seq.int(k + 1L, m)) {
                if (times[l] - times[i] >= 16.6 * nominal) break
                gap3 <- times[l] - times[k]
                if (require_short_window_edges && gap3 >= 4 * nominal) break
                if (!edge_timing_ok(k, l, nominal, 0.20)) next
                intervals <- c(gap1, times[k] - times[j], gap3)
                normalized <- intervals / round(intervals / nominal)
                if (!all(is.finite(normalized)) ||
                    !(stats::sd(normalized) < 0.025)) next
                window <- idx[c(i, j, k, l)]
                out$timing_support[window] <- TRUE
                if (!edge_freq_ok(i, j) || !edge_freq_ok(j, k) ||
                    !edge_freq_ok(k, l)) next
                out$window_pass[idx[i]] <- TRUE
                out$sequence_pass[window] <- TRUE
              }
            }
          }
        }
      }
    }
  }

  # Compute accepted detection counts without changing the accepted set.
  accepted <- out[out$sequence_pass, , drop = FALSE]
  accepted <- dplyr::group_by(accepted, ReceiverSN, Tag_Hex)
  accepted <- dplyr::mutate(accepted, det_count = dplyr::n())
  accepted <- dplyr::ungroup(accepted)
  if (!include_rejected) {
    return(dplyr::select(accepted, -.sequence_order, -timing_support))
  }

  # Always return ALL original rows, with one primary rejection reason.
  # "pri" includes missing PRI, invalid intervals, excessive PRI variation,
  # and fewer than the required 2 or 4 qualifying hits.
  out$det_count <- rep(NA_integer_, n)
  out$det_count[out$sequence_pass] <- accepted$det_count
  out$spurious <- !out$sequence_pass
  out$reject_code <- rep(NA_character_, n)
  out$reject_code[out$spurious] <- "pri"

  if (!is.null(frequency_bounds)) {
    freq <- out$Freq
    bad_freq <- !is.finite(freq)
    if (frequency_inclusive) {
      bad_freq <- bad_freq | freq < frequency_bounds[1] |
        freq > frequency_bounds[2]
    } else {
      bad_freq <- bad_freq | freq <= frequency_bounds[1] |
        freq >= frequency_bounds[2]
    }
    bad_freq[is.na(bad_freq)] <- TRUE
    out$reject_code[out$spurious &
                      (out$timing_support | bad_freq)] <- "freq"
  }
  out$reject_code[out$multipath] <- "multipath"
  dplyr::select(out, -.sequence_order, -timing_support)
}
