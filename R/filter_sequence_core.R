# Internal, technology-neutral JSATS sequence validator.
# A detection passes only if it participates in at least one COMPLETE,
# qualifying two- or four-hit sequence on the same receiver and tag.
# Intervening nonqualifying hits do not break a genuinely qualifying sequence.
# When include_rejected = TRUE, assign a single primary reject_code per row:
# multipath > invalid individual frequency > frequency-inconsistent supported
# timing sequence > missing/failed PRI or incomplete sequence evidence.
#' @keywords internal
filter_detection_sequences <- function(fish_file, hits,
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
  # Mutable standalone vectors: repeated out$column[index] assignments inside
  # the inner loops can repeatedly duplicate a large data frame. Compute the
  # same flags separately, then attach them in their original column order.
  td <- rep(NA_real_, n)
  tdiff <- rep(NA_real_, n)
  multipath <- rep(FALSE, n)
  window_pass <- rep(FALSE, n)
  sequence_pass <- rep(FALSE, n)
  # Complete PRI-valid sequence evidence, regardless of frequency.
  timing_support <- rep(FALSE, n)
  if (!is.null(frequency_bounds)) {
    FreqDiff <- rep(NA_real_, n)
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
          multipath[idx[q]] <- TRUE
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
      td[idx] <- delta
      multiple <- round(delta / pri)
      good_multiple <- is.finite(pri) & pri > 0 &
        is.finite(multiple) & multiple >= 1
      normalized <- rep(NA_real_, m)
      normalized[good_multiple] <-
        delta[good_multiple] / multiple[good_multiple]
      tdiff[idx] <- normalized
      if (!is.null(frequency_bounds)) {
        freq <- out$Freq[idx]
        FreqDiff[idx] <- c(abs(diff(freq)), NA_real_)
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

      if (hits == 4L) {
        # Build each timing-valid connection once. Edges are grouped by
        # source index, and remain ordered by detection time. A valid path
        # i -> j -> k -> l necessarily has a single nominal PRI because
        # edge_timing_ok requires pri[destination] == pri[source].
        edge_to <- vector('list', m)
        edge_normalized <- vector('list', m)
        edge_frequency <- vector('list', m)
        # +1 index guards rounding at an absolute POSIXct epoch boundary;
        # exact differences below always enforce strict < 16.6 * PRI.
        nominal_valid <- is.finite(pri) & pri > 0
        upper <- times + 16.6 * ifelse(nominal_valid, pri, 0)
        max_neighbor <- findInterval(upper, times)
        for (a in seq_len(m - 1L)) {
          if (!nominal_valid[a]) next
          last <- min(m, max_neighbor[a] + 1L)
          if (last <= a) next
          b <- seq.int(a + 1L, last)
          gap <- times[b] - times[a]
          steps <- round(gap / pri[a])
          good <- is.finite(gap) & gap > 0.3 &
            gap < 16.6 * pri[a] & is.finite(pri[b]) &
            pri[b] == pri[a] & is.finite(steps) & steps >= 1 &
            abs(gap / steps - pri[a]) < pri[a] * 0.20
          good[is.na(good)] <- FALSE
          if (!any(good)) next
          b <- b[good]
          edge_to[[a]] <- b
          edge_normalized[[a]] <- gap[good] / steps[good]
          if (is.null(frequency_bounds)) {
            edge_frequency[[a]] <- rep(TRUE, length(b))
          } else {
            edge_frequency[[a]] <- freq_ok[a] & freq_ok[b] &
              abs(freq[b] - freq[a]) < frequency_tolerance
            edge_frequency[[a]][is.na(edge_frequency[[a]])] <- FALSE
          }
        }
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
            timing_support[pair] <- TRUE
            if (!edge_freq_ok(i, j)) next
            window_pass[idx[i]] <- TRUE
            sequence_pass[pair] <- TRUE
          }
        } else {
          # Phase 2 experiment: cache timing-valid candidate edges once for
          # every source hit; enumerate 4-hit paths via these edges, rather
          # than scanning/retesting all raw j/k/l candidates for each i.
          # Edge values are already constrained to the same nominal PRI.
          j_pos <- edge_to[[i]]
          if (!length(j_pos)) next
          keep_j <- j_pos <= m - 2L
          if (require_short_window_edges) {
            keep_j <- keep_j & (times[j_pos] - times[i] < 4 * nominal)
          }
          if (!any(keep_j)) next
          j_vals <- j_pos[keep_j]
          norm_j <- edge_normalized[[i]][keep_j]
          freq_j <- edge_frequency[[i]][keep_j]
          for (jj in seq_along(j_vals)) {
            j <- j_vals[jj]
            k_pos <- edge_to[[j]]
            if (!length(k_pos)) next
            keep_k <- k_pos <= m - 1L &
              (times[k_pos] - times[i] < 16.6 * nominal)
            if (!any(keep_k)) next
            k_vals <- k_pos[keep_k]
            norm_k <- edge_normalized[[j]][keep_k]
            freq_k <- edge_frequency[[j]][keep_k]
            for (kk in seq_along(k_vals)) {
              k <- k_vals[kk]
              l_pos <- edge_to[[k]]
              if (!length(l_pos)) next
              keep_l <- times[l_pos] - times[i] < 16.6 * nominal
              if (require_short_window_edges) {
                keep_l <- keep_l &
                  (times[l_pos] - times[k] < 4 * nominal)
              }
              if (!any(keep_l)) next
              l_vals <- l_pos[keep_l]
              norm_l <- edge_normalized[[k]][keep_l]
              freq_l <- edge_frequency[[k]][keep_l]
              # Vectorized 3-observation sample SD. Use stats::sd for
              # candidates near the exact decision boundary, preserving
              # the reference implementation's strict < 0.025 threshold.
              x <- norm_j[jj]
              y <- norm_k[kk]
              sd_vals <- sqrt(((x - y)^2 + (x - norm_l)^2 +
                               (y - norm_l)^2) / 6)
              borderline <- which(is.finite(sd_vals) &
                abs(sd_vals - 0.025) < 1e-9)
              if (length(borderline)) {
                sd_vals[borderline] <- vapply(borderline, function(z) {
                  stats::sd(c(x, y, norm_l[z]))
                }, numeric(1))
              }
              good_l <- which(is.finite(sd_vals) & sd_vals < 0.025)
              if (!length(good_l)) next
              l_ok <- l_vals[good_l]
              timing_support[idx[c(i, j, k, l_ok)]] <- TRUE
              # Frequency must be valid on all three edges, but cannot
              # alter whether a complete timing-valid window exists.
              if (!freq_j[jj] || !freq_k[kk]) next
              pass_l <- l_ok[freq_l[good_l]]
              if (!length(pass_l)) next
              window_pass[idx[i]] <- TRUE
              sequence_pass[idx[c(i, j, k, pass_l)]] <- TRUE
            }
          }
        }
      }
    }
  }

  # Restore the original output schema and column positions; no acceptance
  # or rejection criterion has been altered by this optimization.
  out$td <- td
  out$tdiff <- tdiff
  out$multipath <- multipath
  out$window_pass <- window_pass
  out$sequence_pass <- sequence_pass
  out$timing_support <- timing_support
  if (!is.null(frequency_bounds)) {
    out$FreqDiff <- FreqDiff
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
