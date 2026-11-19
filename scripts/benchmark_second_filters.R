# Benchmark baseline vs installed optimized implementation on your receiver data.
# From repo root, run devtools::load_all() first, then source this script.
# Baseline is frozen at GitHub master blob 9f3a658bf4be14c3495e04390ba7f33099337e96.
# Usage:
#   source('scripts/benchmark_second_filters.R')
#   compare_file('path/to/receiver_fish_added.csv', hits = 2L)
#   compare_file('path/to/lotek_fish_added.csv', hits = 4L)

reference_path <- file.path('tests', 'fixtures',
                           'filter_sequence_core_baseline.R')
if (!file.exists(reference_path)) {
  stop('Run from the package repository root. Baseline test fixture missing.')
}
source(reference_path, local = TRUE)

compare_file <- function(path, hits = 2L, max_rows = 3000L,
                         repeats = 2L) {
  x <- utils::read.csv(path,
                       colClasses = c(Tag_Hex = 'character',
                                      ReceiverSN = 'character'))
  x$DateTime_Local <- as.POSIXct(x$DateTime_Local,
                                  format = '%Y-%m-%d %H:%M:%OS',
                                  tz = 'Etc/GMT+8')
  if (nrow(x) == 0L) stop('No data found')
  # For a meaningful comparison, retain a substantial continuous interval
  # of the largest receiver/tag group without artificial reshuffling.
  count <- sort(table(interaction(x$ReceiverSN, x$Tag_Hex,
                                  drop = TRUE)), decreasing = TRUE)
  key <- names(count)[1L]
  group <- interaction(x$ReceiverSN, x$Tag_Hex, drop = TRUE)
  x <- x[which(group == key), , drop = FALSE]
  x <- x[order(x$DateTime_Local), , drop = FALSE]
  x <- utils::head(x, max_rows)
  x$.detection_row_id <- seq_len(nrow(x))
  make <- unique(as.character(x$Make))
  if (length(make) != 1L) stop('Expected one technology')
  if (make == 'Lotek') hits <- 4L
  bounds <- if (make == 'ATS') c(416, 419) else
              if (make == 'Tekno') c(390, 445) else NULL
  args <- list(fish_file = x, hits = hits,
               frequency_bounds = bounds,
               frequency_tolerance = if (is.null(bounds)) NULL else
                 if (make == 'ATS') 0.505 else 55,
               frequency_inclusive = identical(make, 'ATS'),
               require_short_window_edges = (hits == 4L && make != 'Lotek'),
               include_rejected = TRUE)
  current <- getFromNamespace('filter_detection_sequences', 'filteRjsats')
  baseline <- function() do.call(filter_detection_sequences_baseline, args)
  optimized <- function() do.call(current, args)
  # Always compare outputs BEFORE displaying speed measurements.
  expected <- baseline()
  observed <- optimized()
  if (!identical(expected, observed)) {
    stop('FAILED: detector-level classifications or diagnostic fields changed')
  }
  times <- lapply(seq_len(repeats), function(r) {
    t_old <- system.time(baseline())[['elapsed']]
    t_new <- system.time(optimized())[['elapsed']]
    data.frame(run = r, baseline_seconds = t_old,
               optimized_seconds = t_new)
  })
  out <- do.call(rbind, times)
  out$receiver <- as.character(x$ReceiverSN[1L])
  out$tag <- as.character(x$Tag_Hex[1L])
  out$make <- make
  out$hits <- hits
  out$rows <- nrow(x)
  out$ratio_baseline_over_optimized <- out$baseline_seconds /
    pmax(out$optimized_seconds, .Machine$double.eps)
  print(out)
  invisible(out)
}
