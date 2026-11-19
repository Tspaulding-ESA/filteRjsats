# Exact equivalence tests against the unmodified master implementation,
# git blob 9f3a658bf4be14c3495e04390ba7f33099337e96.
# Standalone tests/ script, automatically executed by R CMD check.
library(filteRjsats)
reference <- if (file.exists(file.path('fixtures', 'filter_sequence_core_baseline.R'))) {
  file.path('fixtures', 'filter_sequence_core_baseline.R')
} else {
  file.path('tests', 'fixtures', 'filter_sequence_core_baseline.R')
}
stopifnot(file.exists(reference))
source(reference, local = TRUE)
optimized <- getFromNamespace('filter_detection_sequences', 'filteRjsats')

check_identical <- function(x, hits, bounds = NULL, tolerance = NULL,
                            inclusive = FALSE, short = FALSE,
                            include_rejected = TRUE) {
  args <- list(fish_file = x, hits = hits,
               frequency_bounds = bounds,
               frequency_tolerance = tolerance,
               frequency_inclusive = inclusive,
               require_short_window_edges = short,
               include_rejected = include_rejected)
  before <- do.call(filter_detection_sequences_baseline, args)
  after <- do.call(optimized, args)
  if (!identical(before, after)) {
    stop('Exact reference mismatch: ',
         paste(capture.output(all.equal(before, after, tolerance = 0)),
               collapse = '\n'), call. = FALSE)
  }
  invisible(TRUE)
}

make_cases <- function(technology, test) {
  n <- length(test)
  df <- data.frame(
    ReceiverSN = rep('R1', n), Make = rep(technology, n),
    Tag_Hex = rep('00AF', n),
    DateTime_Local = as.POSIXct('2026-05-01 12:00:00',
                               tz = 'Etc/GMT+8') + test,
    tag_pulse_rate_interval_nominal = rep(5, n),
    .detection_row_id = seq_len(n),
    stringsAsFactors = FALSE)
  if (technology != 'Lotek') df$Freq <- rep(417, n)
  df
}

for (tech in c('ATS', 'Tekno', 'Lotek')) {
  base <- make_cases(tech, c(0, 0.2, 0.4, 5.4, 10.4, 15.4,
                             20.4, 30.4, 35.4, 500, 505))
  variants <- list(
    base,
    base[rev(seq_len(nrow(base))), , drop = FALSE],
    transform(base, tag_pulse_rate_interval_nominal =
                    c(5, NA, 5, 5, 5, 10, 5, 5, 5, 5, 5)),
    rbind(base, transform(base, Tag_Hex = 'AB01',
                          ReceiverSN = 'R2', .detection_row_id =
                            .detection_row_id + nrow(base))),
    transform(base, DateTime_Local = as.POSIXct(
      '2026-05-01 12:00:00', tz = 'Etc/GMT+8') +
        c(0, 4, 9, 14, 19, 24, 29, 34, 39, 200, 205))
  )
  if (tech != 'Lotek') {
    bad <- base
    bad$Freq <- c(416, 417, 417.5, 419, 419.1, NA,
                  416.1, 416.2, 417, 418, 417)
    variants[[length(variants) + 1L]] <- bad
  }
  for (x in variants) {
    settings <- if (tech == 'ATS') list(bounds = c(416, 419),
                                        tolerance = 0.505, inclusive = TRUE)
                else if (tech == 'Tekno') list(bounds = c(390, 445),
                                               tolerance = 55)
                else list()
    for (hits in if (tech == 'Lotek') 4L else c(2L, 4L)) {
      for (include_rejected in c(TRUE, FALSE)) {
        do.call(check_identical, c(list(x = x, hits = hits,
                                        include_rejected = include_rejected,
                                        short = (hits == 4L &&
                                                   tech != 'Lotek')),
                                   settings))
      }
    }
  }
}

# Fixed-seed randomized groups exercise missing metadata, interleaved noise,
# out-of-order input, reused tags, identical timestamps and boundary cases.
set.seed(20261119)
for (tech in c('ATS', 'Tekno', 'Lotek')) {
  for (run in seq_len(15L)) {
    n <- 24L
    x <- make_cases(tech, cumsum(sample(
      c(0, 0.1, 0.2, 0.3, 2.0, 4.99, 5, 5.01, 9.98, 10, 20, 90),
      n, replace = TRUE)))
    x$ReceiverSN <- sample(c('R1', 'R2'), n, replace = TRUE)
    x$Tag_Hex <- sample(c('00AF', '00B0'), n, replace = TRUE)
    x$tag_pulse_rate_interval_nominal <- sample(
      c(5, 5, 5, 5, 10, NA, 0), n, replace = TRUE)
    if (tech != 'Lotek') x$Freq <- sample(
      c(415.99, 416, 416.2, 416.7, 417, 418.1, 419, 419.01, NA),
      n, replace = TRUE)
    x <- x[sample(seq_len(n)), , drop = FALSE]
    settings <- if (tech == 'ATS') list(bounds = c(416, 419),
                                        tolerance = 0.505, inclusive = TRUE)
                else if (tech == 'Tekno') list(bounds = c(390, 445),
                                               tolerance = 55)
                else list()
    for (hits in if (tech == 'Lotek') 4L else c(2L, 4L)) {
      for (include_rejected in c(TRUE, FALSE)) {
        do.call(check_identical, c(list(x = x, hits = hits,
                                        include_rejected = include_rejected,
                                        short = (hits == 4L &&
                                                   tech != 'Lotek')),
                                   settings))
      }
    }
  }
}

cat('PASS: Phase 1 exact reference-equivalence cases.\n')
