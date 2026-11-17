# Run in the installed / source package's test environment with:
#   R CMD check .
# Tests use public functions; no external data or network is required.
library(filteRjsats)

mk <- function(tag, seconds, make = "ATS", freq = NULL, pri = 5,
               receiver = "test") {
  x <- data.frame(
    ReceiverSN = receiver,
    Make = make,
    Tag_Hex = tag,
    DateTime_Local = as.POSIXct("2026-04-21 12:00:00", tz = "UTC") + seconds,
    tag_pulse_rate_interval_nominal = pri
  )
  if (!is.null(freq)) x$Freq <- freq
  x
}

# Pass both members of a supported pair, even if one has an invalid next hit.
a <- mk("A", c(0, 5, 10, 100, 105, 300),
        freq = c(416.2, 416.3, 417.9, 415.9, 416.1, 416.5))
b <- mk("B", c(0, 5), freq = c(416.1, 416.2))
ans <- second_filter_ats(rbind(a, b))
stopifnot(nrow(ans) == 4L)
stopifnot(all(ans$Tag_Hex %in% c("A", "B")))
stopifnot(sum(ans$Tag_Hex == "A") == 2L)
stopifnot(sum(ans$Tag_Hex == "B") == 2L)

# Missing observed frequency or missing PRI does not create a valid pair.
nafreq <- mk("C", c(0, 5), freq = c(416.5, NA_real_))
napri <- mk("D", c(0, 5), freq = c(416.5, 416.5), pri = NA_real_)
stopifnot(nrow(second_filter_ats(nafreq)) == 0L)
stopifnot(nrow(second_filter_ats(napri)) == 0L)

# First hit after a noise duplicate can still pair with a later clean hit.
pref <- rbind(mk("A", c(0, 0.1, 5), freq = c(417, 417, 417)),
              mk("B", 0, freq = 417))
pre <- prefilter(pref, reference_tags = character())
stopifnot(nrow(pre) == 2L)
stopifnot(all(pre$Tag_Hex == "A"))
stopifnot(all(pre$CheckMBP))

# Four-hit support propagates to all members of a COMPLETE qualifying window.
ats4 <- rbind(mk("A", c(0, 5, 10, 15), freq = rep(416.5, 4)),
              mk("B", c(0, 5, 10), freq = rep(416.5, 3)))
ats4ans <- second_filter_ats_4h(ats4)
stopifnot(nrow(ats4ans) == 4L, all(ats4ans$Tag_Hex == "A"))

lot <- rbind(mk("A", c(0, 5, 10, 15, 100), make = "Lotek"),
             mk("B", c(0, 5, 10), make = "Lotek"))
lotans <- second_filter_lotek(lot)
stopifnot(nrow(lotans) == 4L, all(lotans$Tag_Hex == "A"))
stopifnot(all(lotans$sequence_pass))

tek <- mk("A", c(0, 5), make = "Tekno", freq = c(416, 417))
stopifnot(nrow(second_filter_tekno(tek)) == 2L)
stopifnot(nrow(second_filter_tekno_4h(tek)) == 0L)

# Whole-file dispatcher retains every input row with exactly one classification.
classified <- second_filter_2h4h(rbind(a, b))
stopifnot(nrow(classified) == nrow(a) + nrow(b))
stopifnot(sum(!classified$spurious) == 4L)
stopifnot(!anyDuplicated(classified$.detection_row_id))

# Duplicate multiplicity is not lost when comparing before/after tables.
two <- mk(c("A", "A"), c(0, 0), freq = c(417, 417))
stopifnot(nrow(compare_detects(two, two[1, , drop = FALSE])) == 1L)

# A still-active fish released >200 days before a detection is eligible.
fish <- data.frame(tag_id_hex = "ABCD",
                   fish_release_date = as.POSIXct("2025-01-01", tz = "UTC"),
                   tag_life = 150)
observation <- mk("ABCD", 0, freq = 417)
observation$DateTime_Local <- as.POSIXct("2025-09-08", tz = "UTC")
stopifnot(nrow(add_fish(observation, fish)) == 1L)

# A false hit interposed between genuine hits must not invalidate the pair.
interleaved2 <- mk("X", c(0, 2, 5),
                   freq = c(416.5, 418.0, 416.5))
i2 <- second_filter_ats(interleaved2)
stopifnot(nrow(i2) == 2L,
          identical(as.numeric(diff(i2$DateTime_Local)), 5),
          !any(i2$DateTime_Local == interleaved2$DateTime_Local[2]))

# Interleaved false hit must not break a qualifying four-hit sequence.
interleaved4 <- mk("Y", c(0, 2, 5, 10, 15),
                   freq = c(416.5, 418.0, 416.5, 416.5, 416.5))
i4 <- second_filter_ats_4h(interleaved4)
stopifnot(nrow(i4) == 4L,
          !any(i4$DateTime_Local == interleaved4$DateTime_Local[2]))

# Both inclusive ATS frequency boundaries are accepted when fully supported.
stopifnot(nrow(second_filter_ats(mk("Z1", c(0, 5),
                                  freq = c(416, 416)))) == 2L)
stopifnot(nrow(second_filter_ats(mk("Z2", c(0, 5),
                                  freq = c(419, 419)))) == 2L)
stopifnot(nrow(second_filter_ats(mk("Z3", c(0, 5),
                                  freq = c(415.99, 415.99)))) == 0L)

# A matching tag detected on two different receivers is NOT a pair.
xr <- rbind(mk("MATCH", 0, receiver = "R1", freq = 417),
            mk("MATCH", 5, receiver = "R2", freq = 417))
stopifnot(nrow(second_filter_ats(xr)) == 0L)

# Incomplete evidence is rejected even when the last available interval looks good.
stopifnot(nrow(second_filter_ats_4h(
  mk("SHORT", c(0, 5, 10), freq = rep(417, 3)))) == 0L)

# Missing timestamps and unsupported tags do not create valid sequences.
bad_time <- mk("BAD", c(0, 5), freq = c(417, 417))
bad_time$DateTime_Local[2] <- as.POSIXct(NA)
stopifnot(nrow(second_filter_ats(bad_time)) == 0L)

# Prefilter must require a partner on the SAME receiver (and tag).
stopifnot(nrow(prefilter(xr, character())) == 0L)

# Empty inputs return a correctly shaped empty data frame from the dispatcher.
empty <- second_filter_2h4h(a[FALSE, , drop = FALSE])
stopifnot(nrow(empty) == 0L, "spurious" %in% names(empty))

# Four-hit Tekno requires a real, complete window and every frequency.
tek4 <- mk("TEK", c(0, 5, 10, 15), make = "Tekno",
           freq = rep(417, 4))
stopifnot(nrow(second_filter_tekno_4h(tek4)) == 4L)
tek4$Freq[3] <- NA_real_
stopifnot(nrow(second_filter_tekno_4h(tek4)) == 0L)

# Four-hit Lotek missing PRI cannot be taken as a pass.
lot4 <- mk("LOT", c(0, 5, 10, 15), make = "Lotek")
lot4$tag_pulse_rate_interval_nominal[3] <- NA_real_
stopifnot(nrow(second_filter_lotek(lot4)) == 0L)

# Duplicate tag metadata are not silently resolved by add_fish().
fish_dupe <- data.frame(
  tag_id_hex = c("7EF2", "7EF2"),
  fish_release_date = as.POSIXct(c("2026-04-08", "2026-04-21"), tz = "UTC"),
  tag_life = c(100, 100), fish_rel_group = c("T1", "T2")
)
obs_dupe <- mk("7EF2", 0, freq = 417)
obs_dupe$DateTime_Local <- as.POSIXct("2026-05-01", tz = "UTC")
stopifnot(nrow(add_fish(obs_dupe, fish_dupe)) == 2L)
