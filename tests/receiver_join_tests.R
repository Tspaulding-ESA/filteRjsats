library(filteRjsats)

pst <- "Etc/GMT+8"
t0 <- as.POSIXct("2026-04-01 00:00:00", tz = pst)
stopifnot(identical(
  format(as.POSIXct(c("2026-01-15 12:00:00", "2026-07-15 12:00:00"),
                    tz = pst), "%z", tz = pst),
  c("-0800", "-0800")))

make_detections <- function(x) data.frame(
  ReceiverSN = rep("1001", length(x)),
  Tag_Hex = rep("00AA", length(x)),
  DateTime_Local = t0 + x * 86400,
  spurious = seq_along(x) %% 2L == 0L,
  reject_code = ifelse(seq_along(x) %% 2L == 0L, "pri", NA_character_)
)

det <- make_detections(c(1, 5, 11))
dep <- data.frame(
  dep_id = c("a", "b"), ReceiverSN = c("1001", "1001"),
  receiver_location = c("old", "new"),
  receiver_start = t0 + c(0, 5) * 86400,
  receiver_end = t0 + c(5, 15) * 86400
)

# Half-open interval: exactly at start matches new deployment, not old.
result <- join_rcvr_data(det, dep)
stopifnot(identical(result$receiver_location, c("old", "new", "new")),
          nrow(result) == nrow(det),
          identical(result$reject_code, det$reject_code),
          identical(result$spurious, det$spurious))

# Ongoing deployment: missing end date matches indefinitely.
open <- dep
open$receiver_end[2] <- as.POSIXct(NA, tz = pst)
future <- det[1, ]
future$DateTime_Local <- as.POSIXct("2027-01-01 00:00:00", tz = pst)
stopifnot(join_rcvr_data(future, open)$receiver_location == "new")

# An ongoing deployment followed by a later deployment is invalid.
invalid <- dep
invalid$receiver_end[1] <- as.POSIXct(NA, tz = pst)
stopifnot(inherits(try(join_rcvr_data(det, invalid), silent = TRUE),
                   "try-error"))

# Any overlap is an error, including when there are no matching detections.
overlap <- dep
overlap$receiver_start[2] <- t0 + 4 * 86400
stopifnot(inherits(try(join_rcvr_data(det, overlap), silent = TRUE),
                   "try-error"))

# Unmatched rows are retained, not assigned an invented location.
missing <- det[1, ]
missing$ReceiverSN <- "UNKNOWN"
check_missing <- suppressWarnings(join_rcvr_data(missing, dep))
stopifnot(nrow(check_missing) == 1L,
          check_missing$receiver_match_status == "unmatched",
          check_missing$receiver_match_count == 0L,
          is.na(check_missing$receiver_location))

# Exact duplicate metadata rows do not multiply detections.
check_duplicate <- join_rcvr_data(det, rbind(dep, dep[1, ]))
stopifnot(nrow(check_duplicate) == nrow(det))

# A duplicate detection row (e.g., repeated fish metadata) is retained twice.
check_repeat_detection <- join_rcvr_data(rbind(det[1, ], det[1, ]), dep)
stopifnot(nrow(check_repeat_detection) == 2L)

# DST does not change receiver assignment: already-parsed America/Los_Angeles
# and UTC clocks are reinterpreted as fixed PST *without shifting wall times*.
summer <- det[1, ]
summer$DateTime_Local <- as.POSIXct("2026-07-15 12:00:00",
                                     tz = "America/Los_Angeles")
summer_dep <- dep[1, ]
summer_dep$receiver_start <- as.POSIXct("2026-07-01 00:00:00", tz = "UTC")
summer_dep$receiver_end <- as.POSIXct(NA, tz = "UTC")
summer_result <- join_rcvr_data(summer, summer_dep)
stopifnot(summer_result$receiver_match_status == "matched",
          format(summer_result$DateTime_Local, "%z") == "-0800",
          format(summer_result$DateTime_Local, "%H:%M:%S") == "12:00:00",
          format(summer_result$receiver_start, "%H:%M:%S") == "00:00:00")

# Empty detection inputs return zero rows with match-status columns.
empty <- join_rcvr_data(det[FALSE, ], dep)
stopifnot(nrow(empty) == 0L,
          "receiver_match_status" %in% names(empty),
          "receiver_match_count" %in% names(empty))
