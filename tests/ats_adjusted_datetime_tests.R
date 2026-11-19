# Standalone regression tests; executed during R CMD check.
library(filteRjsats)

folder <- tempfile("ats_reader_")
dir.create(folder)

header <- paste(
  "", "SiteName", "", "", "DateTime", "TagCode", "Tilt", "VBatt",
  "Temp", "Pressure", "SigStr", "Bit Period", "Threshold",
  sep = ","
)
record <- function(original, adjusted = NULL, code = "G72C6483A", extra = NULL) {
  paste(c("0 00 241C 804D 9525 23 A1", "ATS", "12", "21",
          original, code, "41", "04.15", "26.03", "N/A",
          "137", "239 28/31", "110", extra, adjusted), collapse = ",")
}

# Legacy format: 13 comma-separated boundaries including the trailing comma.
legacy_file <- "SR21027_legacy.csv"
writeLines(c("Site Name: ATS", "Serial Number: 21027", header,
             record("06/20/2021 00:00:02.606865", extra = "")),
           file.path(folder, legacy_file))
legacy <- read_ats(folder, legacy_file)
stopifnot(nrow(legacy) == 1L,
          legacy$Tag_Hex == "6483",
          legacy$Make == "ATS",
          abs(legacy$Freq - 100000 / (239 + 28 / 31)) < 1e-6,
          format(legacy$DateTime_Local, "%H:%M:%OS6") == "00:00:02.606865")

# Additional clock-drift fields: the corrected timestamp overrides DateTime.
corrected_header <- paste(header, "Clock_Offset", "Adjusted_Date_Time", sep = ",")
adjusted_file <- "SR21030_adjusted.csv"
writeLines(c(
  "Site Name: ATS", "Serial Number: 21030", corrected_header,
  record("06/20/2021 00:00:02.606865",
         "06/20/2021 00:00:09.606865", extra = "7"),
  # Repeated ATS headers must not alter the column mapping.
  "File Resume: 06/20/2021 01:00:00", corrected_header,
  record("06/20/2021 01:00:02.000000",
         "2021-06-20 01:00:09.500000", extra = "7")
), file.path(folder, adjusted_file))
adjusted <- read_ats(folder, adjusted_file)
stopifnot(nrow(adjusted) == 2L,
          identical(adjusted$Tag_Hex, c("6483", "6483")),
          format(adjusted$DateTime_Local[1L], "%H:%M:%OS6") == "00:00:09.606865",
          format(adjusted$DateTime_Local[2L], "%H:%M:%OS6") == "01:00:09.500000",
          all(abs(adjusted$Freq - 100000 / (239 + 28 / 31)) < 1e-6))

# If Adjusted_Date_Time is missing, DateTime is NOT an allowed fallback.
missing_file <- "SR21031_adjusted_missing.csv"
writeLines(c("Site Name: ATS", corrected_header,
             record("06/20/2021 01:00:02.123456", "", extra = "7"),
             record("06/20/2021 01:00:07.123456",
                    "06/20/2021 01:00:09.123456", extra = "7")),
           file.path(folder, missing_file))
suppressWarnings(missing_out <- read_ats(folder, missing_file))
stopifnot(nrow(missing_out) == 2L,
          is.na(missing_out$DateTime_Local[1L]),
          format(missing_out$DateTime_Local[2L], "%H:%M:%OS6") ==
            "01:00:09.123456")

# Fixed PST (UTC-8) year-round; daylight-saving offsets must not appear.
stopifnot(format(adjusted$DateTime_Local[1L], "%z") == "-0800")

# read_jsats uses the same corrected ATS logic and fixed-PST default.
wrapped <- read_jsats(folder, adjusted_file)
stopifnot(identical(as.numeric(wrapped$DateTime_Local),
                    as.numeric(adjusted$DateTime_Local)))

# A header in the first row must also be detected (not consumed as the CSV header).
first_header_file <- "SR21032_first_header.csv"
writeLines(c(corrected_header,
             record("06/20/2021 04:00:00.000000",
                    "06/20/2021 04:01:30.000000", extra = "90")),
           file.path(folder, first_header_file))
first_header <- read_ats(folder, first_header_file)
stopifnot(nrow(first_header) == 1L,
          format(first_header$DateTime_Local, "%H:%M:%S") == "04:01:30")

# Extra fields may also appear BETWEEN the original ATS fields.
reordered_file <- "SR21033_reordered.csv"
header_mid <- paste(c("", "SiteName", "", "", "DateTime",
                      "Adjusted_Date_Time", "TagCode", "Tilt", "VBatt",
                      "Temp", "Pressure", "SigStr", "Bit Period",
                      "Threshold", "Other_Field"), collapse = ",")
row_mid <- paste(c("0 00 241C 804D 9525 23 A1", "ATS", "12", "21",
                   "06/20/2021 00:00:01.000000", "06/20/2021 00:00:10.000000",
                   "G72C6483A", "41", "04.15", "26.03", "N/A",
                   "137", "239 28/31", "110", "other"), collapse = ",")
writeLines(c(header_mid, row_mid), file.path(folder, reordered_file))
reordered <- read_ats(folder, reordered_file)
stopifnot(nrow(reordered) == 1L,
          reordered$Tag_Hex == "6483",
          format(reordered$DateTime_Local, "%H:%M:%S") == "00:00:10")

# Quoted CSV headers and values retain correct column matching.
quoted_file <- "SR21034_quoted.csv"
quoted_header <- paste(sprintf('"%s"',
                       c("", "SiteName", "", "", "DateTime", "TagCode",
                         "Tilt", "VBatt", "Temp", "Pressure", "SigStr",
                         "Bit Period", "Threshold", "Adjusted_Date_Time")),
                       collapse = ",")
quoted_row <- paste(sprintf('"%s"',
                    c("0 00 241C 804D 9525 23 A1", "ATS", "12", "21",
                      "06/20/2021 03:00:00.000000", "G72C6483A",
                      "41", "04.15", "26.03", "N/A", "137",
                      "239 28/31", "110", "06/20/2021 03:00:30.000000")),
                    collapse = ",")
writeLines(c(quoted_header, quoted_row), file.path(folder, quoted_file))
quoted <- read_ats(folder, quoted_file)
stopifnot(nrow(quoted) == 1L,
          quoted$Tag_Hex == "6483",
          format(quoted$DateTime_Local, "%H:%M:%S") == "03:00:30")

# Real ATS restart layout: successive file headers, File End / Log Resume,
# embedded NUL padding, clock change messages, and a trailing empty header.
# The NUL block must NOT cause loss of detections after the restart.
restart_file <- "SR17079_restart_fixture.csv"
con <- base::file(file.path(folder, restart_file), open = "wb")
writeBin(charToRaw(paste0(
  "Site Name: ATS,11,16\r\n",
  "Serial Number: 17079\r\n",
  "File Start: 04/02/2026 13:37:28\r\n",
  header, "\r\n",
  "0 0D 0002 F005K0AE9 13 70,ATS,11,16, 04/02/2026 13:38:18, Old Clock, 08, 04.75, 13.44, N/A, -99, 000 00/31, 000,\r\n",
  "File End: 04/02/2026 13:38:18\r\n")), con)
writeBin(as.raw(rep(0L, 2048L)), con)
writeBin(charToRaw(paste0(
  "\r\nSerial Number: 17079\r\n",
  "Log Resume: 04/02/2026 13:40:00\r\n",
  header, "\r\n",
  record("04/02/2026 13:40:07.915972", code = "G720088EE", extra = ""), "\r\n",
  record("04/02/2026 13:40:12.702799", code = "G720088EE", extra = ""), "\r\n",
  "File End: 04/02/2026 13:40:13\r\n")), con)
writeBin(as.raw(rep(0L, 1024L)), con)
writeBin(charToRaw(paste0(
  "\r\nSerial Number: 17079\r\n",
  "Log Resume: 04/02/2026 13:41:00\r\n",
  header, "\r\n",
  record("04/02/2026 13:41:05.513383", code = "G720088EE", extra = ""), "\r\n",
  record("04/02/2026 13:41:10.300779", code = "G720088EE", extra = ""), "\r\n",
  "0 0E 1234 5678,ATS,11,16, 04/02/2026 13:41:15, G72ffffff, 08, 04.75, 13.44, N/A, -99, 000 00/31, 000,\r\n",
  "File End: 04/02/2026 13:41:20\r\n",
  "Serial Number: 17079\r\n",
  "Log Resume: 04/02/2026 13:42:00\r\n",
  header, "\r\n",
  "File End: 04/02/2026 13:42:00\r\n")), con)
close(con)
restarted <- read_ats(folder, restart_file)
stopifnot(nrow(restarted) == 4L,
          all(restarted$ReceiverSN == "17079"),
          identical(restarted$Tag_Hex, rep("0088", 4L)),
          identical(format(restarted$DateTime_Local, "%H:%M:%OS6"),
                    c("13:40:07.915972", "13:40:12.702799",
                      "13:41:05.513383", "13:41:10.300779")),
          all(is.finite(restarted$Freq)),
          !anyDuplicated(restarted$DateTime_Local))

# An adjusted timestamp is used after each restart even if the position of
# Adjusted_Date_Time changes between headers.
restart_corrected <- "SR21035_restart_adjusted.csv"
writeLines(c(
  corrected_header,
  record("06/20/2021 00:00:02.000000", "06/20/2021 00:00:09.000000", extra = "7"),
  "File End: 06/20/2021 00:00:11",
  "Log Resume: 06/20/2021 01:00:00",
  header_mid,
  row_mid
), file.path(folder, restart_corrected))
corrected_restart <- read_ats(folder, restart_corrected)
stopifnot(nrow(corrected_restart) == 2L,
          identical(format(corrected_restart$DateTime_Local, "%H:%M:%S"),
                    c("00:00:09", "00:00:10")))

# A partly corrected file must fail explicitly, rather than silently mixing
# drift-corrected and uncorrected clocks across logging sessions.
mixed_file <- "SR21036_mixed_clock_headers.csv"
writeLines(c(corrected_header,
             record("06/20/2021 00:00:02.000000", "06/20/2021 00:00:09.000000", extra = "7"),
             "Log Resume: 06/20/2021 01:00:00", header,
             record("06/20/2021 01:00:02.000000", extra = "")),
           file.path(folder, mixed_file))
stopifnot(inherits(try(read_ats(folder, mixed_file), silent = TRUE),
                   "try-error"))

unlink(folder, recursive = TRUE)
