library(tibble)

# Parse one FIDE Standard rating list (fixed-width text file).
# Detects whether the file has an "FOA" column (2019+ format) or not
# (2015 format) from the header line, then extracts columns at fixed
# character offsets that shift by 4 depending on the FOA column's presence.
parse_snapshot <- function(path, snapshot_label) {
  lines  <- readLines(path, encoding = "UTF-8", warn = FALSE)
  header <- lines[1]
  rows   <- lines[-1]

  has_foa    <- grepl("FOA", header)
  rtg_start  <- if (has_foa) 114 else 110
  gms_start  <- if (has_foa) 120 else 116
  k_start    <- if (has_foa) 124 else 120
  bday_start <- if (has_foa) 127 else 123
  flag_start <- if (has_foa) 133 else 129

  tibble(
    id       = trimws(substr(rows, 1,  15)),
    fed      = trimws(substr(rows, 77, 79)),
    sex      = trimws(substr(rows, 81, 81)),
    title    = trimws(substr(rows, 85, 88)),
    rating   = as.integer(trimws(substr(rows, rtg_start,  rtg_start + 4))),
    games    = as.integer(trimws(substr(rows, gms_start,  gms_start + 3))),
    k_factor = as.integer(trimws(substr(rows, k_start,    k_start + 2))),
    birthday = as.integer(trimws(substr(rows, bday_start, bday_start + 3))),
    flag     = trimws(substr(rows, flag_start, flag_start + 3)),
    snapshot = snapshot_label
  )
}
