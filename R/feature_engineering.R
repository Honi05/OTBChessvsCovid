library(dplyr)
library(tibble)

# FIDE titles tracked individually; anything else collapses to "none".
TITLE_LEVELS <- c("GM", "IM", "FM", "CM", "WGM", "WIM", "WFM", "WCM", "NM", "WNM")

# Maps each snapshot to the calendar month its "games" column reflects, and
# to its place in the Pre-COVID / COVID / Post-COVID design (NA = excluded,
# used only for the 2015 vs 2026 inflation comparison in section 5.5).
SNAPSHOT_INFO <- tribble(
  ~snapshot,  ~ref_month,            ~period,
  "2015-02",  as.Date("2015-01-01"), NA_character_,
  "2019-01",  as.Date("2018-12-01"), "Pre-COVID",
  "2020-01",  as.Date("2019-12-01"), "Pre-COVID",
  "2020-07",  as.Date("2020-06-01"), "COVID",
  "2021-01",  as.Date("2020-12-01"), "COVID",
  "2022-01",  as.Date("2021-12-01"), "Post-COVID",
  "2026-06",  as.Date("2026-05-01"), "Post-COVID"
)

# Filter to active rated players (FIDE convention: rating > 0, not flagged
# inactive -- the flag column may hold combined codes like "wi" for
# "woman + inactive", so we exclude any flag containing "i") and bucket
# the title column to the tracked FIDE titles.
clean_players <- function(df) {
  df %>%
    filter(rating > 0, !grepl("i", flag, fixed = TRUE)) %>%
    mutate(title = if_else(title %in% TITLE_LEVELS, title, "none"))
}

# Add ref_month, period, age (NA if birthday unknown), title_has, played.
add_features <- function(df) {
  df %>%
    left_join(SNAPSHOT_INFO, by = "snapshot") %>%
    mutate(
      period    = factor(period, levels = c("Pre-COVID", "COVID", "Post-COVID")),
      snap_year = as.integer(substr(snapshot, 1, 4)),
      age       = if_else(birthday == 0L, NA_integer_, snap_year - birthday),
      title_has = title != "none",
      played    = games > 0
    ) %>%
    select(-snap_year)
}
