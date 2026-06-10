library(tidyverse)

source("R/parse_fwf.R")
source("R/feature_engineering.R")

snapshots <- tribble(
  ~path,                                            ~label,
  "data/standard_feb15frl/standard_feb15frl.txt",  "2015-02",
  "data/standard_jan19frl/standard_jan19frl.txt",  "2019-01",
  "data/standard_jan20frl/standard_jan20frl.txt",  "2020-01",
  "data/standard_jul20frl/standard_jul20frl.txt",  "2020-07",
  "data/standard_jan21frl/standard_jan21frl.txt",  "2021-01",
  "data/standard_jan22frl/standard_jan22frl.txt",  "2022-01",
  "data/standard_jun26frl/standard_jun26frl.txt",  "2026-06"
)

players <- map2_dfr(snapshots$path, snapshots$label, parse_snapshot) %>%
  clean_players() %>%
  add_features()

stopifnot(!anyNA(players$rating), !anyNA(players$games))

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(players, "data/processed/players.rds")

cat("Saved", nrow(players), "rows across", n_distinct(players$snapshot), "snapshots\n")
print(count(players, snapshot, ref_month, period))
