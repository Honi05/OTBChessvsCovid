library(tidyverse)

# ── 1. PARSER ──────────────────────────────────────────────────────────────────
# Handles both formats:
#   Feb 2015: no FOA col → rating starts at char position 110
#   All others: FOA col  → rating starts at char position 114

parse_snapshot <- function(path, snapshot_label) {
  lines  <- readLines(path, encoding = "UTF-8", warn = FALSE)
  header <- lines[1]
  rows   <- lines[-1]

  # Detect format from header
  has_foa  <- grepl("FOA", header)
  rtg_start <- if (has_foa) 114 else 110   # 1-indexed for substr
  gms_start <- if (has_foa) 120 else 116
  k_start   <- if (has_foa) 124 else 120
  bday_start <- if (has_foa) 127 else 123
  flag_start <- if (has_foa) 133 else 129

  df <- tibble(
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

  # Clean: keep only rated, active players
  df %>%
    filter(
      rating > 0,       # has a standard rating
      flag != "i"       # not marked inactive
    ) %>%
    mutate(
      title = if_else(title %in% c("GM","IM","FM","CM","WGM","WIM","WFM","WCM","NM","WNM"), title, "none"),
      age   = as.integer(substr(snapshot_label, 1, 4)) - birthday
    )
}

# ── 2. LOAD ALL SNAPSHOTS ──────────────────────────────────────────────────────
snapshots <- tribble(
  ~path,                                                                    ~label,
  "data/standard_feb15frl/standard_feb15frl.txt",  "2015-02",
  "data/standard_jan19frl/standard_jan19frl.txt",  "2019-01",
  "data/standard_jan20frl/standard_jan20frl.txt",  "2020-01",
  "data/standard_jul20frl/standard_jul20frl.txt",  "2020-07",
  "data/standard_jan21frl/standard_jan21frl.txt",  "2021-01",
  "data/standard_jan22frl/standard_jan22frl.txt",  "2022-01",
  "data/standard_jun26frl/standard_jun26frl.txt",  "2026-06"
)

# Set working directory to project folder first:
# setwd("E:/dma2/myproject")

all <- map2_dfr(snapshots$path, snapshots$label, parse_snapshot)

cat("Loaded", nrow(all), "player-snapshots across", n_distinct(all$snapshot), "files\n")

# ── 3. FIRST LOOK ─────────────────────────────────────────────────────────────
# Let the data speak before deciding anything

# 3a. How many active rated players per snapshot?
player_counts <- all %>%
  count(snapshot, name = "active_players") %>%
  arrange(snapshot)

print(player_counts)

# 3b. Rating distribution summary per snapshot
summary_stats <- all %>%
  group_by(snapshot) %>%
  summarise(
    n          = n(),
    mean_rtg   = round(mean(rating), 1),
    median_rtg = median(rating),
    sd_rtg     = round(sd(rating), 1),
    p25        = quantile(rating, 0.25),
    p75        = quantile(rating, 0.75),
    .groups = "drop"
  )

print(summary_stats)

# 3c. K-factor breakdown per snapshot (newcomers vs established vs elite)
k_breakdown <- all %>%
  mutate(tier = case_when(
    k_factor == 40 ~ "newcomer/junior (K=40)",
    k_factor == 20 ~ "established (K=20)",
    k_factor == 10 ~ "elite (K=10)",
    TRUE           ~ "other"
  )) %>%
  count(snapshot, tier) %>%
  group_by(snapshot) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(snapshot, tier)

print(k_breakdown)

# 3d. Title counts per snapshot
title_counts <- all %>%
  filter(title %in% c("GM","IM","FM","CM")) %>%
  count(snapshot, title) %>%
  arrange(snapshot, title)

print(title_counts)

# ── 4. PLOTS ──────────────────────────────────────────────────────────────────

# 4a. Rating distribution: 2015 vs 2026 overlaid
all %>%
  filter(snapshot %in% c("2015-02", "2026-06")) %>%
  ggplot(aes(x = rating, fill = snapshot)) +
  geom_histogram(alpha = 0.5, binwidth = 25, position = "identity") +
  scale_fill_manual(values = c("2015-02" = "#5b9fff", "2026-06" = "#f0c040")) +
  labs(
    title = "FIDE Standard Rating Distribution: Feb 2015 vs Jun 2026",
    x = "Rating", y = "Number of players", fill = "Snapshot"
  ) +
  theme_minimal()

# 4b. Active player count over time
player_counts %>%
  ggplot(aes(x = snapshot, y = active_players, group = 1)) +
  geom_line(color = "#5b9fff", linewidth = 1.2) +
  geom_point(color = "#f0c040", size = 3) +
  labs(
    title = "FIDE Active Standard-Rated Players Over Time",
    x = "Snapshot", y = "Player count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4c. Mean rating over time
summary_stats %>%
  ggplot(aes(x = snapshot, y = mean_rtg, group = 1)) +
  geom_line(color = "#51cf66", linewidth = 1.2) +
  geom_point(color = "#f0c040", size = 3) +
  geom_errorbar(aes(ymin = mean_rtg - sd_rtg/sqrt(n),
                    ymax = mean_rtg + sd_rtg/sqrt(n)), width = 0.2) +
  labs(
    title = "Mean FIDE Standard Rating Over Time",
    x = "Snapshot", y = "Mean rating"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
