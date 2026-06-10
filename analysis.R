library(tidyverse)
library(broom)

players <- readRDS("data/processed/players.rds")

# Subset used for the Pre-COVID / COVID / Post-COVID recovery analyses
# (excludes 2015-02, which is reserved for the inflation comparison)
period_data <- players %>%
  filter(!is.na(period)) %>%
  mutate(log_games = log1p(games))

covid_window <- as.Date(c("2020-03-01", "2021-12-31"))

# ── 5.1 Descriptive time series ──────────────────────────────────────────

active_counts <- players %>%
  count(snapshot, ref_month, name = "active_players") %>%
  arrange(ref_month)

participation <- players %>%
  group_by(snapshot, ref_month) %>%
  summarise(pct_played = 100 * mean(played), .groups = "drop") %>%
  arrange(ref_month)

print(active_counts)
print(participation)

p_active <- ggplot(active_counts, aes(ref_month, active_players)) +
  annotate("rect", xmin = covid_window[1], xmax = covid_window[2],
           ymin = -Inf, ymax = Inf, fill = "grey85") +
  geom_line(color = "#5b9fff", linewidth = 1.2) +
  geom_point(color = "#f0c040", size = 3) +
  labs(title = "Active FIDE-rated players over time",
       x = "Reference month (games played in)", y = "Active players") +
  theme_minimal()

p_participation <- ggplot(participation, aes(ref_month, pct_played)) +
  annotate("rect", xmin = covid_window[1], xmax = covid_window[2],
           ymin = -Inf, ymax = Inf, fill = "grey85") +
  geom_line(color = "#51cf66", linewidth = 1.2) +
  geom_point(color = "#f0c040", size = 3) +
  labs(title = "Share of active players with >=1 rated game that month",
       x = "Reference month", y = "% played") +
  theme_minimal()

p_active
p_participation

# ── 5.2 Chi-squared test: participation by period ────────────────────────

contingency <- table(period_data$period, period_data$played)
print(contingency)

contingency_pct <- round(100 * prop.table(contingency, margin = 1), 1)
print(contingency_pct)

chisq_result <- chisq.test(contingency)
print(chisq_result)

n_total <- sum(contingency)
cramers_v <- sqrt(unname(chisq_result$statistic) / n_total)
cat("Cramer's V:", round(cramers_v, 4), "\n")
