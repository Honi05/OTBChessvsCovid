library(tidyverse)
library(broom)

source("R/clustering.R")

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

# ── 5.3 ANOVA: log1p(games) by period ────────────────────────────────────

aov_fit <- aov(log_games ~ period, data = period_data)
print(summary(aov_fit))

aov_table <- summary(aov_fit)[[1]]
eta_sq <- aov_table["period", "Sum Sq"] / sum(aov_table[["Sum Sq"]])
cat("Eta-squared:", round(eta_sq, 4), "\n")

tukey_result <- TukeyHSD(aov_fit)
print(tukey_result)

# Residual diagnostics (sample for plotting speed with millions of rows)
aug_aov <- broom::augment(aov_fit) %>% slice_sample(n = 20000)

ggplot(aug_aov, aes(.fitted, .resid)) +
  geom_jitter(alpha = 0.1, width = 0.05) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "ANOVA residuals vs fitted", x = "Fitted", y = "Residual") +
  theme_minimal()

ggplot(aug_aov, aes(sample = .resid)) +
  stat_qq(alpha = 0.1) +
  stat_qq_line(color = "red") +
  labs(title = "ANOVA residual Q-Q plot") +
  theme_minimal()

# ── 5.4 Multiple regression: log1p(games) ~ period + age + title_has ────

reg_data <- period_data %>% filter(!is.na(age))
cat("Rows used:", nrow(reg_data), "of", nrow(period_data),
    "(", round(100 * nrow(reg_data) / nrow(period_data), 1), "% )\n")

lm_fit <- lm(log_games ~ period + age + title_has, data = reg_data)
print(summary(lm_fit))

# Residual diagnostics (sampled for plotting speed)
aug_lm <- broom::augment(lm_fit) %>% slice_sample(n = 20000)

ggplot(aug_lm, aes(.fitted, .resid)) +
  geom_point(alpha = 0.1) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Regression residuals vs fitted", x = "Fitted", y = "Residual") +
  theme_minimal()

ggplot(aug_lm, aes(sample = .resid)) +
  stat_qq(alpha = 0.1) +
  stat_qq_line(color = "red") +
  labs(title = "Regression residual Q-Q plot") +
  theme_minimal()

# ── 5.5 Inflation: rating distribution, 2015-02 vs 2026-06 ───────────────

inflation_data <- players %>% filter(snapshot %in% c("2015-02", "2026-06"))

inflation_summary <- inflation_data %>%
  group_by(snapshot) %>%
  summarise(n = n(), mean_rtg = mean(rating), sd_rtg = sd(rating), .groups = "drop")
print(inflation_summary)

# Compare SDs to choose pooled (var.equal=TRUE) vs Welch's t-test.
# Ratio < 1.2 is treated as "close enough" to equal variance for the
# pooled formula; otherwise Welch's correction is used. Whichever is
# chosen is stated explicitly in the deck.
sd_ratio <- max(inflation_summary$sd_rtg) / min(inflation_summary$sd_rtg)
cat("SD ratio:", round(sd_ratio, 3), "\n")

use_pooled <- sd_ratio < 1.2
cat("Using", if (use_pooled) "pooled-variance" else "Welch's", "t-test\n")

t_result <- t.test(rating ~ snapshot, data = inflation_data, var.equal = use_pooled)
print(t_result)

ggplot(inflation_data, aes(rating, fill = snapshot)) +
  geom_histogram(alpha = 0.5, binwidth = 25, position = "identity") +
  scale_fill_manual(values = c("2015-02" = "#5b9fff", "2026-06" = "#f0c040")) +
  labs(
    title = "FIDE Standard rating distribution: Feb 2015 vs Jun 2026",
    x = "Rating", y = "Number of players", fill = "Snapshot"
  ) +
  theme_minimal()

# ── 5.6 Player segmentation via k-means ──────────────────────────────────

cluster_data <- period_data %>% filter(!is.na(age))
cat("Rows used for clustering:", nrow(cluster_data), "of", nrow(period_data),
    "(", round(100 * nrow(cluster_data) / nrow(period_data), 1), "% )\n")

cluster_features <- cluster_data %>%
  select(rating, log_games, age) %>%
  as.matrix() %>%
  scale()

feature_center <- attr(cluster_features, "scaled:center")
feature_scale  <- attr(cluster_features, "scaled:scale")

# Elbow plot: total within-cluster SS for k = 1..10, on a 50k-row sample
# for speed (full data is ~1M rows).
set.seed(42)
elbow_sample <- cluster_features[sample(nrow(cluster_features), 50000), ]
elbow_df <- tibble(
  k   = 1:10,
  wss = map_dbl(1:10, function(k) {
    kmeans(elbow_sample, centers = k, nstart = 10, iter.max = 100,
           algorithm = "Lloyd")$tot.withinss
  })
)
print(elbow_df)

ggplot(elbow_df, aes(k, wss)) +
  geom_line(color = "#5b9fff", linewidth = 1.2) +
  geom_point(color = "#f0c040", size = 3) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "K-means elbow plot (50k-row sample)",
       x = "Number of clusters (k)", y = "Total within-cluster SS") +
  theme_minimal()

# Final fit: k = 4, on the full pooled (standardized) feature matrix --
# one shared centroid set used for every period.
set.seed(42)
km_fit <- kmeans(cluster_features, centers = 4, nstart = 25, iter.max = 100,
                  algorithm = "Lloyd")

# Relabel clusters 1-4 by ascending centroid rating, so cluster numbering
# is stable and interpretable regardless of kmeans' arbitrary initial order.
cluster_rank <- cluster_rank_by_rating(
  centers       = km_fit$centers,
  rating_col    = "rating",
  rating_center = feature_center["rating"],
  rating_scale  = feature_scale["rating"]
)
cluster_data$cluster <- factor(cluster_rank[km_fit$cluster], levels = 1:4)

# Cluster profile: what does each segment look like in original units?
cluster_profile <- cluster_data %>%
  group_by(cluster) %>%
  summarise(
    n           = n(),
    mean_rating = mean(rating),
    mean_games  = mean(games),
    mean_age    = mean(age),
    pct_titled  = 100 * mean(title_has),
    .groups = "drop"
  ) %>%
  mutate(pct_of_pool = 100 * n / sum(n))
print(cluster_profile)

# Cluster share by period -- comparable across periods because the
# standardization and centroids above were fit once on the pooled data.
cluster_shares <- cluster_data %>%
  count(period, cluster, name = "n") %>%
  group_by(period) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup()
print(cluster_shares)

ggplot(cluster_shares, aes(period, pct, fill = cluster)) +
  geom_col(position = "dodge") +
  labs(title = "Player segment share by period",
       x = "Period", y = "% of active players", fill = "Cluster") +
  theme_minimal()

# Chi-squared test: is cluster membership independent of period?
cluster_contingency <- table(cluster_data$period, cluster_data$cluster)
print(cluster_contingency)

cluster_contingency_pct <- round(100 * prop.table(cluster_contingency, margin = 1), 1)
print(cluster_contingency_pct)

cluster_chisq <- chisq.test(cluster_contingency)
print(cluster_chisq)

cluster_n  <- sum(cluster_contingency)
cluster_df <- min(dim(cluster_contingency)) - 1
cluster_cramers_v <- sqrt(unname(cluster_chisq$statistic) / (cluster_n * cluster_df))
cat("Cramer's V (period x cluster):", round(cluster_cramers_v, 4), "\n")
