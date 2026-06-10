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
