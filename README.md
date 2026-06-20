# COVID-19 Effects on Slow Thinking Chess

**Did participation in classical over-the-board chess decline following lockdown and online chess boom?**

**Author:** Honi Arora

**Course:** Data Management and Analysis (Unit 2) — Sapienza University of Rome

---

## The Question

Chess.com grew from 30 million to 150 million users between 2020 and 2023. Fast online formats exploded. But FIDE standard ratings only count **over-the-board (OTB) tournament games** — 4-hour classical chess played in person. This project asks whether that world collapsed during COVID-19, and whether it has come back.

---

## Data

Seven official FIDE Standard Rating Lists (fixed-width `.txt` files):

| Snapshot | Period | 
|---|---|
| Feb 2015 | Long-run baseline |  
| Jan 2019 | Pre-COVID | 
| Jan 2020 | Pre-COVID | 
| Jul 2020 | COVID | 
| Jan 2021 | COVID | 
| Jan 2022 | Post-COVID | 
| Jun 2026 | Post-COVID | 

**Raw records:** 2,534,904 across all 7 lists
**After cleaning** (removed inactive players — zero tournament games in 12+ months): **1,166,786 records × 15 variables**

Data available on [Kaggle](https://www.kaggle.com/datasets/honiarora/fide-chess-covid-recovery).

---

## Analyses

| Section | What it answers |
|---|---|
| 5.1 Descriptive time series | How did active player counts and participation rates move over time? |
| 5.2 Rating probability distribution | How did the shape of the FIDE player pool shift from 2015 to 2026? |
| 5.3 K-means clustering (k = 4) | What player segments exist, and how did their mix change across periods? |

---

## Key Findings

### Participation collapsed — and only partly recovered

The share of active players who played at least one rated OTB game in a given month:

| Period | % played |
|---|---|
| Pre-COVID | ~32% |
| COVID | ~4% |
| Post-COVID | ~24% |

Chi-squared test: χ² = 90,418 · p < .001 · **Cramer's V = 0.29** — a moderate association between period and participation.

### The pool nearly doubled, but average strength fell

Between 2015 and 2026, the number of active FIDE-rated players grew from **115,546 → 217,459** (~88% growth). But mean rating fell from **1799 → 1737** (SD: 310 → 224) — the new players entering the pool are predominantly lower-rated beginners and club players.

### Four player segments — only one was disrupted by COVID

K-means on rating, monthly games, and age (k = 4) identified four stable groups:

| Cluster | Share | Avg age | Avg rating | Avg games/mo |
|---|---|---|---|---|
| Young low-activity | 33% | 19 | 1372 | ~0.3 |
| Senior low-activity | 28% | 62 | 1673 | ~0.4 |
| Strong low-activity | 26% | ~49 | 2003 | ~0.7 |
| Regular players | 14% | ~33 | 1689 | 6.6 |

The **Regular Players** cluster (Cluster 3) dropped to ~2.5% of active players during COVID then rebounded. The other three segments held nearly the same proportional share across all periods.

Chi-squared on cluster × period: χ² = 53,535 · df = 6 · p < .001 · **Cramer's V = 0.16** (weak but significant).

### Bottom line

> OTB chess grew in size, not in engagement. The post-COVID base is larger and more casual than before.

---

## Repository Structure

```
analysis.R               # Main analysis (sections 5.1–5.3)
R/
  clustering.R           # cluster_rank_by_rating helper
  build_dataset.R        # Parses FIDE .txt files → data/processed/players.rds
  parse_fwf.R            # Fixed-width parser (handles both FIDE file formats)
  feature_engineering.R
data/
  standard_*frl/         # Raw FIDE rating list snapshots (gitignored)
  processed/             # players.rds cache (gitignored)
DMA2_Presentation .pptx
```

## How to Run

```r
# 1. Build the processed dataset (once)
source("R/build_dataset.R")

# 2. Run the full analysis
source("analysis.R")
```

Requires R ≥ 4.2 · packages: `tidyverse`
