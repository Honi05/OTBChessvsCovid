# Did Over-the-Board Chess Fully Recover After COVID-19?

**Course:** Data Management and Analysis — Sapienza University of Rome (Prof. Quattrociocchi)
**Author:** Honi Arora

---

## Research Question

> **Did competitive over-the-board (OTB) chess recover after COVID-19 — and if so, who came back?**

The COVID-19 pandemic forced the cancellation of virtually all in-person tournaments in 2020. At the same time, chess experienced an unprecedented *online* boom (Queen's Gambit effect, Lichess/chess.com growth). But FIDE standard ratings only reflect OTB tournament games — not online play. This project asks whether the real-world tournament ecosystem recovered, stagnated, or transformed — and uses player-level data to find out.

---

## Data

Seven FIDE Standard rating list snapshots (fixed-width `.txt` files, ~1–1.7 M rows each):

| Snapshot | Period label | Games reflect |
|---|---|---|
| Feb 2015 | — (baseline) | Jan 2015 |
| Jan 2019 | Pre-COVID | Dec 2018 |
| Jan 2020 | Pre-COVID | Dec 2019 |
| Jul 2020 | COVID | Jun 2020 |
| Jan 2021 | COVID | Dec 2020 |
| Jan 2022 | Post-COVID | Dec 2021 |
| Jun 2026 | Post-COVID | May 2026 |

All raw files are available on [Kaggle](https://www.kaggle.com/honiarora).

---

## Analyses

| Section | Method | Question answered |
|---|---|---|
| 5.1 | Descriptive time series | How did active player counts and game participation move over time? |
| 5.2 | Chi-squared + Cramér's V | Did the probability of playing at least one rated game change across periods? |
| 5.3 | One-way ANOVA + Tukey HSD + η² | Did the number of games played per player differ significantly across periods? |
| 5.4 | Multiple linear regression | Controlling for age and title, how much did period predict game volume? |
| 5.5 | Rating distribution (t-test) | How has the shape of the FIDE player pool shifted from 2015 to 2026? |
| 5.6 | K-means clustering (k = 4) | What player segments exist, and did their composition shift across periods? |

---

## Key Results

- **Participation rate dropped sharply during COVID** (Jul 2020, Jan 2021) and the chi-squared test confirmed this shift is statistically significant (p < 2.2 × 10⁻¹⁶, Cramér's V ≈ 0.08).
- **Game volume recovered but not fully**: the Post-COVID period shows significantly higher game counts than COVID, but the Tukey test reveals Post-COVID remains below Pre-COVID levels (ANOVA η² ≈ 0.02).
- **Age and title both matter**: the regression shows titled players played substantially more games, and older players played fewer — independent of period effects.
- **The player pool has grown and shifted**: by 2026, there are far more active FIDE-rated players than in 2015, with the distribution mean shifting meaningfully (t-test significant, p < 2.2 × 10⁻¹⁶).
- **Four stable player segments** emerged from k-means: low-rated casual players (largest group), mid-range club players, high-rated active competitors, and elite titled players. The COVID period saw the elite and high-rated segments contract proportionally, while casual players' share held steadier.

---

## Repository Structure

```
analysis.R          # Main analysis script (sections 5.1–5.6)
R/
  clustering.R      # K-means helper (cluster_rank_by_rating)
  build_dataset.R   # Parses raw FIDE .txt files → data/processed/players.rds
  parse_fwf.R       # Fixed-width parser for both FIDE file formats
  feature_engineering.R
data/
  standard_*frl/    # Raw FIDE rating list snapshots (7 folders)
  processed/        # players.rds (cached, not tracked in git)
docs/               # Design specs and implementation plans
DMA2_Presentation.pptx
```

---

## How to Run

```r
# 1. Build the processed dataset (only needed once)
source("R/build_dataset.R")

# 2. Run the full analysis
source("analysis.R")
```

Requires R ≥ 4.2 with packages: `tidyverse`, `broom`.
