# EU_ETS: State Ownership and Compliance Under Carbon Pricing

Independent research (Harvard Kennedy School, advised by Robert Stavins) on whether
state-owned enterprises respond differently than private firms to carbon pricing and
environmental regulation — originally built around the EU Emissions Trading System,
later extended to India's PAT scheme as a developing-country comparison.

---

## Research Question

Do SOEs abate more than private firms under carbon pricing, and if so, through what
channel — a market price signal, a non-market policy target, or something ownership-
specific like a welfare-concern mechanism identified in Chemmanur et al. (2025)?
The EU and India sides test this in two very different institutional settings:
the EU has a liquid carbon market; India's PAT scheme does not.

## Repository Structure

```
EU_ETS/
├── data/
│   ├── raw/           # source files (EUTL, Orbis exports, CEA databases, PAT gazettes, etc.)
│   └── processed/      # cleaned/merged panels (.rds, .csv)
├── scripts/
│   ├── 01-10           # EU ETS data pipeline (see below)
│   └── 11-15           # India PAT pipeline (see below)
├── output/
│   ├── figures/
│   └── tables/
└── README.md
```

## Data Sources

**EU side:**
- EU Transaction Log (EUTL) — installation-level verified emissions, allocations
- Orbis — firm ownership data (shareholder structure, used to build `state_ownership_pct_tv`)
- OECD Environmental Policy Stringency (EPS) index — ends 2020, forward-filled thereafter
- V-Dem — institutional quality indices (`v2x_polyarchy`)
- ICAP — daily EUA carbon price
- World Bank / RFF World Carbon Pricing Database — national carbon taxes
- EU NEC Directive — SO2/NOx national emission ceilings, used to build ambition-gap variables
- JRC-PPDB-OPEN — power plant fuel-type/commissioning data (~23% match rate, biased toward large plants)

**India side:**
- CEA CO2 Baseline Database — station-level generation/emissions/capacity/ownership, FY2010-11–FY2024-25
- PAT (Perform, Achieve, Trade) gazette notifications — Cycles I, II, IV, V (Thermal Power Plant sector; Cycle III base notification still missing, Cycle VI+ confirmed to not cover this sector)

## Scripts

### EU ETS pipeline (01–10)

| Script | Purpose | Status |
|---|---|---|
| `01_clean_eutl.R` | Clean EU Transaction Log installation-level data | ⚠️ drafted from memory of a past session — paste in to confirm details |
| `02_merge_orbis.R` | Merge Orbis ownership data, build `state_ownership_pct_tv` (time-varying, forward-filled from biennial shareholder snapshots — 4,259 firms, 2,042 with a government shareholder, 523 with an ownership change over the panel) | ⚠️ confirm details |
| `03_descriptive_stats.R` | Descriptive statistics | ⚠️ **last commit says "changes, need to fix" — likely not currently clean, worth checking before relying on it** |
| `04_cross_sector_analysis.R` | Cross-sector analysis | ⚠️ confirm details |
| `05_merge_policy_indices.R` | Merge OECD EPS and V-Dem institutional-quality indices into the panel | ⚠️ confirm details |
| `06_regressions.R` | Main regressions — `log_verified`/`log_emissions_intensity` ~ ownership × EPS/price interactions by phase; also the SO2-vs-price "horse race" and an ML-vs-`feols` comparison per the commit history | ⚠️ confirm details |
| `07_merge_fuel_type.R` | Merge JRC-PPDB fuel-type/commissioning data (~23% match rate, biased toward large plants) | ⚠️ confirm details |
| `08_carbon_price_merge.R` | Build `combined_carbon_price` = ICAP daily EUA price + World Bank/RFF national carbon tax; used as the Phase 4 substitute for EPS, which is forward-filled and has no within-country variation past 2020 | ⚠️ confirm details |
| `09_nec_emissions_merge.R` | Build NEC Directive SO2/NOx ambition-gap variables (trend-adjusted: legal target vs. pre-2020 business-as-usual trajectory) — the non-market-mechanism counterpart to carbon price | ⚠️ confirm details |
| `10_ml_exploration.R` | ML exploration / comparison against `feols` — likely the natural home for a causal-forest or Double ML extension of the ownership-heterogeneity question, given our recent discussion | ⚠️ confirm details, and worth revisiting given that conversation |

### India PAT pipeline (11–15)

Built together, step by step — I have exact, verified detail on all five.

| Script | Purpose |
|---|---|
| `11_build_cea_panel.R` | Stitches CEA CO2 Baseline Database versions (v11, v12, v17–v21) into one station-year panel, FY2010-11–FY2024-25. Handles: multi-year vs. single-year release formats, footer/summary-row contamination, station-name alias harmonization (typo/whitespace duplicates), blank-field forward/backward-fill within station. |
| `12_extract_pat_thermal_dcs.R` | Extracts Thermal Power Plant Designated Consumer lists from PAT gazette notifications — OCR for scanned PDFs (Cycle I), direct text extraction for born-digital ones (Cycles II, IV, V). |
| `13_match_pat_dcs.R` | Fuzzy-matches PAT DC names to CEA station names using real `rapidfuzz` (via `reticulate`), state-restricted with fallback, flags same-site ambiguity. |
| `14_build_did_panel.R` | Merges the CEA panel with reviewed PAT treatment status into the DiD-ready panel (`TREATED`, `POST`, `TREATED_POST`, `SOE`), applying the manual-review overrides (rejections, reassignments, same-site expansions) from hand-checking all ambiguous matches. |
| `15_did_regressions.R` | Three outcomes (emissions, generation, intensity) × naive TWFE, SOE heterogeneity, and Sun-Abraham event study (`sunab`) — the cohort-staggering-robust estimator, needed because Cycle I/II have no pre-period within the panel window. |

## Known Open Items (India side)

- Cycle III PAT notification — only a 6-item corrigendum sourced, not the base document
- 4 stations still flagged `needs_review` after manual match review (unresolved, not rejected)
- Ownership/cohort collinearity check (are SOE stations disproportionately early-cohort?) — flagged, not yet run
- SOE/private `sunab()` split done for emissions only — generation and intensity splits not yet built
- Formal pre-trend joint significance test (`wald()` on pre-period `sunab` coefficients) — not yet run
- "VCOV matrix not positive semi-definite" warning on all `sunab` models — cause not yet diagnosed

## Dependencies

**R:** `fixest`, `dplyr`, `tidyr`, `purrr`, `stringr`, `readxl`, `reticulate` (for real `rapidfuzz` via Python), `stringdist`
**Python (via reticulate, or standalone for extraction):** `openpyxl`, `pandas`, `pytesseract`, `rapidfuzz`
