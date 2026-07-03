library(tidyverse)
library(data.table)

# ── 1. Load raw data ──────────────────────────────────────────────────────────
installations <- fread('data/raw/installation.csv')
compliance    <- fread('data/raw/compliance.csv')

# quick look
glimpse(installations)
glimpse(compliance)