# ── 1. Load packages ───────────────────────────────────────────────────────────
library(tidyverse)
library(data.table)
library(readxl)

# ── 2. Load cleaned EUTL data ─────────────────────────────────────────────────
df_clean <- readRDS('data/processed/df_clean_with_bvd.rds')

# ── 3. Load Orbis firm-level data (first export) ──────────────────────────────
orbis_raw <- bind_rows(
  read_excel('data/raw/orbis_batch1.xlsx', sheet = 'Results'),
  read_excel('data/raw/orbis_batch2.xlsx', sheet = 'Results')
) %>%
  filter(!is.na(`BvD ID number`))

# ── 4. Load Orbis shareholder data (second export) ────────────────────────────
orbis_sh_raw <- bind_rows(
  read_excel('data/raw/orbis2a_batch1.xlsx', sheet = 'Results'),
  read_excel('data/raw/orbis2b_batch2.xlsx', sheet = 'Results')
) %>%
  filter(!is.na(`BvD ID number`))

# ── 5. Parse shareholder rows ─────────────────────────────────────────────────
orbis_sh_long <- orbis_sh_raw %>%
  select(`BvD ID number`, `Company name Latin alphabet`,
         `Country ISO code`, `CSH - Name`,
         `CSH - Type`, `CSH - Direct %`, `CSH - Total %`) %>%
  separate_rows(`CSH - Name`, `CSH - Type`,
                `CSH - Direct %`, `CSH - Total %`,
                sep = "\n")

# ── 6. Construct state ownership variable ─────────────────────────────────────
state_types <- c("Public authority, state, government", "Public")

state_ownership <- orbis_sh_long %>%
  mutate(
    csh_direct_clean = `CSH - Direct %`,
    csh_direct_clean = na_if(csh_direct_clean, "n.a."),
    csh_direct_clean = na_if(csh_direct_clean, "WO"),
    csh_direct_clean = na_if(csh_direct_clean, ""),
    csh_direct = suppressWarnings(as.numeric(csh_direct_clean)),
    is_state = `CSH - Type` %in% state_types
  ) %>%
  group_by(`BvD ID number`, `Company name Latin alphabet`,
           `Country ISO code`) %>%
  summarise(
    state_ownership_pct = max(csh_direct[is_state], na.rm = TRUE),
    state_owned_binary  = as.integer(any(is_state)),
    n_shareholders      = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    state_ownership_pct = ifelse(is.infinite(state_ownership_pct),
                                 0, state_ownership_pct)
  )

# ── 7. Clean firm-level Orbis data ────────────────────────────────────────────
orbis_firm <- orbis_raw %>%
  select(
    bvdId              = `BvD ID number`,
    company_name       = `Company name Latin alphabet`,
    country            = `Country ISO code`,
    nace               = `NACE Rev. 2, core code (4 digits)`,
    legal_form         = `Standardized legal form`,
    independence       = `OUB - Independence indicator`,
    incorporation_date = `Date of incorporation`,
    guo_name           = `GUO - Name`,
    guo_type           = `GUO - Type`,
    guo_country        = `GUO - Country ISO code`,
    guo_direct_pct     = `GUO - Direct %`,
    guo_total_pct      = `GUO - Total %`,
    quoted             = `Quoted`
  ) %>%
  mutate(
    guo_total_pct = suppressWarnings(as.numeric(na_if(guo_total_pct, "n.a."))),
    guo_state     = as.integer(guo_type %in% state_types),
    foreign_owned = as.integer(!is.na(guo_country) & guo_country != country)
  )

# ── 8. Merge state ownership into firm data ───────────────────────────────────
orbis_merged <- orbis_firm %>%
  left_join(
    state_ownership %>%
      select(`BvD ID number`, state_ownership_pct, state_owned_binary),
    by = c('bvdId' = 'BvD ID number')
  )

# ── 9. Pivot financials to long format ────────────────────────────────────────
assets_long <- orbis_raw %>%
  select(`BvD ID number`, starts_with('Total assets')) %>%
  mutate(across(starts_with('Total assets'),
                ~suppressWarnings(as.numeric(na_if(as.character(.), "n.a."))))) %>%
  pivot_longer(cols = starts_with('Total assets'),
               names_to = 'year', values_to = 'total_assets') %>%
  mutate(year = as.integer(str_extract(year, '\\d{4}')))

revenue_long <- orbis_raw %>%
  select(`BvD ID number`, starts_with('Operating revenue')) %>%
  mutate(across(starts_with('Operating revenue'),
                ~suppressWarnings(as.numeric(na_if(as.character(.), "n.a."))))) %>%
  pivot_longer(cols = starts_with('Operating revenue'),
               names_to = 'year', values_to = 'operating_revenue') %>%
  mutate(year = as.integer(str_extract(year, '\\d{4}')))

employees_long <- orbis_raw %>%
  select(`BvD ID number`, starts_with('Number of employees')) %>%
  mutate(across(starts_with('Number of employees'),
                ~suppressWarnings(as.numeric(na_if(as.character(.), "n.a."))))) %>%
  pivot_longer(cols = starts_with('Number of employees'),
               names_to = 'year', values_to = 'n_employees') %>%
  mutate(year = as.integer(str_extract(year, '\\d{4}')))

financials_long <- assets_long %>%
  left_join(revenue_long, by = c('BvD ID number', 'year')) %>%
  left_join(employees_long, by = c('BvD ID number', 'year')) %>%
  rename(bvdId = `BvD ID number`) %>%
  filter(year >= 2005, year <= 2023)

# ── 10. Merge everything with EUTL data ───────────────────────────────────────
df_panel <- df_clean %>%
  left_join(orbis_merged, by = 'bvdId') %>%
  left_join(financials_long, by = c('bvdId', 'year'))

nrow(df_panel)
n_distinct(df_panel$bvdId)

# ── 11. Save ──────────────────────────────────────────────────────────────────
saveRDS(df_panel, 'data/processed/df_panel_merged.rds')
