# ── 1. Load packages ───────────────────────────────────────────────────────────
library(tidyverse)
library(data.table)
library(corrplot)

# ── 2. Load raw EUTL data (all sectors) ───────────────────────────────────────

# ── all sectors dataset ───────────────────────────────────────────────────────
installations_all <- fread('data/raw/installation.csv') %>%
  filter(
    isAircraftOperator == FALSE,
    isMaritimeOperator == FALSE
  )

compliance_raw <- fread('data/raw/compliance.csv')

df_all <- compliance_raw %>%
  inner_join(
    installations_all %>%
      select(id, registry_id, activity_id, nace_id),
    by = c('installation_id' = 'id')
  ) %>%
  filter(
    compliance_id %in% c('A', 'B', 'E'),
    !is.na(verified),
    verified > 0,
    !is.na(allocatedTotal)
  ) %>%
  mutate(
    phase = case_when(
      euetsPhase == '2005-2007' ~ 1,
      euetsPhase == '2008-2012' ~ 2,
      euetsPhase == '2013-2020' ~ 3,
      euetsPhase == '2021-2030' ~ 4
    ),
    surplus_norm = (allocatedTotal - verified) / verified,
    log_verified = log(verified)
  ) %>%
  filter(
    phase > 1,
    between(surplus_norm, -1, 10)
  )

# check
nrow(df_all)
table(df_all$phase)

# sector breakdown
df_all %>%
  mutate(sector = case_when(
    activity_id %in% c(1, 20) ~ 'Power',
    activity_id %in% c(6, 29) ~ 'Cement',
    activity_id %in% c(5, 24, 25) ~ 'Steel/Metal',
    activity_id %in% c(2, 21) ~ 'Refining',
    activity_id %in% c(9, 35, 36) ~ 'Pulp/Paper',
    TRUE ~ 'Other'
  )) %>%
  count(sector) %>%
  arrange(desc(n))

# ── 3. Build all-sectors compliance dataset ───────────────────────────────────

# ── 4. Merge BvD IDs ──────────────────────────────────────────────────────────
# load the oha_clean object - need to rebuild it from account table
accounts <- fread('data/raw/account.csv')

oha_clean <- accounts %>%
  filter(accountType_id %in% c('100-7', '120-0'),
         installation_id != '',
         bvdId != '') %>%
  select(installation_id, bvdId, accountType_id) %>%
  arrange(installation_id,
          ifelse(accountType_id == '100-7', 0, 1)) %>%
  group_by(installation_id) %>%
  slice(1) %>%
  ungroup()

# merge BvD IDs into all-sectors dataset
df_all <- df_all %>%
  left_join(oha_clean %>% select(installation_id, bvdId),
            by = 'installation_id')

# ── 5. Merge Orbis ownership ──────────────────────────────────────────────────
# load orbis_merged from processed data
orbis_merged <- readRDS('data/processed/df_panel_merged.rds') %>%
  distinct(bvdId, state_ownership_pct, state_owned_binary, 
           guo_state, foreign_owned, guo_type, guo_country)

df_all <- df_all %>%
  left_join(orbis_merged, by = 'bvdId')


df_all <- df_all %>%
  mutate(sector = case_when(
    activity_id %in% c(1, 20) ~ 'Power',
    activity_id %in% c(6, 29) ~ 'Cement',
    activity_id %in% c(5, 24, 25) ~ 'Steel/Metal',
    activity_id %in% c(2, 21) ~ 'Refining',
    activity_id %in% c(9, 35, 36) ~ 'Pulp/Paper',
    TRUE ~ 'Other'
  ))


# check coverage by sector
df_all %>%
  mutate(has_ownership = !is.na(state_ownership_pct)) %>%
  group_by(sector) %>%
  summarise(
    n = n(),
    pct_coverage = round(mean(has_ownership) * 100, 1)
  ) %>%
  arrange(desc(pct_coverage))

# ── 6. Merge financials ───────────────────────────────────────────────────────

# load financials from panel — already pivoted to long format in script 02
df_panel_full <- readRDS('data/processed/df_panel_merged.rds')

# extract just the financial variables
financials <- df_panel_full %>%
  distinct(bvdId, year, total_assets, operating_revenue, n_employees)

# merge into df_all
df_all <- df_all %>%
  left_join(financials, by = c('bvdId', 'year'))

# construct emissions intensity for all sectors
df_all <- df_all %>%
  mutate(
    emissions_intensity = ifelse(
      operating_revenue <= 0, NA, verified / operating_revenue
    ),
    log_emissions_intensity = log(emissions_intensity)
  )

# check coverage by sector
df_all %>%
  mutate(has_revenue = !is.na(operating_revenue)) %>%
  group_by(sector) %>%
  summarise(
    n = n(),
    pct_revenue = round(mean(has_revenue) * 100, 1)
  )

df_all %>%
  filter(phase >= 3) %>%
  mutate(has_revenue = !is.na(operating_revenue)) %>%
  group_by(sector) %>%
  summarise(
    n = n(),
    pct_revenue = round(mean(has_revenue) * 100, 1)
  )


# Testing other variables for deminominator to see if they have better coverage  

df_all %>%
  filter(phase >= 3) %>%
  mutate(
    has_assets = !is.na(total_assets),
    has_emp = !is.na(n_employees),
    has_revenue = !is.na(operating_revenue)
  ) %>%
  group_by(sector) %>%
  summarise(
    n = n(),
    pct_assets = round(mean(has_assets) * 100, 1),
    pct_emp = round(mean(has_emp) * 100, 1),
    pct_revenue = round(mean(has_revenue) * 100, 1)
  )

# ownership distribution — full power sector panel
df_panel %>%
  summarise(
    n = n(),
    mean_state_pct = round(mean(state_ownership_pct, na.rm = TRUE), 1),
    pct_state_owned = round(mean(state_owned_binary == 1, na.rm = TRUE) * 100, 1),
    pct_private = round(mean(state_owned_binary == 0, na.rm = TRUE) * 100, 1)
  )

# ownership distribution — revenue subsample only
df_panel %>%
  filter(!is.na(operating_revenue), operating_revenue > 0) %>%
  summarise(
    n = n(),
    mean_state_pct = round(mean(state_ownership_pct, na.rm = TRUE), 1),
    pct_state_owned = round(mean(state_owned_binary == 1, na.rm = TRUE) * 100, 1),
    pct_private = round(mean(state_owned_binary == 0, na.rm = TRUE) * 100, 1)
  )

# ── 7. Cross-sector correlation matrices ──────────────────────────────────────

# combined — all sectors together
cor_vars_all <- df_all %>%
  select(surplus_norm, log_verified,
         state_ownership_pct, state_owned_binary,
         guo_state, foreign_owned) %>%
  drop_na()

nrow(cor_vars_all)
cor_matrix_all <- cor(cor_vars_all)

png('output/figures/16_correlation_matrix_all_sectors.png',
    width = 10, height = 8, units = 'in', res = 300)
corrplot(cor_matrix_all,
         method = 'color',
         type = 'upper',
         addCoef.col = 'black',
         number.cex = 0.7,
         tl.cex = 0.7,
         title = 'Correlation matrix: all ETS sectors',
         mar = c(0,0,1,0))
dev.off()

# ── 8. Separated by sector ────────────────────────────────────────────────────
sectors_to_plot <- c('Power', 'Refining', 'Steel/Metal', 
                     'Pulp/Paper', 'Cement')

png('output/figures/17_correlation_matrix_by_sector.png',
    width = 16, height = 12, units = 'in', res = 300)
par(mfrow = c(2, 3))

for (s in sectors_to_plot) {
  
  cor_data <- df_all %>%
    filter(sector == s) %>%
    select(surplus_norm, log_verified,
           state_ownership_pct, state_owned_binary,
           guo_state, foreign_owned) %>%
    drop_na()
  
  n_obs <- nrow(cor_data)
  
  if (n_obs > 50) {
    cor_mat <- cor(cor_data)
    corrplot(cor_mat,
             method = 'color',
             type = 'upper',
             addCoef.col = 'black',
             number.cex = 0.6,
             tl.cex = 0.6,
             title = paste0(s, ' (n=', n_obs, ')'),
             mar = c(0,0,2,0))
  }
}

dev.off()
