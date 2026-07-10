# ── 1. Load packages ───────────────────────────────────────────────────────────
library(tidyverse)
library(data.table)

# ── 2. Load panel ─────────────────────────────────────────────────────────────
df_panel <- readRDS('data/processed/df_panel_merged.rds')

# ── 3. Load OECD EPS ──────────────────────────────────────────────────────────
eps_raw <- fread('data/raw/oecd.EPS.csv')

glimpse(eps_raw)
names(eps_raw)
head(eps_raw)

# ── 4. Load V-Dem ─────────────────────────────────────────────────────────────
vdem_raw <- fread('data/raw/V-Dem-CY-Core-v16.csv')

glimpse(vdem_raw)
names(vdem_raw) %>% head(30)

# ── 3. Clean OECD EPS ─────────────────────────────────────────────────────────
eps_clean <- eps_raw %>%
  filter(CLIM_POL == 'EPS') %>%
  select(
    country_iso3 = REF_AREA,
    year         = TIME_PERIOD,
    eps          = OBS_VALUE
  ) %>%
  filter(year >= 2008, year <= 2023)

# check countries covered
unique(eps_clean$country_iso3)
nrow(eps_clean)

# ── 4. Clean V-Dem ────────────────────────────────────────────────────────────
vdem_clean <- vdem_raw %>%
  select(
    country_iso3  = country_text_id,
    year,
    v2x_polyarchy,
    v2x_libdem,
    v2x_corr
  ) %>%
  filter(year >= 2008, year <= 2023)

# ── 5. Merge both into df_panel ───────────────────────────────────────────────
# need ISO3 country code in df_panel — currently have ISO2 in registry_id
# create ISO3 lookup
iso_lookup <- c(
  AT='AUT', BE='BEL', BG='BGR', CY='CYP', CZ='CZE',
  DE='DEU', DK='DNK', EE='EST', ES='ESP', FI='FIN',
  FR='FRA', GB='GBR', GR='GRC', HR='HRV', HU='HUN',
  IE='IRL', IS='ISL', IT='ITA', LI='LIE', LT='LTU',
  LU='LUX', LV='LVA', MT='MLT', NL='NLD', NO='NOR',
  PL='POL', PT='PRT', RO='ROU', SE='SWE', SI='SVN',
  SK='SVK', XI='GBR'  # Northern Ireland → UK
)

df_panel <- df_panel %>%
  mutate(country_iso3 = iso_lookup[registry_id])

# merge EPS
df_panel <- df_panel %>%
  left_join(eps_clean, by = c('country_iso3', 'year'))

# merge V-Dem
df_panel <- df_panel %>%
  left_join(vdem_clean, by = c('country_iso3', 'year'))

# check coverage
df_panel %>%
  summarise(
    n = n(),
    has_eps  = sum(!is.na(eps)),
    has_vdem = sum(!is.na(v2x_polyarchy)),
    pct_eps  = round(mean(!is.na(eps)) * 100, 1),
    pct_vdem = round(mean(!is.na(v2x_polyarchy)) * 100, 1)
  )

# which countries are missing EPS
df_panel %>%
  group_by(registry_id, country_iso3) %>%
  summarise(
    pct_eps = round(mean(!is.na(eps)) * 100, 1),
    n = n(),
    .groups = 'drop'
  ) %>%
  filter(pct_eps < 100) %>%
  arrange(pct_eps)

df_panel %>%
  filter(registry_id %in% c('GR', 'LU')) %>%
  group_by(registry_id, year) %>%
  summarise(has_eps = any(!is.na(eps)), .groups = 'drop') %>%
  filter(has_eps) %>%
  group_by(registry_id) %>%
  summarise(first_year = min(year), last_year = max(year))

#merge to all  ───────────────────────────────────────────────
# add iso3 to df_all
df_all <- df_all %>%
  mutate(country_iso3 = iso_lookup[registry_id])

# merge EPS
df_all <- df_all %>%
  left_join(eps_clean, by = c('country_iso3', 'year'))

# forward fill EPS
df_all <- df_all %>%
  arrange(registry_id, year) %>%
  group_by(registry_id) %>%
  fill(eps, .direction = 'down') %>%
  ungroup()

# merge V-Dem
df_all <- df_all %>%
  left_join(vdem_clean, by = c('country_iso3', 'year'))

# check coverage by sector
df_all %>%
  group_by(sector) %>%
  summarise(
    n = n(),
    pct_eps  = round(mean(!is.na(eps)) * 100, 1),
    pct_vdem = round(mean(!is.na(v2x_polyarchy)) * 100, 1)
  )

# ── Correlation plots ───────────────────────────────────────────────────────────────

png('output/figures/20_correlation_matrix_by_sector_full.png',
    width = 20, height = 14, units = 'in', res = 300)
par(mfrow = c(2, 3))

for (s in sectors_to_plot) {
  
  cor_data <- df_all %>%
    filter(sector == s) %>%
    select(surplus_norm, log_verified,
           state_ownership_pct, state_owned_binary,
           foreign_owned, eps, v2x_polyarchy) %>%
    drop_na()
  
  n_obs <- nrow(cor_data)
  
  if (n_obs > 50) {
    cor_mat <- cor(cor_data)
    corrplot(cor_mat,
             method = 'color',
             type = 'upper',
             addCoef.col = 'black',
             number.cex = 0.55,
             tl.cex = 0.55,
             title = paste0(s, ' (n=', n_obs, ')'),
             mar = c(0,0,2,0))
  }
}

# all sectors combined
cor_data_all <- df_all %>%
  select(surplus_norm, log_verified,
         state_ownership_pct, state_owned_binary,
         foreign_owned, eps, v2x_polyarchy) %>%
  drop_na()

cor_mat_all <- cor(cor_data_all)
corrplot(cor_mat_all,
         method = 'color',
         type = 'upper',
         addCoef.col = 'black',
         number.cex = 0.55,
         tl.cex = 0.55,
         title = paste0('All sectors (n=', nrow(cor_data_all), ')'),
         mar = c(0,0,2,0))

dev.off()

# power sector full matrix
cor_vars_power <- df_panel %>%
  select(surplus_norm, log_verified, log_emissions_intensity,
         state_ownership_pct, state_owned_binary,
         foreign_owned, eps, v2x_polyarchy) %>%
  drop_na()

nrow(cor_vars_power)
cor_matrix_power <- cor(cor_vars_power)

png('output/figures/19_correlation_matrix_power_with_indices.png',
    width = 12, height = 10, units = 'in', res = 300)
corrplot(cor_matrix_power,
         method = 'color',
         type = 'upper',
         addCoef.col = 'black',
         number.cex = 0.65,
         tl.cex = 0.65,
         title = 'Power sector: all variables including policy indices',
         mar = c(0,0,1,0))
dev.off()

#should i pull more data?

# check how many unique BvD IDs exist across all sectors
df_all %>%
  filter(!is.na(bvdId), bvdId != '') %>%
  distinct(bvdId) %>%
  nrow()

# compare to what you already pulled
# your orbis pulls covered ~5,052 firms

existing_bvd <- df_panel %>%
  filter(!is.na(bvdId), bvdId != '') %>%
  distinct(bvdId) %>%
  pull(bvdId)

# now check sectors of new BvD IDs
df_all %>%
  filter(!is.na(bvdId), bvdId != '') %>%
  filter(!bvdId %in% existing_bvd) %>%
  group_by(sector) %>%
  summarise(
    n_installations = n_distinct(installation_id),
    n_firms = n_distinct(bvdId)
  ) %>%
  arrange(desc(n_firms))
