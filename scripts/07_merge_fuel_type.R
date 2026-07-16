# ── 07_merge_fuel_type.R ──────────────────────────────────────────────────────

library(tidyverse)
library(data.table)

# ── 1. Load panel ─────────────────────────────────────────────────────────────
df_panel <- readRDS('data/processed/df_panel_merged.rds')

# ── 2. Load JRC data ──────────────────────────────────────────────────────────
jrc_units <- read_csv('data/raw/JRC_OPEN_UNITS.csv')
jrc_links <- read_csv('data/raw/JRC_OPEN_LINKAGES.csv')

glimpse(jrc_units)
names(jrc_units)
table(jrc_units$type_g)

glimpse(jrc_links)
names(jrc_links)
head(jrc_links, 20)

# look for EUTL or CITL identifiers
jrc_links %>%
  filter(grepl('EUTL|CITL|eutl|citl', source, ignore.case = TRUE)) %>%
  head(10)

# check what sources are available
table(jrc_links$source)

library(tidyverse)

# ── 3. Prepare JRC data for matching ─────────────────────────────────────────
# keep only fossil fuel plants relevant to ETS power sector
jrc_fossil <- jrc_units %>%
  filter(grepl('Fossil|Biomass|Peat|Waste', type_g, ignore.case = TRUE)) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  # simplify fuel type to main categories
  mutate(fuel_type = case_when(
    grepl('coal|lignite', type_g, ignore.case = TRUE) ~ 'coal',
    grepl('gas', type_g, ignore.case = TRUE)          ~ 'gas',
    grepl('oil', type_g, ignore.case = TRUE)          ~ 'oil',
    grepl('biomass', type_g, ignore.case = TRUE)      ~ 'biomass',
    grepl('peat', type_g, ignore.case = TRUE)         ~ 'peat',
    grepl('waste', type_g, ignore.case = TRUE)        ~ 'waste',
    TRUE                                               ~ 'other'
  )) %>%
  # one row per plant (aggregate units to plant level)
  group_by(eic_p, name_p, country, lat, lon) %>%
  summarise(
    fuel_type = first(fuel_type),
    capacity_p = first(capacity_p),
    .groups = 'drop'
  )

nrow(jrc_fossil)
table(jrc_fossil$fuel_type)

# ── 4. Prepare EUTL coordinates ───────────────────────────────────────────────
eutl_coords <- installations_raw %>%
  filter(!is.na(latitudeGoogle), !is.na(longitudeGoogle)) %>%
  select(
    installation_id = id,
    install_name = name,
    registry_id,
    lat_eutl = latitudeGoogle,
    lon_eutl = longitudeGoogle
  ) %>%
  distinct()

nrow(eutl_coords)

install.packages('geosphere')
library(geosphere)

# ── 5. Nearest neighbor matching within country ───────────────────────────────

# standardize country names to ISO2 for joining
# JRC uses full country names, EUTL uses ISO2 registry_id
country_lookup <- c(
  'Austria' = 'AT', 'Belgium' = 'BE', 'Bulgaria' = 'BG',
  'Croatia' = 'HR', 'Cyprus' = 'CY', 'Czech Republic' = 'CZ',
  'Denmark' = 'DK', 'Estonia' = 'EE', 'Finland' = 'FI',
  'France' = 'FR', 'Germany' = 'DE', 'Greece' = 'GR',
  'Hungary' = 'HU', 'Iceland' = 'IS', 'Ireland' = 'IE',
  'Italy' = 'IT', 'Latvia' = 'LV', 'Lithuania' = 'LT',
  'Luxembourg' = 'LU', 'Malta' = 'MT', 'Netherlands' = 'NL',
  'Norway' = 'NO', 'Poland' = 'PL', 'Portugal' = 'PT',
  'Romania' = 'RO', 'Slovakia' = 'SK', 'Slovenia' = 'SI',
  'Spain' = 'ES', 'Sweden' = 'SE', 'United Kingdom' = 'GB',
  'Liechtenstein' = 'LI'
)

jrc_fossil <- jrc_fossil %>%
  mutate(registry_id = country_lookup[country]) %>%
  filter(!is.na(registry_id))

# match each EUTL installation to nearest JRC plant within same country
match_results <- eutl_coords %>%
  left_join(
    jrc_fossil %>%
      select(registry_id, lat_jrc = lat, lon_jrc = lon,
             fuel_type, capacity_p, name_jrc = name_p),
    by = 'registry_id',
    relationship = 'many-to-many'
  ) %>%
  filter(!is.na(lat_jrc)) %>%
  rowwise() %>%
  mutate(
    dist_km = distHaversine(
      c(lon_eutl, lat_eutl),
      c(lon_jrc, lat_jrc)
    ) / 1000
  ) %>%
  ungroup() %>%
  group_by(installation_id) %>%
  slice_min(dist_km, n = 1) %>%
  ungroup()

# check distance distribution
summary(match_results$dist_km)

# how many matched within 5km (same plant)
mean(match_results$dist_km < 5, na.rm = TRUE)
mean(match_results$dist_km < 10, na.rm = TRUE)

# how many good matches (within 5km)
good_matches <- match_results %>%
  filter(dist_km < 5)

nrow(good_matches)

# what share of your panel observations have a good match
df_panel %>%
  left_join(good_matches %>% 
              select(installation_id, fuel_type, dist_km),
            by = 'installation_id') %>%
  summarise(
    pct_matched = round(mean(!is.na(fuel_type)) * 100, 1),
    n_matched = sum(!is.na(fuel_type))
  )

# fuel type distribution among matched installations
table(good_matches$fuel_type)

# look at EUTL installation names vs JRC plant names
head(eutl_coords$install_name, 20)
head(jrc_fossil$name_jrc, 20)

#### name matching -----------------------------------------
install.packages('stringdist')
library(stringdist)

# normalize names for matching
eutl_coords <- eutl_coords %>%
  mutate(name_clean = toupper(trimws(install_name)) %>%
           str_replace_all('[^A-Z0-9]', ''))

jrc_fossil <- jrc_fossil %>%
  mutate(name_clean = toupper(trimws(name_p)) %>%
           str_replace_all('[^A-Z0-9]', ''))

# check a sample
head(eutl_coords$name_clean, 10)
head(jrc_fossil$name_clean, 10)

# ── compare matched vs unmatched installations ────────────────────────────────

df_panel_fuel <- df_panel %>%
  left_join(
    good_matches %>%
      select(installation_id, fuel_type, dist_km) %>%
      distinct(installation_id, .keep_all = TRUE),
    by = 'installation_id'
  ) %>%
  mutate(matched = !is.na(fuel_type))

# compare key variables between matched and unmatched
df_panel_fuel %>%
  group_by(matched) %>%
  summarise(
    n_obs = n(),
    n_installs = n_distinct(installation_id),
    mean_verified = mean(verified, na.rm = TRUE),
    median_verified = median(verified, na.rm = TRUE),
    mean_state_own = mean(state_ownership_pct_tv, na.rm = TRUE),
    pct_state_owned = mean(state_ownership_pct_tv > 0, na.rm = TRUE),
    mean_surplus = mean(surplus_norm, na.rm = TRUE),
    pct_foreign = mean(foreign_owned, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  print(width = Inf)

# country breakdown of matched vs unmatched
df_panel_fuel %>%
  group_by(registry_id, matched) %>%
  summarise(n = n_distinct(installation_id), .groups = 'drop') %>%
  pivot_wider(names_from = matched, values_from = n,
              names_prefix = 'matched_') %>%
  mutate(pct_matched = round(matched_TRUE / 
                               (matched_TRUE + matched_FALSE) * 100, 1)) %>%
  arrange(desc(pct_matched)) %>%
  print(n = 30)

# check JRC linkage file for EIC codes
head(jrc_links)

# how many JRC plants have ENTSO-E EIC codes
# eic_g is the generating unit EIC code — same system ENTSO-E uses
nrow(jrc_units)
sum(!is.na(jrc_units$eic_g))

# check how many of your good coordinate matches have EIC codes
good_matches_eic <- good_matches %>%
  left_join(
    jrc_units %>% select(name_p, lat, lon, eic_p, eic_g),
    by = c('name_jrc' = 'name_p',
           'lat_jrc' = 'lat',
           'lon_jrc' = 'lon')
  )

# how many have EIC codes
sum(!is.na(good_matches_eic$eic_p))
mean(!is.na(good_matches_eic$eic_p))


# ── representativeness check of JRC matched sample ────────────────────────────
df_panel_fuel <- df_panel %>%
  left_join(
    good_matches %>%
      select(installation_id, fuel_type, dist_km) %>%
      distinct(installation_id, .keep_all = TRUE),
    by = 'installation_id'
  ) %>%
  mutate(matched = !is.na(fuel_type))

# matched vs unmatched comparison
rep_check <- df_panel_fuel %>%
  group_by(matched) %>%
  summarise(
    n_obs          = n(),
    n_installs     = n_distinct(installation_id),
    mean_verified  = round(mean(verified, na.rm = TRUE)),
    median_verified = round(median(verified, na.rm = TRUE)),
    mean_state_own = round(mean(state_ownership_pct_tv, na.rm = TRUE), 2),
    pct_state_owned = round(mean(state_ownership_pct_tv > 0, na.rm = TRUE), 3),
    mean_surplus   = round(mean(surplus_norm, na.rm = TRUE), 3),
    pct_foreign    = round(mean(foreign_owned, na.rm = TRUE), 3)
  )

print(rep_check, width = Inf)

# country breakdown
country_check <- df_panel_fuel %>%
  group_by(registry_id, matched) %>%
  summarise(n = n_distinct(installation_id), .groups = 'drop') %>%
  pivot_wider(names_from = matched, values_from = n,
              names_prefix = 'matched_') %>%
  mutate(pct_matched = round(matched_TRUE / 
                               (matched_TRUE + matched_FALSE) * 100, 1)) %>%
  arrange(desc(pct_matched))

print(country_check, n = 30)

# save
write_csv(rep_check, 'output/tables/jrc_representativeness.csv')
write_csv(country_check, 'output/tables/jrc_country_coverage.csv')






# ── 8. Pull ENTSO-E generation data ───────────────────────────────────────────

# save EIC crosswalk for ENTSO-E query
eic_crosswalk <- good_matches_eic %>%
  filter(!is.na(eic_p)) %>%
  select(installation_id, install_name, registry_id,
         name_jrc, fuel_type, dist_km,
         eic_p, eic_g) %>%
  distinct()

nrow(eic_crosswalk)

write_csv(eic_crosswalk, 'data/processed/eic_crosswalk.csv')

