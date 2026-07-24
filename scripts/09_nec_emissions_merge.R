# 09_nec_emissions_merge.R
# Builds annual NOx/SOx/PM emissions series from EEA's National Emission 
# Ceilings (NEC) Directive inventory data, as a proxy for the "non-market 
# based policies" component of EPS (ELV NOx/SOx/PM), to test whether 
# ownership interacts more strongly with command-and-control style 
# regulation than with carbon price (see conversation notes, EPS Figure 1 
# sub-index breakdown)

library(readr)   # read_csv
library(dplyr)   # filter, mutate, group_by, etc.
library(tidyr)   # pivot_longer, if the file turns out wide-format like the tax data was

raw <- read_tsv("data/raw/NEC_2026.csv", 
                col_types = cols(value = col_double(), unit = col_character()))


# ---- 3. filter to target pollutants + national total only ----

nec_raw <- raw 

nec_clean <- nec_raw %>%
  filter(pollutantName %in% c("NOx", "SOx", "PM2.5"),
         parentSectorCode == "NATIONAL TOTAL") %>%
  group_by(countryCode, year, pollutantName) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  select(country = countryCode, year, pollutantName, value)

# ---- 4. pivot to one column per pollutant ----
nec_wide <- nec_clean %>%
  pivot_wider(names_from = pollutantName, values_from = value,
              names_prefix = "emis_") %>%
  rename_with(~ tolower(.), starts_with("emis_"))

# ---- 5. merge into panel ----
df_panel_merged <- readRDS("data/processed/df_panel_merged.rds") %>%
  select(-any_of(c("emis_nox", "emis_sox", "emis_pm2.5"))) %>%
  left_join(nec_wide, by = c("country", "year"))

saveRDS(df_panel_merged, "data/processed/df_panel_merged.rds")

# ---- 6. sanity check ----
df_panel_merged %>%
  distinct(country, year, emis_nox, emis_sox, `emis_pm2.5`) %>%
  filter(country %in% c("DE", "PL")) %>%
  arrange(country, year) %>%
  print(n = 20)



# nec targets 

nec_targets <- read_csv("data/raw/nec_annex2_tableA_targets_iso2.csv")

df_panel_merged <- df_panel_merged %>%
  select(-any_of(c(
    "so2_target_2020_29.x", "so2_target_2030.x", "nox_target_2020_29.x", 
    "nox_target_2030.x", "nmvoc_target_2020_29.x", "nmvoc_target_2030.x",
    "so2_target_2020_29.y", "so2_target_2030.y", "nox_target_2020_29.y",
    "nox_target_2030.y", "nmvoc_target_2020_29.y", "nmvoc_target_2030.y"
  ))) %>%
  left_join(nec_targets, by = "country")

# confirm clean this time - should show exactly 6 columns, no .x/.y
colnames(df_panel_merged) %>% grep("target", ., value = TRUE)

# ---- sanity check ----
df_panel_merged %>%
  distinct(country, so2_target_2020_29, nox_target_2020_29, nmvoc_target_2020_29) %>%
  filter(country %in% c("DE", "PL", "SE", "IS")) %>%
  arrange(country)

#save
saveRDS(df_panel_merged, "data/processed/df_panel_merged.rds")

### by trends

library(MASS); library(dplyr); library(tidyr); library(broom); library(purrr)

# ---- 1. fit pre-2020 trend per country, using NOx only ----


nox_trends <- nec_clean %>%
  filter(pollutantName == "NOx", year >= 2010, year <= 2019, value > 0) %>%
  group_by(country) %>%
  filter(n() >= 7) %>%
  ungroup() %>%
  nest(data = -country) %>%
  mutate(
    model = map(data, ~ rlm(log(value) ~ year, data = .x)),
    log_slope = map_dbl(model, ~ coef(.x)["year"])
  ) %>%
  dplyr::select(country, log_slope)   # explicit now

# ---- BAU projection using compound annual rate instead of linear subtraction ----
baseline_2005 <- nec_clean %>%
  filter(pollutantName == "NOx", year == 2005) %>%
  dplyr::select(country, baseline_2005 = value)

nox_ambition <- nox_trends %>%
  left_join(baseline_2005, by = "country") %>%
  mutate(
    bau_2025 = baseline_2005 * exp(log_slope * (2025 - 2005)),
    bau_pct_reduction = (baseline_2005 - bau_2025) / baseline_2005
  ) %>%
  dplyr::select(country, bau_pct_reduction)

nox_targets_only <- df_panel_merged %>% 
  dplyr::select(country, nox_target_2020_29) %>%
  distinct()

nox_ambition_final <- nox_ambition %>%
  left_join(nox_targets_only, by = "country") %>%
  mutate(nox_ambition_gap = nox_target_2020_29 - bau_pct_reduction)

print(nox_ambition_final, n = 30)

#merge
df_panel_merged <- df_panel_merged %>%
  dplyr::select(-any_of("nox_ambition_gap")) %>%
  left_join(nox_ambition_final %>% dplyr::select(country, nox_ambition_gap), by = "country")

saveRDS(df_panel_merged, "data/processed/df_panel_merged.rds")

