# 08_carbon_price_merge.R
# Builds annual EUA carbon price series from ICAP daily data and merges 
# into df_panel_merged for use as a Phase 4 policy stringency proxy 
# (EPS forward-filled/stale post-2020, see 05_eps_merge.R)

library(readr)
library(dplyr)
library(lubridate)

# ---- 1. load and clean ICAP raw export ----
raw <- read_csv("data/raw/icap-graph-price-data-2008-01-30-2026-07-21.csv",
                skip = 2, col_names = FALSE)

eua_daily <- raw %>%
  transmute(
    date          = ymd(X1),
    primary_old   = as.numeric(X5),
    primary_new   = as.numeric(X10),
    secondary_new = as.numeric(X11)
  ) %>%
  mutate(price = coalesce(secondary_new, primary_new, primary_old)) %>%
  filter(!is.na(price)) %>%
  select(date, price)

# ---- 2. aggregate to annual ----
eua_annual <- eua_daily %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(eua_price = mean(price, na.rm = TRUE), n_obs = n())

# sanity check against known benchmarks before merging
# 2021 ~50, 2022 ~81, 2023 ~85
print(eua_annual %>% filter(year %in% 2021:2023))

# ---- 3. merge into panel ----
df_panel_merged <- readRDS("data/processed/df_panel_merged.rds") %>%
  select(-any_of("eua_price")) %>%   # removes it if present, does nothing if not
  left_join(eua_annual %>% select(year, eua_price), by = "year")

saveRDS(df_panel_merged, "data/processed/df_panel_merged.rds")

#---- 4. correlations ----

country_year_data <- df_panel_merged %>%
  distinct(country, year, eps, eua_price)

clean_data <- country_year_data %>%
  filter(!is.na(eps), !is.na(eua_price))

cor.test(clean_data$eps, clean_data$eua_price)

# scale by country 

year_level <- clean_data %>%
  group_by(year) %>%
  summarise(
    eps_mean = mean(eps, na.rm = TRUE),
    eua_price = first(eua_price)  # same value for every row in a year, so just grab one
  )

nrow(year_level)
range(eua_daily$date) 

cor.test(year_level$eps_mean, year_level$eua_price)

unique(df_panel_merged$country)

library(readxl); library(dplyr); library(tidyr); library(stringr)

# ---- 1. load and reshape national carbon tax data ----
tax_wide <- read_excel("data/raw/data_08_2025.xlsx", sheet = "Compliance_Price", skip = 1)

tax_long <- tax_wide %>%
  filter(`Instrument Type` == "Carbon tax") %>%
  select(`Unique ID`, `Name of the initiative`, matches("^\\d{4}$")) %>%  # keep ID + year cols only
  pivot_longer(cols = matches("^\\d{4}$"), names_to = "year", values_to = "national_tax") %>%
  mutate(year = as.integer(year)) %>%
  filter(!is.na(national_tax))

# map initiative names to country codes matching your panel's `country` column
# (adjust this mapping if your df_panel_merged uses different codes, e.g. ISO2 vs full names)
tax_country_map <- tibble::tribble(
  ~`Unique ID`, ~country,
  "Tax_DK", "DK", "Tax_EE", "EE", "Tax_FI", "FI", "Tax_FR", "FR",
  "Tax_IS", "IS", "Tax_IE", "IE", "Tax_LT", "LV",  # World Bank's own docs confirm this ID is Latvia, not Lithuania
  "Tax_LU", "LU", "Tax_NL", "NL", "Tax_NO", "NO", "Tax_PL", "PL",
  "Tax_PT", "PT", "Tax_SL", "SI", "Tax_ES", "ES", "Tax_SE", "SE",
  "Tax_LI", "LI", "Tax_UK", "GB"  # UK -> GB to match your panel's country codes
)
# note: "Tax_LT" in the source data is actually Latvia carbon Tax (per the name column,
# likely a labeling inconsistency in the raw file - verify against `Name of the initiative`
# before trusting this mapping, don't assume the ID codes are reliable)

tax_clean <- tax_long %>%
  inner_join(tax_country_map, by = "Unique ID") %>%
  select(country, year, national_tax)

# ---- 2. merge into panel, filling non-tax countries with 0 (not NA) ----
df_panel_merged <- df_panel_merged %>%
  left_join(tax_clean, by = c("country", "year")) %>%
  mutate(national_tax = replace_na(national_tax, 0),
         combined_carbon_price = eua_price + national_tax)

# correlations 

df_panel_merged %>%
  distinct(country, year, eua_price, national_tax, combined_carbon_price) %>%
  filter(country == "SE") %>%
  arrange(year)

country_year_data_2 <- df_panel_merged %>%
  distinct(country, year, eps, combined_carbon_price)

clean_data_2 <- country_year_data_2 %>%
  filter(!is.na(eps), !is.na(combined_carbon_price))

cor.test(clean_data_2$eps, clean_data_2$combined_carbon_price)

# scale by country 

year_level_2 <- clean_data_2 %>%
  group_by(year) %>%
  summarise(
    eps_mean = mean(eps, na.rm = TRUE),
    carbon_price_mean = mean(combined_carbon_price, na.rm = TRUE)
  )

cor.test(year_level_2$eps_mean, year_level_2$carbon_price_mean)

