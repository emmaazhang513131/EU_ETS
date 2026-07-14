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

# ── 12. Load and parse new shareholder export (Export_1 and Export_2) ─────────

library(readxl)

#reading in new export from orbis

orbis_sh_1 <- read_excel('data/raw/Export_1.xlsx', sheet = 'Results')
orbis_sh_2 <- read_excel('data/raw/Export_2.xlsx', sheet = 'Results')

orbis_sh_1_clean <- read_excel('data/raw/Export_1.xlsx', sheet = 'Results') %>%
  rename(bvdId = `...1`) %>%
  fill(bvdId, `Company name Latin alphabet`, .direction = 'down') %>%
  filter(!is.na(`SH - Name`))

orbis_sh_2_clean <- orbis_sh_2 %>%
  rename(bvdId = `...1`) %>%
  # forward fill firm identifiers down through shareholder rows
  fill(bvdId, `Company name Latin alphabet`, .direction = 'down') %>%
  # now drop rows that are truly empty (no shareholder data)
  filter(!is.na(`SH - Name`))

orbis_sh_1_clean <- orbis_sh_1_clean %>%
  mutate(across(starts_with('Number of employees'), as.character),
         across(starts_with('Operating revenue'), as.character),
         across(starts_with('SH - Direct %'), as.character))

orbis_sh_2_clean <- orbis_sh_2_clean %>%
  mutate(across(starts_with('Number of employees'), as.character),
         across(starts_with('Operating revenue'), as.character),
         across(starts_with('SH - Direct %'), as.character))

# get BvD IDs from original orbis_raw which has proper BvD IDs
bvd_lookup <- orbis_raw %>%
  select(bvdId = `BvD ID number`, 
         `Company name Latin alphabet`) %>%
  filter(!is.na(bvdId))

# merge BvD IDs into new shareholder data using company name
orbis_sh_1_fixed <- orbis_sh_1_clean %>%
  select(-bvdId) %>%  # drop the wrong row number column
  left_join(bvd_lookup, by = 'Company name Latin alphabet')

orbis_sh_2_fixed <- orbis_sh_2_clean %>%
  select(-bvdId) %>%
  left_join(bvd_lookup, by = 'Company name Latin alphabet')

# stack both fixed exports
orbis_sh_all <- bind_rows(
  orbis_sh_1_fixed %>%
    mutate(across(starts_with('SH - Direct %'), as.character)),
  orbis_sh_2_fixed %>%
    mutate(across(starts_with('SH - Direct %'), as.character))
) %>%
  filter(!is.na(bvdId))

nrow(orbis_sh_all)
n_distinct(orbis_sh_all$bvdId)

# check state shareholder type distribution
table(orbis_sh_all$`SH - Type`) %>%
  sort(decreasing = TRUE) %>%
  head(10)

# ── construct time-varying state ownership ────────────────────────────────────
state_types <- c("Public authority, state, government", "Public")

# pivot SH Direct % columns to long format
sh_long <- orbis_sh_all %>%
  select(bvdId, `Company name Latin alphabet`, 
         `SH - Name`, `SH - Type`,
         starts_with('SH - Direct %')) %>%
  pivot_longer(
    cols = starts_with('SH - Direct %'),
    names_to = 'date',
    values_to = 'sh_direct_pct'
  ) %>%
  mutate(
    year = as.integer(str_extract(date, '\\d{4}')),
    sh_direct_pct = suppressWarnings(
      as.numeric(na_if(na_if(sh_direct_pct, 'n.a.'), '-'))
    ),
    is_state = `SH - Type` %in% state_types
  )

# construct state ownership per firm per year
# sum all government shareholder stakes, cap at 100
state_ownership_tv <- sh_long %>%
  group_by(bvdId, `Company name Latin alphabet`, year) %>%
  summarise(
    state_ownership_pct_tv = min(
      sum(sh_direct_pct[is_state], na.rm = TRUE), 
      100
    ),
    state_owned_binary_tv = as.integer(any(is_state & !is.na(sh_direct_pct))),
    n_gov_shareholders = sum(is_state & !is.na(sh_direct_pct)),
    .groups = 'drop'
  )

# check distribution
summary(state_ownership_tv$state_ownership_pct_tv)
table(state_ownership_tv$state_owned_binary_tv)

# how many firms show ownership changes over time
state_ownership_tv %>%
  group_by(bvdId) %>%
  summarise(
    min_own = min(state_ownership_pct_tv, na.rm = TRUE),
    max_own = max(state_ownership_pct_tv, na.rm = TRUE),
    changed = min_own != max_own
  ) %>%
  filter(changed) %>%
  nrow()

# expand biennial snapshots to annual panel
state_ownership_annual <- state_ownership_tv %>%
  group_by(bvdId) %>%
  complete(year = 2008:2023) %>%
  arrange(bvdId, year) %>%
  fill(state_ownership_pct_tv, state_owned_binary_tv,
       .direction = 'down') %>%
  ungroup()

# merge into df_panel
df_panel <- df_panel %>%
  left_join(
    state_ownership_annual %>%
      select(bvdId, year, state_ownership_pct_tv, state_owned_binary_tv),
    by = c('bvdId', 'year')
  )

# compare old vs new
df_panel %>%
  summarise(
    mean_old = mean(state_ownership_pct, na.rm = TRUE),
    mean_new = mean(state_ownership_pct_tv, na.rm = TRUE),
    cor_old_new = cor(state_ownership_pct, state_ownership_pct_tv,
                      use = 'complete.obs')
  )

# distribution of new time-varying variable
quantile(df_panel$state_ownership_pct_tv, 
         probs = c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1),
         na.rm = TRUE)

# how many observations are exactly zero
mean(df_panel$state_ownership_pct_tv == 0, na.rm = TRUE)

# how many have some state ownership
mean(df_panel$state_ownership_pct_tv > 0, na.rm = TRUE)

# distribution among state-owned firms only
df_panel %>%
  filter(state_ownership_pct_tv > 0) %>%
  pull(state_ownership_pct_tv) %>%
  summary()

# distribution of time-varying state ownership
df_panel %>%
  filter(!is.na(state_ownership_pct_tv),
         state_ownership_pct_tv > 0) %>%
  ggplot(aes(x = state_ownership_pct_tv)) +
  geom_histogram(bins = 50, fill = 'steelblue', color = 'white', alpha = 0.8) +
  labs(
    title = 'Distribution of state ownership % (time-varying)',
    subtitle = 'Firms with any state ownership only (excluding zeros)',
    x = 'State ownership %',
    y = 'Count'
  ) +
  theme_minimal()

# compare old vs new side by side
df_panel %>%
  filter(!is.na(state_ownership_pct),
         !is.na(state_ownership_pct_tv)) %>%
  select(state_ownership_pct, state_ownership_pct_tv) %>%
  pivot_longer(everything(), names_to = 'variable', values_to = 'value') %>%
  filter(value > 0) %>%
  ggplot(aes(x = value, fill = variable)) +
  geom_histogram(bins = 50, alpha = 0.6, position = 'identity') +
  scale_fill_manual(values = c('#E8534A', '#3498DB'),
                    labels = c('CSH (old)', 'SH time-varying (new)')) +
  labs(
    title = 'State ownership distribution: old vs new variable',
    subtitle = 'Firms with any state ownership only',
    x = 'State ownership %',
    y = 'Count',
    fill = 'Variable'
  ) +
  theme_minimal()

saveRDS(df_panel, 'data/processed/df_panel_merged.rds')
