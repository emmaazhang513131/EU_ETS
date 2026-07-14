library(tidyverse)
library(data.table)

# ── 1. Load raw data ──────────────────────────────────────────────────────────
installations <- fread('data/raw/installation.csv')
compliance    <- fread('data/raw/compliance.csv')

# ── 2. Filter installations to power sector only ──────────────────────────────
installations_clean <- installations %>%
  filter(
    isAircraftOperator == FALSE,
    isMaritimeOperator == FALSE,
    activity_id %in% c(1, 20)
  )

# ── 3. Merge installation and compliance ──────────────────────────────────────
df_merged <- compliance %>%
  inner_join(
    installations_clean %>%
      select(id, registry_id, activity_id,
             parentCompany, nace_id,
             latitudeGoogle, longitudeGoogle),
    by = c('installation_id' = 'id')
  )

# ── 4. Clean compliance observations ─────────────────────────────────────────

df_clean <- df_merged %>%
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
      euetsPhase == '2021-2030' ~ 4,
      TRUE ~ NA_real_
    )
  )
nrow(df_clean)
table(df_clean$phase)
table(df_clean$compliance_id)

# how many unique installations remain
n_distinct(df$installation_id)

# construct dependent variables at installation level
df_clean <- df %>%
  mutate(
    surplus_raw  = allocatedTotal - verified,
    surplus_norm = surplus_raw / verified,
    coverage_ratio = allocatedTotal / verified,
    compliant = as.integer(compliance_id %in% c('A', 'E'))
  ) %>%
  arrange(installation_id, year) %>%
  group_by(installation_id) %>%
  mutate(
    emissions_growth = (verified - lag(verified)) / lag(verified)
  ) %>%
  ungroup()

# find the extreme outliers
df_clean %>%
  filter(surplus_norm > 100) %>%
  select(installation_id, year, verified, 
         allocatedTotal, surplus_norm) %>%
  arrange(desc(surplus_norm)) %>%
  head(20)

quantile(df_clean$surplus_norm, 
         probs = c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99),
         na.rm = TRUE)

# trim at 99th percentile on top, keep -1 floor on bottom
df_clean <- df_clean %>%
  filter(surplus_norm >= -1, surplus_norm <= 10)

nrow(df_clean)
summary(df_clean$surplus_norm)

# ── 5. Descriptive Plots ─────────────────────────────────────────

# Distribution of surplus by phase
df_clean %>%
  filter(!is.na(phase)) %>%
  mutate(phase_label = paste('Phase', phase)) %>%
  ggplot(aes(x = surplus_norm)) +
  geom_histogram(bins = 60, fill = 'steelblue', color = 'white', alpha = 0.8) +
  facet_wrap(~phase_label, scales = 'free_y') +
  labs(
    title = 'Distribution of normalized compliance surplus by phase',
    subtitle = 'EU ETS power sector installations',
    x = '(Allocated - Verified) / Verified',
    y = 'Count'
  ) +
  theme_minimal()

ggsave('output/figures/01_surplus_distribution_by_phase.png', 
       width = 12, height = 6, dpi = 300)

df_clean %>%
  group_by(year) %>%
  summarise(
    total_emissions = sum(verified, na.rm = TRUE) / 1e6
  ) %>%
  ggplot(aes(x = year, y = total_emissions)) +
  geom_line(color = 'steelblue', linewidth = 1) +
  geom_point(color = 'steelblue', size = 2) +
  geom_vline(xintercept = c(2008, 2013, 2021), 
             linetype = 'dashed', color = 'grey50') +
  annotate('text', x = 2008.2, y = 1550, 
           label = 'Phase 2', hjust = 0, size = 3) +
  annotate('text', x = 2013.2, y = 1550, 
           label = 'Phase 3', hjust = 0, size = 3) +
  annotate('text', x = 2021.2, y = 1550, 
           label = 'Phase 4', hjust = 0, size = 3) +
  labs(
    title = 'Total verified emissions over time',
    subtitle = 'EU ETS power sector installations',
    x = 'Year',
    y = 'Mt CO2'
  ) +
  theme_minimal()

ggsave('output/figures/02_emissions_over_time.png',
       width = 10, height = 6, dpi = 300)

# 5.3 Median surplus over time 
df_clean %>%
  group_by(year, phase) %>%
  summarise(
    median_surplus = median(surplus_norm, na.rm = TRUE),
    mean_surplus   = mean(surplus_norm, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = year, y = median_surplus)) +
  geom_line(color = 'steelblue', linewidth = 1) +
  geom_point(color = 'steelblue', size = 2) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
  geom_vline(xintercept = c(2008, 2013, 2021),
             linetype = 'dashed', color = 'grey50') +
  annotate('text', x = 2008.2, y = 0.8, label = 'Phase 2', hjust = 0, size = 3) +
  annotate('text', x = 2013.2, y = 0.8, label = 'Phase 3', hjust = 0, size = 3) +
  annotate('text', x = 2021.2, y = 0.8, label = 'Phase 4', hjust = 0, size = 3) +
  labs(
    title = 'Median normalized compliance surplus over time',
    subtitle = 'EU ETS power sector installations — red line = zero surplus',
    x = 'Year',
    y = 'Median (Allocated - Verified) / Verified'
  ) +
  theme_minimal()

ggsave('output/figures/03_median_surplus_over_time.png',
       width = 10, height = 6, dpi = 300)

# 5.4 Non-compliance rate by country 
df_clean %>%
  group_by(registry_id) %>%
  summarise(
    noncompliance_rate = mean(compliance_id == 'B', na.rm = TRUE),
    n_obs = n()
  ) %>%
  filter(n_obs > 100) %>%  # drop tiny countries
  arrange(desc(noncompliance_rate)) %>%
  ggplot(aes(x = reorder(registry_id, noncompliance_rate), 
             y = noncompliance_rate * 100)) +
  geom_col(fill = 'steelblue', alpha = 0.8) +
  coord_flip() +
  labs(
    title = 'Non-compliance rate by country',
    subtitle = 'EU ETS power sector installations — share of installation-years with code B',
    x = 'Country',
    y = 'Non-compliance rate (%)'
  ) +
  theme_minimal()

ggsave('output/figures/04_noncompliance_by_country.png',
       width = 8, height = 8, dpi = 300)

# Italy and France non-compliance rates are very high, which is interesting 
# because they are usually countries with high regulatory agencies

saveRDS(df_clean, 'data/processed/df_clean_installation.rds')


# ── 6. Load and explore account table ─────────────────────────────────────────
accounts <- fread('data/raw/account.csv')

# filter to operator holding accounts only
oha <- accounts %>%
  filter(accountType_id %in% c('100-7', '120-0')) %>%  
  # 100-7 = active OHA, 120-0 = former OHA (pre-2013)
  filter(installation_id != '') %>%  # must be linked to an installation
  filter(bvdId != '') %>%            # must have a BvD ID
  select(installation_id, bvdId, accountType_id, 
         companyRegistrationNumber, isOpen)

# check for installations with multiple BvD IDs
oha %>%
  group_by(installation_id) %>%
  summarise(n_bvd = n_distinct(bvdId)) %>%
  filter(n_bvd > 1) %>%
  nrow()

# resolve multiple BvD IDs by preferring active OHA (100-7) over former (120-0)
oha_clean <- oha %>%
  arrange(installation_id, 
          ifelse(accountType_id == '100-7', 0, 1)) %>%  # active first
  group_by(installation_id) %>%
  slice(1) %>%  # take first row (active OHA preferred)
  ungroup()

# verify
n_distinct(oha_clean$installation_id)
n_distinct(oha_clean$bvdId)

# check no more duplicates
oha_clean %>%
  group_by(installation_id) %>%
  summarise(n_bvd = n_distinct(bvdId)) %>%
  filter(n_bvd > 1) %>%
  nrow()

# how many of our power installations have a BvD ID
power_ids <- df_clean %>% 
  distinct(installation_id)

power_ids %>%
  left_join(oha_clean, by = 'installation_id') %>%
  summarise(
    total = n(),
    has_bvd = sum(!is.na(bvdId) & bvdId != ''),
    missing_bvd = sum(is.na(bvdId) | bvdId == '')
  )

# ── 7. Merge BvD IDs into main dataframe ──────────────────────────────────────
df_clean <- df_clean %>%
  left_join(
    oha_clean %>% select(installation_id, bvdId),
    by = 'installation_id'
  )

# verify
n_distinct(df_clean$bvdId[df_clean$bvdId != '' & !is.na(df_clean$bvdId)])

# save updated version
saveRDS(df_clean, 'data/processed/df_clean_with_bvd.rds')

# coverage by country
df_clean %>%
  mutate(has_bvd = !is.na(bvdId) & bvdId != '') %>%
  group_by(registry_id) %>%
  summarise(
    coverage = round(mean(has_bvd) * 100, 1),
    n_obs = n()
  ) %>%
  arrange(coverage)

df_clean %>%
  mutate(has_bvd = !is.na(bvdId) & bvdId != '') %>%
  group_by(registry_id) %>%
  summarise(
    coverage = round(mean(has_bvd) * 100, 1),
    n_obs = n()
  ) %>%
  arrange(coverage) %>%
  print(n = 30)

# concerning for GR (19%), IT (78%), GB (84%), IE (78%), especially Greece

# how many observations remain if we restrict to has_bvd only
# and how many unique installations
df_clean %>%
  filter(!is.na(bvdId) & bvdId != '') %>%
  summarise(
    n_obs = n(),
    n_installations = n_distinct(installation_id),
    n_firms = n_distinct(bvdId)
  )

saveRDS(df_clean, 'data/processed/df_clean_with_bvd.rds')

# split BvD IDs into two batches for Orbis upload
bvd_ids <- df_clean %>%
  filter(!is.na(bvdId) & bvdId != '') %>%
  distinct(bvdId)

# split in half
halfway <- floor(nrow(bvd_ids) / 2)

bvd_batch1 <- bvd_ids[1:halfway, ]
bvd_batch2 <- bvd_ids[(halfway + 1):nrow(bvd_ids), ]

# check sizes
nrow(bvd_batch1)
nrow(bvd_batch2)

# export both
write.csv(bvd_batch1, 'data/processed/bvd_ids_batch1.csv', row.names = FALSE)
write.csv(bvd_batch2, 'data/processed/bvd_ids_batch2.csv', row.names = FALSE)