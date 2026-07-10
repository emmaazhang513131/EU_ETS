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

# new data

library(readxl)

orbis_sh_new <- read_excel('data/raw/Export_1.xlsx', sheet = 'Results')

names(orbis_sh_new) %>% head(20)
nrow(orbis_sh_new)

head(orbis_sh_new$...1, 10)

# also check remaining column names
names(orbis_sh_new)

i exported each value to a distinct cell i hope that's okayThat's fine for the shareholder name and type columns — but for the Direct % columns with biennial dates it means all the date columns will be spread across separate columns rather than rows, similar to your first export.
So you'll get something like:
BvD ID | SH1_Name | SH1_Type | SH1_Direct_2006 | SH1_Direct_2008 | SH2_Name | SH2_Type | SH2_Direct_2006...
This is actually workable — it's wide format but we can pivot it to long in R. The key question is whether "each value to a distinct cell" gave you multiple shareholders per firm spread across columns, or still just one shareholder per row.
When it downloads, load it and check:
  rorbis_sh_new <- read_excel('data/raw/orbis_sh_new.xlsx', sheet = 'Results') %>%
  filter(!is.na(`BvD ID number`))

nrow(orbis_sh_new)
n_distinct(orbis_sh_new$`BvD ID number`)
names(orbis_sh_new) %>% head(30)
If nrow equals n_distinct(BvD ID number) — one row per firm, shareholders in columns. If nrow is much larger — multiple rows per firm, which is what we want.
Tell me what you see when it downloads.i named it Export_1 and it'ss in raw now. can we explore it first to make ssure it got multiple shareholdersrlibrary(readxl)

orbis_sh_new <- read_excel('data/raw/Export_1.xlsx', sheet = 'Results') %>%
  filter(!is.na(`BvD ID number`))

nrow(orbis_sh_new)
n_distinct(orbis_sh_new$`BvD ID number`)
names(orbis_sh_new) %>% head(40)
Tell me the row count vs distinct firm count and the first 40 column names — that'll tell us immediately whether we got multiple shareholders per firm and whether the biennial date columns are there.Error in filter():
  ℹ In argument: !is.na(BvD ID number).
Caused by error:
  ! object 'BvD ID number' not found
Run rlang::last_trace() to see where the error occurred.
There were 50 or more warnings (use warnings() to see the first 50)The column name is probably slightly different — check what columns actually loaded:
  rorbis_sh_new <- read_excel('data/raw/Export_1.xlsx', sheet = 'Results')

names(orbis_sh_new) %>% head(20)
nrow(orbis_sh_new)

# also check remaining column names
names(orbis_sh_new)
nrow(orbis_sh_new)
n_distinct(orbis_sh_new$...1)


# rename ...1 to bvdId
orbis_sh_new <- orbis_sh_new %>%
  rename(bvdId = `...1`) %>%
  filter(!is.na(bvdId))

#second export
orbis_sh_2 <- read_excel('data/raw/Export_2.xlsx', sheet = 'Results') %>%
  rename(bvdId = `...1`) %>%
  filter(!is.na(bvdId))

nrow(orbis_sh_2)
n_distinct(orbis_sh_2$bvdId)

# stack both batches
orbis_sh_all <- bind_rows(orbis_sh_new, orbis_sh_2)

nrow(orbis_sh_all)
n_distinct(orbis_sh_all$bvdId)

# check Enel now
orbis_sh_all %>%
  filter(grepl("ENEL SPA", `Company name Latin alphabet`, ignore.case = TRUE)) %>%
  select(`Company name Latin alphabet`, `SH - Name`, `SH - Type`,
         `SH - Direct % 01/2006`, `SH - Direct % 01/2012`,
         `SH - Direct % 01/2022`) %>%
  print(width = Inf)




