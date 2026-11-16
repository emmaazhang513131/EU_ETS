# =============================================================================
# 11_build_cea_panel.R
#
# Stitches CEA CO2 Baseline Database vintages into one long station-year
# panel. Port of build_panel.py, restructured as a config-driven pipeline so
# additional version files (v8-v16, if/when pulled) can be added by adding
# one list entry each -- no changes to the reduction logic itself.
#
# NOTE ON SCOPE: this panel covers grid-connected power stations only, i.e.
# the Thermal Power Plant slice of PAT -- NOT the other ~12 PAT sectors
# (cement, steel, aluminum, etc.), which live in ASI, not CEA. A DiD built
# on this panel alone can only speak to the power subsample of PAT.
#
# NOTE ON SCOPE BREAK: CEA restricted the database to stations >25MW
# starting v8.0 (FY2011-12) -- one year before PAT Cycle I. Pre-v8 years
# (v4-v7, FY2007-08 to FY2010-11) have a broader, less consistent sample
# and should not be pooled with post-2011-12 years without flagging this.
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

RAW_DIR <- "/home/claude/india_data/raw"
CLEAN_DIR <- "/home/claude/india_data/clean_r"
dir.create(CLEAN_DIR, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

harmonize_sector <- function(x) {
  x <- str_trim(str_to_upper(as.character(x)))
  case_when(
    x %in% c("PVT", "PRIVATE") ~ "PRIVATE",
    x %in% c("CENTER", "CENTRAL") ~ "CENTRAL",
    x == "STATE" ~ "STATE",
    TRUE ~ NA_character_   # catches "NAN", "NA", blanks, etc. as true missing
  )
}

#' Merge known typo/whitespace variants of the same physical station,
#' confirmed individually via cross-version name-similarity audit (see
#' chat / build_panel.py for the equivalent Python-side fix and how each
#' pair was verified). Ambiguous near-duplicates (KUTTIADI/KUTTIYADI,
#' PALATANA variants, etc.) are deliberately excluded -- flagged for
#' manual review rather than auto-merged.
normalize_name_aliases <- function(name) {
  aliases <- c(
    "ANUPUR TPP" = "ANUPPUR TPP",
    "DHANU" = "DAHANU",
    "LAKSHDWEEP DG" = "LAKSHADWEEP DG",
    "NASIK" = "NASHIK",
    "NIGRIE" = "NIGRI",
    "PYKARA ULIMATE" = "PYKARA ALIMATE",
    "SURATGARH" = "SURAT GARH",
    "VIJESWARAN GT" = "VIJESWARAM GT",
    "EMCO GMR  WARORA TPP" = "EMCO GMR WARORA TPP",
    "G.I.P.C.L. GT(BARODA CCPP)" = "G.I.P.C.L. GT (BARODA CCPP)",
    "INDIA POWER TPP(HALDIA, HIRANMAYE )" = "INDIA POWER TPP (HALDIA, HIRANMAYE )",
    "SEMBCORP GAYATRI(SGPL TPP)" = "SEMBCORP GAYATRI (SGPL TPP)",
    "DHUVARAN CCPPIII" = "DHUVARAN CCPP-III",
    "TEESTA-III" = "TEESTA -III"
  )
  ifelse(name %in% names(aliases), aliases[name], name)
}

#' Read a sheet as a raw matrix-like tibble (no type coercion on ID cols),
#' and normalize the NAME column in place so raw whitespace inconsistencies
#' (e.g. "RATNAGIRI GAS " vs "RATNAGIRI GAS") never reach the grouping step.
read_raw_sheet <- function(path, sheet, name_col_1idx) {
  df <- read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  df[[name_col_1idx]] <- str_trim(str_to_upper(as.character(df[[name_col_1idx]])))
  # Known readxl/openpyxl parsing divergence on a rich-text cell: readxl
  # drops a space that openpyxl (and the visible source) preserves. Confirmed
  # at the byte level against the raw file -- not a general fix, a specific
  # known case. If this pipeline is extended to more files, re-run the
  # cross-tool name-list diff (see chat) before trusting new station names.
  df[[name_col_1idx]] <- str_replace(df[[name_col_1idx]],
    "^MAHADEV PRASAD STPPADHUNIK POWER$", "MAHADEV PRASAD STPP ADHUNIK POWER")
  df[[name_col_1idx]] <- normalize_name_aliases(df[[name_col_1idx]])
  df
}

#' Drop footer/summary rows appended below the real station data in some
#' sheets (e.g. a row literally named "2022-23", or "Total Number of
#' Units") -- these always have a blank/non-numeric S_NO, which real
#' station rows never do. Apply ONLY to the body (post-header), never to
#' the raw sheet before the header is split off, or the header itself
#' (whose S_NO cell literally contains the text "S_NO") gets deleted.
drop_footer_rows <- function(body, s_no_col_1idx) {
  body[!is.na(suppressWarnings(as.numeric(body[[s_no_col_1idx]]))), ]
}

#' Collapse (possibly many) unit-level rows into one row per station-year.
#' Per station-year: if the UNIT_NO==0 (total) row exists AND has nonzero
#' net generation, use it directly. Otherwise sum whatever unit rows (!=0)
#' have real data for that same station-year. Capacity is handled the same
#' way, from whichever source was actually used, to avoid double-counting.
reduce_unit_rows <- function(long_df) {
  if (nrow(long_df) == 0) return(long_df)

  long_df <- long_df %>%
    mutate(
      NET_GENERATION_GWH = suppressWarnings(as.numeric(NET_GENERATION_GWH)),
      ABSOLUTE_EMISSIONS_TCO2 = suppressWarnings(as.numeric(ABSOLUTE_EMISSIONS_TCO2)),
      CAPACITY_MW = suppressWarnings(as.numeric(CAPACITY_MW))
    )

  descriptive_cols <- intersect(names(long_df), c("STATE", "SECTOR", "SYSTEM", "TYPE", "FUEL1", "FUEL2"))

  long_df %>%
    group_by(NAME, FISCAL_YEAR, SOURCE_VERSION) %>%
    summarise(
      zero_gen = sum(NET_GENERATION_GWH[UNIT_NO == 0], na.rm = TRUE),
      use_zero = any(UNIT_NO == 0) && zero_gen != 0,
      NET_GENERATION_GWH = if (use_zero[1]) sum(NET_GENERATION_GWH[UNIT_NO == 0], na.rm = TRUE)
                           else sum(NET_GENERATION_GWH[UNIT_NO != 0], na.rm = TRUE),
      ABSOLUTE_EMISSIONS_TCO2 = if (use_zero[1]) sum(ABSOLUTE_EMISSIONS_TCO2[UNIT_NO == 0], na.rm = TRUE)
                                else sum(ABSOLUTE_EMISSIONS_TCO2[UNIT_NO != 0], na.rm = TRUE),
      CAPACITY_MW = if (use_zero[1]) sum(CAPACITY_MW[UNIT_NO == 0], na.rm = TRUE)
                    else sum(CAPACITY_MW[UNIT_NO != 0], na.rm = TRUE),
      across(all_of(descriptive_cols), ~ first(na.omit(.x)), .names = "{.col}"),
      .groups = "drop"
    ) %>%
    select(-zero_gen, -use_zero) %>%
    mutate(SPECIFIC_EMISSIONS_TCO2_MWH = if_else(
      NET_GENERATION_GWH != 0,
      ABSOLUTE_EMISSIONS_TCO2 / (NET_GENERATION_GWH * 1000),
      NA_real_
    ))
}

# ---------------------------------------------------------------------------
# Multi-year (wide, repeated-block) parser -- used for v17, v18
# ---------------------------------------------------------------------------

#' id_cols: named list, 1-indexed column positions, must include NAME, UNIT_NO
#' block_start: 1-indexed column where the first year-block begins
#' block_width: number of columns per year block
#' block_map: named list {out_name: 0-indexed offset within block} for the
#'            fields we want (NET_GENERATION_GWH, ABSOLUTE_EMISSIONS_TCO2)
#' n_blocks: number of year blocks in the sheet
parse_multiyear <- function(path, sheet, id_cols, block_start, block_width,
                             block_map, n_blocks, source_version) {
  raw <- read_raw_sheet(path, sheet, name_col_1idx = id_cols$NAME)
  header <- raw[1, ]

  year_labels <- map_chr(0:(n_blocks - 1), function(b) {
    col <- block_start + b * block_width
    lab <- as.character(header[[col]])
    m <- str_extract(lab, "\\d{4}-\\d{2}")
    if (is.na(m)) paste0("block", b) else m
  })

  body <- drop_footer_rows(raw[-1, ], s_no_col_1idx = id_cols$S_NO)

  out <- map_dfr(0:(n_blocks - 1), function(b) {
    col0 <- block_start + b * block_width
    tibble(
      NAME = body[[id_cols$NAME]],
      UNIT_NO = suppressWarnings(as.numeric(body[[id_cols$UNIT_NO]])),
      STATE = body[[id_cols$STATE]],
      SECTOR = body[[id_cols$SECTOR]],
      SYSTEM = body[[id_cols$SYSTEM]],
      TYPE = body[[id_cols$TYPE]],
      FUEL1 = body[[id_cols$FUEL1]],
      FUEL2 = body[[id_cols$FUEL2]],
      CAPACITY_MW = body[[id_cols$CAPACITY_MW]],
      FISCAL_YEAR = year_labels[b + 1],
      NET_GENERATION_GWH = body[[col0 + block_map$NET_GENERATION_GWH]],
      ABSOLUTE_EMISSIONS_TCO2 = body[[col0 + block_map$ABSOLUTE_EMISSIONS_TCO2]],
      SOURCE_VERSION = source_version
    )
  }) %>%
    filter(!is.na(UNIT_NO))

  reduce_unit_rows(out)
}

# ---------------------------------------------------------------------------
# Single-year parser -- used for v19, v20, v21
# ---------------------------------------------------------------------------

#' field_map: named list {out_name: 1-indexed column} for NET_GENERATION_GWH
#' and ABSOLUTE_EMISSIONS_TCO2
parse_singleyear <- function(path, sheet, id_cols, field_map, fiscal_year, source_version) {
  raw <- read_raw_sheet(path, sheet, name_col_1idx = id_cols$NAME)
  body <- drop_footer_rows(raw[-1, ], s_no_col_1idx = id_cols$S_NO)

  out <- tibble(
    NAME = body[[id_cols$NAME]],
    UNIT_NO = suppressWarnings(as.numeric(body[[id_cols$UNIT_NO]])),
    STATE = body[[id_cols$STATE]],
    SECTOR = body[[id_cols$SECTOR]],
    SYSTEM = body[[id_cols$SYSTEM]],
    TYPE = body[[id_cols$TYPE]],
    FUEL1 = body[[id_cols$FUEL1]],
    FUEL2 = body[[id_cols$FUEL2]],
    CAPACITY_MW = body[[id_cols$CAPACITY_MW]],
    FISCAL_YEAR = fiscal_year,
    NET_GENERATION_GWH = body[[field_map$NET_GENERATION_GWH]],
    ABSOLUTE_EMISSIONS_TCO2 = body[[field_map$ABSOLUTE_EMISSIONS_TCO2]],
    SOURCE_VERSION = source_version
  ) %>%
    filter(!is.na(UNIT_NO))

  reduce_unit_rows(out)
}

# ---------------------------------------------------------------------------
# File configuration -- add v8-v16 here later, one entry each, once fetched
# ---------------------------------------------------------------------------

v11 <- parse_multiyear(
  file.path(RAW_DIR, "2016-04-29_CEA__database_for_publishing.xlsm"), sheet = "Data",
  id_cols = list(S_NO = 1, NAME = 2, UNIT_NO = 3, STATE = 6, SECTOR = 7, SYSTEM = 8,
                 TYPE = 9, FUEL1 = 10, FUEL2 = 11, CAPACITY_MW = 5),
  block_start = 12, block_width = 5, n_blocks = 5,
  block_map = list(NET_GENERATION_GWH = 0, ABSOLUTE_EMISSIONS_TCO2 = 1),
  source_version = "v11"
)

v12 <- parse_multiyear(
  file.path(RAW_DIR, "2015-16__Upload_.xlsm"), sheet = "Data",
  id_cols = list(S_NO = 1, NAME = 2, UNIT_NO = 3, STATE = 6, SECTOR = 7, SYSTEM = 8,
                 TYPE = 9, FUEL1 = 10, FUEL2 = 11, CAPACITY_MW = 5),
  block_start = 12, block_width = 5, n_blocks = 5,
  block_map = list(NET_GENERATION_GWH = 0, ABSOLUTE_EMISSIONS_TCO2 = 1),
  source_version = "v12"
)

v17 <- parse_multiyear(
  file.path(RAW_DIR, "2020-21__CEA_database_.xlsm"), sheet = "Data",
  id_cols = list(S_NO = 1, NAME = 2, UNIT_NO = 3, STATE = 6, SECTOR = 7, SYSTEM = 8,
                 TYPE = 9, FUEL1 = 10, FUEL2 = 11, CAPACITY_MW = 5),
  block_start = 12, block_width = 5, n_blocks = 5,
  block_map = list(NET_GENERATION_GWH = 0, ABSOLUTE_EMISSIONS_TCO2 = 1),
  source_version = "v17"
)

v18 <- parse_multiyear(
  file.path(RAW_DIR, "2021-22_CO2_database_final_VM281222_Publish.xlsm"), sheet = "Data",
  id_cols = list(S_NO = 1, NAME = 2, UNIT_NO = 3, STATE = 8, SECTOR = 9, SYSTEM = 10,
                 TYPE = 13, FUEL1 = 14, FUEL2 = 15, CAPACITY_MW = 5),
  block_start = 16, block_width = 22, n_blocks = 5,
  block_map = list(NET_GENERATION_GWH = 2, ABSOLUTE_EMISSIONS_TCO2 = 12),
  source_version = "v18"
)

v19 <- parse_singleyear(
  file.path(RAW_DIR, "CO2_DatabaseVersion_19_2022_23.xlsx"), sheet = "DataBase",
  id_cols = list(S_NO = 1, NAME = 2, UNIT_NO = 3, STATE = 7, SECTOR = 8, SYSTEM = 9,
                 TYPE = 6, FUEL1 = 10, FUEL2 = 11, CAPACITY_MW = 5),
  field_map = list(NET_GENERATION_GWH = 14, ABSOLUTE_EMISSIONS_TCO2 = 24),
  fiscal_year = "2022-23", source_version = "v19"
)

v20 <- parse_singleyear(
  file.path(RAW_DIR, "CO2_Database_Version_20_0_2023_24.xlsx"), sheet = "Database",
  id_cols = list(S_NO = 2, NAME = 3, UNIT_NO = 4, STATE = 8, SECTOR = 9, SYSTEM = 10,
                 TYPE = 7, FUEL1 = 11, FUEL2 = 12, CAPACITY_MW = 6),
  field_map = list(NET_GENERATION_GWH = 13, ABSOLUTE_EMISSIONS_TCO2 = 19),
  fiscal_year = "2023-24", source_version = "v20"
)

v21 <- parse_singleyear(
  file.path(RAW_DIR, "CO2_Database_V_21_0.xlsx"), sheet = "Data",
  id_cols = list(S_NO = 1, NAME = 2, UNIT_NO = 3, STATE = 6, SECTOR = 7, SYSTEM = 8,
                 TYPE = 9, FUEL1 = 10, FUEL2 = 11, CAPACITY_MW = 5),
  field_map = list(NET_GENERATION_GWH = 12, ABSOLUTE_EMISSIONS_TCO2 = 16),
  fiscal_year = "2024-25", source_version = "v21"
)

# ---------------------------------------------------------------------------
# Stack, harmonize SECTOR, report
# ---------------------------------------------------------------------------

panel_raw <- bind_rows(v11, v12, v17, v18, v19, v20, v21) %>%
  mutate(SECTOR = harmonize_sector(SECTOR))

cat("Station-year rows by version:\n")
panel_raw %>% count(SOURCE_VERSION) %>% print()
cat("\nYears covered:", paste(sort(unique(panel_raw$FISCAL_YEAR)), collapse = ", "), "\n")
cat("\nSECTOR distribution:\n")
panel_raw %>% count(SECTOR) %>% print()

write.csv(panel_raw, file.path(CLEAN_DIR, "cea_station_panel_raw_stacked.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------
# Resolve overlaps: prefer higher version number when years overlap
# (v18 > v17 for FY2017-18 -> FY2020-21), then patch known v18 emissions
# gaps back to v17 (real 0-in-source-file cases, confirmed against raw data)
# ---------------------------------------------------------------------------

version_rank <- c(v11 = -1, v12 = 0, v17 = 1, v18 = 2, v19 = 3, v20 = 4, v21 = 5)

final_panel <- panel_raw %>%
  mutate(.rank = version_rank[SOURCE_VERSION]) %>%
  arrange(NAME, FISCAL_YEAR, .rank) %>%
  group_by(NAME, FISCAL_YEAR) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(-.rank) %>%
  arrange(NAME, FISCAL_YEAR)

known_v18_emissions_gaps <- tribble(
  ~NAME, ~FISCAL_YEAR,
  "BARAMURA", "2019-20",
  "GODAVARI GT", "2019-20",
  "GODAVARI GT", "2020-21",
  "I.P.GT", "2020-21",
  "KARAIKAL", "2017-18",
  "URAN GT", "2019-20"
)

v17_lookup <- v17 %>% select(NAME, FISCAL_YEAR, ABSOLUTE_EMISSIONS_TCO2, NET_GENERATION_GWH)

final_panel <- final_panel %>%
  left_join(v17_lookup, by = c("NAME", "FISCAL_YEAR"), suffix = c("", "_v17patch")) %>%
  mutate(
    is_patch = paste(NAME, FISCAL_YEAR) %in% paste(known_v18_emissions_gaps$NAME,
                                                     known_v18_emissions_gaps$FISCAL_YEAR),
    ABSOLUTE_EMISSIONS_TCO2 = if_else(is_patch, ABSOLUTE_EMISSIONS_TCO2_v17patch, ABSOLUTE_EMISSIONS_TCO2),
    SPECIFIC_EMISSIONS_TCO2_MWH = if_else(
      is_patch & NET_GENERATION_GWH_v17patch != 0,
      ABSOLUTE_EMISSIONS_TCO2_v17patch / (NET_GENERATION_GWH_v17patch * 1000),
      SPECIFIC_EMISSIONS_TCO2_MWH
    ),
    SOURCE_VERSION = if_else(is_patch, "v18(gen)+v17(emissions patch)", SOURCE_VERSION)
  ) %>%
  select(-ABSOLUTE_EMISSIONS_TCO2_v17patch, -NET_GENERATION_GWH_v17patch, -is_patch)

cat("\n=== Final deduplicated panel ===\n")
cat("Rows:", nrow(final_panel), " | Unique stations:", n_distinct(final_panel$NAME), "\n")
cat("Rows per year:\n")
final_panel %>% count(FISCAL_YEAR) %>% print()
cat("\nSECTOR distribution:\n")
final_panel %>% count(SECTOR) %>% print()

write.csv(final_panel, file.path(CLEAN_DIR, "cea_station_panel_FINAL.csv"), row.names = FALSE)
cat("\nSaved final panel to", file.path(CLEAN_DIR, "cea_station_panel_FINAL.csv"), "\n")

# ---------------------------------------------------------------------------
# v17 vs v18 overlap validation (sanity check against the Python build)
# ---------------------------------------------------------------------------

overlap_years <- c("2017-18", "2018-19", "2019-20", "2020-21")
a <- v17 %>% filter(FISCAL_YEAR %in% overlap_years) %>%
  select(NAME, FISCAL_YEAR, NET_GENERATION_GWH, ABSOLUTE_EMISSIONS_TCO2)
b <- v18 %>% filter(FISCAL_YEAR %in% overlap_years) %>%
  select(NAME, FISCAL_YEAR, NET_GENERATION_GWH, ABSOLUTE_EMISSIONS_TCO2)

merged <- inner_join(a, b, by = c("NAME", "FISCAL_YEAR"), suffix = c("_v17", "_v18")) %>%
  mutate(gen_pct_diff = (NET_GENERATION_GWH_v18 - NET_GENERATION_GWH_v17) / NET_GENERATION_GWH_v17)

cat("\n=== v17 vs v18 overlap check ===\n")
cat("Matched station-years:", nrow(merged), "\n")
cat("Generation >10% diff count:", sum(abs(merged$gen_pct_diff) > 0.10, na.rm = TRUE), "/", nrow(merged), "\n")

write.csv(merged, file.path(CLEAN_DIR, "v17_v18_overlap_check_R.csv"), row.names = FALSE)
