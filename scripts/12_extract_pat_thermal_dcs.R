# =============================================================================
# 12_extract_pat_thermal_dcs.R
#
# Extracts Thermal Power Plant Designated Consumer lists from PAT cycle
# gazette notifications (text-layer PDFs and scanned/OCR PDFs), then
# fuzzy-matches them against the CEA station panel (built in
# 11_build_cea_panel.R) to construct a PAT treatment indicator.
#
# Two input formats, handled automatically:
#   - Text-layer PDFs (Cycle II, IV, V, ...): pdftools::pdf_text() directly.
#   - Scanned/image PDFs (Cycle I): no embedded font -> rasterize with
#     pdftools::pdf_convert() and OCR each page via the system `tesseract`
#     CLI (no R package needed -- shells out to the same binary Python
#     would use).
#
# Both formats funnel into one shared parser, since the underlying gazette
# table structure is identical: "<item no?> <name/address...> TPP#### <state
# abbrev> <baseline heat rate> <output MU> <target heat rate>", wrapped
# across an unpredictable number of physical lines.
# =============================================================================

library(pdftools)
library(stringr)
library(stringdist)
library(dplyr)
library(purrr)
library(tidyr)

STATES <- c(
  "Andhra Pradesh","Arunachal Pradesh","Assam","Bihar","Chhattisgarh",
  "Goa","Gujarat","Haryana","Himachal Pradesh","Jammu","Jharkhand","Karnataka",
  "Kerala","Madhya Pradesh","Maharashtra","Manipur","Meghalaya","Mizoram","Nagaland",
  "Odisha","Orissa","Punjab","Rajasthan","Sikkim","Tamil Nadu","Tripura",
  "Uttar Pradesh","Uttarakhand","West Bengal","Delhi","Puducherry","Chandigarh"
)
STATE_PATTERN <- paste(STATES, collapse = "|")

# ---------------------------------------------------------------------------
# 1. Text extraction: auto-detect scanned vs. text-layer PDFs
# ---------------------------------------------------------------------------

#' Does this PDF have an embedded, extractable text layer?
#' (Cheap check: try pdftools::pdf_text and see if anything non-blank comes back.)
has_text_layer <- function(pdf_path) {
  txt <- tryCatch(pdftools::pdf_text(pdf_path), error = function(e) character(0))
  any(nchar(str_trim(txt)) > 20)
}

#' OCR a scanned PDF page-by-page via pdftoppm + system tesseract CLI.
#' Returns one character string per page, same shape as pdftools::pdf_text().
ocr_pdf_text <- function(pdf_path, dpi = 150, tmp_dir = tempfile("ocr_")) {
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  prefix <- file.path(tmp_dir, "page")
  system2("pdftoppm", c("-png", "-r", dpi, shQuote(pdf_path), shQuote(prefix)))
  png_files <- sort(list.files(tmp_dir, pattern = "\\.png$", full.names = TRUE))

  page_texts <- character(length(png_files))
  for (i in seq_along(png_files)) {
    out_base <- file.path(tmp_dir, paste0("ocr_", i))
    system2("tesseract", c(shQuote(png_files[i]), shQuote(out_base), "--psm", "6"),
            stdout = FALSE, stderr = FALSE)
    txt_path <- paste0(out_base, ".txt")
    page_texts[i] <- if (file.exists(txt_path)) paste(readLines(txt_path, warn = FALSE), collapse = "\n") else ""
  }
  page_texts
}

#' Unified entry point: dispatches to text extraction or OCR automatically.
extract_pdf_text <- function(pdf_path) {
  if (has_text_layer(pdf_path)) {
    message(basename(pdf_path), ": text layer found, using pdf_text()")
    pdftools::pdf_text(pdf_path)
  } else {
    message(basename(pdf_path), ": no text layer, falling back to OCR")
    ocr_pdf_text(pdf_path)
  }
}

# ---------------------------------------------------------------------------
# 2. Locate the "Thermal Power Plant" sector table within the full document
# ---------------------------------------------------------------------------

#' Given all page texts, return just the lines belonging to the Thermal
#' Power Plant sector table (from its heading to the next sector heading).
extract_thermal_section <- function(page_texts) {
  full_text <- paste(page_texts, collapse = "\n")
  lines <- str_split(full_text, "\n")[[1]]
  lines <- str_trim(lines)
  lines <- lines[nchar(lines) > 0]

  # Drop known gazette page-break/footer noise: transliterated Devanagari
  # page headers (e.g. "Hkkx II...jkti=k...vlk/kj.k"), bare page numbers,
  # and column-label repeats that appear at the top of every page.
  noise_re <- regex(
    "Hkkx|jkti=k|vlk/kj\\.k|^\\d{1,3}$|^THE GAZETTE OF INDIA|Registration|Baseline [Ee]nergy|Net Heat Rate|Net Product|Sr\\.?\\s*No\\.?$|Designated Consumer$",
    ignore_case = FALSE
  )
  lines <- lines[!str_detect(lines, noise_re)]

  start_idx <- which(str_detect(lines, regex("Thermal Power Plant", ignore_case = TRUE)))[1]
  if (is.na(start_idx)) stop("Could not find a 'Thermal Power Plant' heading in this document.")

  after_start <- lines[(start_idx + 1):length(lines)]
  next_heading_offset <- which(str_detect(after_start, "^[IVXLC]+\\.\\s+[A-Z]"))[1]
  end_idx <- if (is.na(next_heading_offset)) length(lines) else start_idx + next_heading_offset - 1

  lines[(start_idx + 1):end_idx]
}

# ---------------------------------------------------------------------------
# 3. Parse the sector table into structured rows
# ---------------------------------------------------------------------------

#' Core parser. Anchors on the line containing "TPP####XX <num> <num> <num>"
#' (present in every record regardless of which sub-format the item number
#' uses), and attaches any preceding unnumbered lines forward as the start
#' of that record's name/address text.
parse_thermal_table <- function(lines, cycle_label) {
  tpp_line_re <- regex(
    "^(?:(\\d{1,3})\\.\\s*)?(.*?)\\s+(TPP\\d{4}[A-Z]{2})\\s+([\\d.]+)\\s+([\\d.]+)\\s+([\\d.]+)\\s*$"
  )

  records <- list()
  buffer <- character(0)

  for (line in lines) {
    # str_match column layout: [,1]=full match,[,2]=item_no,[,3]=name_frag,
    # [,4]=reg_no,[,5]=baseline heat rate,[,6]=output,[,7]=target heat rate
    m <- str_match(line, tpp_line_re)
    if (!is.na(m[1, 4])) {  # reg_no present -> this is a data line
      name_prefix <- str_trim(paste(c(buffer, m[1, 3]), collapse = " "))
      records[[length(records) + 1]] <- list(
        cycle = cycle_label,
        item_no = m[1, 2],
        name_prefix = name_prefix,
        reg_no = m[1, 4],
        baseline_heat_rate = m[1, 5],
        output_mu = m[1, 6],
        target_heat_rate = m[1, 7]
      )
      buffer <- character(0)
    } else {
      buffer <- c(buffer, line)
    }
  }

  bind_rows(records)
}

#' Clean the name field: strip any leftover tail from the PREVIOUS record's
#' address (state name + optional trailing pincode/dash junk) that got
#' attached forward by parse_thermal_table(), and extract this record's
#' own trailing state.
clean_names_and_states <- function(df) {
  # after a state name, optionally skip a dash/comma + 4-7 digit pincode,
  # then require real whitespace/comma before the "real" next name starts
  strip_re <- regex(
    paste0("^.*(", STATE_PATTERN, ")\\s*(?:[-,\u2013]\\s*(?:Pin[-\\s]*)?\\d{4,7}|,?\\s*Pin[-\\s]*\\d{4,7})?\\s*[,\\s]+(.*)$"),
    ignore_case = TRUE
  )

  df %>%
    mutate(
      .strip_match = str_match(name_prefix, strip_re),
      name_clean = if_else(
        !is.na(.strip_match[, 1]) & nchar(.strip_match[, 3]) > 3,
        str_trim(str_replace_all(.strip_match[, 3], "^[,\\s]+", "")),
        name_prefix
      ),
      state = {
        all_states <- str_extract_all(name_prefix, regex(STATE_PATTERN, ignore_case = TRUE))
        map_chr(all_states, ~ if (length(.x) > 0) .x[length(.x)] else NA_character_)
      }
    ) %>%
    select(-.strip_match)
}

# ---------------------------------------------------------------------------
# 4. Full per-file pipeline
# ---------------------------------------------------------------------------

extract_thermal_dcs <- function(pdf_path, cycle_label) {
  page_texts <- extract_pdf_text(pdf_path)
  section_lines <- extract_thermal_section(page_texts)
  raw <- parse_thermal_table(section_lines, cycle_label)
  clean_names_and_states(raw)
}

# ---------------------------------------------------------------------------
# 5. State-restricted fuzzy matching against the CEA panel, with fallback
# ---------------------------------------------------------------------------

STATE_ALIASES <- c(
  "ORISSA" = "ODISHA", "PONDICHERRY" = "PUDUCHERRY",
  "UTTARANCHAL" = "UTTARAKHAND", "UTHARAKHAND" = "UTTARAKHAND",
  "RAJSTHAN" = "RAJASTHAN"
)
norm_state <- function(s) {
  s <- str_to_upper(str_trim(s))
  ifelse(s %in% names(STATE_ALIASES), STATE_ALIASES[s], s)
}

#' For each PAT DC, find the best-matching CEA station name.
#' Restricts candidates to the same state when known; falls back to the
#' full CEA station list (flagged explicitly) when state is missing.
#' Scored with composite_similarity() (see above) rather than a single
#' whole-string metric.
#' Composite similarity score approximating rapidfuzz's token_sort_ratio /
#' partial_ratio behavior, since stringdist's whole-string metrics (Jaro-
#' Winkler included) score badly when one string (PAT's name+address) is
#' much longer than the other (CEA's short station name) even when the
#' station name is a perfect substring match. Combines:
#'   1. Exact substring containment (CEA name found inside PAT name) -> 100
#'   2. Token overlap (asymmetric: candidate tokens found in query / all
#'      candidate tokens) -> catches subset matches when containment isn't
#'      exact (OCR noise, extra punctuation), without being diluted by the
#'      query's many irrelevant address tokens the way Jaccard would be
#'   3. Plain Jaro-Winkler on full strings -> fallback for everything else
#' Takes the max of the three per candidate.
composite_similarity <- function(query, candidates) {
  query_u <- str_to_upper(query)
  query_tokens <- str_split(query_u, "[^A-Z0-9]+")[[1]]
  query_tokens <- query_tokens[nchar(query_tokens) > 0]

  vapply(candidates, function(cand) {
    cand_u <- str_to_upper(cand)
    containment <- if (nchar(cand_u) >= 4 && str_detect(query_u, fixed(cand_u))) 100 else 0

    cand_tokens <- str_split(cand_u, "[^A-Z0-9]+")[[1]]
    cand_tokens <- cand_tokens[nchar(cand_tokens) > 0]
    # asymmetric overlap: what fraction of the CANDIDATE's own tokens are
    # found in the (much longer) query? Deliberately not Jaccard/union --
    # dividing by the union would let the query's many irrelevant address
    # tokens dilute the score even when the full candidate name is present.
    token_overlap <- if (length(cand_tokens) > 0) {
      100 * length(intersect(query_tokens, cand_tokens)) / length(cand_tokens)
    } else 0

    jw <- 100 * (1 - stringdist::stringdist(query_u, cand_u, method = "jw"))

    max(containment, token_overlap, jw)
  }, numeric(1))
}

match_to_cea <- function(pat_dcs, cea_panel) {
  cea_by_station <- cea_panel %>%
    distinct(NAME, STATE) %>%
    mutate(STATE_NORM = norm_state(STATE))

  all_cea_names <- cea_by_station$NAME

  pat_dcs %>%
    mutate(state_norm = norm_state(state)) %>%
    rowwise() %>%
    mutate(
      candidates_df = list({
        if (is.na(state_norm)) {
          all_cea_names
        } else {
          cands <- cea_by_station$NAME[cea_by_station$STATE_NORM == state_norm]
          if (length(cands) == 0) all_cea_names else cands
        }
      }),
      state_fallback_used = is.na(state_norm) ||
        length(cea_by_station$NAME[cea_by_station$STATE_NORM == state_norm]) == 0,
      sim_scores = list(composite_similarity(name_clean, candidates_df)),
      best_idx = if (length(sim_scores) > 0) which.max(sim_scores) else NA_integer_,
      matched_cea_name = if (!is.na(best_idx)) candidates_df[best_idx] else NA_character_,
      score = if (!is.na(best_idx)) round(sim_scores[best_idx], 1) else NA_real_,
      candidate_pool_size = length(candidates_df)
    ) %>%
    ungroup() %>%
    select(-candidates_df, -sim_scores, -best_idx)
}

#' Flag matches where the CEA site has multiple sub-units sharing the same
#' first word (e.g. "KONDAPALLI" vs "KONDAPALLI WHR") -- these need a human
#' check regardless of match score, since the score can't distinguish
#' between two real candidates at the same physical site.
flag_same_site_ambiguity <- function(matched_df, cea_panel) {
  cea_by_state <- cea_panel %>%
    distinct(NAME, STATE) %>%
    mutate(STATE_NORM = norm_state(STATE))

  matched_df %>%
    rowwise() %>%
    mutate(
      first_token = word(matched_cea_name, 1),
      same_site = list({
        if (is.na(first_token) || is.na(state_norm)) {
          character(0)
        } else {
          cands <- cea_by_state$NAME[cea_by_state$STATE_NORM == state_norm]
          cands[word(cands, 1) == first_token]
        }
      }),
      site_has_multiple_units = length(same_site) > 1,
      other_units_at_site = paste(setdiff(same_site, matched_cea_name), collapse = "; ")
    ) %>%
    ungroup() %>%
    select(-first_token, -same_site)
}

# =============================================================================
# 6. Driver -- run this section to (re)build the dataset end to end
# =============================================================================

run_pat_thermal_pipeline <- function() {
  RAW_DIR <- "/home/claude/pat_more"      # adjust to wherever the gazette PDFs live
  CLEAN_DIR <- "/home/claude/india_data/clean_r"

  files_and_cycles <- list(
    list(path = file.path(RAW_DIR, "..", "pat_cycle1", "pat1.pdf"), cycle = "Cycle I (2012)"),
    list(path = file.path(RAW_DIR, "PAT2_Target.pdf"),               cycle = "Cycle II (2016)"),
    list(path = file.path(RAW_DIR, "Target_Notification_-PAT_Cycle_IV.pdf"), cycle = "Cycle IV (2018)"),
    list(path = file.path(RAW_DIR, "RTF5.pdf"),                      cycle = "Cycle V (2019)")
  )

  all_dcs <- map_dfr(files_and_cycles, ~ extract_thermal_dcs(.x$path, .x$cycle))
  cat("Total DCs extracted across all cycles:", nrow(all_dcs), "\n")
  print(count(all_dcs, cycle))

  cea_panel <- read.csv(file.path(CLEAN_DIR, "cea_station_panel_FINAL.csv"), stringsAsFactors = FALSE)

  matched <- match_to_cea(all_dcs, cea_panel)
  matched <- flag_same_site_ambiguity(matched, cea_panel)

  cat("\nHigh confidence (>=80):", sum(matched$score >= 80, na.rm = TRUE), "\n")
  cat("Review needed (60-79):", sum(matched$score >= 60 & matched$score < 80, na.rm = TRUE), "\n")
  cat("Low/no match (<60):", sum(matched$score < 60, na.rm = TRUE), "\n")
  cat("State fallback used (needs review regardless of score):", sum(matched$state_fallback_used), "\n")
  cat("Same-site ambiguous:", sum(matched$site_has_multiple_units), "\n")

  write.csv(matched, file.path(CLEAN_DIR, "pat_thermal_dcs_cea_matched.csv"), row.names = FALSE)
  cat("\nSaved pat_thermal_dcs_cea_matched.csv\n")
  invisible(matched)
}

# Call run_pat_thermal_pipeline() to execute. Not run automatically on
# source() so this file can also be loaded as a library of functions
# (e.g. from an R Markdown doc or another script) without side effects.
