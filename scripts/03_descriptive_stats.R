# ── 1. Load packages ───────────────────────────────────────────────────────────
library(tidyverse)
install.packages("corrplot")
library(corrplot)

#some helper functions

# redefine ownership helper and colors
ownership_levels <- c('Private (0%)', 'Majority state', 'Fully state (100%)')
ownership_colors <- c(
  'Private (0%)'       = '#E8534A',
  'Majority state'     = '#2ECC71',
  'Fully state (100%)' = '#3498DB'
)

make_ownership_group <- function(df) {
  df %>%
    mutate(
      ownership_group = case_when(
        state_ownership_pct == 0 ~ 'Private (0%)',
        state_ownership_pct >= 50 & state_ownership_pct < 100 ~ 'Majority state',
        state_ownership_pct == 100 ~ 'Fully state (100%)'
      ),
      ownership_group = factor(ownership_group, levels = ownership_levels)
    ) %>%
    filter(!is.na(ownership_group))
}

# ── 2. Load panel ─────────────────────────────────────────────────────────────
df_panel <- readRDS('data/processed/df_panel_merged.rds')

df_panel %>%
  mutate(emissions_intensity = verified / operating_revenue) %>%
  summarise(
    n_total = n(),
    n_nonmissing = sum(!is.na(emissions_intensity)),
    n_missing = sum(is.na(emissions_intensity)),
    pct_coverage = round(mean(!is.na(emissions_intensity)) * 100, 1)
  )

# missingness by phase
df_panel %>%
  mutate(emissions_intensity = verified / operating_revenue) %>%
  group_by(phase) %>%
  summarise(
    n = n(),
    pct_coverage = round(mean(!is.na(emissions_intensity)) * 100, 1)
  )

# missingness by country
df_panel %>%
  mutate(emissions_intensity = verified / operating_revenue) %>%
  group_by(registry_id) %>%
  summarise(
    n = n(),
    pct_coverage = round(mean(!is.na(emissions_intensity)) * 100, 1)
  ) %>%
  arrange(pct_coverage)

# missingness by ownership type
df_panel %>%
  mutate(emissions_intensity = verified / operating_revenue) %>%
  group_by(state_owned_binary) %>%
  summarise(
    n = n(),
    pct_coverage = round(mean(!is.na(emissions_intensity)) * 100, 1)
  )

df_panel %>%
  filter(phase >= 3) %>%
  mutate(emissions_intensity = verified / operating_revenue) %>%
  summarise(
    n_total = n(),
    n_nonmissing = sum(!is.na(emissions_intensity)),
    pct_coverage = round(mean(!is.na(emissions_intensity)) * 100, 1)
  )

# ── construct emissions intensity ──────────────────────────────────────────────
df_panel <- df_panel %>%
  mutate(
    emissions_intensity = ifelse(
      operating_revenue <= 0, NA, verified / operating_revenue
    ),
    log_emissions_intensity = ifelse(
      operating_revenue <= 0 | emissions_intensity <= 0, 
      NA, log(emissions_intensity)
    ),
    log_verified = log(verified)
  )

# check how many negative/zero revenue observations
sum(df_panel$operating_revenue <= 0, na.rm = TRUE)

# recheck summary
summary(df_panel$emissions_intensity)
summary(df_panel$log_emissions_intensity)

# check for remaining extreme outliers
quantile(df_panel$emissions_intensity, 
         probs = c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99),
         na.rm = TRUE)

# trim top 1% before logging
p99 <- quantile(df_panel$emissions_intensity, 0.99, na.rm = TRUE)

df_panel <- df_panel %>%
  mutate(
    emissions_intensity_trim = ifelse(emissions_intensity > p99, 
                                      NA, emissions_intensity),
    log_emissions_intensity = log(emissions_intensity_trim)
  )

summary(df_panel$log_emissions_intensity)

# save updated panel
saveRDS(df_panel, 'data/processed/df_panel_merged.rds')

# plot log emissions intensity by ownership and phase
df_panel %>%
  filter(!is.na(log_emissions_intensity),
         !is.na(state_ownership_pct),
         between(log_emissions_intensity, -10, 2)) %>%
  make_ownership_group() %>%
  ggplot(aes(x = ownership_group, y = log_emissions_intensity,
             fill = ownership_group)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.1) +
  facet_wrap(~phase) +
  scale_fill_manual(values = ownership_colors) +
  labs(
    title = 'Log emissions intensity by ownership and phase',
    subtitle = 'Log(verified emissions / operating revenue) — trimmed to [-10, 2]',
    x = 'Ownership',
    y = 'Log(tonnes CO2 / revenue)'
  ) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('output/figures/14_log_intensity_by_ownership_trimmed.png',
       width = 12, height = 8, dpi = 300)

$checked dropped
df_panel %>%
  filter(!is.na(log_emissions_intensity)) %>%
  summarise(
    total = n(),
    kept = sum(between(log_emissions_intensity, -10, 2)),
    dropped = sum(!between(log_emissions_intensity, -10, 2)),
    pct_dropped = round(mean(!between(log_emissions_intensity, -10, 2)) * 100, 1)
  )








# quick look at key variables
df_panel %>%
  select(surplus_norm, coverage_ratio, compliant,
         state_ownership_pct, state_owned_binary,
         guo_state, foreign_owned, independence,
         total_assets, operating_revenue, n_employees) %>%
  summary()

# ── 3. Correlation matrix ─────────────────────────────────────────────────────
library(corrplot)

# ── updated correlation matrix with emissions intensity ────────────────────────
cor_vars2 <- df_panel %>%
  select(
    surplus_norm,
    log_verified,
    log_emissions_intensity,
    state_ownership_pct,
    state_owned_binary,
    guo_state,
    foreign_owned
  ) %>%
  drop_na()

nrow(cor_vars2)

cor_matrix2 <- cor(cor_vars2)

png('output/figures/15_correlation_matrix_v2.png',
    width = 10, height = 8, units = 'in', res = 300)
corrplot(cor_matrix2,
         method = 'color',
         type = 'upper',
         addCoef.col = 'black',
         number.cex = 0.7,
         tl.cex = 0.7,
         title = 'Correlation matrix v2: all dependent variables vs ownership',
         mar = c(0,0,1,0))
dev.off()

df_panel %>%
  filter(!is.na(log_verified),
         !is.na(operating_revenue),
         operating_revenue > 0) %>%
  summarise(
    n = n(),
    n_firms = n_distinct(bvdId)
  )





# select key variables and drop NAs
cor_vars <- df_panel %>%
  select(
    # dependent variable candidates
    surplus_norm,
    coverage_ratio,
    compliant,
    emissions_growth,
    # ownership variables
    state_ownership_pct,
    state_owned_binary,
    guo_state,
    foreign_owned
  ) %>%
  drop_na()

# compute correlation matrix
cor_matrix <- cor(cor_vars)

# plot
corrplot(cor_matrix, 
         method = 'color',
         type = 'upper',
         addCoef.col = 'black',
         number.cex = 0.7,
         tl.cex = 0.7,
         title = 'Correlation matrix: compliance outcomes vs ownership',
         mar = c(0,0,1,0))

# save correlation matrix
png('output/figures/07_correlation_matrix.png', 
    width = 10, height = 8, units = 'in', res = 300)

corrplot(cor_matrix,
         method = 'color',
         type = 'upper',
         addCoef.col = 'black',
         number.cex = 0.7,
         tl.cex = 0.7,
         title = 'Correlation: compliance outcomes vs ownership',
         mar = c(0,0,1,0))

dev.off()

# ── 3. Surplus vs state ownership scatter ─────────────────────────────────────
df_panel %>%
  filter(!is.na(state_ownership_pct),
         !is.na(surplus_norm)) %>%
  ggplot(aes(x = state_ownership_pct, y = surplus_norm)) +
  geom_point(alpha = 0.1, size = 0.5, color = 'steelblue') +
  geom_smooth(method = 'lm', color = 'red', se = TRUE) +
  labs(
    title = 'Compliance surplus vs state ownership',
    subtitle = 'EU ETS power sector installations 2005-2023',
    x = 'State ownership (%)',
    y = '(Allocated - Verified) / Verified'
  ) +
  theme_minimal()

ggsave('output/figures/05_surplus_vs_ownership.png',
       width = 10, height = 6, dpi = 300)


# ── 3b. By phase ──────────────────────────────────────────────────────────────
df_panel %>%
  filter(!is.na(state_ownership_pct),
         !is.na(surplus_norm),
         !is.na(phase)) %>%
  mutate(phase_label = paste('Phase', phase)) %>%
  ggplot(aes(x = state_ownership_pct, y = surplus_norm)) +
  geom_point(alpha = 0.1, size = 0.5, color = 'steelblue') +
  geom_smooth(method = 'lm', color = 'red', se = TRUE) +
  facet_wrap(~phase_label) +
  labs(
    title = 'Compliance surplus vs state ownership by phase',
    subtitle = 'EU ETS power sector installations',
    x = 'State ownership (%)',
    y = '(Allocated - Verified) / Verified'
  ) +
  theme_minimal()

ggsave('output/figures/06_surplus_vs_ownership_by_phase.png',
       width = 12, height = 8, dpi = 300)


# box plot — cleaner way to show the relationship
df_panel %>%
  filter(!is.na(state_owned_binary),
         !is.na(surplus_norm),
         between(surplus_norm, -1, 2)) %>%
  mutate(
    ownership_group = case_when(
      state_ownership_pct == 0 ~ 'Private (0%)',
      state_ownership_pct > 0 & state_ownership_pct < 50 ~ 'Minority state (<50%)',
      state_ownership_pct >= 50 & state_ownership_pct < 100 ~ 'Majority state (50-99%)',
      state_ownership_pct == 100 ~ 'Fully state (100%)'
    ),
    ownership_group = factor(ownership_group, 
                             levels = c('Private (0%)', 
                                        'Minority state (<50%)',
                                        'Majority state (50-99%)',
                                        'Fully state (100%)'))
  ) %>%
  filter(!is.na(ownership_group)) %>%
  ggplot(aes(x = ownership_group, y = surplus_norm, fill = ownership_group)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.1) +
  facet_wrap(~phase) +
  labs(
    title = 'Compliance surplus by ownership type and phase',
    subtitle = 'EU ETS power sector installations',
    x = 'Ownership category',
    y = '(Allocated - Verified) / Verified'
  ) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('output/figures/06_surplus_by_ownership_phase.png',
       width = 12, height = 8, dpi = 300)

# ── proxy for government progressivity using country groups ───────────────────
green_govts <- c('FR', 'DE', 'SE', 'DK', 'NL', 'AT', 'FI', 'NO')
laggard_govts <- c('HU', 'PL', 'CZ', 'BG', 'RO', 'SK')

df_panel %>%
  filter(!is.na(state_ownership_pct),
         !is.na(surplus_norm),
         between(surplus_norm, -1, 2),
         registry_id %in% c(green_govts, laggard_govts)) %>%
  mutate(
    govt_type = ifelse(registry_id %in% green_govts, 
                       'Green government', 'Laggard government'),
    ownership_group = case_when(
      state_ownership_pct == 0 ~ 'Private',
      state_ownership_pct >= 50 ~ 'State-owned'
    )
  ) %>%
  filter(!is.na(ownership_group)) %>%
  ggplot(aes(x = ownership_group, y = surplus_norm, fill = ownership_group)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.1) +
  facet_grid(phase ~ govt_type) +
  labs(
    title = 'Compliance surplus by ownership and government type',
    x = 'Ownership',
    y = '(Allocated - Verified) / Verified'
  ) +
  theme_minimal() +
  theme(legend.position = 'none')

ggsave('output/figures/08_ownership_by_govt_type.png',
       width = 12, height = 10, dpi = 300)


# set consistent ownership colors and levels
ownership_levels <- c('Private (0%)', 'Majority state', 'Fully state (100%)')
ownership_colors <- c(
  'Private (0%)'       = '#E8534A',   
  'Majority state'     = '#2ECC71',     
  'Fully state (100%)' = '#3498DB'    
)

# helper to create ownership group variable
make_ownership_group <- function(df) {
  df %>%
    mutate(
      ownership_group = case_when(
        state_ownership_pct == 0 ~ 'Private (0%)',
        state_ownership_pct >= 50 & state_ownership_pct < 100 ~ 'Majority state',
        state_ownership_pct == 100 ~ 'Fully state (100%)'
      ),
      ownership_group = factor(ownership_group, levels = ownership_levels)
    ) %>%
    filter(!is.na(ownership_group))
}

# ── Coverage ratio by ownership and phase ─────────────────────────────────
df_panel %>%
  filter(!is.na(state_ownership_pct),
         !is.na(coverage_ratio),
         between(coverage_ratio, 0, 3)) %>%
  make_ownership_group() %>%
  ggplot(aes(x = ownership_group, y = coverage_ratio, fill = ownership_group)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.1) +
  geom_hline(yintercept = 1, linetype = 'dashed', color = 'red') +
  facet_wrap(~phase) +
  scale_fill_manual(values = ownership_colors) +
  labs(
    title = 'Coverage ratio by ownership and phase',
    subtitle = 'Red line = allocation exactly covers emissions',
    x = 'Ownership',
    y = 'Allocated / Verified'
  ) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('output/figures/09_coverage_ratio_by_ownership.png',
       width = 12, height = 8, dpi = 300)

# ── Emissions trajectory within phase ─────────────────────────────────────
df_panel %>%
  filter(!is.na(state_ownership_pct),
         !is.na(verified),
         !is.na(verifiedCummulative),
         verifiedCummulative > 0) %>%
  mutate(emissions_share = verified / verifiedCummulative) %>%
  filter(between(emissions_share, 0, 1)) %>%
  make_ownership_group() %>%
  ggplot(aes(x = ownership_group, y = emissions_share, fill = ownership_group)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.1) +
  facet_wrap(~phase) +
  scale_fill_manual(values = ownership_colors) +
  labs(
    title = 'Annual emissions share of phase total by ownership',
    subtitle = 'Lower values = emissions declining within phase',
    x = 'Ownership',
    y = 'Verified / Cumulative verified'
  ) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('output/figures/10_emissions_trajectory_by_ownership.png',
       width = 12, height = 8, dpi = 300)

# ── Log verified emissions by ownership and phase ─────────────────────────
df_panel %>%
  filter(!is.na(state_ownership_pct),
         !is.na(verified),
         verified > 0) %>%
  mutate(log_verified = log(verified)) %>%
  make_ownership_group() %>%
  ggplot(aes(x = ownership_group, y = log_verified, fill = ownership_group)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.1) +
  facet_wrap(~phase) +
  scale_fill_manual(values = ownership_colors) +
  labs(
    title = 'Log verified emissions by ownership and phase',
    subtitle = 'Captures scale differences across ownership types',
    x = 'Ownership',
    y = 'Log(Verified emissions)'
  ) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('output/figures/11_log_emissions_by_ownership.png',
       width = 12, height = 8, dpi = 300)

# ── foreign ownership vs surplus by phase ─────────────────────────────────────
foreign_colors <- c(
  'Domestic private' = '#E8534A',
  'Domestic state'   = '#3498DB',
  'Foreign owned'    = '#2ECC71'
)

df_panel %>%
  filter(!is.na(state_owned_binary),
         !is.na(foreign_owned),
         !is.na(surplus_norm),
         between(surplus_norm, -1, 2)) %>%
  mutate(
    ownership_type = case_when(
      foreign_owned == 1 ~ 'Foreign owned',
      foreign_owned == 0 & state_owned_binary == 1 ~ 'Domestic state',
      foreign_owned == 0 & state_owned_binary == 0 ~ 'Domestic private'
    ),
    ownership_type = factor(ownership_type,
                            levels = c('Domestic private',
                                       'Domestic state',
                                       'Foreign owned'))
  ) %>%
  filter(!is.na(ownership_type)) %>%
  ggplot(aes(x = ownership_type, y = surplus_norm, fill = ownership_type)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.1) +
  facet_wrap(~phase) +
  scale_fill_manual(values = foreign_colors) +
  labs(
    title = 'Compliance surplus by ownership type and phase',
    subtitle = 'Domestic private vs domestic state vs foreign owned',
    x = 'Ownership type',
    y = '(Allocated - Verified) / Verified'
  ) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('output/figures/12_surplus_by_foreign_ownership.png',
       width = 12, height = 8, dpi = 300)

# ── four-way ownership classification plot ────────────────────────────────────
ownership_4way_colors <- c(
  'Domestic private' = '#E8534A',
  'Domestic state'   = '#3498DB',
  'Foreign private'  = '#2ECC71',
  'Foreign state'    = '#9B59B6'
)

df_panel %>%
  filter(!is.na(ownership_4way),
         !is.na(surplus_norm),
         between(surplus_norm, -1, 2)) %>%
  ggplot(aes(x = ownership_4way, y = surplus_norm, fill = ownership_4way)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.1) +
  facet_wrap(~phase) +
  scale_fill_manual(values = ownership_4way_colors) +
  labs(
    title = 'Compliance surplus by four-way ownership classification',
    subtitle = 'Domestic vs foreign × private vs state',
    x = 'Ownership type',
    y = '(Allocated - Verified) / Verified'
  ) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('output/figures/13_surplus_four_way_ownership.png',
       width = 12, height = 8, dpi = 300)

# ── 3. Coverage vs state ownership scatter ─────────────────────────────────────
df_panel %>%
  filter(!is.na(state_ownership_pct),
         !is.na(coverage_ratio)) %>%
  ggplot(aes(x = state_ownership_pct, y = coverage_ratio)) +
  geom_point(alpha = 0.1, size = 0.5, color = 'steelblue') +
  geom_smooth(method = 'lm', color = 'red', se = TRUE) +
  labs(
    title = 'Compliance surplus vs state ownership',
    subtitle = 'EU ETS power sector installations 2005-2023',
    x = 'State ownership (%)',
    y = 'Allocated / Verified'
  ) +
  theme_minimal()

ggsave('output/figures/12_coverage_vs_ownership.png',
       width = 10, height = 6, dpi = 300)

names(df_panel) %>% grep('ummul', ., value = TRUE, ignore.case = TRUE)

