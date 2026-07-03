# ── 1. Load packages ───────────────────────────────────────────────────────────
library(tidyverse)
install.packages("corrplot")
library(corrplot)

# ── 2. Load panel ─────────────────────────────────────────────────────────────
df_panel <- readRDS('data/processed/df_panel_merged.rds')

# quick look at key variables
df_panel %>%
  select(surplus_norm, coverage_ratio, compliant,
         state_ownership_pct, state_owned_binary,
         guo_state, foreign_owned, independence,
         total_assets, operating_revenue, n_employees) %>%
  summary()

# ── 3. Correlation matrix ─────────────────────────────────────────────────────
library(corrplot)

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

