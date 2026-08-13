library(fixest)
library(here)

#phase dummies

df_panel <- df_panel %>% 
  mutate(
    phase3 = as.integer(phase == 3),
    phase4 = as.integer(phase == 4)
  )

#baseline 

m1 <- feols(log_emissions_intensity ~ state_ownership_pct_tv |
              registry_id + year,
            data = df_panel, cluster = ~bvdId)

m2 <- feols(log_emissions_intensity ~ state_ownership_pct_tv +
              log(n_employees + 1) |
              registry_id + year,
            data = df_panel, cluster = ~bvdId)

summary(m1)
summary(m2)

m1_install <- feols(
  log_emissions_intensity ~ state_ownership_pct_tv |
    installation_id + year,
  data = df_panel,
  cluster = ~bvdId
)

m2_install <- feols(
  log_emissions_intensity ~ 
    state_ownership_pct_tv +
    eps + v2x_polyarchy |
    installation_id + year,
  data = df_panel,
  cluster = ~bvdId
)

summary(m1_install)
summary(m2_install)

m3_lv <- feols(
  log_verified ~ state_ownership_pct_tv +
    eps + v2x_polyarchy |
    installation_id + year,
  data = df_panel,
  cluster = ~bvdId
)

summary(m3_lv)

m4_sn <- feols(
  surplus_norm ~ state_ownership_pct_tv +
    eps + v2x_polyarchy |
    installation_id + year,
  data = df_panel,
  cluster = ~bvdId
)

summary(m4_sn)

m4_p2 <- feols(
  surplus_norm ~ state_ownership_pct_tv +
    eps + v2x_polyarchy |
    installation_id + year,
  data = df_panel %>% filter(phase == 2),
  cluster = ~bvdId
)

summary(m4_p2)

m4_p3 <- feols(
  surplus_norm ~ state_ownership_pct_tv +
    eps + v2x_polyarchy |
    installation_id + year,
  data = df_panel %>% filter(phase == 3),
  cluster = ~bvdId
)

summary(m4_p3)

#policy
m3 <- feols(
  log_emissions_intensity ~ 
    state_ownership_pct_tv +
    log(n_employees + 1) +
    eps + v2x_polyarchy |
    registry_id + year,
  data = df_panel,
  cluster = ~bvdId
)

summary(m3)

#interactions

m_interaction <- feols(
  log_verified ~ 
    state_ownership_pct_tv * eps +
    v2x_polyarchy |
    registry_id + year,
  data = df_panel,
  cluster = ~bvdId
)

summary(m_interaction)

# what are the EPS percentiles in your sample?
quantile(df_panel$eps, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# marginal effect of ownership at each EPS level
# = 0.0154 + (-0.00618) × EPS
b_own <- 0.015413
b_int <- -0.006184

eps_values <- quantile(df_panel$eps, 
                       probs = c(0.25, 0.5, 0.75), 
                       na.rm = TRUE)

marginal_effects <- b_own + b_int * eps_values
print(marginal_effects)

#diagnostics
# how much within-installation variation does state_ownership_pct_tv actually have?
df_panel %>%
  filter(!is.na(state_ownership_pct_tv),
         !is.na(log_emissions_intensity)) %>%
  group_by(installation_id) %>%
  summarise(
    sd_tv = sd(state_ownership_pct_tv, na.rm = TRUE),
    mean_tv = mean(state_ownership_pct_tv, na.rm = TRUE),
    n = n()
  ) %>%
  filter(sd_tv > 0) %>%  # only installations with variation
  nrow()

df_panel %>%
  filter(!is.na(state_ownership_pct_tv),
         !is.na(log_emissions_intensity)) %>%
  group_by(installation_id) %>%
  summarise(
    sd_tv = sd(state_ownership_pct_tv, na.rm = TRUE),
    mean_tv = mean(state_ownership_pct_tv, na.rm = TRUE),
    n = n(),
    registry_id = first(registry_id)
  ) %>%
  filter(sd_tv > 0) %>%
  summarise(
    mean_sd = mean(sd_tv),
    median_sd = median(sd_tv),
    mean_ownership = mean(mean_tv),
    country_breakdown = list(table(registry_id))
  )

df_panel %>%
  filter(!is.na(state_ownership_pct_tv),
         !is.na(log_emissions_intensity)) %>%
  group_by(installation_id) %>%
  summarise(
    sd_tv = sd(state_ownership_pct_tv, na.rm = TRUE),
    min_own = min(state_ownership_pct_tv),
    max_own = max(state_ownership_pct_tv),
    change = max_own - min_own
  ) %>%
  filter(sd_tv > 0) %>%
  pull(change) %>%
  summary()


df_panel %>%
  filter(!is.na(state_ownership_pct_tv),
         !is.na(log_emissions_intensity)) %>%
  group_by(installation_id) %>%
  summarise(
    sd_tv = sd(state_ownership_pct_tv, na.rm = TRUE),
    registry_id = first(registry_id)
  ) %>%
  filter(sd_tv > 0) %>%
  count(registry_id) %>%
  arrange(desc(n))

m_no_poland <- feols(
  log_verified ~ state_ownership_pct_tv +
    eps + v2x_polyarchy |
    installation_id + year,
  data = df_panel %>% filter(registry_id != 'PL'),
  cluster = ~bvdId
)

summary(m_no_poland)

# only ownership 

# get installation IDs with ownership variation
changing_installations <- df_panel %>%
  filter(!is.na(state_ownership_pct_tv)) %>%
  group_by(installation_id) %>%
  summarise(sd_tv = sd(state_ownership_pct_tv, na.rm = TRUE)) %>%
  filter(sd_tv > 0) %>%
  pull(installation_id)

length(changing_installations)

# regression on only changing firms
m_changers <- feols(
  log_verified ~ state_ownership_pct_tv +
    eps + v2x_polyarchy |
    installation_id + year,
  data = df_panel %>% 
    filter(installation_id %in% changing_installations),
  cluster = ~bvdId
)

summary(m_changers)

m_int_changers <- feols(
  log_verified ~ 
    state_ownership_pct_tv * eps +
    v2x_polyarchy |
    installation_id + year,
  data = df_panel %>%
    filter(installation_id %in% changing_installations),
  cluster = ~bvdId
)

summary(m_int_changers)

#### by phase 

# ── Block 5: by phase ─────────────────────────────────────────────────────────
# forward fill EPS in df_panel
df_panel <- df_panel %>%
  arrange(registry_id, year) %>%
  group_by(registry_id) %>%
  fill(eps, .direction = 'down') %>%
  ungroup()

# check coverage now
df_panel %>%
  group_by(phase) %>%
  summarise(pct_eps = round(mean(!is.na(eps)) * 100, 1))

dvs <- c('log_verified', 'surplus_norm', 'log_emissions_intensity')
phases <- c(2, 3, 4)

phase_results <- list()

for (dv in dvs) {
  for (ph in phases) {
    
    formula <- as.formula(paste(
      dv, '~ state_ownership_pct_tv + eps + v2x_polyarchy | registry_id + year'
    ))
    
    m <- feols(
      formula,
      data = df_panel %>% filter(phase == ph),
      cluster = ~bvdId
    )
    
    phase_results[[paste(dv, ph, sep = '_p')]] <- m
  }
}

# print all results
lapply(names(phase_results), function(name) {
  cat('\n====', name, '====\n')
  print(summary(phase_results[[name]]))
})

#looking at interaction results

# interaction by phase
m_int_p2 <- feols(
  log_verified ~ state_ownership_pct_tv * eps + v2x_polyarchy |
    registry_id + year,
  data = df_panel %>% filter(phase == 2),
  cluster = ~bvdId
)

m_int_p3 <- feols(
  log_verified ~ state_ownership_pct_tv * eps + v2x_polyarchy |
    registry_id + year,
  data = df_panel %>% filter(phase == 3),
  cluster = ~bvdId
)

summary(m_int_p2)
summary(m_int_p3)

# ── kitchen sink ──────────────────────────────────────────────────────────────

m_full2 <- feols(
  log_verified ~ 
    state_ownership_pct_tv * eps +
    v2x_polyarchy +
    log(n_employees + 1) +
    foreign_owned |
    registry_id + year,
  data = df_panel,
  cluster = ~bvdId
)

summary(m_full2)

# does df_all have state_ownership_pct_tv?
names(df_all) %>% grep('ownership', ., value = TRUE, ignore.case = TRUE)

# does it have eps and v2x_polyarchy?
names(df_all) %>% grep('eps|polyarchy', ., value = TRUE, ignore.case = TRUE)

# merge time-varying ownership into df_all
df_all <- df_all %>%
  left_join(
    state_ownership_annual %>%
      select(bvdId, year, state_ownership_pct_tv, state_owned_binary_tv),
    by = c('bvdId', 'year')
  )

# check coverage
df_all %>%
  group_by(sector) %>%
  summarise(
    n = n(),
    pct_ownership_tv = round(mean(!is.na(state_ownership_pct_tv)) * 100, 1),
    pct_eps = round(mean(!is.na(eps)) * 100, 1),
    pct_vdem = round(mean(!is.na(v2x_polyarchy)) * 100, 1),
    pct_employees = round(mean(!is.na(n_employees)) * 100, 1)
  )


# ── log_emissions_intensity specs ─────────────────────────────────────────────
ei1 <- feols(log_emissions_intensity ~ state_ownership_pct_tv |
               registry_id + year,
             data = df_panel, cluster = ~bvdId)

ei2 <- feols(log_emissions_intensity ~ state_ownership_pct_tv +
               eps + v2x_polyarchy |
               registry_id + year,
             data = df_panel, cluster = ~bvdId)

ei3 <- feols(log_emissions_intensity ~ state_ownership_pct_tv * eps +
               v2x_polyarchy |
               registry_id + year,
             data = df_panel, cluster = ~bvdId)

ei4 <- feols(log_emissions_intensity ~ state_ownership_pct_tv * eps +
               v2x_polyarchy + log(n_employees + 1) + foreign_owned |
               registry_id + year,
             data = df_panel, cluster = ~bvdId)

ei5 <- feols(log_emissions_intensity ~ state_ownership_pct_tv * eps +
               v2x_polyarchy |
               installation_id + year,
             data = df_panel, cluster = ~bvdId)

ei6 <- feols(log_emissions_intensity ~ state_ownership_pct_tv * eps +
               v2x_polyarchy |
               registry_id + year,
             data = df_panel %>% filter(phase == 2),
             cluster = ~bvdId)

ei7 <- feols(log_emissions_intensity ~ state_ownership_pct_tv * eps +
               v2x_polyarchy |
               registry_id + year,
             data = df_panel %>% filter(phase == 3),
             cluster = ~bvdId)

ei8 <- feols(log_emissions_intensity ~ state_ownership_pct_tv * eps +
               v2x_polyarchy |
               registry_id + year,
             data = df_panel %>% filter(phase == 4),
             cluster = ~bvdId)

library(modelsummary)

modelsummary(
  list(
    'Baseline' = ei1,
    '+Policy' = ei2,
    '+Interaction' = ei3,
    '+Controls' = ei4,
    'Install FE' = ei5,
    'Phase 2' = ei6,
    'Phase 3' = ei7,
    'Phase 4' = ei8
  ),
  coef_map = c(
    'state_ownership_pct_tv' = 'State ownership %',
    'state_ownership_pct_tv:eps' = 'State ownership × EPS',
    'eps' = 'EPS',
    'v2x_polyarchy' = 'V-Dem polyarchy',
    'log(n_employees + 1)' = 'Log employees',
    'foreign_owned' = 'Foreign owned'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  title = 'Log emissions intensity: EU ETS power sector',
  output = 'output/tables/table_emissions_intensity.html'
)

# ── phase-specific baseline and policy specs ──────────────────────────────────

# phase 2
ei_p2_1 <- feols(log_emissions_intensity ~ state_ownership_pct_tv |
                   registry_id + year,
                 data = df_panel %>% filter(phase == 2),
                 cluster = ~bvdId)

ei_p2_2 <- feols(log_emissions_intensity ~ state_ownership_pct_tv +
                   eps + v2x_polyarchy |
                   registry_id + year,
                 data = df_panel %>% filter(phase == 2),
                 cluster = ~bvdId)

ei_p2_3 <- feols(log_emissions_intensity ~ state_ownership_pct_tv + eps +
                   v2x_polyarchy |
                   registry_id + year,
                 data = df_panel %>% filter(phase == 2),
                 cluster = ~bvdId)

# phase 3
ei_p3_1 <- feols(log_emissions_intensity ~ state_ownership_pct_tv |
                   registry_id + year,
                 data = df_panel %>% filter(phase == 3),
                 cluster = ~bvdId)

ei_p3_2 <- feols(log_emissions_intensity ~ state_ownership_pct_tv +
                   eps + v2x_polyarchy |
                   registry_id + year,
                 data = df_panel %>% filter(phase == 3),
                 cluster = ~bvdId)

ei_p3_3 <- feols(log_emissions_intensity ~ state_ownership_pct_tv + eps +
                   v2x_polyarchy |
                   registry_id + year,
                 data = df_panel %>% filter(phase == 3),
                 cluster = ~bvdId)

# phase 4
ei_p4_1 <- feols(log_emissions_intensity ~ state_ownership_pct_tv |
                   registry_id + year,
                 data = df_panel %>% filter(phase == 4),
                 cluster = ~bvdId)

ei_p4_2 <- feols(log_emissions_intensity ~ state_ownership_pct_tv +
                   v2x_polyarchy |
                   registry_id + year,
                 data = df_panel %>% filter(phase == 4),
                 cluster = ~bvdId)

ei_p4_3 <- feols(log_emissions_intensity ~ state_ownership_pct_tv +
                   v2x_polyarchy |
                   registry_id + year,
                 data = df_panel %>% filter(phase == 4),
                 cluster = ~bvdId)

# ── table ───────
modelsummary(
  list(
    'P2 Baseline' = ei_p2_1,
    'P2 +Policy'  = ei_p2_2,
    'P2 +Inter'   = ei_p2_3,
    'P3 Baseline' = ei_p3_1,
    'P3 +Policy'  = ei_p3_2,
    'P3 +Inter'   = ei_p3_3,
    'P4 Baseline' = ei_p4_1,
    'P4 +Policy'  = ei_p4_2,
    'P4 +Inter'   = ei_p4_3
  ),
  coef_map = c(
    'state_ownership_pct_tv'       = 'State ownership %',
    'state_ownership_pct_tv:eps'   = 'State ownership × EPS',
    'eps'                          = 'EPS',
    'v2x_polyarchy'                = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  title = 'Log emissions intensity by phase: EU ETS power sector',
  output = 'output/tables/table_emissions_intensity_by_phase.html'
)

# ── installation FE by phase ───────────────────────────────────────────────────

# phase 2
ei_p2_install <- feols(log_emissions_intensity ~ state_ownership_pct_tv + eps +
                         v2x_polyarchy |
                         installation_id + year,
                       data = df_panel %>% filter(phase == 2),
                       cluster = ~bvdId)

# phase 3
ei_p3_install <- feols(log_emissions_intensity ~ state_ownership_pct_tv + eps +
                         v2x_polyarchy |
                         installation_id + year,
                       data = df_panel %>% filter(phase == 3),
                       cluster = ~bvdId)

# phase 4 — no EPS
ei_p4_install <- feols(log_emissions_intensity ~ state_ownership_pct_tv +
                         v2x_polyarchy |
                         installation_id + year,
                       data = df_panel %>% filter(phase == 4),
                       cluster = ~bvdId)

modelsummary(
  list(
    'P2 Country FE' = ei_p2_3,
    'P2 Install FE' = ei_p2_install,
    'P3 Country FE' = ei_p3_3,
    'P3 Install FE' = ei_p3_install,
    'P4 Country FE' = ei_p4_3,
    'P4 Install FE' = ei_p4_install
  ),
  coef_map = c(
    'state_ownership_pct_tv'       = 'State ownership %',
    'state_ownership_pct_tv:eps'   = 'State ownership × EPS',
    'eps'                          = 'EPS',
    'v2x_polyarchy'                = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  title = 'Log emissions intensity by phase and FE: EU ETS power sector',
  output = 'output/tables/table_intensity_phase_fe.html'
)

library(tidyverse)
library(fixest)
library(modelsummary)

df_panel <- readRDS('data/processed/df_panel_merged.rds')

# ── log_verified main table ───────────────────────────────────────────────────
lv1 <- feols(log_verified ~ state_ownership_pct_tv + log(n_employees + 1) |
               registry_id + year,
             data = df_panel, cluster = ~bvdId)

lv2 <- feols(log_verified ~ state_ownership_pct_tv +
               eps + v2x_polyarchy + log(n_employees + 1)|
               registry_id + year,
             data = df_panel, cluster = ~bvdId)

lv3 <- feols(log_verified ~ state_ownership_pct_tv * eps +
               v2x_polyarchy + log(n_employees + 1) |
               registry_id + year,
             data = df_panel, cluster = ~bvdId)

lv4 <- feols(log_verified ~ state_ownership_pct_tv * eps +
               v2x_polyarchy + log(n_employees + 1) + foreign_owned |
               registry_id + year,
             data = df_panel, cluster = ~bvdId)

lv5 <- feols(log_verified ~ state_ownership_pct_tv * eps +
               v2x_polyarchy |
               installation_id + year,
             data = df_panel, cluster = ~bvdId)

modelsummary(
  list('Baseline' = lv1, '+Policy' = lv2, '+Interaction' = lv3,
       '+Controls' = lv4, 'Install FE' = lv5),
  coef_map = c(
    'state_ownership_pct_tv'     = 'State ownership %',
    'state_ownership_pct_tv:eps' = 'State ownership × EPS',
    'eps'                        = 'EPS',
    'v2x_polyarchy'              = 'V-Dem polyarchy',
    'log(n_employees + 1)'       = 'Log employees',
    'foreign_owned'              = 'Foreign owned'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  title = 'Log verified emissions: EU ETS power sector',
  output = 'output/tables/table_log_verified.html'
)

# all log_verified specs include log(n_employees + 1)
lv1 <- feols(log_verified ~ state_ownership_pct_tv +
               log(n_employees + 1) |
               registry_id + year,
             data = df_panel, cluster = ~bvdId)

lv2 <- feols(log_verified ~ state_ownership_pct_tv +
               eps + v2x_polyarchy +
               log(n_employees + 1) |
               registry_id + year,
             data = df_panel, cluster = ~bvdId)

lv3 <- feols(log_verified ~ state_ownership_pct_tv * eps +
               v2x_polyarchy + log(n_employees + 1) |
               registry_id + year,
             data = df_panel, cluster = ~bvdId)

lv4 <- feols(log_verified ~ state_ownership_pct_tv * eps +
               v2x_polyarchy |
               installation_id + year,
             data = df_panel, cluster = ~bvdId)

modelsummary(
  list('Baseline' = lv1, '+Policy' = lv2,
       '+Interaction' = lv3, 'Install FE' = lv4),
  coef_map = c(
    'state_ownership_pct_tv'     = 'State ownership %',
    'state_ownership_pct_tv:eps' = 'State ownership × EPS',
    'eps'                        = 'EPS',
    'v2x_polyarchy'              = 'V-Dem polyarchy',
    'log(n_employees + 1)'       = 'Log employees'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  title = 'Log verified emissions: EU ETS power sector',
  output = 'output/tables/table_log_verified.html'
)


# log verified installation fixed effects by phase
lv_p2 <- feols(log_verified ~ state_ownership_pct_tv * eps +
                 v2x_polyarchy |
                 installation_id + year,
               data = df_panel %>% filter(phase == 2),
               cluster = ~bvdId)

lv_p3 <- feols(log_verified ~ state_ownership_pct_tv * eps +
                 v2x_polyarchy |
                 installation_id + year,
               data = df_panel %>% filter(phase == 3),
               cluster = ~bvdId)

lv_p4 <- feols(log_verified ~ state_ownership_pct_tv +
                 v2x_polyarchy |
                 installation_id + year,
               data = df_panel %>% filter(phase == 4),
               cluster = ~bvdId)

modelsummary(
  list('Phase 2' = lv_p2, 'Phase 3' = lv_p3, 'Phase 4' = lv_p4),
  coef_map = c(
    'state_ownership_pct_tv'     = 'State ownership %',
    'state_ownership_pct_tv:eps' = 'State ownership × EPS',
    'eps'                        = 'EPS',
    'v2x_polyarchy'              = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  title = 'Log verified emissions by phase: installation FE',
  output = 'output/tables/table_log_verified_by_phase.html'
)


lv_inst1 <- feols(log_verified ~ state_ownership_pct_tv |
                    installation_id + year,
                  data = df_panel, cluster = ~bvdId)

lv_inst2 <- feols(log_verified ~ state_ownership_pct_tv +
                    eps + v2x_polyarchy |
                    installation_id + year,
                  data = df_panel, cluster = ~bvdId)

lv_inst3 <- feols(log_verified ~ state_ownership_pct_tv * eps +
                    v2x_polyarchy |
                    installation_id + year,
                  data = df_panel, cluster = ~bvdId)

modelsummary(
  list('Baseline' = lv_inst1, '+Policy' = lv_inst2,
       '+Interaction' = lv_inst3),
  coef_map = c(
    'state_ownership_pct_tv'     = 'State ownership %',
    'state_ownership_pct_tv:eps' = 'State ownership × EPS',
    'eps'                        = 'EPS',
    'v2x_polyarchy'              = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  title = 'Log verified emissions: installation FE',
  output = 'output/tables/table_log_verified_install_fe.html'
)

#phase dummies, installation FE

lv_phase_base <- feols(
  log_verified ~ 
    state_ownership_pct_tv +
    state_ownership_pct_tv:phase3 +
    state_ownership_pct_tv:phase4 |
    installation_id + year,
  data = df_panel,
  cluster = ~bvdId
)

lv_phase_policy <- feols(
  log_verified ~ 
    state_ownership_pct_tv +
    state_ownership_pct_tv:phase3 +
    state_ownership_pct_tv:phase4 +
    eps + v2x_polyarchy |
    installation_id + year,
  data = df_panel,
  cluster = ~bvdId
)

lv_phase_inter <- feols(
  log_verified ~ 
    state_ownership_pct_tv +
    state_ownership_pct_tv:phase3 +
    state_ownership_pct_tv:phase4 +
    state_ownership_pct_tv:eps +
    eps + v2x_polyarchy |
    installation_id + year,
  data = df_panel,
  cluster = ~bvdId
)

modelsummary(
  list('Baseline' = lv_phase_base,
       '+Policy'  = lv_phase_policy,
       '+Interaction' = lv_phase_inter),
  coef_map = c(
    'state_ownership_pct_tv'        = 'State ownership % (Phase 2)',
    'state_ownership_pct_tv:phase3' = 'State ownership × Phase 3',
    'state_ownership_pct_tv:phase4' = 'State ownership × Phase 4',
    'state_ownership_pct_tv:eps'    = 'State ownership × EPS',
    'eps'                           = 'EPS',
    'v2x_polyarchy'                 = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  title = 'Log verified emissions: phase interactions, installation FE',
  output = 'output/tables/table_log_verified_phase_interactions.html'
)



# ── log_emissions_intensity: phase interactions, installation FE ───────────────
ei_phase_base <- feols(
  log_emissions_intensity ~ 
    state_ownership_pct_tv +
    state_ownership_pct_tv:phase3 +
    state_ownership_pct_tv:phase4 |
    installation_id + year,
  data = df_panel,
  cluster = ~bvdId
)

ei_phase_policy <- feols(
  log_emissions_intensity ~ 
    state_ownership_pct_tv +
    state_ownership_pct_tv:phase3 +
    state_ownership_pct_tv:phase4 +
    eps + v2x_polyarchy |
    installation_id + year,
  data = df_panel,
  cluster = ~bvdId
)

ei_phase_inter <- feols(
  log_emissions_intensity ~ 
    state_ownership_pct_tv +
    state_ownership_pct_tv:phase3 +
    state_ownership_pct_tv:phase4 +
    state_ownership_pct_tv:eps +
    eps + v2x_polyarchy |
    installation_id + year,
  data = df_panel,
  cluster = ~bvdId
)

# ── surplus_norm: phase interactions, installation FE ─────────────────────────
sn_phase_base <- feols(
  surplus_norm ~ 
    state_ownership_pct_tv +
    state_ownership_pct_tv:phase3 +
    state_ownership_pct_tv:phase4 |
    installation_id + year,
  data = df_panel,
  cluster = ~bvdId
)

sn_phase_policy <- feols(
  surplus_norm ~ 
    state_ownership_pct_tv +
    state_ownership_pct_tv:phase3 +
    state_ownership_pct_tv:phase4 +
    eps + v2x_polyarchy |
    installation_id + year,
  data = df_panel,
  cluster = ~bvdId
)

sn_phase_inter <- feols(
  surplus_norm ~ 
    state_ownership_pct_tv +
    state_ownership_pct_tv:phase3 +
    state_ownership_pct_tv:phase4 +
    state_ownership_pct_tv:eps +
    eps + v2x_polyarchy |
    installation_id + year,
  data = df_panel,
  cluster = ~bvdId
)

# ── tables ────────────────────────────────────────────────────────────────────
coef_labels <- c(
  'state_ownership_pct_tv'        = 'State ownership % (Phase 2)',
  'state_ownership_pct_tv:phase3' = 'State ownership × Phase 3',
  'state_ownership_pct_tv:phase4' = 'State ownership × Phase 4',
  'state_ownership_pct_tv:eps'    = 'State ownership × EPS',
  'eps'                           = 'EPS',
  'v2x_polyarchy'                 = 'V-Dem polyarchy'
)

modelsummary(
  list('Baseline' = ei_phase_base,
       '+Policy'  = ei_phase_policy,
       '+Interaction' = ei_phase_inter),
  coef_map = coef_labels,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  title = 'Log emissions intensity: phase interactions, installation FE',
  output = 'output/tables/table_intensity_phase_interactions.html'
)

modelsummary(
  list('Baseline' = sn_phase_base,
       '+Policy'  = sn_phase_policy,
       '+Interaction' = sn_phase_inter),
  coef_map = coef_labels,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  title = 'Surplus norm: phase interactions, installation FE',
  output = 'output/tables/table_surplus_phase_interactions.html'
)

# how much does employment vary within installations over time?
df_panel %>%
  filter(!is.na(n_employees)) %>%
  group_by(installation_id) %>%
  summarise(
    cv_employees = sd(n_employees, na.rm = TRUE) / 
      mean(n_employees, na.rm = TRUE)
  ) %>%
  summary()


# ── country FE, log_verified, with employees ──────────────────────────────────

# load panel
df_panel <- readRDS('data/processed/df_panel_merged.rds')

# check it loaded correctly
nrow(df_panel)
names(df_panel) %>% grep('verified|ownership|eps', ., value = TRUE)

# recreate phase dummies
df_panel <- df_panel %>%
  mutate(
    phase3 = as.integer(phase == 3),
    phase4 = as.integer(phase == 4)
  )


# pooled
lv_c1 <- feols(log_verified ~ state_ownership_pct_tv +
                 log(n_employees + 1) |
                 registry_id + year,
               data = df_panel, cluster = ~bvdId)

lv_c2 <- feols(log_verified ~ state_ownership_pct_tv +
                 eps + v2x_polyarchy +
                 log(n_employees + 1) |
                 registry_id + year,
               data = df_panel, cluster = ~bvdId)

lv_c3 <- feols(log_verified ~ state_ownership_pct_tv * eps +
                 v2x_polyarchy +
                 log(n_employees + 1) |
                 registry_id + year,
               data = df_panel, cluster = ~bvdId)

# phase 2
lv_c_p2 <- feols(log_verified ~ state_ownership_pct_tv * eps +
                   v2x_polyarchy + log(n_employees + 1) |
                   registry_id + year,
                 data = df_panel %>% filter(phase == 2),
                 cluster = ~bvdId)

# phase 3
lv_c_p3 <- feols(log_verified ~ state_ownership_pct_tv * eps +
                   v2x_polyarchy + log(n_employees + 1) |
                   registry_id + year,
                 data = df_panel %>% filter(phase == 3),
                 cluster = ~bvdId)

# phase 4
lv_c_p4 <- feols(log_verified ~ state_ownership_pct_tv +
                   v2x_polyarchy + log(n_employees + 1) |
                   registry_id + year,
                 data = df_panel %>% filter(phase == 4),
                 cluster = ~bvdId)

modelsummary(
  list('Baseline' = lv_c1, '+Policy' = lv_c2,
       '+Interaction' = lv_c3,
       'Phase 2' = lv_c_p2, 'Phase 3' = lv_c_p3,
       'Phase 4' = lv_c_p4),
  coef_map = c(
    'state_ownership_pct_tv'     = 'State ownership %',
    'state_ownership_pct_tv:eps' = 'State ownership × EPS',
    'eps'                        = 'EPS',
    'v2x_polyarchy'              = 'V-Dem polyarchy',
    'log(n_employees + 1)'       = 'Log employees'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  title = 'Log verified emissions: country FE with employees control',
  output = 'output/tables/table_lv_country_fe_employees.html')
  
# regressions with new carbon price

df_panel <- readRDS("data/processed/df_panel_merged.rds")
colnames(df_panel) %>% grep("carbon", ., value = TRUE, ignore.case = TRUE)  # confirm it's there now

price_inst1 <- feols(log_verified ~ state_ownership_pct_tv |
                    installation_id + year,
                  data = df_panel, cluster = ~bvdId)

price_inst2 <- feols(log_verified ~ state_ownership_pct_tv +
                       combined_carbon_price + v2x_polyarchy |
                    installation_id + year,
                  data = df_panel, cluster = ~bvdId)

price_inst3 <- feols(log_verified ~ state_ownership_pct_tv * combined_carbon_price +
                    v2x_polyarchy |
                    installation_id + year,
                  data = df_panel, cluster = ~bvdId)

modelsummary(
  list('Baseline' = price_inst1, '+Policy' = price_inst2,
       '+Interaction' = price_inst3),
  coef_map = c(
    'state_ownership_pct_tv'                          = 'State ownership %',
    'state_ownership_pct_tv:combined_carbon_price'    = 'State ownership × Combined price',
    'combined_carbon_price'                           = 'Combined carbon price',
    'v2x_polyarchy'                                    = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  title = 'Log verified emissions: installation FE',
  output = 'output/tables/table_log_verified_price_install_fe.html'
)

# phase by phase 

lv_price_p2 <- feols(log_verified ~ state_ownership_pct_tv * combined_carbon_price +
                       v2x_polyarchy |
                       installation_id + year,
                     data = df_panel %>% filter(phase == 2),
                     cluster = ~bvdId)

lv_price_p3 <- feols(log_verified ~ state_ownership_pct_tv * combined_carbon_price +
                       v2x_polyarchy |
                       installation_id + year,
                     data = df_panel %>% filter(phase == 3),
                     cluster = ~bvdId)

lv_price_p4 <- feols(log_verified ~ state_ownership_pct_tv * combined_carbon_price +
                       v2x_polyarchy |
                       installation_id + year,
                     data = df_panel %>% filter(phase == 4),
                     cluster = ~bvdId)

modelsummary(
  list('Phase 2' = lv_price_p2, 'Phase 3' = lv_price_p3, 'Phase 4' = lv_price_p4),
  coef_map = c(
    'state_ownership_pct_tv'                       = 'State ownership %',
    'state_ownership_pct_tv:combined_carbon_price'  = 'State ownership × Combined price',
    'combined_carbon_price'                         = 'Combined carbon price',
    'v2x_polyarchy'                                 = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  title = 'Log verified emissions by phase (combined carbon price): installation FE',
  output = 'output/tables/table_log_verified_by_phase_price.html'
)



price_phase_inter <- feols(
  log_verified ~ 
    state_ownership_pct_tv +
    state_ownership_pct_tv:phase3 +
    state_ownership_pct_tv:phase4 +
    state_ownership_pct_tv:combined_carbon_price +
    combined_carbon_price + v2x_polyarchy |
    installation_id + year,
  data = df_panel,
  cluster = ~bvdId
)
summary(price_phase_inter)

price_phase_inter_full <- feols(
  log_verified ~ 
    state_ownership_pct_tv +
    state_ownership_pct_tv:phase3 +
    state_ownership_pct_tv:phase4 +
    state_ownership_pct_tv:combined_carbon_price +
    state_ownership_pct_tv:combined_carbon_price:phase3 +
    state_ownership_pct_tv:combined_carbon_price:phase4 +
    combined_carbon_price + v2x_polyarchy |
    installation_id + year,
  data = df_panel,
  cluster = ~bvdId
)

summary(price_phase_inter_full)



### TARGET CHECKS 

library(tidyverse); library(fixest); library(modelsummary)

df_panel <- readRDS("data/processed/df_panel_merged.rds") %>%
  mutate(phase3 = as.integer(phase == 3), phase4 = as.integer(phase == 4))

# ── NOx target: installation FE, pooled + phase interactions ──────────────────
nox1 <- feols(log_verified ~ state_ownership_pct_tv |
                installation_id + year,
              data = df_panel, cluster = ~bvdId)

nox2 <- feols(log_verified ~ state_ownership_pct_tv + nox_target_2020_29 + v2x_polyarchy |
                installation_id + year,
              data = df_panel, cluster = ~bvdId)

nox3 <- feols(log_verified ~ state_ownership_pct_tv * nox_target_2020_29 + v2x_polyarchy |
                installation_id + year,
              data = df_panel, cluster = ~bvdId)

nox_phase <- feols(log_verified ~ 
                     state_ownership_pct_tv +
                     state_ownership_pct_tv:phase3 +
                     state_ownership_pct_tv:phase4 +
                     state_ownership_pct_tv:nox_target_2020_29 +
                     nox_target_2020_29 + v2x_polyarchy |
                     installation_id + year,
                   data = df_panel, cluster = ~bvdId)

modelsummary(
  list('Baseline' = nox1, '+Policy' = nox2, '+Interaction' = nox3, '+Phase' = nox_phase),
  coef_map = c(
    'state_ownership_pct_tv'                         = 'State ownership %',
    'state_ownership_pct_tv:phase3'                  = 'State ownership × Phase 3',
    'state_ownership_pct_tv:phase4'                  = 'State ownership × Phase 4',
    'state_ownership_pct_tv:nox_target_2020_29'      = 'State ownership × NOx target',
    'nox_target_2020_29'                             = 'NOx reduction target',
    'v2x_polyarchy'                                  = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  title = 'Log verified emissions: NOx reduction commitment (non-market proxy)',
  output = 'output/tables/table_nox_target_install_fe.html'
)

# ── SO2 target: same structure ─────────────────────────────────────────────────

df_panel <- readRDS("data/processed/df_panel_merged.rds")

df_panel <- df_panel %>%
  mutate(
    phase3 = as.integer(phase == 3),
    phase4 = as.integer(phase == 4)
  )


so21 <- feols(log_verified ~ state_ownership_pct_tv |
                installation_id + year,
              data = df_panel, cluster = ~bvdId)

so22 <- feols(log_verified ~ state_ownership_pct_tv + so2_target_2020_29 + v2x_polyarchy |
                installation_id + year,
              data = df_panel, cluster = ~bvdId)

so23 <- feols(log_verified ~ state_ownership_pct_tv * so2_target_2020_29 + v2x_polyarchy |
                installation_id + year,
              data = df_panel, cluster = ~bvdId)

so2_phase <- feols(log_verified ~ 
                     state_ownership_pct_tv +
                     state_ownership_pct_tv:phase3 +
                     state_ownership_pct_tv:phase4 +
                     state_ownership_pct_tv:so2_target_2020_29 +
                     so2_target_2020_29 + v2x_polyarchy |
                     installation_id + year,
                   data = df_panel, cluster = ~bvdId)

modelsummary(
  list('Baseline' = so21, '+Policy' = so22, '+Interaction' = so23, '+Phase' = so2_phase),
  coef_map = c(
    'state_ownership_pct_tv'                        = 'State ownership %',
    'state_ownership_pct_tv:phase3'                 = 'State ownership × Phase 3',
    'state_ownership_pct_tv:phase4'                 = 'State ownership × Phase 4',
    'state_ownership_pct_tv:so2_target_2020_29'     = 'State ownership × SO2 target',
    'so2_target_2020_29'                            = 'SO2 reduction target',
    'v2x_polyarchy'                                 = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  title = 'Log verified emissions: SO2 reduction commitment (non-market proxy)',
  output = 'output/tables/table_so2_target_install_fe.html'
)

# ── favoritism check: does ownership predict penalty, conditional on non-compliance ──
df_panel %>% filter(compliant == 0) %>% count(penalty)

favoritism <- feols(penalty ~ state_ownership_pct_tv |
                      installation_id + year,
                    data = df_panel %>% filter(compliant == 0), cluster = ~bvdId)
summary(favoritism)


raw_compliance <- read_csv("data/raw/compliance.csv")  # or whatever your original source file is called
raw_compliance %>% summarise(pct_na = mean(is.na(penalty)))


# institutional quality 

feols(log_verified ~ state_ownership_pct_tv * nox_target_2020_29 * v2x_corr |
        installation_id + year, data = df_panel, cluster = ~bvdId)


# tables yipeee

# ── triple interactions: institutional quality moderation ─────────────────────
nox_triple <- feols(log_verified ~ state_ownership_pct_tv * nox_target_2020_29 * v2x_corr +
                      v2x_polyarchy |
                      installation_id + year, data = df_panel, cluster = ~bvdId)

so2_triple <- feols(log_verified ~ state_ownership_pct_tv * so2_target_2020_29 * v2x_corr +
                      v2x_polyarchy |
                      installation_id + year, data = df_panel, cluster = ~bvdId)

# price_phase_inter_full already exists from earlier in this session
# (ownership x combined_carbon_price x phase3/phase4)

modelsummary(
  list(
    'NOx target × Corruption' = nox_triple,
    'SO2 target × Corruption' = so2_triple,
    'Price × Phase' = price_phase_inter_full
  ),
  coef_map = c(
    'state_ownership_pct_tv'                                       = 'State ownership %',
    'state_ownership_pct_tv:nox_target_2020_29'                    = 'Ownership × NOx target',
    'state_ownership_pct_tv:v2x_corr'                               = 'Ownership × Corruption',
    'state_ownership_pct_tv:nox_target_2020_29:v2x_corr'           = 'Ownership × NOx target × Corruption',
    'state_ownership_pct_tv:so2_target_2020_29'                    = 'Ownership × SO2 target',
    'state_ownership_pct_tv:so2_target_2020_29:v2x_corr'           = 'Ownership × SO2 target × Corruption',
    'state_ownership_pct_tv:phase3'                                 = 'Ownership × Phase 3',
    'state_ownership_pct_tv:phase4'                                 = 'Ownership × Phase 4',
    'state_ownership_pct_tv:combined_carbon_price'                 = 'Ownership × Price',
    'state_ownership_pct_tv:phase3:combined_carbon_price'          = 'Ownership × Price × Phase 3',
    'state_ownership_pct_tv:phase4:combined_carbon_price'          = 'Ownership × Price × Phase 4',
    'combined_carbon_price'                                         = 'Combined carbon price',
    'v2x_corr'                                                       = 'Corruption (V-Dem)',
    'v2x_polyarchy'                                                  = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  fmt = fmt_decimal(5),
  title = 'Triple interactions: ownership, stringency mechanism, and moderators',
  output = 'output/tables/table_triple_interactions_combined.html'
)

corr_values <- quantile(df_panel$v2x_corr, probs = c(0.25, 0.75), na.rm = TRUE)
print(corr_values)  # confirm what "low" and "high" corruption actually mean in real terms

# pull the four coefficients from your nox_triple model (with polyarchy control)
b_own_nox     <- coef(nox_triple)["state_ownership_pct_tv:nox_target_2020_29"]
b_triple      <- coef(nox_triple)["state_ownership_pct_tv:nox_target_2020_29:v2x_corr"]

# the ownership x NOx-target slope, evaluated at each corruption quantile
slope_low_corr  <- b_own_nox + b_triple * corr_values[1]
slope_high_corr <- b_own_nox + b_triple * corr_values[2]

# convert to approximate % effect on verified emissions, per unit of NOx target
slope_low_corr_pct  <- slope_low_corr * 100
slope_high_corr_pct <- slope_high_corr * 100

corr_extremes <- c(low = 0.002, high = 0.646)

slope_at_extremes <- b_own_nox + b_triple * corr_extremes
slope_at_extremes_pct <- slope_at_extremes * 100
print(slope_at_extremes_pct)

df_panel %>% filter(v2x_corr > 0.3) %>% distinct(country, year) %>% nrow()
df_panel %>% filter(v2x_corr > 0.3) %>% distinct(country) %>% pull(country)


# ambition 

df_panel <- readRDS("data/processed/df_panel_merged.rds") %>%
  mutate(phase3 = as.integer(phase == 3), phase4 = as.integer(phase == 4))

# ── NOx ambition gap: installation FE, pooled + phase interactions ────────────
amb1 <- feols(log_verified ~ state_ownership_pct_tv |
                installation_id + year,
              data = df_panel, cluster = ~bvdId)

amb2 <- feols(log_verified ~ state_ownership_pct_tv + nox_ambition_gap + v2x_polyarchy |
                installation_id + year,
              data = df_panel, cluster = ~bvdId)

amb3 <- feols(log_verified ~ state_ownership_pct_tv * nox_ambition_gap + v2x_polyarchy |
                installation_id + year,
              data = df_panel, cluster = ~bvdId)

amb_phase <- feols(log_verified ~ 
                     state_ownership_pct_tv +
                     state_ownership_pct_tv:phase3 +
                     state_ownership_pct_tv:phase4 +
                     state_ownership_pct_tv:nox_ambition_gap +
                     nox_ambition_gap + v2x_polyarchy |
                     installation_id + year,
                   data = df_panel, cluster = ~bvdId)

modelsummary(
  list('Baseline' = amb1, '+Policy' = amb2, '+Interaction' = amb3, '+Phase' = amb_phase),
  coef_map = c(
    'state_ownership_pct_tv'                     = 'State ownership %',
    'state_ownership_pct_tv:phase3'              = 'State ownership × Phase 3',
    'state_ownership_pct_tv:phase4'               = 'State ownership × Phase 4',
    'state_ownership_pct_tv:nox_ambition_gap'    = 'State ownership × NOx ambition gap',
    'nox_ambition_gap'                            = 'NOx ambition gap',
    'v2x_polyarchy'                               = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  title = 'Log verified emissions: NOx ambition gap (trend-adjusted non-market proxy)',
  output = 'output/tables/table_nox_ambition_install_fe.html'
)



library(fixest); library(dplyr)

df_panel <- readRDS("data/processed/df_panel_merged.rds") %>%
  mutate(phase3 = as.integer(phase == 3), phase4 = as.integer(phase == 4))

# ── triple interaction: ownership x ambition gap x corruption ─────────────────
amb_triple <- feols(log_verified ~ state_ownership_pct_tv * nox_ambition_gap * v2x_corr +
                      v2x_polyarchy |
                      installation_id + year, data = df_panel, cluster = ~bvdId)

summary(amb_triple)

# ── implied slope at true min/max corruption ───────────────────────────────────
corr_extremes <- c(low = 0.002, high = 0.646)

b_own_amb <- coef(amb_triple)["state_ownership_pct_tv:nox_ambition_gap"]
b_triple  <- coef(amb_triple)["state_ownership_pct_tv:nox_ambition_gap:v2x_corr"]

slope_at_extremes <- b_own_amb + b_triple * corr_extremes
slope_at_extremes_pct <- slope_at_extremes * 100

print(slope_at_extremes_pct)

# ── observation check at high-corruption tail ───────────────────────────────────
df_panel %>% filter(v2x_corr > 0.3) %>% distinct(country, year) %>% nrow()
df_panel %>% filter(v2x_corr > 0.3) %>% distinct(country) %>% pull(country)

# general binding rules

uses_gbr <- c("AT","BE","DE","DK","FR","NL","SE")
df_panel <- df_panel %>% mutate(gbr_strict = as.integer(country %in% uses_gbr))


df_panel <- df_panel %>% mutate(phase3 = as.integer(phase == 3), phase4 = as.integer(phase == 4))

# ── GBR strictness: installation FE, pooled + phase interactions ──────────────
gbr1 <- feols(log_verified ~ state_ownership_pct_tv |
                installation_id + year,
              data = df_panel, cluster = ~bvdId)

gbr2 <- feols(log_verified ~ state_ownership_pct_tv + gbr_strict + v2x_polyarchy |
                installation_id + year,
              data = df_panel, cluster = ~bvdId)

gbr3 <- feols(log_verified ~ state_ownership_pct_tv * gbr_strict + v2x_polyarchy |
                installation_id + year,
              data = df_panel, cluster = ~bvdId)

gbr_phase <- feols(log_verified ~ 
                     state_ownership_pct_tv +
                     state_ownership_pct_tv:phase3 +
                     state_ownership_pct_tv:phase4 +
                     state_ownership_pct_tv:gbr_strict +
                     gbr_strict + v2x_polyarchy |
                     installation_id + year,
                   data = df_panel, cluster = ~bvdId)

modelsummary(
  list('Baseline' = gbr1, '+Policy' = gbr2, '+Interaction' = gbr3, '+Phase' = gbr_phase),
  coef_map = c(
    'state_ownership_pct_tv'                  = 'State ownership %',
    'state_ownership_pct_tv:phase3'           = 'State ownership × Phase 3',
    'state_ownership_pct_tv:phase4'           = 'State ownership × Phase 4',
    'state_ownership_pct_tv:gbr_strict'       = 'State ownership × GBR-strict',
    'gbr_strict'                               = 'Uses General Binding Rules',
    'v2x_polyarchy'                            = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  fmt = fmt_decimal(5),
  title = 'Log verified emissions: GBR strictness (firm-facing non-market proxy)',
  output = 'output/tables/table_gbr_strict_install_fe.html'
)



gbr_triple <- feols(log_verified ~ 
                      state_ownership_pct_tv +
                      state_ownership_pct_tv:phase3 +
                      state_ownership_pct_tv:phase4 +
                      state_ownership_pct_tv:gbr_strict +
                      state_ownership_pct_tv:gbr_strict:phase3 +
                      state_ownership_pct_tv:gbr_strict:phase4 +
                      gbr_strict + v2x_polyarchy |
                      installation_id + year,
                    data = df_panel, cluster = ~bvdId)

summary(gbr_triple)



# combined index 
df_panel <- df_panel %>%
  mutate(
    price_z = scale(combined_carbon_price)[,1],
    gbr_z = scale(gbr_strict)[,1],
    self_eps = (price_z + gbr_z) / 2
  )

self_eps_triple <- feols(log_verified ~ 
                           state_ownership_pct_tv +
                           state_ownership_pct_tv:phase3 +
                           state_ownership_pct_tv:phase4 +
                           state_ownership_pct_tv:self_eps +
                           state_ownership_pct_tv:self_eps:phase3 +
                           state_ownership_pct_tv:self_eps:phase4 +
                           self_eps + v2x_polyarchy |
                           installation_id + year,
                         data = df_panel, cluster = ~bvdId)

summary(self_eps_triple)


modelsummary(
  list('Price alone' = price_phase_inter_full, 'Price + GBR (self-EPS)' = self_eps_triple),
  coef_map = c(
    'state_ownership_pct_tv'                              = 'State ownership %',
    'state_ownership_pct_tv:phase3'                        = 'Ownership × Phase 3',
    'state_ownership_pct_tv:phase4'                        = 'Ownership × Phase 4',
    'state_ownership_pct_tv:combined_carbon_price'         = 'Ownership × Price',
    'state_ownership_pct_tv:phase3:combined_carbon_price'  = 'Ownership × Price × Phase 3',
    'state_ownership_pct_tv:phase4:combined_carbon_price'  = 'Ownership × Price × Phase 4',
    'state_ownership_pct_tv:self_eps'                       = 'Ownership × Self-EPS',
    'state_ownership_pct_tv:phase3:self_eps'               = 'Ownership × Self-EPS × Phase 3',
    'state_ownership_pct_tv:phase4:self_eps'               = 'Ownership × Self-EPS × Phase 4',
    'combined_carbon_price'                                 = 'Combined carbon price',
    'self_eps'                                              = 'Self-made EPS (price + GBR)',
    'v2x_polyarchy'                                         = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  fmt = fmt_decimal(5),
  title = 'Comparison: price-only vs. combined price+GBR index',
  output = 'output/tables/table_price_vs_selfeps_comparison.html'
)


# hw they react to democracy and corruptin 

# ── Market mechanism (price) × institutional moderators ───────────────────────
price_corr <- feols(log_verified ~ state_ownership_pct_tv * combined_carbon_price * v2x_corr |
                      installation_id + year, data = df_panel, cluster = ~bvdId)

price_dem <- feols(log_verified ~ state_ownership_pct_tv * combined_carbon_price * v2x_polyarchy |
                     installation_id + year, data = df_panel, cluster = ~bvdId)

# ── Non-market mechanism (GBR) × institutional moderators ─────────────────────
gbr_corr <- feols(log_verified ~ state_ownership_pct_tv * gbr_strict * v2x_corr |
                    installation_id + year, data = df_panel, cluster = ~bvdId)

gbr_dem <- feols(log_verified ~ state_ownership_pct_tv * gbr_strict * v2x_polyarchy |
                   installation_id + year, data = df_panel, cluster = ~bvdId)

modelsummary(
  list(
    'Price × Corruption' = price_corr,
    'Price × Democracy'  = price_dem,
    'GBR × Corruption'   = gbr_corr,
    'GBR × Democracy'    = gbr_dem
  ),
  coef_map = c(
    'state_ownership_pct_tv'                              = 'State ownership %',
    'state_ownership_pct_tv:combined_carbon_price'        = 'Ownership × Price',
    'state_ownership_pct_tv:v2x_corr'                     = 'Ownership × Corruption',
    'state_ownership_pct_tv:v2x_polyarchy'                = 'Ownership × Democracy',
    'state_ownership_pct_tv:combined_carbon_price:v2x_corr'    = 'Ownership × Price × Corruption',
    'state_ownership_pct_tv:combined_carbon_price:v2x_polyarchy' = 'Ownership × Price × Democracy',
    'state_ownership_pct_tv:gbr_strict'                    = 'Ownership × GBR',
    'state_ownership_pct_tv:gbr_strict:v2x_corr'          = 'Ownership × GBR × Corruption',
    'state_ownership_pct_tv:gbr_strict:v2x_polyarchy'     = 'Ownership × GBR × Democracy',
    'combined_carbon_price'                                = 'Combined carbon price',
    'gbr_strict'                                           = 'Uses GBR',
    'v2x_corr'                                             = 'Corruption (V-Dem)',
    'v2x_polyarchy'                                        = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  fmt = fmt_decimal(5),
  title = 'Market vs. non-market stringency: interaction with institutional quality',
  output = 'output/tables/table_market_nonmarket_institutions.html'
)


# ── reload and rebuild what you need ───────────────────────────────────────────
library(tidyverse); library(fixest); library(modelsummary)

df_panel <- readRDS("data/processed/df_panel_merged.rds") %>%
  mutate(phase3 = as.integer(phase == 3), phase4 = as.integer(phase == 4))

uses_gbr <- c("AT","BE","DE","DK","FR","NL","SE")

df_panel <- df_panel %>%
  mutate(gbr_strict = as.integer(country %in% uses_gbr))

# ── NEW: country FE instead of installation FE, so the direct effect of 
# country-fixed policy variables (NEC target, GBR) is actually estimable ──────

nox_country <- feols(log_verified ~ state_ownership_pct_tv * nox_target_2020_29 + v2x_polyarchy |
                       registry_id + year,
                     data = df_panel, cluster = ~bvdId)

gbr_country <- feols(log_verified ~ state_ownership_pct_tv * gbr_strict + v2x_polyarchy |
                       registry_id + year,
                     data = df_panel, cluster = ~bvdId)

modelsummary(
  list('NOx target (country FE)' = nox_country, 'GBR strict (country FE)' = gbr_country),
  coef_map = c(
    'state_ownership_pct_tv'                     = 'State ownership %',
    'nox_target_2020_29'                          = 'NOx reduction target',
    'state_ownership_pct_tv:nox_target_2020_29'  = 'Ownership × NOx target',
    'gbr_strict'                                   = 'Uses GBR',
    'state_ownership_pct_tv:gbr_strict'           = 'Ownership × GBR',
    'v2x_polyarchy'                                = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  fmt = fmt_decimal(5),
  title = 'Country FE: direct effect of non-market policy instruments',
  output = 'output/tables/table_nonmarket_country_fe.html'
)




nox_country <- feols(log_verified ~ state_ownership_pct_tv * nox_target_2020_29 + v2x_polyarchy |
                       registry_id + year,
                     data = df_panel, cluster = ~bvdId)


df_panel <- readRDS("data/processed/df_panel_merged.rds") %>%
  mutate(phase3 = as.integer(phase == 3), phase4 = as.integer(phase == 4))

so2_country <- feols(log_verified ~ state_ownership_pct_tv * so2_target_2020_29 + v2x_polyarchy |
                       registry_id + year,
                     data = df_panel, cluster = ~bvdId)

modelsummary(
  list('NOx target (country FE)' = nox_country, 'SO2 target (country FE)' = so2_country),
  coef_map = c(
    'state_ownership_pct_tv'                     = 'State ownership %',
    'nox_target_2020_29'                          = 'NOx reduction target',
    'state_ownership_pct_tv:nox_target_2020_29'  = 'Ownership × NOx target',
    'so2_target_2020_29'                          = 'SO2 reduction target',
    'state_ownership_pct_tv:so2_target_2020_29'  = 'Ownership × SO2 target',
    'v2x_polyarchy'                                = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  fmt = fmt_decimal(5),
  title = 'Country FE: NOx vs SO2 reduction targets',
  output = 'output/tables/table_nox_so2_country_fe.html'
)

library(tidyverse); library(fixest); library(modelsummary)

df_panel <- readRDS("data/processed/df_panel_merged.rds") %>%
  mutate(phase3 = as.integer(phase == 3), phase4 = as.integer(phase == 4))

nox1 <- feols(log_verified ~ state_ownership_pct_tv |
                installation_id + year,
              data = df_panel, cluster = ~bvdId)

nox_phase <- feols(log_verified ~ 
                     state_ownership_pct_tv +
                     state_ownership_pct_tv:phase3 +
                     state_ownership_pct_tv:phase4 +
                     state_ownership_pct_tv:nox_target_2020_29 +
                     nox_target_2020_29 + v2x_polyarchy |
                     installation_id + year,
                   data = df_panel, cluster = ~bvdId)

so2_1 <- feols(log_verified ~ state_ownership_pct_tv |
                 installation_id + year,
               data = df_panel, cluster = ~bvdId)

so2_phase <- feols(log_verified ~ 
                     state_ownership_pct_tv +
                     state_ownership_pct_tv:phase3 +
                     state_ownership_pct_tv:phase4 +
                     state_ownership_pct_tv:so2_target_2020_29 +
                     so2_target_2020_29 + v2x_polyarchy |
                     installation_id + year,
                   data = df_panel, cluster = ~bvdId)





modelsummary(
  list(
    'Baseline' = nox1, 'NOx +Phase' = nox_phase, 'SO2 +Phase' = so2_phase
  ),
  coef_map = c(
    'state_ownership_pct_tv'                     = 'State ownership %',
    'state_ownership_pct_tv:phase3'              = 'Ownership × Phase 3',
    'state_ownership_pct_tv:phase4'              = 'Ownership × Phase 4',
    'state_ownership_pct_tv:nox_target_2020_29'  = 'Ownership × NOx target',
    'state_ownership_pct_tv:so2_target_2020_29'  = 'Ownership × SO2 target',
    'nox_target_2020_29'                          = 'NOx target',
    'so2_target_2020_29'                          = 'SO2 target',
    'v2x_polyarchy'                                = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  fmt = fmt_decimal(5),
  title = 'Log verified emissions: NOx vs SO2 targets, installation FE',
  output = 'output/tables/table_nox_so2_install_fe.html'
)


# by phase 


nox_triple_phase <- feols(log_verified ~ 
                            state_ownership_pct_tv +
                            state_ownership_pct_tv:phase3 +
                            state_ownership_pct_tv:phase4 +
                            state_ownership_pct_tv:nox_target_2020_29 +
                            state_ownership_pct_tv:nox_target_2020_29:phase3 +
                            state_ownership_pct_tv:nox_target_2020_29:phase4 +
                            nox_target_2020_29 + v2x_polyarchy |
                            installation_id + year,
                          data = df_panel, cluster = ~bvdId)

so2_triple_phase <- feols(log_verified ~ 
                            state_ownership_pct_tv +
                            state_ownership_pct_tv:phase3 +
                            state_ownership_pct_tv:phase4 +
                            state_ownership_pct_tv:so2_target_2020_29 +
                            state_ownership_pct_tv:so2_target_2020_29:phase3 +
                            state_ownership_pct_tv:so2_target_2020_29:phase4 +
                            so2_target_2020_29 + v2x_polyarchy |
                            installation_id + year,
                          data = df_panel, cluster = ~bvdId)

summary(nox_triple_phase)
summary(so2_triple_phase)


df_panel %>%
  distinct(country, year, eps, nox_target_2020_29, so2_target_2020_29) %>%
  filter(!is.na(eps)) %>%
  summarise(
    cor_nox = cor(eps, nox_target_2020_29, use = "complete.obs"),
    cor_so2 = cor(eps, so2_target_2020_29, use = "complete.obs")
  )

# using ambition variables 


library(dplyr); library(tidyr); library(purrr); library(MASS)


library(readr); library(dplyr)

nec_raw <- read_tsv("data/raw/NEC_2026.csv",
                    col_types = cols(value = col_double(), unit = col_character()))

nec_clean <- nec_raw %>%
  filter(pollutantName %in% c("NOx", "SOx", "PM2.5"),
         parentSectorCode == "NATIONAL TOTAL") %>%
  group_by(countryCode, year, pollutantName) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  dplyr::select(country = countryCode, year, pollutantName, value)


# ---- SO2 only - this is the new part ----
so2_trends <- nec_clean %>%
  filter(pollutantName == "SOx", year >= 2010, year <= 2019, value > 0) %>%
  group_by(country) %>%
  filter(n() >= 7) %>%
  ungroup() %>%
  nest(data = -country) %>%
  mutate(
    model = map(data, ~ rlm(log(value) ~ year, data = .x)),
    log_slope = map_dbl(model, ~ coef(.x)["year"])
  ) %>%
  dplyr::select(country, log_slope)

so2_baseline_2005 <- nec_clean %>%
  filter(pollutantName == "SOx", year == 2005) %>%
  dplyr::select(country, baseline_2005 = value)

so2_ambition_final <- so2_trends %>%
  left_join(so2_baseline_2005, by = "country") %>%
  mutate(
    bau_2025 = baseline_2005 * exp(log_slope * (2025 - 2005)),
    bau_pct_reduction = (baseline_2005 - bau_2025) / baseline_2005
  ) %>%
  left_join(df_panel_merged %>% dplyr::select(country, so2_target_2020_29) %>% distinct(), by = "country") %>%
  mutate(so2_ambition_gap = so2_target_2020_29 - bau_pct_reduction) %>%
  dplyr::select(country, so2_ambition_gap)

print(so2_ambition_final, n = 30)

nec_clean %>% filter(pollutantName == "SOx", country == "DE") %>% arrange(year) %>% print(n=20)

df_panel_merged <- df_panel_merged %>%
  dplyr::select(-any_of("so2_ambition_gap")) %>%
  left_join(so2_ambition_final, by = "country")

saveRDS(df_panel_merged, "data/processed/df_panel_merged.rds")


df_panel <- readRDS("data/processed/df_panel_merged.rds") %>%
  mutate(phase3 = as.integer(phase == 3), phase4 = as.integer(phase == 4))

nox_amb_triple_phase <- feols(log_verified ~ 
                                state_ownership_pct_tv +
                                state_ownership_pct_tv:phase3 +
                                state_ownership_pct_tv:phase4 +
                                state_ownership_pct_tv:nox_ambition_gap +
                                state_ownership_pct_tv:nox_ambition_gap:phase3 +
                                state_ownership_pct_tv:nox_ambition_gap:phase4 +
                                nox_ambition_gap + v2x_polyarchy |
                                installation_id + year,
                              data = df_panel, cluster = ~bvdId)

so2_amb_triple_phase <- feols(log_verified ~ 
                                state_ownership_pct_tv +
                                state_ownership_pct_tv:phase3 +
                                state_ownership_pct_tv:phase4 +
                                state_ownership_pct_tv:so2_ambition_gap +
                                state_ownership_pct_tv:so2_ambition_gap:phase3 +
                                state_ownership_pct_tv:so2_ambition_gap:phase4 +
                                so2_ambition_gap + v2x_polyarchy |
                                installation_id + year,
                              data = df_panel, cluster = ~bvdId)

summary(nox_amb_triple_phase)
summary(so2_amb_triple_phase)

modelsummary(
  list(
    'NOx Ambition Gap' = nox_amb_triple_phase,
    'SO2 Ambition Gap' = so2_amb_triple_phase
  ),
  coef_map = c(
    'state_ownership_pct_tv'                                    = 'State ownership %',
    'state_ownership_pct_tv:phase3'                             = 'Ownership × Phase 3',
    'state_ownership_pct_tv:phase4'                             = 'Ownership × Phase 4',
    'state_ownership_pct_tv:nox_ambition_gap'                  = 'Ownership × NOx ambition gap',
    'state_ownership_pct_tv:phase3:nox_ambition_gap'           = 'Ownership × NOx gap × Phase 3',
    'state_ownership_pct_tv:phase4:nox_ambition_gap'           = 'Ownership × NOx gap × Phase 4',
    'state_ownership_pct_tv:so2_ambition_gap'                  = 'Ownership × SO2 ambition gap',
    'state_ownership_pct_tv:phase3:so2_ambition_gap'           = 'Ownership × SO2 gap × Phase 3',
    'state_ownership_pct_tv:phase4:so2_ambition_gap'           = 'Ownership × SO2 gap × Phase 4',
    'nox_ambition_gap'                                          = 'NOx ambition gap',
    'so2_ambition_gap'                                          = 'SO2 ambition gap',
    'v2x_polyarchy'                                             = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  fmt = fmt_decimal(5),
  title = 'Log verified emissions: ambition-gap (trend-adjusted) non-market proxies, by phase',
  output = 'output/tables/table_ambition_gap_phase_comparison.html'
)


df_panel %>%
  distinct(country, year, eps, nox_ambition_gap, so2_ambition_gap) %>%
  filter(!is.na(eps)) %>%
  summarise(
    cor_nox_amb = cor(eps, nox_ambition_gap, use = "complete.obs"),
    cor_so2_amb = cor(eps, so2_ambition_gap, use = "complete.obs")
  )



# NOx ambition gap version, phase-decomposed
nox_amb_triple_phase <- feols(log_verified ~ 
                                state_ownership_pct_tv +
                                state_ownership_pct_tv:phase3 +
                                state_ownership_pct_tv:phase4 +
                                state_ownership_pct_tv:nox_ambition_gap +
                                state_ownership_pct_tv:nox_ambition_gap:phase3 +
                                state_ownership_pct_tv:nox_ambition_gap:phase4 +
                                nox_ambition_gap + v2x_polyarchy |
                                installation_id + year,
                              data = df_panel, cluster = ~bvdId)
summary(nox_amb_triple_phase)

df_panel %>% distinct(country, v2x_corr, v2x_polyarchy) %>% 
  filter(country %in% c("ES", "PT"))




so2_p2p3_epscontrol <- feols(log_verified ~ 
                               state_ownership_pct_tv * so2_ambition_gap * phase3 +
                               eps + v2x_polyarchy |
                               installation_id + year,
                             data = df_panel %>% filter(phase %in% c(2,3)),
                             cluster = ~bvdId)
summary(so2_p2p3_epscontrol)


df_panel %>% filter(country %in% c("ES","PT")) %>%
  group_by(phase) %>%
  summarise(n_soe_installations = sum(state_owned_binary_tv == 1, na.rm=TRUE),
            n_total = n())


df_panel %>% filter(country %in% c("ES","PT"), phase == 2, state_owned_binary_tv == 1) %>%
  distinct(installation_id, company_name)

df_panel %>% filter(country %in% c("ES","PT"), phase == 3, state_owned_binary_tv == 1) %>%
  distinct(installation_id, company_name)


df_panel %>% filter(state_owned_binary_tv == 1) %>%
  distinct(installation_id, company_name, country) %>%
  filter(!grepl("power|electric|energy|generat|thermal|coal|gas", company_name, ignore.case = TRUE)) %>%
  arrange(country)


df_panel %>% filter(state_owned_binary_tv == 1) %>%
  distinct(installation_id, company_name, country, nace, activity_id) %>%
  count(activity_id, sort = TRUE)

df_panel %>% distinct(activity_id, activity_type) %>% arrange(activity_id)


df_panel %>% filter(installation_id %in% c("ES_1510","ES_203767","ES_205812","ES_210672",
                                           "ES_214580","ES_90030","ES_90040","PT_194","PT_195")) %>%
  distinct(installation_id, company_name, activity_id)

df_panel %>% filter(state_owned_binary_tv == 1) %>%
  distinct(nace, nace_id) %>%
  arrange(nace)

df_panel %>% distinct(nace) %>% count()
df_panel %>% distinct(nace_id) %>% arrange(nace_id) %>% head(20)
str(df_panel$nace)
str(df_panel$nace_id)

df_panel %>% distinct(activity_id) %>% arrange(activity_id)


df_panel %>% filter(country %in% c("SE","DE"), phase == 2, state_owned_binary_tv == 1) %>%
  distinct(installation_id, company_name, country)

df_panel %>% filter(country %in% c("SE","DE"), phase == 2, state_owned_binary_tv == 1) %>%
  summarise(n_installations = n_distinct(installation_id), n_companies = n_distinct(company_name))


#rebuilding 
library(tidyverse)
library(fixest)
library(modelsummary)

df_panel <- readRDS("data/processed/df_panel_merged.rds") %>%
  mutate(phase3 = as.integer(phase == 3), phase4 = as.integer(phase == 4))

price_phase_inter_full <- feols(
  log_verified ~ 
    state_ownership_pct_tv +
    state_ownership_pct_tv:phase3 +
    state_ownership_pct_tv:phase4 +
    state_ownership_pct_tv:combined_carbon_price +
    state_ownership_pct_tv:combined_carbon_price:phase3 +
    state_ownership_pct_tv:combined_carbon_price:phase4 +
    combined_carbon_price + v2x_polyarchy |
    installation_id + year,
  data = df_panel, cluster = ~bvdId
)


modelsummary(
  list('Price × Phase' = price_phase_inter_full),
  coef_map = c(
    'state_ownership_pct_tv'                                = 'State ownership %',
    'state_ownership_pct_tv:phase3'                          = 'Ownership × Phase 3',
    'state_ownership_pct_tv:phase4'                          = 'Ownership × Phase 4',
    'state_ownership_pct_tv:combined_carbon_price'          = 'Ownership × Price',
    'state_ownership_pct_tv:phase3:combined_carbon_price'   = 'Ownership × Price × Phase 3',
    'state_ownership_pct_tv:phase4:combined_carbon_price'   = 'Ownership × Price × Phase 4',
    'combined_carbon_price'                                  = 'Combined carbon price',
    'v2x_polyarchy'                                          = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  fmt = fmt_decimal(5),
  title = 'Log verified emissions: ownership × combined carbon price, by phase',
  output = 'output/tables/table_price_phase_final.html'
)

names(coef(price_phase_inter_full))

#combined tests 

combined_test <- feols(log_verified ~ 
                         state_ownership_pct_tv * combined_carbon_price +
                         state_ownership_pct_tv * so2_ambition_gap +
                         v2x_polyarchy |
                         installation_id + year,
                       data = df_panel, cluster = ~bvdId)
summary(combined_test)


combined_full <- feols(log_verified ~ 
                         state_ownership_pct_tv +
                         state_ownership_pct_tv:phase3 +
                         state_ownership_pct_tv:phase4 +
                         state_ownership_pct_tv:combined_carbon_price +
                         state_ownership_pct_tv:combined_carbon_price:phase3 +
                         state_ownership_pct_tv:combined_carbon_price:phase4 +
                         state_ownership_pct_tv:so2_ambition_gap +
                         state_ownership_pct_tv:so2_ambition_gap:phase3 +
                         state_ownership_pct_tv:so2_ambition_gap:phase4 +
                         combined_carbon_price + so2_ambition_gap + v2x_polyarchy |
                         installation_id + year,
                       data = df_panel, cluster = ~bvdId)
summary(combined_full)

# double check robustness for price in combined regression 

# get the exact set of rows that survived the combined_full regression
sample_ids <- df_panel %>%
  filter(!is.na(state_ownership_pct_tv), !is.na(combined_carbon_price),
         !is.na(so2_ambition_gap), !is.na(v2x_polyarchy), !is.na(log_verified)) %>%
  pull(installation_id) %>% unique()

df_panel_matched <- df_panel %>% filter(installation_id %in% sample_ids)

price_only_matched <- feols(log_verified ~ 
                              state_ownership_pct_tv +
                              state_ownership_pct_tv:phase3 +
                              state_ownership_pct_tv:phase4 +
                              state_ownership_pct_tv:combined_carbon_price +
                              state_ownership_pct_tv:combined_carbon_price:phase3 +
                              state_ownership_pct_tv:combined_carbon_price:phase4 +
                              combined_carbon_price + v2x_polyarchy |
                              installation_id + year,
                            data = df_panel_matched, cluster = ~bvdId)
summary(price_only_matched)

modelsummary(
  list(
    'Price only' = price_phase_inter_full,
    'Price only (matched sample)' = price_only_matched,
    'Price + SO2 (horse race)' = combined_full
  ),
  coef_map = c(
    'state_ownership_pct_tv'                                = 'State ownership %',
    'state_ownership_pct_tv:phase3'                          = 'Ownership × Phase 3',
    'state_ownership_pct_tv:phase4'                          = 'Ownership × Phase 4',
    'state_ownership_pct_tv:combined_carbon_price'          = 'Ownership × Price',
    'state_ownership_pct_tv:phase3:combined_carbon_price'   = 'Ownership × Price × Phase 3',
    'state_ownership_pct_tv:phase4:combined_carbon_price'   = 'Ownership × Price × Phase 4',
    'state_ownership_pct_tv:so2_ambition_gap'                = 'Ownership × SO2 gap',
    'state_ownership_pct_tv:phase3:so2_ambition_gap'        = 'Ownership × SO2 gap × Phase 3',
    'state_ownership_pct_tv:phase4:so2_ambition_gap'        = 'Ownership × SO2 gap × Phase 4',
    'combined_carbon_price'                                  = 'Combined carbon price',
    'so2_ambition_gap'                                       = 'SO2 ambition gap',
    'v2x_polyarchy'                                          = 'V-Dem polyarchy'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  fmt = fmt_decimal(5),
  title = 'Market vs. non-market mechanism: price and SO2 ambition gap horse race',
  output = 'output/tables/table_price_so2_horserace.html'
)
