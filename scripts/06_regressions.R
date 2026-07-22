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
  output = 'output/tables/table_lv_country_fe_employees.html'
)