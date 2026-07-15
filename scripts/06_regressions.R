library(fixest)

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



