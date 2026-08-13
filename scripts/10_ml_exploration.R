# Exploratory machine learning cross-check on the ownership-emissions 
# relationship: causal forest (grf) for heterogeneous treatment effects, 
# as a discovery/cross-check tool against the manually-specified triple 
# interactions built throughout this summer's analysis. NOT a replacement 
# for the main feols specifications - identification assumptions still apply.

install.packages("grf")

library(grf); library(dplyr); library(tidyverse)

df_panel <- readRDS("data/processed/df_panel_merged.rds") %>%
  mutate(phase3 = as.integer(phase == 3), phase4 = as.integer(phase == 4))

# ---- 1. build a clean, complete-case matrix ----
# grf doesn't handle NAs natively - restrict to rows with all key vars present
ml_data <- df_panel %>%
  select(log_verified, state_ownership_pct_tv, combined_carbon_price,
         v2x_corr, v2x_polyarchy, phase3, phase4, eps) %>%
  filter(complete.cases(.))

nrow(ml_data)  # check how much of your panel survives - eps will cut this a lot given Phase 4 gap

# ---- 2. set up treatment, outcome, covariates ----
Y <- ml_data$log_verified
W <- ml_data$state_ownership_pct_tv
X <- ml_data %>% select(combined_carbon_price, v2x_corr, v2x_polyarchy, 
                        phase3, phase4, eps) %>% as.matrix()

# ---- 3. fit the causal forest ----
cf <- causal_forest(X = X, Y = Y, W = W, num.trees = 2000)

# ---- 4. average treatment effect + variable importance ----
average_treatment_effect(cf)
variable_importance(cf)

# ---- 5. best linear projection - which moderators actually drive heterogeneity ----
best_linear_projection(cf, X)




####Double ML! 

library(dplyr); library(data.table); library(DoubleML); library(mlr3); library(mlr3learners)

df_panel <- readRDS("data/processed/df_panel_merged.rds") %>%
  mutate(phase3 = as.integer(phase == 3), phase4 = as.integer(phase == 4))

ml_data <- df_panel %>%
  select(log_verified, state_ownership_pct_tv, combined_carbon_price,
         v2x_corr, v2x_polyarchy, phase3, phase4, eps) %>%
  filter(complete.cases(.))

nrow(ml_data)

# ---- 6. build the DML data object ----
dml_table <- as.data.table(ml_data)

dml_df <- DoubleMLData$new(
  dml_table,
  y_col = "log_verified",
  d_cols = "state_ownership_pct_tv",
  x_cols = c("combined_carbon_price", "v2x_corr", "v2x_polyarchy", "phase3", "phase4", "eps")
)

# ---- 7. specify ML learners ----
ml_g <- lrn("regr.ranger", num.trees = 500)
ml_m <- lrn("regr.ranger", num.trees = 500)

# ---- 8. fit the model ----
dml_plr <- DoubleMLPLR$new(dml_df, ml_g = ml_g, ml_m = ml_m, n_folds = 5)
dml_plr$fit()
print(dml_plr)

####Doing Double ML and casual forests with installation fixed effects?? 

library(dplyr); library(fixest); library(grf); library(DoubleML); library(mlr3); library(mlr3learners); library(data.table)

df_panel <- readRDS("data/processed/df_panel_merged.rds") %>%
  mutate(phase3 = as.integer(phase == 3), phase4 = as.integer(phase == 4))

ml_data <- df_panel %>%
  select(log_verified, state_ownership_pct_tv, combined_carbon_price,
         v2x_corr, v2x_polyarchy, eps, installation_id, year) %>%
  filter(complete.cases(.))

# ---- demean Y, D, and continuous X by installation_id + year (two-way FE) ----
demeaned <- demean(
  X = ml_data %>% select(log_verified, state_ownership_pct_tv, combined_carbon_price,
                         v2x_corr, v2x_polyarchy, eps),
  f = ml_data %>% select(installation_id, year)
)

colnames(demeaned) <- paste0(colnames(demeaned), "_dm")
ml_data_dm <- bind_cols(ml_data, demeaned)


# Causal forest, installation+year demeaned
X_dm <- ml_data_dm %>% select(combined_carbon_price_dm, v2x_corr_dm, v2x_polyarchy_dm, eps_dm) %>% as.matrix()
cf_dm <- causal_forest(X = X_dm, Y = ml_data_dm$log_verified_dm, W = ml_data_dm$state_ownership_pct_tv_dm, num.trees = 1000)
average_treatment_effect(cf_dm)

# DML, installation+year demeaned
library(DoubleML); library(mlr3); library(mlr3learners); library(data.table)

dml_table_dm <- as.data.table(ml_data_dm %>% 
                                select(log_verified_dm, state_ownership_pct_tv_dm, combined_carbon_price_dm, v2x_corr_dm, v2x_polyarchy_dm, eps_dm))

dml_df_dm <- DoubleMLData$new(
  dml_table_dm,
  y_col = "log_verified_dm",
  d_cols = "state_ownership_pct_tv_dm",
  x_cols = c("combined_carbon_price_dm", "v2x_corr_dm", "v2x_polyarchy_dm", "eps_dm")
)

ml_l <- lrn("regr.ranger", num.trees = 500)
ml_m <- lrn("regr.ranger", num.trees = 500)
dml_plr_dm <- DoubleMLPLR$new(dml_df_dm, ml_l = ml_l, ml_m = ml_m, n_folds = 5)
dml_plr_dm$fit() 
print(dml_plr_dm)

# check if foels matches ml interpretation 

feols_matching_dml <- feols(log_verified ~ state_ownership_pct_tv + combined_carbon_price + 
                              v2x_corr + v2x_polyarchy + eps | installation_id + year,
                            data = df_panel, cluster = ~bvdId)
summary(feols_matching_dml)

# table 

cf_dm_summary <- average_treatment_effect(cf_dm)
cat(sprintf("Causal forest (demeaned): estimate = %.5f, SE = %.5f\n", 
            cf_dm_summary["estimate"], cf_dm_summary["std.err"]))

# pull each estimate/SE by hand
feols_est <- coef(feols_matching_dml)["state_ownership_pct_tv"]
feols_se  <- feols_matching_dml$se["state_ownership_pct_tv"]

dml_est <- dml_plr_dm$coef["state_ownership_pct_tv_dm"]
dml_se  <- dml_plr_dm$se["state_ownership_pct_tv_dm"]

cf_summary <- average_treatment_effect(cf_dm)
cf_est <- cf_summary["estimate"]
cf_se  <- cf_summary["std.err"]

comparison_table <- data.frame(
  Method = c("feols (matched sample)", "DML (demeaned)", "Causal forest (demeaned)"),
  Estimate = c(feols_est, dml_est, cf_est),
  Std.Error = c(feols_se, dml_se, cf_se),
  row.names = NULL
)

library(knitr); library(kableExtra)

comparison_table %>%
  kable(digits = 5, row.names = FALSE, 
        col.names = c("Method", "Estimate", "Std. Error"),
        caption = "Ownership effect on log verified emissions: OLS vs. ML methods") %>%
  kable_styling() %>%
  save_kable("output/tables/table_ml_comparison.html")







modelsummary(
  list(
    'feols (matched)' = feols_matching_dml,
    'DML (demeaned)' = dml_plr_dm,
    'Causal forest (demeaned)' = NULL
  ),
  coef_map = c(
    'state_ownership_pct_tv'      = 'State ownership %',
    'state_ownership_pct_tv_dm'   = 'State ownership % (demeaned)',
    'combined_carbon_price'       = 'Combined carbon price',
    'v2x_corr'                    = 'Corruption (V-Dem)',
    'v2x_polyarchy'               = 'V-Dem polyarchy',
    'eps'                         = 'EPS'
  ),
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  gof_map = c('nobs', 'r.squared', 'adj.r.squared'),
  fmt = fmt_decimal(5),
  title = 'Ownership effect on log verified emissions: OLS vs. ML methods',
  output = 'output/tables/table_ml_comparison.html'
)


