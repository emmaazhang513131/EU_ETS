# =============================================================================
# 10_ml_exploration.R
#
# Exploratory machine learning cross-check on the ownership-emissions
# relationship: causal forest (grf) for heterogeneous treatment effects,
# and DoubleML, as a discovery/cross-check tool against the manually-specified
# triple interactions built throughout this summer's analysis. NOT a
# replacement for the main feols specifications - identification assumptions
# still apply.
#
# FIXES APPLIED (see chat):
#   - readRDS('data/processed/df_panel_merged.rds') was called three times
#     across the script, reloading the identical file each time -- loaded
#     once at the top and reused throughout instead.
#   - Several libraries (dplyr, DoubleML, mlr3, mlr3learners, data.table)
#     were reloaded 3-4 times across different sections -- consolidated to
#     the top.
#   - install.packages("grf") ran unconditionally -- changed to a
#     conditional install.
#   - A modelsummary() call at the end tried to build a comparison table
#     from the feols, DoubleMLPLR, and causal forest objects together --
#     this errors, because modelsummary cannot extract estimates from
#     DoubleMLPLR or grf objects (confirmed: this is the literal error from
#     the original session, "could not extract the required information
#     from a model of class DoubleMLPLR"). It sat AFTER an already-working
#     kable()-based version of the same table a few lines above -- removed
#     as dead, superseded, broken code.
#   - No per-phase decomposition section exists in your actual current file
#     (unlike my earlier reconstruction, which included an [INCOMPLETE]
#     skeleton for one based on an older conversation) -- removed that
#     section entirely since it's not part of your current working script.
# =============================================================================

if (!requireNamespace("grf", quietly = TRUE)) install.packages("grf")

library(grf); library(dplyr); library(tidyverse)
library(data.table); library(DoubleML); library(mlr3); library(mlr3learners)
library(fixest); library(knitr); library(kableExtra)

df_panel <- readRDS("data/processed/df_panel_merged.rds") %>%
  mutate(phase3 = as.integer(phase == 3), phase4 = as.integer(phase == 4))

# -----------------------------------------------------------------------------
# 1. Causal forest -- no fixed effects
# -----------------------------------------------------------------------------

# ---- build a clean, complete-case matrix ----
# grf doesn't handle NAs natively - restrict to rows with all key vars present
ml_data <- df_panel %>%
  select(log_verified, state_ownership_pct_tv, combined_carbon_price,
         v2x_corr, v2x_polyarchy, phase3, phase4, eps) %>%
  filter(complete.cases(.))

nrow(ml_data)  # check how much of your panel survives - eps will cut this a lot given Phase 4 gap

# ---- set up treatment, outcome, covariates ----
Y <- ml_data$log_verified
W <- ml_data$state_ownership_pct_tv
X <- ml_data %>% select(combined_carbon_price, v2x_corr, v2x_polyarchy,
                        phase3, phase4, eps) %>% as.matrix()

# ---- fit the causal forest ----
cf <- causal_forest(X = X, Y = Y, W = W, num.trees = 2000)

# ---- average treatment effect + variable importance ----
average_treatment_effect(cf)
variable_importance(cf)

# ---- best linear projection - which moderators actually drive heterogeneity ----
best_linear_projection(cf, X)

# -----------------------------------------------------------------------------
# 2. Double ML -- no fixed effects
# -----------------------------------------------------------------------------

# ---- build the DML data object ----
dml_table <- as.data.table(ml_data)

dml_df <- DoubleMLData$new(
  dml_table,
  y_col = "log_verified",
  d_cols = "state_ownership_pct_tv",
  x_cols = c("combined_carbon_price", "v2x_corr", "v2x_polyarchy", "phase3", "phase4", "eps")
)

# ---- specify ML learners ----
ml_g <- lrn("regr.ranger", num.trees = 500)
ml_m <- lrn("regr.ranger", num.trees = 500)

# ---- fit the model ----
# NOTE: this call uses ml_g/ml_m as argument names, while the demeaned
# version in Section 3 uses ml_l/ml_m -- worth double-checking which
# parameter name your installed DoubleML version actually expects, since
# this inconsistency was present in the original file, not introduced here.
dml_plr <- DoubleMLPLR$new(dml_df, ml_g = ml_g, ml_m = ml_m, n_folds = 5)
dml_plr$fit()
print(dml_plr)

# -----------------------------------------------------------------------------
# 3. Doing causal forest and DML with installation + year fixed effects
# -----------------------------------------------------------------------------

ml_data_fe <- df_panel %>%
  select(log_verified, state_ownership_pct_tv, combined_carbon_price,
         v2x_corr, v2x_polyarchy, eps, installation_id, year) %>%
  filter(complete.cases(.))

# ---- demean Y, D, and continuous X by installation_id + year (two-way FE) ----
demeaned <- demean(
  X = ml_data_fe %>% select(log_verified, state_ownership_pct_tv, combined_carbon_price,
                         v2x_corr, v2x_polyarchy, eps),
  f = ml_data_fe %>% select(installation_id, year)
)

colnames(demeaned) <- paste0(colnames(demeaned), "_dm")
ml_data_dm <- bind_cols(ml_data_fe, demeaned)

# ---- causal forest, demeaned ----
X_dm <- ml_data_dm %>% select(combined_carbon_price_dm, v2x_corr_dm, v2x_polyarchy_dm, eps_dm) %>% as.matrix()
cf_dm <- causal_forest(X = X_dm, Y = ml_data_dm$log_verified_dm, W = ml_data_dm$state_ownership_pct_tv_dm, num.trees = 1000)
average_treatment_effect(cf_dm)

# ---- DML, demeaned ----
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

# -----------------------------------------------------------------------------
# 4. Check if feols matches ML interpretation
# -----------------------------------------------------------------------------

feols_matching_dml <- feols(log_verified ~ state_ownership_pct_tv + combined_carbon_price +
                              v2x_corr + v2x_polyarchy + eps | installation_id + year,
                            data = df_panel, cluster = ~bvdId)
summary(feols_matching_dml)

# -----------------------------------------------------------------------------
# 5. Comparison table
# -----------------------------------------------------------------------------

cf_dm_summary <- average_treatment_effect(cf_dm)
cat(sprintf("Causal forest (demeaned): estimate = %.5f, SE = %.5f\n",
            cf_dm_summary["estimate"], cf_dm_summary["std.err"]))

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

comparison_table %>%
  kable(digits = 5, row.names = FALSE,
        col.names = c("Method", "Estimate", "Std. Error"),
        caption = "Ownership effect on log verified emissions: OLS vs. ML methods") %>%
  kable_styling() %>%
  save_kable("output/tables/table_ml_comparison.html")
)


