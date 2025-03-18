# Packages ----------------------------------------------------------------
library(fixest)
library(data.table)
library(modelsummary)
library(haven)
library(car)

results <- list()

# Function ----------------------------------------------------------------

p_value_joint_test = function(dataset, test_var){
  hyp_test = linearHypothesis(dataset, test_var, test = "F")
  p_value = hyp_test$`Pr(>F)`[2]
  return(p_value)
}

# Load and Merge data ------------------------------------------------------
dt_followup <- fread("C:\\Users\\Dell\\replication_package\\data\\input_primary\\rct1_followup.csv")
dt_baseline_temp <- fread("C:\\Users\\Dell\\replication_package\\data\\input_primary\\rct1_baseline.csv")
dt_baseline <- as.data.table(dt_baseline_temp)[, .(farmer_id)]
dt <- merge(dt_followup, dt_baseline, by = "farmer_id", all.x = TRUE)
dt <- dt[farmer_id %in% dt_baseline$farmer_id]

# Compute variables ----------------------------------------------------------
dt[, purea := 16]
dt[, ptsp := 22]
dt[, pmop := 14]
dt[, pother := 17.3333333]

# generate fertilizer expenditures per bigah
price_vars <- c(urea = "purea", tsp = "ptsp", mop = "pmop", other = "pother")
fertilizers <- names(price_vars)  # "urea", "tsp", "mop", "other"
for(j in fertilizers) {
  dose_vars <- paste0("a_dose", 1:5, "_", j)
  dt[, paste0("a_kg_", j) := rowSums(.SD, na.rm = TRUE), .SDcols = dose_vars]
  dt[, paste0("a_spend_", j) := get(paste0("a_kg_", j)) * get(price_vars[j])]
  dt[, paste0("a_spendbg_", j) := get(paste0("a_spend_", j)) / a_areacult_bg]
}

dt[, totalfert_bg := a_spendbg_urea + a_spendbg_tsp + a_spendbg_mop + a_spendbg_other]
all_cols <- names(dt)
pos <- match("a_dose5_other", all_cols)
cols_to_move <- grep("^(a_kg_|a_spend)", all_cols, value = TRUE)
new_order <- c(all_cols[1:pos], cols_to_move, setdiff(all_cols, c(all_cols[1:pos], cols_to_move)))
setcolorder(dt, new_order)

# Generate family labor per bigah 
dt[, a_famhar_bg := (a_famhar * a_harwage) / a_areacult_bg]
dt[, a_famplant_bg := (a_famplant * a_plantwage) / a_areacult_bg]
dt[, weed_wage := rowMeans(.SD, na.rm = TRUE), .SDcols = c("a_plantwage", "a_harwage")]
dt[, a_famweed_bg := (a_famweed * weed_wage) / a_areacult_bg]


# Generate interaction term between treatment and volumetric prices
dt[, t_marg := anymarginal * treatment]

#Convert everything to per acre (for making units consistent across tables) 
vars_to_convert <- c("a_spendbg_urea", "a_spendbg_tsp", "a_spendbg_mop", "a_spendbg_other",
                     "a_pesticides_bg", "a_herbicides_bg", "a_plantspend_bg", "a_weedspend_bg", 
                     "a_harspend_bg", "a_famplant_bg", "a_famhar_bg", "a_famweed_bg", 
                     "a_watercost", "totalfert_bg")

dt[, (vars_to_convert) := lapply(.SD, function(x) x * 3), .SDcols = vars_to_convert]

# self-reported water use --------------------------------------------------------------
dt[, loga_watercost := log(a_watercost)]
formula <- as.formula(paste("loga_watercost", "~ treatment + t_marg + anymarginal | Upazila"))
mod1 <- feols(formula, data = dt, cluster = ~village_id)

# compute p_value of the wald test: 
p_value = p_value_joint_test(mod1, "treatment + t_marg = 0")

# Stock results
results[[paste0("loga_watercost", "_het")]] <- list(
  model = mod1,
  heterogeneity_p_value = p_value
)

# Revenues and profit ---------------------------------------------------------------------

# mean fill on price 
dt[, a_price_per_kg := a_price * (1 / 40)]
dt[, a_price := ifelse(is.na(a_price), mean(a_price, na.rm = TRUE), a_price)]

# generate variables 
dt[, a_yield_kgac := a_yield * 120]
dt[, a_rev_ac := a_yield * a_price * 3]
dt[, a_cost_ac := rowSums(.SD, na.rm = TRUE), .SDcols = c("a_spendbg_urea", "a_spendbg_tsp", "a_spendbg_mop", "a_spendbg_other",
                                                           "a_pesticides_bg", "a_herbicides_bg", "a_plantspend_bg", 
                                                           "a_weedspend_bg", "a_harspend_bg", "a_famplant_bg", 
                                                           "a_famhar_bg", "a_famweed_bg", "a_watercost")]
dt[, a_profit_ac := a_rev_ac - a_cost_ac]
dt[, a_nonwater_ac := a_cost_ac - a_watercost]
for(i in c("a_yield_kgac", "a_rev_ac", "a_profit_ac", "a_nonwater_ac")) {
  dt[, paste0("log_", i) := log(get(i))]
}

# run regression

# Reg yield
formula <- as.formula(paste("log_a_yield_kgac", "~ treatment + t_marg + anymarginal | Upazila"))
mod2 <- feols(formula, data = dt, cluster = "village_id",keep_data = TRUE)
mf2 <- model.frame(mod2)

# Test 
p_value = p_value_joint_test(mod2, "treatment + t_marg = 0")


results[[paste0("log_a_yield_kgac", "_het")]] <- list(
  model = mod2,
  heterogeneity_p_value = p_value
)

# reg Revenu
formula <- as.formula(paste("log_a_rev_ac", "~ treatment + t_marg + anymarginal | Upazila"))
mod3 <- feols(formula, data = dt, cluster = "village_id",keep_data = TRUE)
mf3 <- model.frame(mod3)

# Test 
p_value = p_value_joint_test(mod3, "treatment + t_marg = 0")

results[[paste0("log_a_rev_ac", "_het")]] <- list(
  model = mod3,
  heterogeneity_p_value = p_value
)

# reg profit
formula <- as.formula(paste("log_a_profit_ac", "~ treatment + t_marg + anymarginal | Upazila"))
mod4 <- feols(formula, data = dt, cluster = "village_id",keep_data = TRUE)
mf4 <- model.frame(mod4)


# Test 
p_value = p_value_joint_test(mod4, "treatment + t_marg = 0")

results[[paste0("log_a_profit_ac", "_het")]] <- list(
  model = mod4,
  heterogeneity_p_value = p_value
)

# Heterogeneity by holding own prepaid card
dt[, hourcard := as.numeric(a_paymethod == 1)]
dt[, t_hourcard := hourcard * treatment]
dt_sub <- dt[!is.na(a_paymethod)]

#Reg loga_watercost
formula_logcost <- as.formula("loga_watercost ~ treatment + t_hourcard + hourcard | Upazila")
mod_logcost <- feols(formula_logcost, data = dt_sub, cluster = "village_id", keep_data = TRUE)
mf_logcost <- model.frame(mod_logcost)

# compute p_value of the wald test by hand because I couldn't manage to handle the function wald
beta <- coef(mod_logcost)
V <- vcov(mod_logcost)
R <- matrix(c(1, 1, 0), nrow = 1)
wald_stat <- as.numeric((R %*% beta)^2 / (R %*% V %*% t(R)))
p_value_manual <- 1 - pchisq(wald_stat, df = 1)

# Stock results
results[["logcost_card"]] <- list(
  model = mod_logcost,
  heterogeneity_p_value = p_value_manual
)

# Reg log_a_profit_ac
formula_logprofit <- as.formula("log_a_profit_ac ~ treatment + t_hourcard + hourcard | Upazila")
mod_logprofit <- feols(formula_logprofit, data = dt_sub, cluster = "village_id", keep_data = TRUE)
mf_logprofit <- model.frame(mod_logprofit)

beta <- coef(mod_logprofit)
V <- vcov(mod_logprofit)
R <- matrix(c(1, 1, 0), nrow = 1)
wald_stat <- as.numeric((R %*% beta)^2 / (R %*% V %*% t(R)))
p_value_manual <- 1 - pchisq(wald_stat, df = 1)

# Stock results
results[["logprofit_card"]] <- list(
  model = mod_logprofit,
  heterogeneity_p_value = p_value_manual
)


cat("Modèle log_a_profit_ac (logprofit_card) :\n")
print(summary(results[["logprofit_card"]]$model))
cat("Moyenne du groupe contrôle (log_a_profit_ac) : ", results[["logprofit_card"]]$mean_control, "\n")
cat("P-value du test (treatment + t_hourcard = 0) : ", results[["logprofit_card"]]$heterogeneity_p_value, "\n")

 
# Define models
models_table3_A <- list(
  "Profit"      = results[["log_a_profit_ac_het"]][["model"]],
  "Water Cost"  = results[["loga_watercost_het"]][["model"]],
  "Yield"       = results[["log_a_yield_kgac_het"]][["model"]],
  "Revenue"     = results[["log_a_rev_ac_het"]][["model"]]
)
models_table3_B <- list (
  "Water Cost Rajshahi" = results[["logcost_card"]][["model"]],
  "Profit Rajshahi"     = results[["logprofit_card"]][["model"]]
  )
length(results[["log_a_profit_ac_het"]]$heterogeneity_p_value)
length(results[["log_a_profit_ac_het"]]$model$nobs)
length(as.numeric(results[["log_a_profit_ac_het"]]$model$r.squared))


# Ajouter les lignes (p-values, N obs, R squared)
# Ajout des lignes scalaires spécifiques (p-value, nombre d'observations, R²)

get_scalar <- function(x) {
  if(length(x) == 0) NA else x[1]
}

add_rows_df_table3 <- data.frame(
  term = c("p-Value: Treatment +Treatment x Volumetric"),
  
  Profit = c(
    get_scalar(results[["log_a_profit_ac_het"]]$heterogeneity_p_value)
  ),
  
  `Water Cost` = c(
    get_scalar(results[["loga_watercost_het"]]$heterogeneity_p_value)
  ),
  
  Yield = c(
    get_scalar(results[["log_a_yield_kgac_het"]]$heterogeneity_p_value)
  ),
  
  Revenue = c(
    get_scalar(results[["log_a_rev_ac_het"]]$heterogeneity_p_value)
  ),
  
  `Water Cost Rajshahi` = c(
    get_scalar(results[["logcost_card"]]$heterogeneity_p_value)
  ),
  
  `Profit Rajshahi` = c(
    get_scalar(results[["logprofit_card"]]$heterogeneity_p_value)
  )
)

# Générer la table LaTeX finale avec modelsummary
options("modelsummary_format_numeric_latex" = "plain")
gof_custom <- tibble::tribble(
  ~raw, ~clean, ~fmt,
  "r.squared", "R²", "%.3f",
  "nobs", "Observations", "%.0f" 
)
tbl <- modelsummary(
             models =  c(models_table3_A, models_table3_B),
             statistic = c("{estimate}", "({std.error})"),
             add_rows = add_rows_df_table3,
             coef_omit = "^_cons$",
             coef_order = c("treatment", "t_marg", "anymarginal", "t_hourcard", "hourcard"),
             notes = "Upazila Fixed Effects = _IUpa*",
             output = "latex",
             gof_map = gof_custom,
             coef_map = c(
                   "treatment" = "Treatment",
                   "t_marg" = "Treatment x Volumetric Pricing",
                   "anymarginal" = "Volumetric Pricing",
                   "t_hourcard"="Treatment x Has Card",
                   "hourcard"="Has Card"),
             title = "Effects of Conservation Technology on Log Costs, Revenues, and Profits", 
             file = NULL
)

# Sauvegarder la table dans le fichier .tex
tbl_text <- paste(tbl, collapse = "\n")
file_path <- "C:\\Users\\Dell\\replication_package\\table3.tex"
writeLines(tbl_text, file_path)

if (file.exists(file_path)) {
  cat("Fichier sauvegardé avec succès dans", file_path, "\n")
} else {
  cat("Le fichier n'a pas été créé.\n")
}
