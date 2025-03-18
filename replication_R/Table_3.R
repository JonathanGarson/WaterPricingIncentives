# Packages ----------------------------------------------------------------
library(fixest)
library(data.table)
library(modelsummary)
library(haven)
library(car)

results <- list()


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

outcomes <- c("a_irrigations", "a_timedry", "a_watercost", "loga_watercost")
formula <- as.formula(paste("loga_watercost", "~ treatment + t_marg + anymarginal | Upazila"))
mod1 <- feols(formula, data = dt, cluster = ~village_id)
mean_control1 <- dt[treatment == 0, mean(get("loga_watercost"), na.rm = TRUE)]

# compute p_value of the wald test by hand because I couldn't manage to handle the function wald
beta <- coef(mod1)
V <- vcov(mod1)
R <- matrix(c(1, 1, 0), nrow = 1)
wald_stat <- as.numeric((R %*% beta)^2 / (R %*% V %*% t(R)))
p_value_manual <- 1 - pchisq(wald_stat, df = 1)

print(wald_stat)
print(p_value_manual)

# Stock results
results[[paste0("loga_watercost", "_het")]] <- list(
  model = mod1,
  mean_control = mean_control1,
  heterogeneity_test = wald_stat,
  heterogeneity_p_value = p_value_manual
)

cat("Modèle:", "loga_watercost_het", "\n")
print(summary(results[["loga_watercost_het"]]$model))
cat("-------------------------------------------------------------\n\n")
cat("P-value du test d'hétérogénéité:", results[["loga_watercost_het"]]$heterogeneity_p_value, "\n\n")

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
outcomes <- c("log_a_yield_kgac", "log_a_rev_ac", "log_a_profit_ac")

for (j in outcomes) {
  formula <- as.formula(paste(j, "~ treatment + t_marg + anymarginal | Upazila"))
  mod2 <- feols(formula, data = dt, cluster = "village_id",keep_data = TRUE)
  mf2 <- model.frame(mod2)
  mean_control2 <- mean(mf2[[j]][mf2$treatment == 0], na.rm = TRUE)

  # Test 
  test_result <- linearHypothesis(mod2, "treatment + t_marg = 0")
  phet <- test_result$`Pr(>F)`[2]

  # compute p_value of the wald test by hand because I couldn't manage to handle the function wald
  beta <- coef(mod2)
  V <- vcov(mod2)
  R <- matrix(c(1, 1, 0), nrow = 1)
  wald_stat <- as.numeric((R %*% beta)^2 / (R %*% V %*% t(R)))
  p_value_manual <- 1 - pchisq(wald_stat, df = 1)

  results[[paste0(j, "_het")]] <- list(
    model = mod2,
    mean_control = mean_control2,
    heterogeneity_test = wald_stat,
    heterogeneity_p_value = p_value_manual
  )

}

cat("Modèle:", "log_a_yield_kgac_het", "\n")
cat("Moyenne du groupe contrôle:", results[["log_a_yield_kgac_het"]]$mean_control, "\n")
cat("P-value du test d'hétérogénéité:", results[["log_a_yield_kgac_het"]]$heterogeneity_p_value, "\n\n")
print(summary(results[["log_a_yield_kgac_het"]]$model))
cat("-------------------------------------------------------------\n\n")

cat("Modèle:", "log_a_rev_ac_het", "\n")
cat("Moyenne du groupe contrôle:", results[["log_a_rev_ac_het"]]$mean_control, "\n")
cat("P-value du test d'hétérogénéité:", results[["log_a_rev_ac_het"]]$heterogeneity_p_value, "\n\n")
print(summary(results[["log_a_rev_ac_het"]]$model))
cat("-------------------------------------------------------------\n\n")

cat("Modèle:", "log_a_profit_ac_het", "\n")
cat("Moyenne du groupe contrôle:", results[["log_a_profit_ac_het"]]$mean_control, "\n")
cat("P-value du test d'hétérogénéité:", results[["log_a_profit_ac_het"]]$heterogeneity_p_value, "\n\n")
print(summary(results[["log_a_profit_ac_het"]]$model))
cat("-------------------------------------------------------------\n\n")

### Heterogeneity by holding own prepaid card

# Créer la variable hourcard : 1 si a_paymethod vaut 1, sinon 0
dt[, hourcard := as.numeric(a_paymethod == 1)]
# Créer l'interaction entre hourcard et treatment
dt[, t_hourcard := hourcard * treatment]

# Filtrer pour garder les observations où a_paymethod n'est pas manquant
dt_sub <- dt[!is.na(a_paymethod)]

### Régression sur loga_watercost

# Spécifier la formule avec effets fixes par Upazila
formula_logcost <- as.formula("loga_watercost ~ treatment + t_hourcard + hourcard | Upazila")

# Estimer le modèle avec cluster sur village_id
mod_logcost <- feols(formula_logcost, data = dt_sub, cluster = "village_id", keep_data = TRUE)

# Calculer la moyenne de loga_watercost pour le groupe contrôle (treatment == 0) dans l'échantillon utilisé
mf_logcost <- model.frame(mod_logcost)
mean_control_logcost <- mean(mf_logcost$loga_watercost[mf_logcost$treatment == 0], na.rm = TRUE)

# compute p_value of the wald test by hand because I couldn't manage to handle the function wald
beta <- coef(mod_logcost)
V <- vcov(mod_logcost)
R <- matrix(c(1, 1, 0), nrow = 1)
wald_stat <- as.numeric((R %*% beta)^2 / (R %*% V %*% t(R)))
p_value_manual <- 1 - pchisq(wald_stat, df = 1)

# Stocker les résultats dans une liste sous le nom "logcost_card"
results[["logcost_card"]] <- list(
  model = mod_logcost,
  mean_control = mean_control_logcost,
  heterogeneity_test = wald_stat,
  heterogeneity_p_value = p_value_manual
)

### Régression sur log_a_profit_ac
formula_logprofit <- as.formula("log_a_profit_ac ~ treatment + t_hourcard + hourcard | Upazila")
mod_logprofit <- feols(formula_logprofit, data = dt_sub, cluster = "village_id", keep_data = TRUE)
mf_logprofit <- model.frame(mod_logprofit)
mean_control_logprofit <- mean(mf_logprofit$log_a_profit_ac[mf_logprofit$treatment == 0], na.rm = TRUE)
beta <- coef(mod_logprofit)
V <- vcov(mod_logprofit)
R <- matrix(c(1, 1, 0), nrow = 1)
wald_stat <- as.numeric((R %*% beta)^2 / (R %*% V %*% t(R)))
p_value_manual <- 1 - pchisq(wald_stat, df = 1)

# Stocke results
results[["logprofit_card"]] <- list(
  model = mod_logprofit,
  mean_control = mean_control_logprofit,
  heterogeneity_test = wald_stat,
  heterogeneity_p_value = p_value_manual
)

cat("Modèle loga_watercost (logcost_card) :\n")
print(summary(results[["logcost_card"]]$model))
cat("Moyenne du groupe contrôle (loga_watercost) : ", results[["logcost_card"]]$mean_control, "\n")
cat("P-value du test (treatment + t_hourcard = 0) : ", results[["logcost_card"]]$heterogeneity_p_value, "\n\n")

cat("Modèle log_a_profit_ac (logprofit_card) :\n")
print(summary(results[["logprofit_card"]]$model))
cat("Moyenne du groupe contrôle (log_a_profit_ac) : ", results[["logprofit_card"]]$mean_control, "\n")
cat("P-value du test (treatment + t_hourcard = 0) : ", results[["logprofit_card"]]$heterogeneity_p_value, "\n")

 names(results)

# Définition correcte des modèles (en référence à la liste "results")
models_table3 <- list(
  "Profit"      = results[["log_a_profit_ac_het"]][["model"]],
  "Water Cost"  = results[["loga_watercost_het"]][["model"]],
  "Yield"       = results[["log_a_yield_kgac_het"]][["model"]],
  "Revenue"     = results[["log_a_rev_ac_het"]][["model"]],
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
  term = c("p-Value: Treat+Treat*Volumetric"),
  
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

tbl <- modelsummary(models_table3,
             statistic = c("{estimate}", "({std.error})"),
             stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01),
             add_rows = add_rows_df_table3,
             coef_omit = "^_cons$",
             coef_order = c("treatment", "t_marg", "anymarginal", "t_hourcard", "hourcard"),
             notes = "Upazila Fixed Effects = _IUpa*",
             output = "latex",
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
