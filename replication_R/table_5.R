##### TABLE 5 #####

# Packages ----------------------------------------------------------------
library(fixest)
library(data.table)
library(modelsummary)
library(haven)
library(car)

# Constructing baseline_covariates dataset (derived) -----------------------

dt_baseline <- fread("C:\\Users\\Dell\\replication_package\\data\\input_primary\\rct1_baseline.csv")

# calculate boro rice revenue per acre
for(i in c("1", "2")) {
  price_col <- paste0("boro_price_", i)
  yield_col <- paste0("boro_yield_", i)
  rev_col   <- paste0("boro_revperacre_", i)
  
  dt_baseline[, (rev_col) := 3 * (get(price_col) * get(yield_col))]
  
  # If a value is missing, replace using mean_boro_price_i ("mean fill")
  price_mean <- dt_baseline[[price_col]]
  price_mean <- mean(price_mean, na.rm = TRUE)
  dt_baseline[is.na(get(rev_col)), (rev_col) := 3 * (price_mean * get(yield_col))]
  
  # If still missing, replace by 0
  dt_baseline[is.na(get(rev_col)), (rev_col) := 0]
}

# Revenue per acre from other two seasons
for(i in c("1", "2")) {
  for(j in c("winter", "aman")) {
    for(k in c("ov", "")) {
      price_var <- paste0(j, "_price", k, "_", i)
      yield_var <- paste0(j, "_yield", k, "_", i)
      rev_var   <- paste0(j, "_", k, "peracre_", i) 
      
      dt_baseline[, (rev_var) := 3 * (get(price_var) * get(yield_var))]
      
      price_mean <- mean(dt_baseline[[price_var]], na.rm = TRUE)
      dt_baseline[is.na(get(rev_var)), (rev_var) := 3 * (price_mean * get(yield_var))]
      
      dt_baseline[is.na(get(rev_var)), (rev_var) := 0]
    }
  }
}

cols_order <- c(grep("^winter_ovper", names(dt_baseline), value = TRUE),
                grep("^winter_yield", names(dt_baseline), value = TRUE),
                grep("^winter_", names(dt_baseline), value = TRUE))

# boro and aman rice costs 
for(j in c("boro", "aman")) {
  i <- "1"  
  vars_cost <- paste0(j, "_rvcost", c("plant", "weed", "har", "fert", "herb"), "_", i)
  cost_var <- paste0(j, "cost_", i) 
  
  dt_baseline[, (cost_var) := rowSums(.SD, na.rm = TRUE), .SDcols = vars_cost]
  
  area_var <- paste0("area", i, "_bg")
  dt_baseline[, (cost_var) := (get(cost_var) / get(area_var)) * 3]
}


# boro water costs 

dt_baseline[, boro_watercost_1 := 0]
dt_baseline[boro_how_1 == 1, boro_watercost_1 := boro_fixed_1]
dt_baseline[boro_how_1 == 2, boro_watercost_1 := boro_pb_1 * area1_bg]
dt_baseline[boro_how_1 == 3, boro_watercost_1 := boro_rvirrig_1 * boro_hourly_1 * 0.5] 
dt_baseline[boro_how_1 == 4, boro_watercost_1 := boro_rvirrig_1 * boro_diesel_1]     
dt_baseline[boro_how_1 == 6, boro_watercost_1 := boro_fixed_1 + boro_rvirrig_1 * boro_diesel_1]
dt_baseline[boro_how_1 == 8, boro_watercost_1 := (area1_bg * boro_pb_1) + boro_rvirrig_1 * boro_diesel_1]


dt_baseline[, boro_watercostpacre_1 := 3 * (boro_watercost_1 / area1_bg)]

# Total costs per acre 
dt_baseline[, boro_tcost_1 := boro_watercostpacre_1 + borocost_1]

# Generate other variables 
dt_baseline[, hhsize := nadults + nyoung]          
dt_baseline[, landhold := totarea_bg / 3]           
dt_baseline[, knowawd := (heardawd == "yes")]        
dt_baseline[, renter := as.numeric(ten1 %in% c(2,3))] 
dt_baseline[, area1_acre := area1_bg / 3]            

# generate asset ownership variables
for(i in 1:9) {
  colname <- paste0("assetheld_", i)
  dt_baseline[, (colname) := as.numeric(grepl(paste0("\\b", i, "\\b"), assets) & !grepl("10", assets))]
}

# Keep baseline
vars_keep <- c("farmer_id", "age", "edu", "hhsize", "livestock", "landhold",
               "assetheld_1", "assetheld_5", "assetheld_9", "knowawd", "renter",
               "area1_acre", "anymarginal", "ncrop1", "rice_rice_sys",
               "boro_rvirrig_1", "boro_revperacre_1", "boro_tcost_1", "boro_watercostpacre_1",
               "aman_peracre_1")

baseline_covariates <- dt_baseline[, ..vars_keep]

# Save dataset
write_dta(baseline_covariates, "C:\\Users\\Dell\\replication_package\\data\\derived\\baseline_covariates.dta")

# Merge data ---------------------------------------------------------------

dt_watermeasure <- fread("C:\\Users\\Dell\\replication_package\\data\\input_primary\\rct1_watermeasure.csv")
dt_watermeasure <- dt_watermeasure[!is.na(waterunit)]
gis_cov <- fread("C:\\Users\\Dell\\replication_package\\data\\input_secondary\\GIS_covariates.csv")
gis_cov <- gis_cov[, .(farmer_id, srtm_eleva, soilclay, soilsand, soilcarbon, soilwater)]
dt <- merge(dt_watermeasure, gis_cov, by = "farmer_id", all.x = TRUE)
dt <- dt[!is.na(srtm_eleva)]

# Create interaction variable------------------------------------------------
dt[, t_marg := treatment * anymarginal]

# Merge with baseline covariates --------------------------------------------
baseline_cov <- fread("C:\\Users\\Dell\\replication_package\\data\\derived\\baseline_covariates.csv")
baseline_cov <- baseline_cov[, .(farmer_id, age, edu, hhsize, livestock, landhold,
                                 assetheld_1, assetheld_5, assetheld_9, knowawd, renter,
                                 area1_acre, ncrop1, rice_rice_sys, boro_rvirrig_1, 
                                 boro_revperacre_1, boro_tcost_1, boro_watercostpacre_1,
                                 aman_peracre_1)]

dt <- merge(dt, baseline_cov, by = "farmer_id", all.x = TRUE)
print(names(dt))

# Demean----------------------------------------------------------------------

# This chunk demeans control variables globally and by geographic units (upazila),
# creating interactions with the treatment variable.

# demean control variables and create upazila-level mean
vars <- c("age", "edu", "hhsize", "livestock", "landhold", 
          "assetheld_1", "assetheld_5", "assetheld_9", "knowawd", 
          "renter", "area1_acre", "ncrop1", "rice_rice_sys", 
          "boro_rvirrig_1", "boro_revperacre_1", "boro_tcost_1", 
          "boro_watercostpacre_1", "aman_peracre_1")

for(k in vars) {
  dt[, paste0("bc_", k) := get(k)]
  overall_mean <- dt[, mean(get(paste0("bc_", k)), na.rm = TRUE)]
  dt[, paste0("dmean_", k) := get(paste0("bc_", k)) - overall_mean]
  dt[, paste0("t_dmean_", k) := treatment * get(paste0("dmean_", k))]
  dt[, paste0("upamean_", k) := mean(get(paste0("bc_", k)), na.rm = TRUE), by = upazila_id]
  upa_overall <- dt[, mean(get(paste0("upamean_", k)), na.rm = TRUE)]
  dt[, paste0("dupamean_", k) := get(paste0("upamean_", k)) - upa_overall]
  dt[, paste0("t_dupamean_", k) := treatment * get(paste0("dupamean_", k))]
}

# *demean control variables and create upazila-level mean (geographic variables)


vars_geo <- c("srtm_eleva", "soilclay", "soilsand", "soilcarbon", "soilwater")

for(k in vars_geo) {
  dt[, paste0("bc_", k) := get(k)]
  overall_mean <- dt[, mean(get(paste0("bc_", k)), na.rm = TRUE)]
  dt[, paste0("dmeanGeo_", k) := get(paste0("bc_", k)) - overall_mean]
  dt[, paste0("t_dmeanGeo_", k) := treatment * get(paste0("dmeanGeo_", k))]
  dt[, paste0("upameanGeo_", k) := mean(get(paste0("bc_", k)), na.rm = TRUE), by = upazila_id]
  upa_overall <- dt[, mean(get(paste0("upameanGeo_", k)), na.rm = TRUE)]
  dt[, paste0("dupameanGeo_", k) := get(paste0("upameanGeo_", k)) - upa_overall]
  dt[, paste0("t_dupameanGeo_", k) := treatment * get(paste0("dupameanGeo_", k))]
}

# Regression ----------------------------------------------------------------

# regression wlevel_rob
dmean_vars <- grep("^dmean_", names(dt), value = TRUE)
t_dmean_vars <- grep("^t_dmean_", names(dt), value = TRUE)
rhs <- paste(c("treatment", "anymarginal", "t_marg", dmean_vars, t_dmean_vars), collapse = " + ")
formula_str <- paste("waterlevel ~", rhs, "| Upazila")
formula <- as.formula(formula_str)
mod <- feols(formula, data = dt, cluster = "village_id")

lh_test <- linearHypothesis(mod, "treatment + t_marg = 0")
p_value <- lh_test[2, "Pr(>Chisq)"]

# Calcul immédiat de la moyenne du groupe contrôle
vars_used <- c("waterlevel", "treatment", "anymarginal", "t_marg", dmean_vars, t_dmean_vars, "upazila_id", "village_id")
dt_model_geo <- dt[complete.cases(dt[, ..vars_used])]
mean_control_geo <- dt_model_geo[treatment == 0, mean(waterlevel, na.rm = TRUE)]
cat("Moyenne du groupe contrôle (waterlevel):", mean_control_geo, "\n")

# Y a-t-il des données pour chaque groupe de traitement après filtrage?
dt_diagnostics <- dt[complete.cases(dt[, ..vars_needed])]
table(dt_diagnostics$treatment)



wlevel_rob <- list(
  model = mod,
  heterogeneity_p_value = p_value,
  mean_control = mean_control,
  test_result = lh_test
)

cat("=== Résultats pour wlevel_rob ===\n")
print(summary(wlevel_rob$model))
cat("\nMoyenne du groupe contrôle (waterlevel) :", wlevel_rob$mean_control, "\n")
cat("P-value du test (treatment + t_marg = 0) :", wlevel_rob$heterogeneity_p_value, "\n")

# regression wlevel_rob_geo

dmean_vars_general <- grep("^dmean_(?!Geo)", names(dt), perl = TRUE, value = TRUE)
t_dmean_vars_general <- grep("^t_dmean_(?!Geo)", names(dt), perl = TRUE, value = TRUE)
dmeanGeo_vars <- grep("^dmeanGeo_", names(dt), value = TRUE)
t_dmeanGeo_vars <- grep("^t_dmeanGeo_", names(dt), value = TRUE)
rhs_vars <- c("treatment", "anymarginal", "t_marg",
              dmean_vars_general, t_dmean_vars_general,
              dmeanGeo_vars, t_dmeanGeo_vars)
rhs <- paste(rhs_vars, collapse = " + ")
formula_str <- paste("waterlevel ~", rhs, "| Upazila")
formula <- as.formula(formula_str)
mod <- feols(formula, data = dt_model, cluster = "village_id")

# Calcul de la moyenne du groupe de controle
dt_model <- copy(dt)
vars_model <- c("waterlevel", "treatment", "anymarginal", "t_marg", 
                dmean_vars_general, t_dmean_vars_general, dmeanGeo_vars, t_dmeanGeo_vars, "upazila")
dt_model <- dt[complete.cases(dt[, ..vars_model])]
mod2 <- feols(formula, data = dt_model, cluster = "village_id")
mean_control <- dt_model[treatment == 0, mean(waterlevel, na.rm = TRUE)]
cat("Moyenne du groupe contrôle (waterlevel) :", mean_control, "\n")

# Extraire le résumé du modèle
s <- summary(mod)
global_R2 <- s$r2
adj_R2 <- s$adj.r2
within_R2 <- s$r2.within 

# Test de l'hypothèse : treatment + t_marg = 0 à l'aide de linearHypothesis()
lh_test <- linearHypothesis(mod, "treatment + t_marg = 0")
p_value <- lh_test[2, "Pr(>Chisq)"]


# Stockage des résultats dans une liste, simulant 'estimates store wlevel_rob_geo'
wlevel_rob_geo <- list(
  model = mod,
  adj_R2 = adj_R2,
  within_R2 = within_R2,
  heterogeneity_p_value = p_value,
  mean_control = mean_control,
  test_result = lh_test
)

# Print qummary to check results
cat("=== Résultats du modèle wlevel_rob_geo ===\n")
print(summary(wlevel_rob_geo$model))
cat("\nMoyenne du groupe contrôle (waterlevel) :", wlevel_rob_geo$mean_control, "\n")
cat("P-value du test (treatment + t_marg = 0) :", wlevel_rob_geo$heterogeneity_p_value, "\n")

# Regression upasoil_soilclay, upasoil_soilsand, upasoil_soilcarbon, upasoil_soilwater

dmean_vars_general <- grep("^dmean_(?!Geo)", names(dt), perl = TRUE, value = TRUE)
t_dmean_vars_general <- grep("^t_dmean_(?!Geo)", names(dt), perl = TRUE, value = TRUE)
geo_vars <- c("srtm_eleva", "soilclay", "soilsand", "soilcarbon", "soilwater")
upasoil_results <- list()

for(k in geo_vars) {

  t_dupameanGeo_var <- paste0("t_dupameanGeo_", k)
  rhs_vars <- c("treatment", "anymarginal", "t_marg",
                dmean_vars_general, t_dmean_vars_general,
                t_dupameanGeo_var)
  rhs <- paste(rhs_vars, collapse = " + ")
  

  formula_str <- paste("waterlevel ~", rhs, "| Upazila")
  formula <- as.formula(formula_str)
  
  cat("Formule utilisée pour", k, ":\n", formula_str, "\n\n")
  
  # Model estimation
  mod <- feols(formula, data = dt, cluster = "village_id")
  
  # Test hypothesis: treatment + t_marg = 0
  lh_test <- linearHypothesis(mod, "treatment + t_marg = 0")
  p_value <- lh_test[2, "Pr(>Chisq)"]
  
  # Compute mean waterlevel for the controm group (treatment == 0)
  mean_control <- dt_model[treatment == 0, mean(waterlevel, na.rm = TRUE)]
  
  upasoil_results[[paste0("upasoil_", k)]] <- list(
    model = mod,
    heterogeneity_test = lh_test,
    heterogeneity_p_value = p_value,
    mean_control = mean_control
  )
  
  cat("Pour", k, ":\n")
  cat("Moyenne du groupe contrôle (waterlevel) :", mean_control, "\n")
  cat("P-value du test (treatment + t_marg = 0) :", p_value, "\n\n")
}

# Print summaries to check the results
print(summary(upasoil_results[["upasoil_soilsand"]][["model"]]))
print(summary(upasoil_results[["upasoil_soilclay"]][["model"]]))
print(summary(upasoil_results[["upasoil_soilcarbon"]][["model"]]))
print(summary(upasoil_results[["upasoil_soilwater"]][["model"]]))

# Create the table as a tex file -------------------------------------------
models <- list(
  "wlevel_rob" = wlevel_rob$model,
  "wlevel_rob_geo" = wlevel_rob_geo$model,
  "upasoil_soilclay" = upasoil_results[["upasoil_soilclay"]][["model"]],
  "upasoil_soilsand" = upasoil_results[["upasoil_soilsand"]][["model"]],
  "upasoil_soilcarbon" = upasoil_results[["upasoil_soilcarbon"]][["model"]],
  "upasoil_soilwater" = upasoil_results[["upasoil_soilwater"]][["model"]]
)

# 2. Add rows for  "Mean in Control" (waterlevel mean when treatment == 0)  an "p-Value: Treat+Treat*Volumetric"
add_rows_df <- data.frame(
  term = c("Mean in Control", "p-Value: Treat+Treat*Volumetric"),
  wlevel_rob = c(wlevel_rob$mean_control, wlevel_rob$heterogeneity_p_value),
  wlevel_rob_geo = c(wlevel_rob_geo$mean_control, wlevel_rob_geo$heterogeneity_p_value),
  upasoil_soilclay = c(upasoil_results[["upasoil_soilclay"]][["mean_control"]],
                        upasoil_results[["upasoil_soilclay"]][["heterogeneity_p_value"]]),
  upasoil_soilsand = c(upasoil_results[["upasoil_soilsand"]][["mean_control"]],
                        upasoil_results[["upasoil_soilsand"]][["heterogeneity_p_value"]]),
  upasoil_soilcarbon = c(upasoil_results[["upasoil_soilcarbon"]][["mean_control"]],
                         upasoil_results[["upasoil_soilcarbon"]][["heterogeneity_p_value"]]),
  upasoil_soilwater = c(upasoil_results[["upasoil_soilwater"]][["mean_control"]],
                        upasoil_results[["upasoil_soilwater"]][["heterogeneity_p_value"]])
)

# Create LaTex Table
options("modelsummary_format_numeric_latex" = "plain")
tbl <- modelsummary(models,
   statistic = c("{estimate}", "({std.error})"),
   stars = c(`*` = 0.10, `**` = 0.05, `***` = 0.01),
   add_rows = add_rows_df,
   coef_omit = "^_cons$",
   output = "latex",
   coef_order = c("treatment", "t_marg", "anymarginal", 
                  "t_dupameanGeo_soilclay", "t_dupameanGeo_soilsand", 
                  "t_dupameanGeo_soilcarbon", "t_dupameanGeo_soilwater"),
   notes = "Upazila Fixed Effects = _IUpa*; Controls = dmean_*; Controls X Treatment = t_dmean_*; Geo Controls = dmeanGeo*; Geo Controls X Treatment = t_dmeanGeo_*",
   file = NULL  
)

# Save file
tbl_text <- paste(tbl, collapse = "\n")
writeLines(tbl_text, "C:\\Users\\Dell\\replication_package\\table5_water_robust_interaction.tex")
