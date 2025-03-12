# Packages ----------------------------------------------------------------
library(fixest)
library(data.table)
library(modelsummary)
library(haven)
library(car)

### Imput Use
## Importer les fichiers

# Charger le fichier de suivi
dt_followup <- fread("C:\\Users\\Dell\\replication_package\\data\\input_primary\\rct1_followup.csv")

# Charger le fichier baseline
dt_baseline_temp <- fread("C:\\Users\\Dell\\replication_package\\data\\input_primary\\rct1_baseline.csv")
# Convertir en data.table et ne garder que la colonne farmer_id
dt_baseline <- as.data.table(dt_baseline_temp)[, .(farmer_id)]

# Fusionner les deux fichiers sur farmer_id (en gardant uniquement les observations avec match)
dt <- merge(dt_followup, dt_baseline, by = "farmer_id", all.x = TRUE)
dt <- dt[farmer_id %in% dt_baseline$farmer_id]


# Définir les prix
dt[, purea := 16]
dt[, ptsp := 22]
dt[, pmop := 14]
dt[, pother := 17.3333333]

library(data.table)

## generate fertilizer expenditures per bigah

# Définir la correspondance entre chaque engrais et la variable de prix correspondante
price_vars <- c(urea = "purea", tsp = "ptsp", mop = "pmop", other = "pother")
fertilizers <- names(price_vars)  # "urea", "tsp", "mop", "other"

# Pour chaque engrais, calculer la quantité totale, la dépense et la dépense par bigah
for(j in fertilizers) {
  # Créer la liste des colonnes de dose pour l'engrais j
  dose_vars <- paste0("a_dose", 1:5, "_", j)
  
  # Calculer la somme des doses par ligne
  dt[, paste0("a_kg_", j) := rowSums(.SD, na.rm = TRUE), .SDcols = dose_vars]
  
  # Calculer la dépense totale en multipliant la quantité par le prix
  dt[, paste0("a_spend_", j) := get(paste0("a_kg_", j)) * get(price_vars[j])]
  
  # Calculer la dépense par bigah
  dt[, paste0("a_spendbg_", j) := get(paste0("a_spend_", j)) / a_areacult_bg]
}

# Créer la variable totalfert_bg en additionnant les dépenses par bigah de tous les engrais
dt[, totalfert_bg := a_spendbg_urea + a_spendbg_tsp + a_spendbg_mop + a_spendbg_other]

# Réorganiser l'ordre des colonnes pour placer celles qui commencent par a_kg_ et a_spend* après "a_dose5_other"
all_cols <- names(dt)
pos <- match("a_dose5_other", all_cols)
# Sélectionner les colonnes commençant par a_kg_ ou a_spend_
cols_to_move <- grep("^(a_kg_|a_spend)", all_cols, value = TRUE)
new_order <- c(all_cols[1:pos], cols_to_move, setdiff(all_cols, c(all_cols[1:pos], cols_to_move)))
setcolorder(dt, new_order)

## Generate family labor per bigah 

# Calcul de la dépense de récolte par bigah
dt[, a_famhar_bg := (a_famhar * a_harwage) / a_areacult_bg]

# Calcul de la dépense de plantation par bigah
dt[, a_famplant_bg := (a_famplant * a_plantwage) / a_areacult_bg]

# Calcul du salaire moyen pour le désherbage à partir des salaires de plantation et de récolte
dt[, weed_wage := rowMeans(.SD, na.rm = TRUE), .SDcols = c("a_plantwage", "a_harwage")]

# Calcul de la dépense de désherbage par bigah
dt[, a_famweed_bg := (a_famweed * weed_wage) / a_areacult_bg]


## Generate interaction term between treatment and volumetric prices 
dt[, t_marg := anymarginal * treatment]

## Convert everything to per acre (for making units consistent across tables)
# Liste des variables à convertir (multiplication par 3)
vars_to_convert <- c("a_spendbg_urea", "a_spendbg_tsp", "a_spendbg_mop", "a_spendbg_other",
                     "a_pesticides_bg", "a_herbicides_bg", "a_plantspend_bg", "a_weedspend_bg", 
                     "a_harspend_bg", "a_famplant_bg", "a_famhar_bg", "a_famweed_bg", 
                     "a_watercost", "totalfert_bg")

# Multiplier chaque variable par 3 (de manière vectorisée)
dt[, (vars_to_convert) := lapply(.SD, function(x) x * 3), .SDcols = vars_to_convert]

### self-reported water use

# Créer la variable logarithmique de la dépense en eau
dt[, loga_watercost := log(a_watercost)]

# Liste des variables de résultats
outcomes <- c("a_irrigations", "a_timedry", "a_watercost", "loga_watercost")

# Deuxième modèle : régression avec variables additionnelles
formula2 <- as.formula(paste("loga_watercost", "~ treatment + t_marg + anymarginal + i(Upazila)"))
mod1 <- feols(formula2, data = dt, cluster = ~village_id)
mean_control1 <- dt[treatment == 0, mean(get("loga_watercost"), na.rm = TRUE)]

# Test de restriction : somme de treatment et t_marg égale à 0
test_result <- linearHypothesis(mod1, "treatment + t_marg = 0")
# Extraire le p-value du test ; la structure de test_result peut varier, ici on suppose une colonne "Pr(>F)"
phet <- test_result$`Pr(>F)`[2]

# Stockage des résultats dans la liste results avec _het ajouté à la fin de la variable outcome
results[[paste0("loga_watercost", "_het")]] <- list(
  model = mod1,
  mean_control = mean_control1,
  heterogeneity_test = test_result,
  heterogeneity_p_value = phet
)


cat("Modèle:", "loga_watercost_het", "\n")
cat("Moyenne du groupe contrôle:", results[["loga_watercost_het"]]$mean_control, "\n")
cat("P-value du test d'hétérogénéité:", results[["loga_watercost_het"]]$heterogeneity_p_value, "\n\n")
print(summary(results[["loga_watercost_het"]]$model))
cat("-------------------------------------------------------------\n\n")





### Revenues and profit

## mean fill on price 

# 1. Calcul du prix par kg
dt[, a_price_per_kg := a_price * (1 / 40)]

# 2. Remplacement des valeurs manquantes de a_price par la moyenne
dt[, a_price := ifelse(is.na(a_price), mean(a_price, na.rm = TRUE), a_price)]

## generate variables 
# 3. Conversion du rendement de monn per bigha à kg per acre
dt[, a_yield_kgac := a_yield * 120]

# 4. Calcul des revenus par acre
dt[, a_rev_ac := a_yield * a_price * 3]

# 5. Calcul du coût total par acre (somme des dépenses)
dt[, a_cost_ac := rowSums(.SD, na.rm = TRUE), .SDcols = c("a_spendbg_urea", "a_spendbg_tsp", "a_spendbg_mop", "a_spendbg_other",
                                                           "a_pesticides_bg", "a_herbicides_bg", "a_plantspend_bg", 
                                                           "a_weedspend_bg", "a_harspend_bg", "a_famplant_bg", 
                                                           "a_famhar_bg", "a_famweed_bg", "a_watercost")]

# 6. Calcul du profit par acre
dt[, a_profit_ac := a_rev_ac - a_cost_ac]

# 7. Calcul du coût sans eau par acre
dt[, a_nonwater_ac := a_cost_ac - a_watercost]

# 8. Calcul des logarithmes des variables
for(i in c("a_yield_kgac", "a_rev_ac", "a_profit_ac", "a_nonwater_ac")) {
  dt[, paste0("log_", i) := log(get(i))]
}

## run regression
# Liste des variables de résultats
outcomes <- c("log_a_yield_kgac", "log_a_rev_ac", "log_a_profit_ac")

# Liste pour stocker les résultats
results <- list()

for (j in outcomes) {
  
  # Deuxième modèle : régression avec variables d'hétérogénéité
  # La formule inclut treatment, t_marg, anymarginal et les effets fixes pour Upazila
  formula2 <- as.formula(paste(j, "~ treatment + t_marg + anymarginal | Upazila"))
  
  # Estimation du modèle avec erreurs standard clusterisées par village_id
  mod2 <- feols(formula2, data = dt, cluster = "village_id")
  
  # Extraction de l'échantillon utilisé pour l'estimation
  mf2 <- model.frame(mod2)
  
  # Calcul de la moyenne de j pour le groupe contrôle dans cet échantillon
  mean_control2 <- mean(mf2[[j]][mf2$treatment == 0], na.rm = TRUE)
  
  # Stockage du modèle et des résultats dans la liste results
  results[[paste0(j, "_het")]] <- list(
    model = mod2,
    mean_control = mean_control2,
    heterogeneity_test = lh_test,
    heterogeneity_p_value = p_value
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

# Charger les packages nécessaires
library(data.table)
library(fixest)
library(car)

# On suppose que votre data.table s'appelle dt
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
mod_logcost <- feols(formula_logcost, data = dt_sub, cluster = "village_id")

# Calculer la moyenne de loga_watercost pour le groupe contrôle (treatment == 0) dans l'échantillon utilisé
mf_logcost <- model.frame(mod_logcost)
mean_control_logcost <- mean(mf_logcost$loga_watercost[mf_logcost$treatment == 0], na.rm = TRUE)

# Tester l'hypothèse que treatment + t_hourcard = 0 à l'aide de linearHypothesis()
lh_logcost <- linearHypothesis(mod_logcost, "treatment + t_hourcard = 0")
# Extraire la p-value (seconde ligne, colonne "Pr(>Chisq)")
p_value_logcost <- lh_logcost[2, "Pr(>Chisq)"]

# Stocker les résultats dans une liste sous le nom "logcost_card"
results <- list()
results[["logcost_card"]] <- list(
  model = mod_logcost,
  mean_control = mean_control_logcost,
  heterogeneity_test = lh_logcost,
  heterogeneity_p_value = p_value_logcost
)

### Régression sur log_a_profit_ac

# Spécifier la formule avec effets fixes par Upazila
formula_logprofit <- as.formula("log_a_profit_ac ~ treatment + t_hourcard + hourcard | Upazila")

# Estimer le modèle avec cluster sur village_id
mod_logprofit <- feols(formula_logprofit, data = dt_sub, cluster = "village_id")

# Calculer la moyenne de log_a_profit_ac pour le groupe contrôle dans l'échantillon utilisé
mf_logprofit <- model.frame(mod_logprofit)
mean_control_logprofit <- mean(mf_logprofit$log_a_profit_ac[mf_logprofit$treatment == 0], na.rm = TRUE)

# Tester l'hypothèse que treatment + t_hourcard = 0
lh_logprofit <- linearHypothesis(mod_logprofit, "treatment + t_hourcard = 0")
p_value_logprofit <- lh_logprofit[2, "Pr(>Chisq)"]

# Stocker les résultats dans une liste sous le nom "logprofit_card"
results[["logprofit_card"]] <- list(
  model = mod_logprofit,
  mean_control = mean_control_logprofit,
  heterogeneity_test = lh_logprofit,
  heterogeneity_p_value = p_value_logprofit
)

### Affichage des résultats

cat("Modèle loga_watercost (logcost_card) :\n")
print(summary(results[["logcost_card"]]$model))
cat("Moyenne du groupe contrôle (loga_watercost) : ", results[["logcost_card"]]$mean_control, "\n")
cat("P-value du test (treatment + t_hourcard = 0) : ", results[["logcost_card"]]$heterogeneity_p_value, "\n\n")

cat("Modèle log_a_profit_ac (logprofit_card) :\n")
print(summary(results[["logprofit_card"]]$model))
cat("Moyenne du groupe contrôle (log_a_profit_ac) : ", results[["logprofit_card"]]$mean_control, "\n")
cat("P-value du test (treatment + t_hourcard = 0) : ", results[["logprofit_card"]]$heterogeneity_p_value, "\n")

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
mod_logcost <- feols(formula_logcost, data = dt_sub, cluster = "village_id")

# Calculer la moyenne de loga_watercost pour le groupe contrôle (treatment == 0) dans l'échantillon utilisé
mf_logcost <- model.frame(mod_logcost)
mean_control_logcost <- mean(mf_logcost$loga_watercost[mf_logcost$treatment == 0], na.rm = TRUE)

# Tester l'hypothèse que treatment + t_hourcard = 0 à l'aide de linearHypothesis()
lh_logcost <- linearHypothesis(mod_logcost, "treatment + t_hourcard = 0")
# Extraire la p-value (seconde ligne, colonne "Pr(>Chisq)")
p_value_logcost <- lh_logcost[2, "Pr(>Chisq)"]

# Stocker les résultats dans une liste sous le nom "logcost_card"
results <- list()
results[["logcost_card"]] <- list(
  model = mod_logcost,
  mean_control = mean_control_logcost,
  heterogeneity_test = lh_logcost,
  heterogeneity_p_value = p_value_logcost
)

### Régression sur log_a_profit_ac

# Spécifier la formule avec effets fixes par Upazila
formula_logprofit <- as.formula("log_a_profit_ac ~ treatment + t_hourcard + hourcard | Upazila")

# Estimer le modèle avec cluster sur village_id
mod_logprofit <- feols(formula_logprofit, data = dt_sub, cluster = "village_id")

# Calculer la moyenne de log_a_profit_ac pour le groupe contrôle dans l'échantillon utilisé
mf_logprofit <- model.frame(mod_logprofit)
mean_control_logprofit <- mean(mf_logprofit$log_a_profit_ac[mf_logprofit$treatment == 0], na.rm = TRUE)

# Tester l'hypothèse que treatment + t_hourcard = 0
lh_logprofit <- linearHypothesis(mod_logprofit, "treatment + t_hourcard = 0")
p_value_logprofit <- lh_logprofit[2, "Pr(>Chisq)"]

# Stocker les résultats dans une liste sous le nom "logprofit_card"
results[["logprofit_card"]] <- list(
  model = mod_logprofit,
  mean_control = mean_control_logprofit,
  heterogeneity_test = lh_logprofit,
  heterogeneity_p_value = p_value_logprofit
)

### Affichage des résultats

cat("Modèle loga_watercost (logcost_card) :\n")
print(summary(results[["logcost_card"]]$model))
cat("Moyenne du groupe contrôle (loga_watercost) : ", results[["logcost_card"]]$mean_control, "\n")
cat("P-value du test (treatment + t_hourcard = 0) : ", results[["logcost_card"]]$heterogeneity_p_value, "\n\n")

cat("Modèle log_a_profit_ac (logprofit_card) :\n")
print(summary(results[["logprofit_card"]]$model))
cat("Moyenne du groupe contrôle (log_a_profit_ac) : ", results[["logprofit_card"]]$mean_control, "\n")
cat("P-value du test (treatment + t_hourcard = 0) : ", results[["logprofit_card"]]$heterogeneity_p_value, "\n")




