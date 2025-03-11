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

# Créer une liste pour stocker les résultats
results <- list()

for(j in outcomes){
  
  # Premier modèle : régression de base
  # La syntaxe "i(Upazila)" crée les effets fixes pour Upazila
  formula1 <- as.formula(paste(j, "~ treatment + i(Upazila)"))
  reg1 <- feols(formula1, data = dt, cluster = ~village_id)
  
  # Calcul de la moyenne de j pour le groupe contrôle (treatment == 0) parmi les observations utilisées
  mean_control <- dt[treatment == 0, mean(get(j), na.rm = TRUE)]
  
  # Stocker le résultat et la moyenne
  results[[j]] <- list(reg = reg1, mean = mean_control)
  
  # Deuxième modèle : régression avec variables additionnelles
  formula2 <- as.formula(paste(j, "~ treatment + t_marg + anymarginal + i(Upazila)"))
  reg2 <- feols(formula2, data = dt, cluster = ~village_id)
  
  mean_control2 <- dt[treatment == 0, mean(get(j), na.rm = TRUE)]
  
  # Test de restriction : somme de treatment et t_marg égale à 0
  test_result <- linearHypothesis(reg2, "treatment + t_marg = 0")
  # Extraire le p-value du test ; la structure de test_result peut varier, ici on suppose une colonne "Pr(>F)"
  phet <- test_result$`Pr(>F)`[2]
  
  # Stocker le second modèle et le p-value du test
  results[[paste0(j, "_het")]] <- list(reg = reg2, mean = mean_control2, phet = phet)
}

# Pour afficher un résumé du premier modèle sur "a_irrigations", par exemple :
print('a_irrigations')
print(summary(results[["a_irrigations"]]$reg))
print('a_irrigations_het')
print(summary(results[["a_irrigations_het"]]$reg))

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

# Vérifier les résultats
print(head(dt))

