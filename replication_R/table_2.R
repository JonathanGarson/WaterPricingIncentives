# This files replicate the findings of Ujjayant Chakravorty, Kyle Emerick, and Manzoor Dar
# For the paper "Inefficient water pricing and incentives for conservation" published in the American Economic Review in 2023
# Table 2

# Packages ----------------------------------------------------------------
library(fixest)
library(data.table)
library(dplyr)
library(modelsummary)
library(dplyr)
library(car)


# Data --------------------------------------------------------------------

rct1 = fread(get_data("input_primary/rct1_watermeasure.csv"))
rct1_baseline = fread(get_data("input_primary/rct1_baseline.csv"))

# Function ----------------------------------------------------------------

p_value_joint_test = function(dataset, test_var){
  hyp_test = linearHypothesis(dataset, test_var, test = "F")
  p_value = hyp_test$`Pr(>F)`[2]
  return(p_value)
}

# Panel A - Main Results --------------------------------------------------

rct1 = rct1[!is.na(waterunit),] #we get rid of na

# Average treatment effects
reg_waterlevel = feols(waterlevel ~ treatment |Upazila, cluster = ~village_id ,data = rct1)

# heterogeneity by volumetric pricing
reg_het_water_vp = feols(waterlevel ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1)

p_value_overall = p_value_joint_test(reg_het_water_vp, "treatment + treatment:anymarginal = 0")
# p_value_overall = fitstat(reg_het_water_vp, ~ f + wald)$wald$p

# Control mean
control_mean_overall = round(rct1[treatment == 0, .(mean = mean(waterlevel))], 2)$mean

## 70 ----------------------------------------------------------------------
reg_inf70_waterlevel = feols(waterlevel ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat <= 70])
reg_sup70_waterlevel = feols(waterlevel ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat > 70])
reg_inf70_waterlevel_baseline = feols(waterlevel ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat <= 70])
reg_sup70_waterlevel_baseline = feols(waterlevel ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat > 70])

control_mean_inf70 = round(rct1[treatment == 0 & dat < 70, .(mean = mean(waterlevel))], 2)$mean
control_mean_sup70 = round(rct1[treatment == 0 & dat > 70, .(mean = mean(waterlevel))], 2)$mean

# p_value_inf70 = fitstat(reg_inf70_waterlevel, ~ f + wald)
# p_value_sup70 = fitstat(reg_sup70_waterlevel, ~ f + wald)

p_value_inf70 = p_value_joint_test(reg_inf70_waterlevel, "treatment + treatment:anymarginal = 0")
p_value_sup70 = p_value_joint_test(reg_sup70_waterlevel, "treatment + treatment:anymarginal = 0")

## Tables Panel A ----------------------------------------------------------
control_mean_df <- data.frame(
  term = "Control Mean",  # The row label
  Overall_Baseline = control_mean_overall,  
  Overall_Interacted = control_mean_overall,  
  Inf70_Baseline = control_mean_inf70,  
  Inf70_Interacted = control_mean_inf70,  
  Sup70_Baseline = control_mean_sup70,  
  Sup70_Interacted = control_mean_sup70  
)

p_value_df = data.frame(
  term = "p-value",
  Overall_Baseline = NA,  
  Overall_Interacted = p_value_overall,  
  Inf70_Baseline = NA,  
  Inf70_Interacted = p_value_inf70,  
  Sup70_Baseline = NA,  
  Sup70_Interacted = p_value_sup70  
)

add_to_table = rbind(control_mean_df, p_value_df)

# Generate the LaTeX table
table2A = modelsummary(
  models = list(
    "Overall" = list("(1)" = reg_waterlevel, "(2)" = reg_het_water_vp),
    "0–70 days after \nplanting" = list("(3)" = reg_inf70_waterlevel_baseline, "(4)" = reg_inf70_waterlevel),
    "70+ days after \nplanting" = list("(5)" = reg_sup70_waterlevel_baseline, "(6)" = reg_sup70_waterlevel)
  ),
  shape = "cbind",
  gof_omit = "R2|Adj\\.|Within|AIC|BIC|RMSE|Std.Errors|FE",
  coef_rename = c("Treatment", "Volumetric Pricing", "Treatment x Volumetric Pricing"),
  add_rows = add_to_table,
  modelsummary_format_numeric_latex = "plain",
  output = "latex"
)

table_2A_char = as.character(table2A)
writeLines(table_2A_char, output_folder("tables2_A.tex"))

# Panel B - Only in between Upazila variation in volumetric pricing -----------------
rct1_baseline[, upamean_marg := mean(anymarginal), by = upazila]
rct1_baseline[, t_upamean_marg := treatment*upamean_marg, by = upazila]
upamean_db = rct1_baseline[, .(farmer_id, upamean_marg, t_upamean_marg)]
rct1 = merge(rct1, upamean_db, by = "farmer_id")

## Overall -----------------------------------------------------------------
reg_waterlevel_overall_bw = feols(waterlevel ~ treatment + t_upamean_marg, fixef = c("Upazila"), cluster = ~village_id, data = rct1)
p_test_overall_B = p_value_joint_test(reg_waterlevel_overall_bw, "treatment + t_upamean_marg = 0")
# p_test_overall_B = fitstat(reg_waterlevel_overall_bw, ~wald)$wald$p

## <70 days ---------------------------------------------------------------
reg_waterlevel_inf70_bw = feols(waterlevel ~ treatment + t_upamean_marg, fixef = c("Upazila"), cluster = ~village_id, data = rct1[dat <= 70])
p_test_overall_B_inf70 = p_value_joint_test(reg_waterlevel_inf70_bw, "treatment + t_upamean_marg = 0")
# p_test_overall_B_inf70 = fitstat(reg_waterlevel_inf70_bw, ~wald)$wald$p

## >70 days ---------------------------------------------------------------
reg_waterlevel_sup70_bw = feols(waterlevel ~ treatment + t_upamean_marg, fixef = c("Upazila"), cluster = ~village_id, data = rct1[dat > 70])
p_test_overall_B_sup70 = p_value_joint_test(reg_waterlevel_sup70_bw, "treatment + t_upamean_marg = 0")
# p_test_overall_B_sup70 = fitstat(reg_waterlevel_sup70_bw, ~wald)$wald$p

## Tables Panel B ----------------------------------------------------------
add_to_table_B = data.frame(
  term = "p-value",
  Overall_Interacted = p_test_overall_B,  
  Inf70_Interacted = p_test_overall_B_inf70,  
  Sup70_Interacted = p_test_overall_B_sup70
)

table2B = modelsummary(
  models = list(
    "Overall" = list("(1)" = reg_waterlevel_overall_bw),
    "0–70 days after \nplanting" = list("(2)" =reg_waterlevel_inf70_bw),
    "70+ days after \nplanting" = list("(3)" = reg_waterlevel_sup70_bw)
  ),
  shape = "cbind",
  gof_omit = "R2|Adj\\.|Within|AIC|BIC|RMSE|Std.Errors|FE",
  coef_rename = c("Treatment","Treatment x Volumetric Pricing Upazila Mean"),  # Disable automatic GOF statistics
  add_rows = add_to_table_B,
  output = "latex"
)

table_2B_char = as.character(table2B)
writeLines(table_2B_char, output_folder("tables2_B.tex"))

# Panel C - Within Upazila variation --------------------------------------
## Overall -----------------------------------------------------------------
rct1[, treatment_within := treatment - mean(treatment, na.rm = TRUE), by = Upazila]
rct1[, t_marg := anymarginal*treatment]

# With interacted fixed effects for place and treatment
# reg_waterlevel_within_interact = feols(waterlevel ~ treatment + anymarginal + t_marg | Upazila^treatment, cluster = ~village_id, data = rct1)
# With interacted fixed effects alternative
# reg_waterlevel_within_interact = feols(waterlevel ~ treatment + anymarginal + t_marg + i(Upazila, treatment, ref = "BAGMARA"), cluster = ~village_id, data = rct1)
# p_test_overall_C = p_value_joint_test(reg_waterlevel_within_interact , "treatment + anymarginal + t_marg = 0")

# With demean treatment by upazila to avoid colinearity issues.
reg_waterlevel_within = feols(waterlevel ~ treatment_within*anymarginal | Upazila, cluster = ~village_id, data = rct1)
p_test_overall_C = p_value_joint_test(reg_waterlevel_within , "treatment_within + anymarginal +treatment_within:anymarginal = 0")
p_test_overall_C_fit = fitstat(reg_waterlevel_within, ~ f + wald)$wald$p

## <70 days ---------------------------------------------------------------
reg_waterlevel_inf70_wth = feols(waterlevel ~ treatment_within*anymarginal, fixef = c("Upazila"), cluster = ~village_id, data = rct1[dat <= 70])
p_test_inf70_C = p_value_joint_test(reg_waterlevel_inf70_wth, "treatment_within + anymarginal + treatment_within:anymarginal = 0")
p_test_inf70_C_fit = fitstat(reg_waterlevel_inf70_wth, ~ wald)$wald$p

## >70 days ---------------------------------------------------------------
reg_waterlevel_sup70_wth = feols(waterlevel ~ treatment_within*anymarginal, fixef = c("Upazila"), cluster = ~village_id, data = rct1[dat > 70])
p_test_sup70_C = p_value_joint_test(reg_waterlevel_sup70_wth, "treatment_within + anymarginal + treatment_within:anymarginal= 0")
p_test_sup70_C_fit = fitstat(reg_waterlevel_sup70_wth, ~ wald)$wald$p

## Tables Panel C ----------------------------------------------------------

p_value = data.frame(
  term = "p-value (car)",
  Overall_Interacted = p_test_overall_C,  
  Inf70_Interacted = p_test_inf70_C,  
  Sup70_Interacted = p_test_sup70_C
)

p_value_fit = data.frame(
  term = "p-value (fixest)",
  Overall_Interacted = p_test_overall_C_fit,  
  Inf70_Interacted = p_test_inf70_C_fit,  
  Sup70_Interacted = p_test_sup70_C_fit
)

add_to_table_C = rbind(p_value, p_value_fit)

table2C = modelsummary(
  models = list(
    "Overall" = list("(1)" = reg_waterlevel_within),
    "0–70 days after \nplanting" = list("(2)" = reg_waterlevel_inf70_wth),
    "70+ days after \nplanting" = list("(3)" = reg_waterlevel_sup70_wth)
  ),
  shape = "cbind",
  gof_omit = "R2|Adj\\.|Within|AIC|BIC|RMSE|Std.Errors|FE",
  coef_rename = c("Treatment", "Volumetric Pricing","Treatment x Volumetric Pricing"),  # Disable automatic GOF statistics
  add_rows = add_to_table_C,
  output = "latex"
)

table_2C_char = as.character(table2C)
writeLines(table_2C_char, output_folder("tables2_C.tex"))
