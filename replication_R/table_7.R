# This files replicate the findings of Ujjayant Chakravorty, Kyle Emerick, and Manzoor Dar
# For the paper "Inefficient water pricing and incentives for conservation" published in the American Economic Review in 2023
# Table 7

# Packages ----------------------------------------------------------------
library(fixest)
library(data.table)
library(modelsummary)
library(knitr)
library(dplyr)
library(lmtest)       
library(sandwich)     
library(car)          

# Data --------------------------------------------------------------------

rct2 = fread(get_data("input_primary/rct2_baseline.csv"), select = c("farmer_id", "waterprice", "waterirrig", "waterwhopay", "nadults", "nyoung"))
rct2_usage = fread(get_data("input_primary/rct2_usage.csv"))
rct2_pipesales = fread(get_data("input_primary/rct2_pipesales.csv"), select = c("farmer_id", "adoption"))
rct2 = merge(rct2, rct2_usage, by = "farmer_id")
rct2 = merge(rct2, rct2_pipesales, by = "farmer_id")
rm(rct2_usage, rct2_pipesales)
gc()

rct2[, `:=` (logprice = log(price),
             cardtreat = as.integer(treatment == "card"),  
             cardtreat_price = as.integer(treatment == "card") * price,
             cardtreat_logprice = as.integer(treatment == "card") * log(price),  
             awd_use = as.integer(awdirri == 1 | awdoth == 1),  
             dpaba = as.integer(Upazila == "PABA"),
             dtanore = as.integer(Upazila == "TANORE"))]

for (j in c("dpaba", "dtanore")) {
  rct2[, paste0(j, "_d") := get(j) - mean(get(j), na.rm = TRUE)]
}


# AWD Installed -------------------------------------------------------------
# Column 1
model_awdins= feols(awdmeas ~ cardtreat + price, fixef = 'upazila', cluster = ~village_id, data = rct2)

# AWD Installed, Interaction -------------------------------------------------------------
# Column 2
model_awdins_i = feols(awdmeas ~ cardtreat + price + cardtreat_price, fixef = 'upazila', cluster = ~village_id, data = rct2)

mean_control_awdins = rct2[cardtreat == 0, mean(awdmeas, na.rm = TRUE)]

# Water Level -------------------------------------------------------------
# Column 3
rct2[, waterlevel_comp := waterlevel_c_cm]
rct2[, waterlevel_comp := ifelse(is.na(waterlevel_c_cm), waterlevel_f_cm, waterlevel_c_cm)]
model_wl= feols(waterlevel_comp ~ cardtreat + price, fixef = 'upazila', cluster = ~village_id, data = rct2)

# Water Level, Interacted -------------------------------------------------------------
# Column 3
model_wl_i= feols(waterlevel_comp ~ cardtreat + price + cardtreat_price, fixef = 'upazila', cluster = ~village_id, data = rct2)
mean_control_wl = rct2[cardtreat == 0, mean(waterlevel_comp, na.rm = TRUE)]

# AWD Purchased -----------------------------------------------------------
# Column 5
# model_ldp = feols(adoption ~ cardtreat + price + dpaba + dtanore, fixef = "upazila" ,cluster = ~village_id,data = rct2)
model_ldp = lm(adoption ~ cardtreat + price + dpaba + dtanore, ,data = rct2)
vcov_ldp <- vcovCL(model_ldp, cluster = rct2$village_id)
coeftest_ldp <- coeftest(model_ldp, vcov. = vcov_ldp)
mean_adoption_control <- rct2[cardtreat == 0, mean(adoption, na.rm = TRUE)]

coef_ldp <- coef(model_ldp)
epst_ldp <- coef_ldp["price"] * 55 / (coef_ldp["(Intercept)"] + coef_ldp["cardtreat"] + coef_ldp["price"] * 55)
epsc_ldp <- coef_ldp["price"] * 55 / (coef_ldp["(Intercept)"] + coef_ldp["price"] * 55)

# AWD Purchasing, Interaction -----------------------------------
# Column 6
model_ldp_i <- lm(adoption ~ cardtreat + price + cardtreat_price + dpaba_d + dtanore_d,data = rct2)
vcov_ldp_i <- vcovCL(model_ldp_i, cluster = rct2$village_id)
coeftest_ldp_i <- coeftest(model_ldp_i, vcov. = vcov_ldp_i)

# Create a new column "pvalue" to store test p-values
rct2[, pvalue := NA_real_]

# Loop over specific price values and test: cardtreat + i*cardtreat_price = 0
for(i in c(20, 30, 40, 50, 60, 70, 80, 90)) {
  hyp_str <- paste0("cardtreat + ", i, " * cardtreat_price = 0")
  lh_test <- linearHypothesis(model_ldp_i, hyp_str, vcov = vcov_ldp_i, test = "F")
  p_val <- lh_test[2, "Pr(>F)"]
  rct2[price == i, pvalue := p_val]
  cat("Price:", i, "p-value:", p_val, "\n")
}

coef_ldp_i <- coef(model_ldp_i)
epst_ldp_i <- (coef_ldp_i["price"] + coef_ldp_i["cardtreat_price"]) * 55 /
  (coef_ldp_i["(Intercept)"] + coef_ldp_i["cardtreat"] + coef_ldp_i["price"] * 55 + coef_ldp_i["cardtreat_price"] * 55)
epsc_ldp_i <- coef_ldp_i["price"] * 55 /
  (coef_ldp_i["(Intercept)"] + coef_ldp_i["price"] * 55)

delta_expr_ldp_i <- "((price+cardtreat_price)*55/((Intercept)+cardtreat+price*55+cardtreat_price*55)) - (price*55/((Intercept)+price*55))"
dm_ldp_i <- deltaMethod(coef_ldp_i, delta_expr_ldp_i, vcov = vcov_ldp_i)
t_val_ldp_i <- dm_ldp_i[1, "Estimate"] / dm_ldp_i[1, "SE"]
df_ldp_i <- df.residual(model_ldp_i)
pelas_ldp_i <- 2 * (1 - pt(abs(t_val_ldp_i), df_ldp_i))

# AWD Use -----------------------------------------------------
# Column 7 
model_ldu = lm(awd_use ~ cardtreat + price + dpaba_d + dtanore_d ,data = rct2)
vcov_ldu <- vcovCL(model_ldu, cluster = rct2$village_id)
coeftest_ldu <- coeftest(model_ldu, vcov. = vcov_ldu)

mean_awd_use_control <- rct2[cardtreat == 0, mean(awd_use, na.rm = TRUE)]

coef_ldu <- coef(model_ldu)
epst_ldu <- coef_ldu["price"] * 55 / (coef_ldu["(Intercept)"] + coef_ldu["cardtreat"] + coef_ldu["price"] * 55)
epsc_ldu <- coef_ldu["price"] * 55 / (coef_ldu["(Intercept)"] + coef_ldu["price"] * 55)

# AWD Use Interaction ----------------------------------------
# Column 8
model_ldu_i = lm(awd_use ~ cardtreat + price + cardtreat_price + dpaba_d + dtanore_d, data = rct2)
vcov_ldu_i <- vcovCL(model_ldu_i, cluster = rct2$village_id)
coeftest_ldu_i <- coeftest(model_ldu_i, vcov. = vcov_ldu_i)

# Create a new column "pvalue_u" for usage p-values
rct2[, pvalue_u := NA_real_]
for(i in c(20, 30, 40, 50, 60, 70, 80, 90)) {
  hyp_str <- paste0("cardtreat + ", i, " * cardtreat_price = 0")
  lh_test <- linearHypothesis(model_ldu_i, hyp_str, vcov = vcov_ldu_i, test = "F")
  p_val <- lh_test[2, "Pr(>F)"]
  rct2[price == i, pvalue_u := p_val]
  cat("Price:", i, "p-value (usage):", p_val, "\n")
}

coef_ldu_i <- coef(model_ldu_i)
epst_ldu_i <- (coef_ldu_i["price"] + coef_ldu_i["cardtreat_price"]) * 55 /
  (coef_ldu_i["(Intercept)"] + coef_ldu_i["cardtreat"] + coef_ldu_i["price"] * 55 + coef_ldu_i["cardtreat_price"] * 55)
epsc_ldu_i <- coef_ldu_i["price"] * 55 /
  (coef_ldu_i["(Intercept)"] + coef_ldu_i["price"] * 55)

delta_expr_ldu_i <- "((price+cardtreat_price)*55/((Intercept)+cardtreat+price*55+cardtreat_price*55)) - (price*55/((Intercept)+price*55))"
dm_ldu_i <- deltaMethod(coef_ldu_i, delta_expr_ldu_i, vcov = vcov_ldu_i)
t_val_ldu_i <- dm_ldu_i[1, "Estimate"] / dm_ldu_i[1, "SE"]
df_ldu_i <- df.residual(model_ldu_i)
pelas_ldu_i <- 2 * (1 - pt(abs(t_val_ldu_i), df_ldu_i))

# Tables ------------------------------------------------------------------
mean_df = data.frame(
  term = "Mean in control",
  a = mean_control_awdins,
  b = mean_control_awdins,
  c = mean_control_wl,
  d = mean_control_wl,
  e = mean_adoption_control,
  f = mean_adoption_control,
  h = mean_awd_use_control,
  j = mean_awd_use_control
)

elasticity_treat_df = data.frame(
  term = "Elasticity at price = 55 (treat)",
  a = NA,
  b = NA,
  c = NA,
  d = NA,
  e = epst_ldp,
  f = epst_ldp_i,
  h = epst_ldu,
  j = epst_ldu_i
)

elasticity_control_df = data.frame(
  term = "Elasticity at price = 55 (control)",
  a = NA,
  b = NA,
  c = NA,
  d = NA,
  e = epsc_ldp,
  f = epsc_ldp_i,
  h = epsc_ldu,
  j = epsc_ldu_i
)

p_value = data.frame(
  term = "p-value: Equal elasticities",
  a = NA,
  b = NA,
  c = NA,
  d = NA,
  e = NA,
  f = pelas_ldp_i,
  h = NA,
  j = pelas_ldu_i
)

add_to_table = rbind(mean_df, elasticity_treat_df)
add_to_table = rbind(add_to_table, elasticity_control_df)
add_to_table = rbind(add_to_table, p_value)

table7 = modelsummary(
  models = list(
    "AWD Install" = list("(1)" = model_awdins, "(2)" = model_awdins_i),
    "Water Level" = list("(3)" = model_wl, "(4)" = model_wl_i),
    "Purchase AWD" = list("(5)" = model_ldp, "(6)" = model_ldp_i),
    "Use AWD" = list("(7)" = model_ldu, "(8)" = model_ldu_i)
  ),
  shape = "cbind",
  coef_omit = c(4:8),
  coef_rename = c("Cart Treatment", "Pipe Price", "Pipe Price x \n Card Treatment" ),
  gof_omit = "Adj\\.|Within|AIC|BIC|RMSE|Std.Errors|Log.Lik.",
  add_rows = add_to_table,
  output = "latex"
)

table7_char = as.character(table7)
writeLines(table7_char, output_folder("table7.tex"))

