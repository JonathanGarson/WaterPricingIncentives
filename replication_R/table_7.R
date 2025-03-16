# This files replicate the findings of Ujjayant Chakravorty, Kyle Emerick, and Manzoor Dar
# For the paper "Inefficient water pricing and incentives for conservation" published in the American Economic Review in 2023
# Table 7

# Packages ----------------------------------------------------------------
library(fixest)
library(data.table)
library(stargazer)
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

# Linear Demand -----------------------------------------------------------

model_ldp <- lm(adoption ~ cardtreat + price + dpaba_d + dtanore_d, data = rct2)
vcov_ldp <- vcovCL(model_ldp, cluster = rct2$village_id)
coeftest_ldp <- coeftest(model_ldp, vcov. = vcov_ldp)

# Compute the mean adoption in control group (cardtreat==0)
mean_adoption_control <- rct2[cardtreat == 0, mean(adoption, na.rm = TRUE)]

coef_ldp <- coef(model_ldp)
epst_ldp <- coef_ldp["price"] * 55 / (coef_ldp["(Intercept)"] + coef_ldp["cardtreat"] + coef_ldp["price"] * 55)
epsc_ldp <- coef_ldp["price"] * 55 / (coef_ldp["(Intercept)"] + coef_ldp["price"] * 55)

lindem <- list(model = model_ldp,
               vcov = vcov_ldp,
               coeftest = coeftest_ldp,
               mean_adoption_control = mean_adoption_control,
               epst = epst_ldp,
               epsc = epsc_ldp)


# Linear Demand Using -----------------------------------------------------
# Outcome: awd_use; Predictors: cardtreat, price, dpaba_d, dtanore_d
model_ldu <- lm(awd_use ~ cardtreat + price + dpaba_d + dtanore_d, data = rct2)
vcov_ldu <- vcovCL(model_ldu, cluster = rct2$village_id)
coeftest_ldu <- coeftest(model_ldu, vcov. = vcov_ldu)

mean_awd_use_control <- rct2[cardtreat == 0, mean(awd_use, na.rm = TRUE)]

coef_ldu <- coef(model_ldu)
epst_ldu <- coef_ldu["price"] * 55 / (coef_ldu["(Intercept)"] + coef_ldu["cardtreat"] + coef_ldu["price"] * 55)
epsc_ldu <- coef_ldu["price"] * 55 / (coef_ldu["(Intercept)"] + coef_ldu["price"] * 55)

lindem_use <- list(model = model_ldu,
                   vcov = vcov_ldu,
                   coeftest = coeftest_ldu,
                   mean_awd_use_control = mean_awd_use_control,
                   epst = epst_ldu,
                   epsc = epsc_ldu)

# Linear Demand Purchasing, Interaction -----------------------------------
#Outcome: adoption; Predictors: cardtreat, price, cardtreat_price, dpaba_d, dtanore_d
model_ldp_i <- lm(adoption ~ cardtreat + price + cardtreat_price + dpaba_d + dtanore_d, data = rct2)
vcov_ldp_i <- vcovCL(model_ldp_i, cluster = rct2$village_id)
coeftest_ldp_i <- coeftest(model_ldp_i, vcov. = vcov_ldp_i)

# Compute choke prices
choke_control <- -coef(model_ldp_i)["(Intercept)"] / coef(model_ldp_i)["price"]
choke_treatment <- (-coef(model_ldp_i)["(Intercept)"] - coef(model_ldp_i)["cardtreat"]) /
  (coef(model_ldp_i)["price"] + coef(model_ldp_i)["cardtreat_price"])
cat("Choke price in control:", choke_control, "\n")
cat("Choke price in treatment:", choke_treatment, "\n")

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

mean_adoption_control_i <- rct2[cardtreat == 0, mean(adoption, na.rm = TRUE)]

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

lindem_i <- list(model = model_ldp_i,
                 vcov = vcov_ldp_i,
                 coeftest = coeftest_ldp_i,
                 mean_adoption_control = mean_adoption_control_i,
                 epst = epst_ldp_i,
                 epsc = epsc_ldp_i,
                 elasticity_diff = dm_ldp_i$Estimate,
                 elasticity_diff_se = dm_ldp_i$SE,
                 pelas = pelas_ldp_i)


# Linear Demand Using, Interaction ----------------------------------------
# Outcome: awd_use; Predictors: cardtreat, price, cardtreat_price, dpaba_d, dtanore_d

model_ldu_i <- lm(awd_use ~ cardtreat + price + cardtreat_price + dpaba_d + dtanore_d, data = rct2)
vcov_ldu_i <- vcovCL(model_ldu_i, cluster = rct2$village_id)
coeftest_ldu_i <- coeftest(model_ldu_i, vcov. = vcov_ldu_i)

choke_control_u <- -coef(model_ldu_i)["(Intercept)"] / coef(model_ldu_i)["price"]
choke_treatment_u <- (-coef(model_ldu_i)["(Intercept)"] - coef(model_ldu_i)["cardtreat"]) /
  (coef(model_ldu_i)["price"] + coef(model_ldu_i)["cardtreat_price"])
cat("Usage - Choke price in control:", choke_control_u, "\n")
cat("Usage - Choke price in treatment:", choke_treatment_u, "\n")

# Create a new column "pvalue_u" for usage p-values
rct2[, pvalue_u := NA_real_]
for(i in c(20, 30, 40, 50, 60, 70, 80, 90)) {
  hyp_str <- paste0("cardtreat + ", i, " * cardtreat_price = 0")
  lh_test <- linearHypothesis(model_ldu_i, hyp_str, vcov = vcov_ldu_i, test = "F")
  p_val <- lh_test[2, "Pr(>F)"]
  rct2[price == i, pvalue_u := p_val]
  cat("Price:", i, "p-value (usage):", p_val, "\n")
}

mean_awd_use_control_i <- rct2[cardtreat == 0, mean(awd_use, na.rm = TRUE)]

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

lindem_i_use <- list(model = model_ldu_i,
                     vcov = vcov_ldu_i,
                     coeftest = coeftest_ldu_i,
                     mean_awd_use_control = mean_awd_use_control_i,
                     epst = epst_ldu_i,
                     epsc = epsc_ldu_i,
                     elasticity_diff = dm_ldu_i$Estimate,
                     elasticity_diff_se = dm_ldu_i$SE,
                     pelas = pelas_ldu_i)


# Log Demand --------------------------------------------------------------
# Outcome: adoption; Predictors: cardtreat, logprice, dpaba_d, dtanore_d

model_logd <- lm(adoption ~ cardtreat + logprice + dpaba_d + dtanore_d, data = rct2)
vcov_logd <- vcovCL(model_logd, cluster = rct2$village_id)
coeftest_logd <- coeftest(model_logd, vcov. = vcov_logd)

mean_adoption_logd_control <- rct2[cardtreat == 0, mean(adoption, na.rm = TRUE)]

coef_logd <- coef(model_logd)
epst_logd <- coef_logd["logprice"] /
  (coef_logd["(Intercept)"] + coef_logd["cardtreat"] + coef_logd["logprice"] * log(55))
epsc_logd <- coef_logd["logprice"] /
  (coef_logd["(Intercept)"] + coef_logd["logprice"] * log(55))

logdem <- list(model = model_logd,
               vcov = vcov_logd,
               coeftest = coeftest_logd,
               mean_adoption_control = mean_adoption_logd_control,
               epst = epst_logd,
               epsc = epsc_logd)

# Log Demand Using --------------------------------------------------------
# Outcome: awd_use; Predictors: cardtreat, logprice, dpaba_d, dtanore_d

model_logdu <- lm(awd_use ~ cardtreat + logprice + dpaba_d + dtanore_d, data = rct2)
vcov_logdu <- vcovCL(model_logdu, cluster = rct2$village_id)
coeftest_logdu <- coeftest(model_logdu, vcov. = vcov_logdu)

mean_awd_use_logd_control <- rct2[cardtreat == 0, mean(awd_use, na.rm = TRUE)]

coef_logdu <- coef(model_logdu)
epst_logdu <- coef_logdu["logprice"] /
  (coef_logdu["(Intercept)"] + coef_logdu["cardtreat"] + coef_logdu["logprice"] * log(55))
epsc_logdu <- coef_logdu["logprice"] /
  (coef_logdu["(Intercept)"] + coef_logdu["logprice"] * log(55))

logdem_use <- list(model = model_logdu,
                   vcov = vcov_logdu,
                   coeftest = coeftest_logdu,
                   mean_awd_use_control = mean_awd_use_logd_control,
                   epst = epst_logdu,
                   epsc = epsc_logdu)

# Log Demand with Interaction ---------------------------------------------
# Outcome: adoption; Predictors: cardtreat, logprice, cardtreat_logprice, dpaba_d, dtanore_d

model_logd_i <- lm(adoption ~ cardtreat + logprice + cardtreat_logprice + dpaba_d + dtanore_d, data = rct2)
vcov_logd_i <- vcovCL(model_logd_i, cluster = rct2$village_id)
coeftest_logd_i <- coeftest(model_logd_i, vcov. = vcov_logd_i)

for(i in c(20, 30, 40, 50, 60, 70, 80, 90)) {
  j <- log(i)
  hyp_str <- paste0("cardtreat + ", j, " * cardtreat_logprice = 0")
  lh_test <- linearHypothesis(model_logd_i, hyp_str, vcov = vcov_logd_i, test = "F")
  cat("For price =", i, ", hypothesis p-value =", lh_test[2, "Pr(>F)"], "\n")
}

choke_control_log <- exp(-coef(model_logd_i)["(Intercept)"] / coef(model_logd_i)["logprice"])
choke_treatment_log <- exp((-coef(model_logd_i)["(Intercept)"] - coef(model_logd_i)["cardtreat"]) /
                             (coef(model_logd_i)["logprice"] + coef(model_logd_i)["cardtreat_logprice"]))
cat("Log Demand - Choke price in control:", choke_control_log, "\n")
cat("Log Demand - Choke price in treatment:", choke_treatment_log, "\n")

mean_adoption_logd_i_control <- rct2[cardtreat == 0, mean(adoption, na.rm = TRUE)]

coef_logd_i <- coef(model_logd_i)
epst_logd_i <- (coef_logd_i["logprice"] + coef_logd_i["cardtreat_logprice"]) /
  (coef_logd_i["(Intercept)"] + coef_logd_i["cardtreat"] +
     coef_logd_i["logprice"] * log(55) + coef_logd_i["cardtreat_logprice"] * log(55))
epsc_logd_i <- coef_logd_i["logprice"] /
  (coef_logd_i["(Intercept)"] + coef_logd_i["logprice"] * log(55))

delta_expr_logd_i <- "((logprice+cardtreat_logprice)/((Intercept)+cardtreat+logprice*log(55)+cardtreat_logprice*log(55))) - (logprice/((Intercept)+logprice*log(55)))"
dm_logd_i <- deltaMethod(coef_logd_i, delta_expr_logd_i, vcov = vcov_logd_i)
t_val_logd_i <- dm_logd_i[1, "Estimate"] / dm_logd_i[1, "SE"]
df_logd_i <- df.residual(model_logd_i)
pelas_logd_i <- 2 * (1 - pt(abs(t_val_logd_i), df_logd_i))

logdem_i <- list(model = model_logd_i,
                 vcov = vcov_logd_i,
                 coeftest = coeftest_logd_i,
                 mean_adoption_control = mean_adoption_logd_i_control,
                 epst = epst_logd_i,
                 epsc = epsc_logd_i,
                 elasticity_diff = dm_logd_i$Estimate,
                 elasticity_diff_se = dm_logd_i$SE,
                 pelas = pelas_logd_i)


#  Log Demand Using with Interaction --------------------------------------
#  Outcome: awd_use; Predictors: cardtreat, logprice, cardtreat_logprice, dpaba_d, dtanore_d

model_logdu_i <- lm(awd_use ~ cardtreat + logprice + cardtreat_logprice + dpaba_d + dtanore_d, data = rct2)
vcov_logdu_i <- vcovCL(model_logdu_i, cluster = rct2$village_id)
coeftest_logdu_i <- coeftest(model_logdu_i, vcov. = vcov_logdu_i)

for(i in c(20, 30, 40, 50, 60, 70, 80, 90)) {
  j <- log(i)
  hyp_str <- paste0("cardtreat + ", j, " * cardtreat_logprice = 0")
  lh_test <- linearHypothesis(model_logdu_i, hyp_str, vcov = vcov_logdu_i, test = "F")
  cat("For price =", i, " (usage), hypothesis p-value =", lh_test[2, "Pr(>F)"], "\n")
}

mean_awd_use_logd_i_control <- rct2[cardtreat == 0, mean(awd_use, na.rm = TRUE)]

coef_logdu_i <- coef(model_logdu_i)
epst_logdu_i <- (coef_logdu_i["logprice"] + coef_logdu_i["cardtreat_logprice"]) /
  (coef_logdu_i["(Intercept)"] + coef_logdu_i["cardtreat"] +
     coef_logdu_i["logprice"] * log(55) + coef_logdu_i["cardtreat_logprice"] * log(55))
epsc_logdu_i <- coef_logdu_i["logprice"] /
  (coef_logdu_i["(Intercept)"] + coef_logdu_i["logprice"] * log(55))

dm_logdu_i <- deltaMethod(coef_logdu_i, delta_expr_logd_i, vcov = vcov_logdu_i)
t_val_logdu_i <- dm_logdu_i[1, "Estimate"] / dm_logdu_i[1, "SE"]
df_logdu_i <- df.residual(model_logdu_i)
pelas_logdu_i <- 2 * (1 - pt(abs(t_val_logdu_i), df_logdu_i))

logdem_i_use <- list(model = model_logdu_i,
                     vcov = vcov_logdu_i,
                     coeftest = coeftest_logdu_i,
                     mean_awd_use_control = mean_awd_use_logd_i_control,
                     epst = epst_logdu_i,
                     epsc = epsc_logdu_i,
                     elasticity_diff = dm_logdu_i$Estimate,
                     elasticity_diff_se = dm_logdu_i$SE,
                     pelas = pelas_logdu_i)



# Regressions of Usage on Price (Screening Regressions) -------------------
# 9.1 With price (linear specification)
model_use_price_cond <- lm(awd_use ~ cardtreat + price + cardtreat_price + dpaba_d + dtanore_d,
                           data = rct2[adoption == 1])
vcov_use_price_cond <- vcovCL(model_use_price_cond, cluster = rct2[adoption == 1]$village_id)
coeftest_use_price_cond <- coeftest(model_use_price_cond, vcov. = vcov_use_price_cond)

mean_awd_use_cond <- rct2[adoption == 1 & cardtreat == 0, mean(awd_use, na.rm = TRUE)]

lh_test_use_price <- linearHypothesis(model_use_price_cond, "price + cardtreat_price = 0",
                                      vcov = vcov_use_price_cond, test = "F")
pval_use_price <- lh_test_use_price[2, "Pr(>F)"]

use_price_conditional <- list(model = model_use_price_cond,
                              vcov = vcov_use_price_cond,
                              coeftest = coeftest_use_price_cond,
                              mean_awd_use_control = mean_awd_use_cond,
                              pval = pval_use_price)

# 9.2 With logprice (log specification)
model_use_logprice_cond <- lm(awd_use ~ cardtreat + logprice + cardtreat_logprice + dpaba_d + dtanore_d,
                              data = rct2[adoption == 1])
vcov_use_logprice_cond <- vcovCL(model_use_logprice_cond, cluster = rct2[adoption == 1]$village_id)
coeftest_use_logprice_cond <- coeftest(model_use_logprice_cond, vcov. = vcov_use_logprice_cond)

mean_awd_use_log_cond <- rct2[adoption == 1 & cardtreat == 0, mean(awd_use, na.rm = TRUE)]

lh_test_use_logprice <- linearHypothesis(model_use_logprice_cond, "logprice + cardtreat_logprice = 0",
                                         vcov = vcov_use_logprice_cond, test = "F")
pval_use_logprice <- lh_test_use_logprice[2, "Pr(>F)"]

use_logprice_conditional <- list(model = model_use_logprice_cond,
                                 vcov = vcov_use_logprice_cond,
                                 coeftest = coeftest_use_logprice_cond,
                                 mean_awd_use_control = mean_awd_use_log_cond,
                                 pval = pval_use_logprice)

# Tables ------------------------------------------------------------------

# Create a list of all regression models to be included in the table
models <- list(
  model_ldp, model_ldp_i,  # AWD installed
  model_ldu, model_ldu_i,  # Water level
  model_logd, model_logd_i,  # Purchase AWD
  model_logdu, model_logdu_i  # Use AWD
)

# Define additional statistics for the table (p-values, elasticities, etc.)
add_stats <- list(
  c(mean_adoption_control, mean_adoption_control_i, mean_awd_use_control, mean_awd_use_control_i,
    mean_adoption_logd_control, mean_adoption_logd_i_control, mean_awd_use_logd_control, mean_awd_use_logd_i_control),
  c(epst_ldp, epst_ldp_i, epst_ldu, epst_ldu_i, epst_logd, epst_logd_i, epst_logdu, epst_logdu_i),  # Elasticity treatment
  c(epsc_ldp, epsc_ldp_i, epsc_ldu, epsc_ldu_i, epsc_logd, epsc_logd_i, epsc_logdu, epsc_logdu_i),  # Elasticity control
  c(pelas_ldp_i, pelas_ldu_i, pelas_logd_i, pelas_logdu_i),  # p-value for elasticity difference
  c(summary(model_ldp)$r.squared, summary(model_ldp_i)$r.squared,
    summary(model_ldu)$r.squared, summary(model_ldu_i)$r.squared,
    summary(model_logd)$r.squared, summary(model_logd_i)$r.squared,
    summary(model_logdu)$r.squared, summary(model_logdu_i)$r.squared)  # R-squared
)

# Column names for additional statistics
add_stat_labels <- c("Mean in control",
                     "Elasticity at price = 55 (Treatment)",
                     "Elasticity at price = 55 (Control)",
                     "p-value: Equal Elasticities",
                     "R²")

# Generate LaTeX table with Stargazer
stargazer(models, 
          type = "html",
          title = "Impacts of Hourly Irrigation Cards on Water Usage and AWD Demand",
          align = TRUE,
          se = list(coeftest_ldp[,2], coeftest_ldp_i[,2], coeftest_ldu[,2], coeftest_ldu_i[,2],
                    coeftest_logd[,2], coeftest_logd_i[,2], coeftest_logdu[,2], coeftest_logdu_i[,2]), # Standard Errors
          omit.stat = c("ser", "adj.rsq", "f"),
          add.lines = Map(c, add_stat_labels, add_stats),
          dep.var.labels.include = FALSE,
          column.labels = c("AWD Installed", "AWD Installed", 
                            "Water Level", "Water Level", 
                            "Purchase AWD", "Purchase AWD",
                            "Use AWD", "Use AWD"),
          covariate.labels = c("Card Treatment", "Pipe Price", "Pipe Price × Card Treatment"),
          notes = "Standard errors in parentheses. All regressions include Upazila fixed effects.",
          notes.align = "l",
          digits = 4,
          out = "/Users/GARSON/sciencespo/m2/development/WaterPricingIncentives/replication_R/output/table_7.html")


