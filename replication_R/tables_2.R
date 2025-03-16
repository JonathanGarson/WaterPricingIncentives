# This files replicate the findings of Ujjayant Chakravorty, Kyle Emerick, and Manzoor Dar
# For the paper "Inefficient water pricing and incentives for conservation" published in the American Economic Review in 2023
# Table 2

# Packages ----------------------------------------------------------------
library(fixest)
library(data.table)
library(dplyr)
library(modelsummary)
library(broom)
library(knitr)
library(dplyr)

# Data --------------------------------------------------------------------

rct1 = fread(get_data("input_primary/rct1_watermeasure.csv"))
rct1_baseline = fread(get_data("input_primary/rct1_baseline.csv"))

# Table 2 -----------------------------------------------------------------

## Panel A - Main Results --------------------------------------------------

rct1 = rct1[!is.na(waterunit),] #we get rid of na
set.seed(123)

# Average treatment effects
# waterlevel 
reg_waterlevel = feols(waterlevel ~ treatment |Upazila, cluster = ~village_id ,data = rct1)

# dryfield
reg_dryfield = feols(dryfield ~ treatment |Upazila, cluster = ~village_id ,data = rct1)

# heterogeneity by volumetric pricing
reg_het_water_vp = feols(waterlevel ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1)
reg_het_dryfield_vp = feols(dryfield ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1)

### splitting data into pre and post flowering period (60 days - 80  --------

#### 70 ----------------------------------------------------------------------
reg_inf70_waterlevel = feols(waterlevel ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat <= 70])
reg_sup70_waterlevel = feols(waterlevel ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat > 70])
reg_inf70_waterlevel_baseline = feols(waterlevel ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat <= 70])
reg_sup70_waterlevel_baseline = feols(waterlevel ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat > 70])

reg_inf70_dryfield = feols(dryfield ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat <= 70])
reg_sup70_dryfield = feols(dryfield ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat > 70])
reg_inf70_dryfield_baseline = feols(dryfield ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat <= 70])
reg_sup70_dryfield_baseline = feols(dryfield ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat > 70])

#### 60 ----------------------------------------------------------------------
# reg_inf60_waterlevel = feols(waterlevel ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat <= 60]) #significant results
# reg_sup60_waterlevel = feols(waterlevel ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat > 60])
# reg_inf60_waterlevel_baseline = feols(waterlevel ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat <= 60]) #significant results
# reg_sup60_waterlevel_baseline = feols(waterlevel ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat > 60])
# 
# reg_inf60_dryfield = feols(dryfield ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat <= 60]) # significant results
# reg_sup60_dryfield = feols(dryfield ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat > 60])
# reg_inf60_dryfield_baseline = feols(dryfield ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat <= 60]) #significant results
# reg_sup60_dryfield_baseline = feols(dryfield ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat > 60])

#### 80 ----------------------------------------------------------------------
# reg_inf80_waterlevel = feols(waterlevel ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat <= 80])
# reg_sup80_waterlevel = feols(waterlevel ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat > 80])
# reg_inf80_waterlevel_baseline = feols(waterlevel ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat <= 80]) 
# reg_sup80_waterlevel_baseline = feols(waterlevel ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat > 80])
# 
# reg_inf80_dryfield = feols(dryfield ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat <= 80]) 
# reg_sup80_dryfield = feols(dryfield ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat > 80])
# reg_inf80_dryfield_baseline = feols(dryfield ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat <= 80])
# reg_sup80_dryfield_baseline = feols(dryfield ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat > 80])


## Regression without Upazila fixed effects --------------------------------

# reg_waterlevel_no_fe = feols(waterlevel ~ anymarginal, cluster = ~village_id, data = rct1)
# reg_dryfield_no_fe = feols(dryfield ~ anymarginal, cluster = ~village_id, data = rct1)
# 
# reg_waterlevel_no_fe_interac = feols(waterlevel ~ anymarginal*treatment , cluster = ~village_id, data = rct1)
# reg_dryfield_no_fe_interac = feols(dryfield ~ anymarginal*treatment , cluster = ~village_id, data = rct1)

## Panel B - Only in between Upazila variation in volumetric pricing -----------------
rct1_baseline[, upamean_marg := mean(anymarginal), by = upazila]
rct1_baseline[, t_upamean_marg := treatment*upamean_marg, by = upazila]
upamean_db = rct1_baseline[, .(farmer_id, upamean_marg, t_upamean_marg)]
rct1 = merge(rct1, upamean_db, by = "farmer_id")


### Overall -----------------------------------------------------------------
reg_waterlevel_overall_bw = feols(waterlevel ~ treatment + t_upamean_marg, fixef = c("Upazila"), cluster = ~village_id, data = rct1)

### <70 days ---------------------------------------------------------------
reg_waterlevel_inf70_bw = feols(waterlevel ~ treatment + t_upamean_marg, fixef = c("Upazila"), cluster = ~village_id, data = rct1[dat <= 70])

### >70 days ---------------------------------------------------------------
reg_waterlevel_sup70_bw = feols(waterlevel ~ treatment + t_upamean_marg, fixef = c("Upazila"), cluster = ~village_id, data = rct1[dat > 70])

## Panel C - Within Upazila variation --------------------------------------
### Overall -----------------------------------------------------------------
# reg_waterlevel_overall_wth = feols(waterlevel ~ treatment*anymarginal | Upazila^treatment, cluster = ~village_id, data = rct1) # this brings colinearity with the treatment due to the FE
# We need changes to adapt it
rct1[, treatment_within := treatment - mean(treatment, na.rm = TRUE), by = Upazila]
reg_waterlevel_within = feols(waterlevel ~ treatment_within*anymarginal | Upazila, cluster = ~village_id, data = rct1)

### <70 days ---------------------------------------------------------------
reg_waterlevel_inf70_wth = feols(waterlevel ~ treatment_within*anymarginal, fixef = c("Upazila"), cluster = ~village_id, data = rct1[dat <= 70])

### >70 days ---------------------------------------------------------------
reg_waterlevel_sup70_wth = feols(waterlevel ~ treatment_within*anymarginal, fixef = c("Upazila"), cluster = ~village_id, data = rct1[dat > 70])

# Table Generation --------------------------------------------------------
# NOT WORKING YET
models <- list(reg_waterlevel, reg_dryfield ,reg_waterlevel_overall_bw, reg_sup70_dryfield, reg_sup70_dryfield,reg_sup70_dryfield)  # Replace with your actual models

# Create custom additional statistics for the table
add_stats <- list(
  c(control_mean1, control_mean2, control_mean3, control_mean4, control_mean5, control_mean6),
  c(num_observations1, num_observations2, num_observations3, num_observations4, num_observations5, num_observations6),
  c(p_value1, p_value2, p_value3, p_value4, p_value5, p_value6)
)

add_stat_labels <- c("Control Mean", "Number of Observations", "p-value: Treat + Treat × Volumetric")

# Save the LaTeX table to a file

stargazer(models, 
          type = "html",
          title = "Effects of Conservation Technology on Water Levels",
          align = TRUE,
          se = list(se_model1, se_model2, se_model3, se_model4, se_model5, se_model6),
          omit.stat = c("ser", "adj.rsq", "f"),
          add.lines = Map(c, add_stat_labels, add_stats),
          dep.var.labels.include = FALSE,
          column.labels = c("Overall", "0-70 days after planting", "70+ days after planting"),
          covariate.labels = c("Treatment", "Treatment × Volumetric Pricing", "Volumetric Pricing"),
          notes = "Standard errors in parentheses. Panels indicate variations across groups.",
          notes.align = "l",
          digits = 4,
          out = "/Users/GARSON/sciencespo/m2/development/WaterPricingIncentives/replication_R/output/table_2.html")

