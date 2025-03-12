# This files replicate the findings of Ujjayant Chakravorty, Kyle Emerick, and Manzoor Dar
# For the paper "Inefficient water pricing and incentives for conservation" published in the American Economic Review in 2023

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

# Panel A - Main Results
models_panel_a <- list(
  "Treatment" = reg_waterlevel,
  "Treatment × Volumetric Pricing" = reg_het_water_vp,
  "< 70 Days" = reg_inf70_waterlevel,
  "> 70 Days" = reg_sup70_waterlevel
)

# Panel B - Between-Upazila Variation
models_panel_b <- list(
  "Treatment" = reg_waterlevel_overall_bw,
  "< 70 Days" = reg_waterlevel_inf70_bw,
  "> 70 Days" = reg_waterlevel_sup70_bw
)

# Panel C - Within-Upazila Variation
models_panel_c <- list(
  "Treatment" = reg_waterlevel_within,
  "< 70 Days" = reg_waterlevel_inf70_wth,
  "> 70 Days" = reg_waterlevel_sup70_wth
)

# Create a custom title for each panel
table_panel_a <- modelsummary(models_panel_a, title = "Panel A - Main Results", output = "markdown")
table_panel_b <- modelsummary(models_panel_b, title = "Panel B - Between-Upazila Variation", output = "markdown")
table_panel_c <- modelsummary(models_panel_c, title = "Panel C - Within-Upazila Variation", output = "markdown")

# Define a helper function to extract a coefficient estimate and standard error
extract_coef <- function(model, term_pattern) {
  # Use broom::tidy to extract coefficient info
  tb <- broom::tidy(model)
  # Use grepl on the 'term' column so that we can match the desired coefficient
  row <- tb[grepl(term_pattern, tb$term), ]
  if (nrow(row) == 0) {
    return(NA)
  }
  # Format the coefficient and standard error (adjust number formatting as needed)
  sprintf("%.3f (%.3f)", row$estimate[1], row$std.error[1])
}

# Build the results table by row.
# Note: Adjust the term patterns if needed depending on how fixest names them.
results_df <- data.frame(
  Panel = c("Panel A: Treatment",
            "Panel A: Treatment × anymarginal",
            "Panel B: Treatment",
            "Panel C: Treatment"),
  
  Overall = c(
    extract_coef(reg_waterlevel, "^treatment$"),
    extract_coef(reg_het_water_vp, "treatment:anymarginal"),
    extract_coef(reg_waterlevel_overall_bw, "^treatment$"),
    extract_coef(reg_waterlevel_within, "^treatment_within$")
  ),
  
  `0-70 Days` = c(
    extract_coef(reg_inf70_waterlevel_baseline, "^treatment$"),
    extract_coef(reg_inf70_waterlevel, "treatment:anymarginal"),
    extract_coef(reg_waterlevel_inf70_bw, "^treatment$"),
    extract_coef(reg_waterlevel_inf70_wth, "^treatment_within$")
  ),
  
  `70+ Days` = c(
    extract_coef(reg_sup70_waterlevel_baseline, "^treatment$"),
    extract_coef(reg_sup70_waterlevel, "treatment:anymarginal"),
    extract_coef(reg_waterlevel_sup70_bw, "^treatment$"),
    extract_coef(reg_waterlevel_sup70_wth, "^treatment_within$")
  ),
  
  stringsAsFactors = FALSE
)

# Display the table as HTML
kable(results_df, format = "latex", caption = "Compact Regression Table") %>% 
  kable_save(output_folder("table2.tex"))
