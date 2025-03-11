# This files replicate the findings of Ujjayant Chakravorty, Kyle Emerick, and Manzoor Dar
# For the paper "Inefficient water pricing and incentives for conservation" published in the American Economic Review in 2023

# Packages ----------------------------------------------------------------
library(fixest)
library(data.table)
library(modelsummary)

# Data --------------------------------------------------------------------

rct1 = fread(get_data("input_primary/rct1_watermeasure.csv"))

# Table 1 -----------------------------------------------------------------

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

## splitting data into pre and post flowering period (60 days - 80  --------

### 60 ----------------------------------------------------------------------
reg_inf60_waterlevel = feols(waterlevel ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat <= 60]) #significant results
reg_sup60_waterlevel = feols(waterlevel ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat > 60])
reg_inf60_waterlevel_baseline = feols(waterlevel ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat <= 60]) #significant results
reg_sup60_waterlevel_baseline = feols(waterlevel ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat > 60])


reg_inf60_dryfield = feols(dryfield ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat <= 60]) # significant results
reg_sup60_dryfield = feols(dryfield ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat > 60])
reg_inf60_dryfield_baseline = feols(dryfield ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat <= 60]) #significant results
reg_sup60_dryfield_baseline = feols(dryfield ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat > 60])

### 70 ----------------------------------------------------------------------
reg_inf70_waterlevel = feols(waterlevel ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat <= 70]) 
reg_sup70_waterlevel = feols(waterlevel ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat > 70])
reg_inf70_waterlevel_baseline = feols(waterlevel ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat <= 70]) 
reg_sup70_waterlevel_baseline = feols(waterlevel ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat > 70])

reg_inf70_dryfield = feols(dryfield ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat <= 70]) 
reg_sup70_dryfield = feols(dryfield ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat > 70])
reg_inf70_dryfield_baseline = feols(dryfield ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat <= 70]) 
reg_sup70_dryfield_baseline = feols(dryfield ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat > 70])

### 80 ----------------------------------------------------------------------
reg_inf80_waterlevel = feols(waterlevel ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat <= 80])
reg_sup80_waterlevel = feols(waterlevel ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat > 80])
reg_inf80_waterlevel_baseline = feols(waterlevel ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat <= 80]) 
reg_sup80_waterlevel_baseline = feols(waterlevel ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat > 80])

reg_inf80_dryfield = feols(dryfield ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat <= 80]) 
reg_sup80_dryfield = feols(dryfield ~ treatment*anymarginal | Upazila, cluster = ~village_id, data = rct1[dat > 80])
reg_inf80_dryfield_baseline = feols(dryfield ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat <= 80])
reg_sup80_dryfield_baseline = feols(dryfield ~ treatment | Upazila, cluster = ~village_id, data = rct1[dat > 80])
