*******************************************************************************************************************************
/*Replicate results for "Inefficient water pricing and incentives for conservation*/
*Code to Replicate results for August 2021 Version of "Inefficient Water Prices and Incentives for Conservation" 
*Authors: Ujjayant Chakravorty, Kyle Emerick, and Manzoor Dar 
*The comments in the code give the exact name of the table or figure
*To locate code that replicates the table/figure, just search the do file using the exact table/figure name OR the table/figure number
*Stata Version: 17
*Contact Kyle Emerick (kyle.emerick@tufts.edu) with any questions
*****************************************************************************************************************

********************SET PATHS***********************************
clear all
set more off, permanently

*Set directory where the "repl_files_ineff_water_pricing" folder is located. 

cd "/Users/kemeri01/dropbox/bangladesh awd/AEJApp Submission/repl_files_ineff_water_pricing" 
*cd "C:/Users/letid/Dropbox/bangladesh awd/AEJApp Submission/repl_files_ineff_water_pricing"

*Location to store results, figures and tables:
global latex "results/"

/****************************************************************
Constructing baseline_covariates dataset (derived)
*****************************************************************/
{
use "data/input_primary/rct1_baseline.dta", clear
*calculate boro rice revenue per acre
foreach i in 1 2 { 
qui sum boro_price_`i'
gen boro_revperacre_`i' = 3*(boro_price_`i'*boro_yield_`i')
replace boro_revperacre_`i' = 3*(r(mean)*boro_yield_`i') if boro_revperacre_`i'==. /*mean fill*/
replace boro_revperacre_`i'=0 if boro_revperacre_`i'==.
}
*revenue per acre from other two seasons
foreach i in 1 2 { 
foreach j in winter aman { 
foreach k in ov "" {
qui sum `j'_price`k'_`i'
gen `j'_`k'peracre_`i' = 3*(`j'_price`k'_`i'*`j'_yield`k'_`i')
replace `j'_`k'peracre_`i' = 3*(r(mean)*`j'_yield`k'_`i') if `j'_`k'peracre_`i'==. /*mean fill*/
replace `j'_`k'peracre_`i'=0 if `j'_`k'peracre_`i'==. 
}
}
}
order winter_ovper* winter_yield* winter_* 
*boro and aman rice costs 
foreach j in boro aman { 
foreach i in 1 { 
egen `j'cost_`i' = rowtotal(`j'_rvcostplant_`i' `j'_rvcostweed_`i' `j'_rvcosthar_`i' `j'_rvcostfert_`i' `j'_rvcostherb_`i')
replace `j'cost_`i' = (`j'cost_`i' / area`i'_bg)*3
}
}
*boro water costs 
gen boro_watercost_1 = 0 
replace boro_watercost_1 = boro_fixed_1 if boro_how_1==1
replace boro_watercost_1 = (boro_pb_1)*area1_bg if boro_how_1==2
replace boro_watercost_1 = boro_rvirrig_1*boro_hourly_1*0.5 if boro_how_1==3 /*30 minutes per irrigation*/
replace boro_watercost_1 = boro_rvirrig_1*boro_diesel_1 if boro_how_1==4/*1 litres per irrigation*/
replace boro_watercost_1 = boro_fixed_1 + boro_rvirrig_1*boro_diesel_1 if boro_how_1==6
replace boro_watercost_1 = (area1_bg*boro_pb_1) + boro_rvirrig_1*boro_diesel_1 if boro_how_1==8
gen boro_watercostpacre_1 = 3*(boro_watercost_1 / area1_bg)
*total costs per acre 
gen boro_tcost_1 = boro_watercostpacre_1+borocost_1
*generate other variables 
gen hhsize = nadults+nyoung
gen landhold = totarea_bg/3
gen knowawd = (heardawd=="yes")
gen renter=(inlist(ten1,2,3))
gen area1_acre = area1_bg/3
*generate asset ownership variables 
foreach i in 1 2 3 4 5 6 7 8 9 { 
gen assetheld_`i' = (regex(assets,"`i'")>0 & regex(assets,"10")==0)
}
*save baseline covariates for use later in the replication file
keep farmer_id age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre anymarginal ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1
save "data/derived/baseline_covariates.dta", replace 
}

/**********************************
		TABLE 1 AND TABLE A1 
***********************************/
{
******************************************************************************************************************RCT 1 BASELINE BALANCE AND SUMMARY STATISTICS
*SUMMARY STATISTICS AND COVARIATE BALANCE BY TREATMENT (TABLE 1) 
*SUMMARY STATISTICS AND COVARIATE BALANCE BY TREATMENT FOR PLACES WITH VOLUMETRIC WATER PRICING (TABLE A1)  
******************************************************************************************************************
use "data/input_primary/rct1_baseline.dta", clear
*calculate boro rice revenue per acre
foreach i in 1 2 { 
qui sum boro_price_`i'
gen boro_revperacre_`i' = 3*(boro_price_`i'*boro_yield_`i')
replace boro_revperacre_`i' = 3*(r(mean)*boro_yield_`i') if boro_revperacre_`i'==. /*mean fill*/
replace boro_revperacre_`i'=0 if boro_revperacre_`i'==.
}
*revenue per acre from other two seasons
foreach i in 1 2 { 
foreach j in winter aman { 
foreach k in ov "" {
qui sum `j'_price`k'_`i'
gen `j'_`k'peracre_`i' = 3*(`j'_price`k'_`i'*`j'_yield`k'_`i')
replace `j'_`k'peracre_`i' = 3*(r(mean)*`j'_yield`k'_`i') if `j'_`k'peracre_`i'==. /*mean fill*/
replace `j'_`k'peracre_`i'=0 if `j'_`k'peracre_`i'==. 
}
}
}
order winter_ovper* winter_yield* winter_* 
*boro and aman rice costs 
foreach j in boro aman { 
foreach i in 1 { 
egen `j'cost_`i' = rowtotal(`j'_rvcostplant_`i' `j'_rvcostweed_`i' `j'_rvcosthar_`i' `j'_rvcostfert_`i' `j'_rvcostherb_`i')
replace `j'cost_`i' = (`j'cost_`i' / area`i'_bg)*3
}
}
*boro water costs 
gen boro_watercost_1 = 0 
replace boro_watercost_1 = boro_fixed_1 if boro_how_1==1
replace boro_watercost_1 = (boro_pb_1)*area1_bg if boro_how_1==2
replace boro_watercost_1 = boro_rvirrig_1*boro_hourly_1*0.5 if boro_how_1==3 /*30 minutes per irrigation*/
replace boro_watercost_1 = boro_rvirrig_1*boro_diesel_1 if boro_how_1==4/*1 litres per irrigation*/
replace boro_watercost_1 = boro_fixed_1 + boro_rvirrig_1*boro_diesel_1 if boro_how_1==6
replace boro_watercost_1 = (area1_bg*boro_pb_1) + boro_rvirrig_1*boro_diesel_1 if boro_how_1==8
gen boro_watercostpacre_1 = 3*(boro_watercost_1 / area1_bg)
*total costs per acre 
gen boro_tcost_1 = boro_watercostpacre_1+borocost_1
*generate other variables 
gen hhsize = nadults+nyoung
gen landhold = totarea_bg/3
gen knowawd = (heardawd=="yes")
gen renter=(inlist(ten1,2,3))
gen area1_acre = area1_bg/3
*generate asset ownership variables 
foreach i in 1 2 3 4 5 6 7 8 9 { 
gen assetheld_`i' = (regex(assets,"`i'")>0 & regex(assets,"10")==0)
}

*Balance tables: 
*set locals for table 
loc nvar=19
loc vlist "age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre anymarginal ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1" 
*set up vectors for results 
foreach k in c1 c2 p c1v c2v pv s1 s2 s1v s2v {  
mat `k'=J(1,`nvar',0)
mat colnames `k' = `vlist'
}
*run regressions and store means, and p values in those vectors 
loc k=1
foreach var in `vlist' { 
xi: reg `var' treatment i.Upazila, vce(cl village_id) 
test treatment 
mat p[1,`k']=r(p)
sum `var' if treatment==0  & e(sample)==1 
mat c1[1,`k']=r(mean)
mat s1[1,`k']=r(sd)
sum `var' if treatment==1  & e(sample)==1 
mat c2[1,`k']=r(mean)
mat s2[1,`k']=r(sd)

xi: reg `var' treatment i.Upazila if anymarginal==1, vce(cl village_id) 
test treatment 
mat pv[1,`k']=r(p)
sum `var' if treatment==0  & e(sample)==1 
mat c1v[1,`k']=r(mean)
mat s1v[1,`k']=r(sd)
sum `var' if treatment==1  & e(sample)==1 
mat c2v[1,`k']=r(mean)
mat s2v[1,`k']=r(sd)

loc k = `k'+1 /*add to counter*/
}
*run espost to make table look better
foreach j in c1 c2 p c1v c2v pv {
estpost ttest `vlist', by(treatment)
matrix rename `j' kyl, replace
estadd matrix kyl, replace /*overwrite existing e(p)*/ 
estimates store col_`j'
}
foreach k in 1 2 1v 2v { 
estimates restore col_c`k' 
matrix rename s`k' sdev, replace 
estadd matrix sdev, replace 
estimates store col_c`k'
}
*label variables better
label var age "Age" 
label var edu "Years Education" 
label var hhsize "Household Size" 
label var livestock "Number Livestock Owned" 
label var landhold "Landholdings in Acres" 
label var knowawd "Heard of AWD?" 
label var renter "Plot is Rented or Sharecropped" 
label var area1_acre "Area in Acres" 
label var anymarginal "Volumetric Water Price" 
label var ncrop1 "Number Crops Grown" 
label var rice_rice_sys "Rice-Rice Cropping System" 
label var boro_rvirrig_1 "Number Irrigations in Boro" 
label var boro_revperacre_1 "Revenue per Acre in Boro" 
label var boro_tcost_1 "Cost per Acre in Boro" 
label var boro_watercostpacre_1 "Water Cost per Acre in Boro" 
label var aman_peracre_1 "Revenue per Acre in Aman"
label var assetheld_1 "Owns Television"
label var assetheld_5 "Owns Refrigerator"
label var assetheld_9 "Owns Irrigation Shallow Tubewell"
*export two tables to tex format 
*Table 1 
esttab col_c1 col_c2 col_p using "${latex}table1_baseline_balance.tex", cells("kyl(fmt(a3))" "sdev(par)") notes replace ///
noobs mlabels("Control" "Treatment" "p-value") nonumbers collabels(,none) label alignment(ccc) ///
gaps width(\hsize) mgroups("\multicolumn{2}{c}{Means} \\ \cline{2-3} ", pattern(1 1 0)) ///
refcat(age "\emph{\underline{Panel A: Household Characteristics}}" renter "\emph{\underline{Panel B: Characteristics of Study Plot}}", nolabel)
*Table A1
esttab col_c1v col_c2v col_pv using "${latex}tableA1_baseline_balance_vol.tex", cells("kyl(fmt(a3))" "sdev(par)") notes replace ///
noobs mlabels("Control" "Treatment" "p-value") nonumbers collabels(,none) label alignment(ccc) ///
gaps width(\hsize) wrap drop(anymarginal) mgroups("\multicolumn{2}{c}{Means} \\ \cline{2-3} ", pattern(1 1 0)) ///
refcat(age "\emph{\underline{Panel A: Household Characteristics}}" renter "\emph{\underline{Panel B: Characteristics of Study Plot}}", nolabel)
}


/*********************************
	TABLE 2 - TABLE A2 - TABLE A3 - TABLE A4 - TABLE A5 - TABLE A6 - TABLE A7 
**********************************/
{
*****************************************************************************************************************
*RCT 1: WATER USE FROM OBJECTIVE MEASUREMENTS 
******************************************************************************************************************
use "data/input_primary/rct1_watermeasure.dta", clear 
keep if waterunit != . /*keep only completed measurements*/
set seed 76561

*average treatment effects 
xi: reg waterlevel treatment i.Upazila, vce(cl village_id) 
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel

xi: reg dryfield treatment i.Upazila, vce(cl village_id) 
sum dryfield if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store dfield

*heterogeneity by volumetric pricing 
gen t_marg = treatment*anymarginal
xi: reg waterlevel treatment anymarginal t_marg i.Upazila, vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_het
xi: reg waterlevel treatment anymarginal t_marg i.Upazila, vce(cl upazila)
boottest t_marg
matrix pvalues=r(p)
mat colnames pvalues = t_marg
estimates restore wlevel_het
estadd matrix pvalues = pvalues
estimates store wlevel_het_wc  


xi: reg dryfield treatment anymarginal t_marg i.Upazila, vce(cl village_id) 
test treatment+t_marg=0 
estadd sca phet=r(p)
sum dryfield if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store dfield_het
xi: reg dryfield treatment anymarginal t_marg i.Upazila, vce(cl upazila) 
boottest t_marg
matrix pvalues=r(p)
mat colnames pvalues = t_marg
estimates restore dfield_het
estadd matrix pvalues = pvalues
estimates store dfield_het_wc  

*splitting data into pre and post flowering period (60 days-80 days)
foreach j in 60 70 80 { 
xi: reg waterlevel treatment anymarginal t_marg i.Upazila if dat<=`j', vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_preflower`j'
xi: reg waterlevel treatment anymarginal t_marg i.Upazila if dat<=`j', vce(cl upazila)
boottest t_marg
matrix pvalues=r(p)
mat colnames pvalues = t_marg
estimates restore wlevel_preflower`j'
estadd matrix pvalues = pvalues
estimates store wlevel_preflower`j'_wc  

xi: reg waterlevel treatment anymarginal t_marg i.Upazila if dat>`j', vce(cl village_id) 
test treatment+t_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_postflower`j'
xi: reg waterlevel treatment anymarginal t_marg i.Upazila if dat>`j', vce(cl upazila) 
boottest t_marg
matrix pvalues=r(p)
mat colnames pvalues = t_marg
estimates restore wlevel_postflower`j'
estadd matrix pvalues = pvalues
estimates store wlevel_postflower`j'_wc 


xi: reg dryfield treatment anymarginal t_marg i.Upazila if dat<=`j', vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum dryfield if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store dfield_preflower`j'
xi: reg dryfield treatment anymarginal t_marg i.Upazila if dat<=`j', vce(cl upazila)
boottest t_marg
matrix pvalues=r(p)
mat colnames pvalues = t_marg
estimates restore dfield_preflower`j'
estadd matrix pvalues = pvalues
estimates store dfield_preflower`j'_wc 


xi: reg dryfield treatment anymarginal t_marg i.Upazila if dat>`j', vce(cl village_id) 
test treatment+t_marg=0 
estadd sca phet=r(p)
sum dryfield if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store dfield_postflower`j'
xi: reg dryfield treatment anymarginal t_marg i.Upazila if dat>`j', vce(cl upazila) 
boottest t_marg
matrix pvalues=r(p)
mat colnames pvalues = t_marg
estimates restore dfield_postflower`j'
estadd matrix pvalues = pvalues
estimates store dfield_postflower`j'_wc


xi: reg waterlevel treatment i.Upazila if dat<=`j', vce(cl village_id)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_preflowern`j'

xi: reg waterlevel treatment i.Upazila if dat>`j', vce(cl village_id)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_postflowern`j'

xi: reg dryfield treatment i.Upazila if dat<=`j', vce(cl village_id)
sum dryfield if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store dfield_preflowern`j'

xi: reg dryfield treatment i.Upazila if dat>`j', vce(cl village_id)
sum dryfield if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store dfield_postflowern`j'
}


*Regressions without strata fixed effects
xi: reg dryfield anymarginal, vce(cl village_id)
sum dryfield if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store dfield_nsfe

xi: reg waterlevel anymarginal, vce(cl village_id)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_nsfe

xi: reg dryfield treatment anymarginal t_marg, vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum dryfield if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store dfield_i_nsfe

xi: reg waterlevel treatment anymarginal t_marg, vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_i_nsfe

*REGRESSIONS USING ONLY BETWEEN UPAZILA VARIATION IN VOLUMETRIC PRICING
*create upazila-level mean
use "data/input_primary/rct1_baseline.dta", clear
egen upamean_marg = mean(anymarginal), by(upazila) 
gen t_upamean_marg = treatment*upamean_marg
keep farmer_id upamean_marg t_upamean_marg
tempfile upamean 
save "`upamean'", replace
*water measurements data
use "data/input_primary/rct1_watermeasure.dta", clear 
keep if waterunit != . /*keep only completed measurements*/
merge n:1 farmer_id using "`upamean'" 
drop if _merge==2
drop _merge
gen t_marg = treatment*anymarginal
*****regressions***********
*0-70 days
xi: reg waterlevel treatment t_upamean_marg i.Upazila if dat<=70, vce(cl village_id)
test treatment+t_upamean_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_upa70
xi: reg waterlevel treatment t_upamean_marg i.Upazila if dat<=70, vce(cl village_id)
boottest t_upamean_marg
matrix pvalues=r(p)
mat colnames pvalues = t_upamean_marg
estimates restore wlevel_upa70
estadd matrix pvalues = pvalues
estimates store wlevel_upa70_wc   

*more than 70 days
xi: reg waterlevel treatment t_upamean_marg i.Upazila if dat>70, vce(cl village_id)
test treatment+t_upamean_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_upa70plus
xi: reg waterlevel treatment t_upamean_marg i.Upazila if dat>70, vce(cl village_id)
boottest t_upamean_marg
matrix pvalues=r(p)
mat colnames pvalues = t_upamean_marg
estimates restore wlevel_upa70plus
estadd matrix pvalues = pvalues
estimates store wlevel_upa70plus_wc   


*overall
xi: reg waterlevel treatment t_upamean_marg i.Upazila, vce(cl village_id)
test treatment+t_upamean_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_upa
xi: reg waterlevel treatment t_upamean_marg i.Upazila, vce(cl upazila)
boottest t_upamean_marg
matrix pvalues=r(p)
mat colnames pvalues = t_upamean_marg
estimates restore wlevel_upa
estadd matrix pvalues = pvalues
estimates store wlevel_upa_wc   

*REGRESSIONS USING ONLY WITHIN UPAZILA VARIATION IN VOLUMETRIC PRICING
*0-70 days
xi: reg waterlevel anymarginal t_marg i.Upazila*treatment if dat<=70, vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_ufe70
xi: reg waterlevel anymarginal t_marg i.Upazila*treatment if dat<=70, vce(cl upazila)
boottest t_marg
matrix pvalues=r(p)
mat colnames pvalues = t_marg
estimates restore wlevel_ufe70
estadd matrix pvalues = pvalues
estimates store wlevel_ufe70_wc

*more than 70 days
xi: reg waterlevel anymarginal t_marg i.Upazila*treatment if dat>70, vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_ufe70plus
xi: reg waterlevel anymarginal t_marg i.Upazila*treatment if dat>70, vce(cl upazila)
boottest t_marg
matrix pvalues=r(p)
mat colnames pvalues = t_marg
estimates restore wlevel_ufe70plus
estadd matrix pvalues = pvalues
estimates store wlevel_ufe70plus_wc

*overall
xi: reg waterlevel anymarginal t_marg i.Upazila*treatment, vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_ufe
xi: reg waterlevel anymarginal t_marg i.Upazila*treatment, vce(cl upazila)
boottest t_marg
matrix pvalues=r(p)
mat colnames pvalues = t_marg
estimates restore wlevel_ufe
estadd matrix pvalues = pvalues
estimates store wlevel_ufe_wc

*OUTPUT REGRESSION TABLES 
label var anymarginal "Volumetric Pricing" 
label var treatment "Treatment" 
label var t_marg "Treatment * Volumetric Pricing" 
label var t_upamean_marg "Treatment * Volumetric Pricing Upazila Mean"

*Make table: TABLE 2 EFFECTS OF CONSERVATION TECHNOLOGY ON WATER LEVELS
*panel A
esttab wlevel wlevel_het_wc wlevel_preflowern70 wlevel_preflower70_wc wlevel_postflowern70 wlevel_postflower70_wc using "${latex}table2_waterlevel_multipanel.tex", cells(b(fmt(a3) label(" ") star pvalue(p)) se(fmt(a3) label(" ") par) pvalues(fmt(a3) label(" ") par([ ]))) ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label eqlabels(none) collabels(none) ///
scalars("phet p-Value: Treat+Treat*Volumetric" "ym Control Mean" "N Number of Obs.") sfmt(%10.3f %10.2f %10.0f) ///
nomtitles align(cccccc) width(\hsize) keep(treatment t_marg anymarginal) order(treatment t_marg anymarginal) nonotes gaps   ///
refcat(treatment "\underline{\emph{Panel A: Main Results}}", nolabel) plain fragment mgroups("\multicolumn{2}{c}{Overall} & \multicolumn{2}{c}{0-70 Days} & \multicolumn{2}{c}{70+ Days} \\ & \multicolumn{2}{c}{} & \multicolumn{2}{c}{After Planting} & \multicolumn{2}{c}{After Planting} \\ \cline{2-3} \cline{4-5} \cline{6-7}", pattern(1 0 1 0 1 0)) ///
prehead("\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular*}{\hsize}{@{\hskip\tabcolsep\extracolsep\fill}l*{6}{cccccc}} \hline\hline")

*panel B
esttab wlevel_upa_wc wlevel_upa70_wc wlevel_upa70plus_wc using "${latex}table2_waterlevel_multipanel.tex", cells(b(fmt(a3) label(" ") star pvalue(p)) se(fmt(a3) label(" ") par) pvalues(fmt(a3) label(" ") par([ ]))) ///
b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label eqlabels(none) collabels(none) ///
scalars("phet p-Value: Treat+Treat*Volumetric") sfmt(%10.3f) extracols(1 2 3) ///
nomtitles align(cccccc) width(\hsize) keep(treatment t_upamean_marg) order(treatment t_upamean_marg) nonotes gaps plain refcat(treatment "\underline{\emph{Panel B: Between-Upazila Variation}}", nolabel) fragment append

*panel C
esttab wlevel_ufe_wc wlevel_ufe70_wc wlevel_ufe70plus_wc using "${latex}table2_waterlevel_multipanel.tex", cells(b(fmt(a3) label(" ") star pvalue(p)) se(fmt(a3) label(" ") par) pvalues(fmt(a3) label(" ") par([ ]))) ///
b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label eqlabels(none) collabels(none) ///
scalars("phet p-Value: Treat+Treat*Volumetric") sfmt(%10.3f) extracols(1 2 3) /// 
nomtitles align(cccccc) width(\hsize) keep(treatment t_marg) order(treatment t_marg) nonotes gaps plain refcat(treatment "\underline{\emph{Panel C: Within-Upazila Variation}}", nolabel) fragment postfoot("\hline\hline \end{tabular*}") append

*Make Table A2. Effects on conservation technology when omitting strata fixed effects
esttab wlevel_nsfe dfield_nsfe wlevel_i_nsfe dfield_i_nsfe  using "${latex}tableA2_waterlevel_nostrataFE.tex", ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control" "phet p-Value: Treat+Treat*Volumetric"  "N Number of Observations" "r2 R squared") sfmt(%10.2f %10.3f %10.0f %10.3f) ///
mtitles ("Water Level" "Dry" "Water Level" "Dry") align(cccc) order(anymarginal treatment t_marg) nonotes gaps drop(_cons)

*Make table: Appendix Table A3 Effects of conservation technology on the probability of fields being dried
esttab dfield dfield_het_wc dfield_preflowern70 dfield_preflower70_wc dfield_postflowern70 dfield_postflower70_wc using "${latex}tableA3_dryfield.tex", cells(b(fmt(a3) label(" ") star pvalue(p)) se(fmt(a3) label(" ") par) pvalues(fmt(a3) label(" ") par([ ]))) ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label eqlabels(none) collabels(none) ///
scalars("ym Mean in Control" "phet p-Value: Treat+Treat*Volumetric"  "N Number of Observations" "r2 R squared") sfmt(%10.2f %10.3f %10.0f %10.3f) ///
nomtitles align(cccccc) width(\hsize) order(treatment t_marg anymarginal) nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = _IUpa*") mgroups("\multicolumn{2}{c}{Overall} & \multicolumn{2}{c}{0-70 Days} & \multicolumn{2}{c}{70+ Days} \\ & \multicolumn{2}{c}{} & \multicolumn{2}{c}{After Planting} & \multicolumn{2}{c}{After Planting} \\ \cline{2-3} \cline{4-5} \cline{6-7}", pattern(1 0 1 0 1 0))


*Separate effects by time of growing season, 0-60 and 60+ days after planting (Table A4) 
esttab wlevel_preflowern60 dfield_preflowern60 wlevel_postflowern60 dfield_postflowern60 using "${latex}tableA4_waterlevel_timesplitn60.tex", ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control"  "N Number of Observations" "r2 R squared") sfmt(%10.2f %10.0f %10.3f) ///
mtitles ("Water Level" "Dry" "Water Level" "Dry") align(cccc) width(\hsize) order(treatment) nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = _IUpa*") ///
mgroups("\multicolumn{2}{c}{0-60 Days After Planting} & \multicolumn{2}{c}{60+ Days After Planting} \\ \cline{2-3} \cline{4-5}", pattern(1 0 1 0))

*Separate effects by time of growing season, 0-80 and 80+ days after planting (Table A5)
esttab wlevel_preflowern80 dfield_preflowern80 wlevel_postflowern80 dfield_postflowern80 using "${latex}tableA5_waterlevel_timesplitn80.tex", ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control"  "N Number of Observations" "r2 R squared") sfmt(%10.2f %10.0f %10.3f) ///
mtitles ("Water Level" "Dry" "Water Level" "Dry") align(cccc) width(\hsize) order(treatment) nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = _IUpa*") ///
mgroups("\multicolumn{2}{c}{0-80 Days After Planting} & \multicolumn{2}{c}{80+ Days After Planting} \\ \cline{2-3} \cline{4-5}", pattern(1 0 1 0))

*Heterogeneous effects by first 60 days of the growing season (Table A6) 
esttab wlevel_preflower60 dfield_preflower60 wlevel_postflower60 dfield_postflower60 using "${latex}tableA6_waterlevel_timesplit60.tex", ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control" "phet p-Value: Treat+Treat*Volumetric"  "N Number of Observations" "r2 R squared") sfmt(%10.2f %10.3f %10.0f %10.3f) ///
mtitles ("Water Level" "Dry" "Water Level" "Dry") align(cccc) width(\hsize) order(treatment t_marg anymarginal) nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = _IUpa*") ///
mgroups("\multicolumn{2}{c}{0-60 Days After Planting} & \multicolumn{2}{c}{60+ Days After Planting} \\ \cline{2-3} \cline{4-5}", pattern(1 0 1 0))

*Heterogeneous effects by first 80 days of the growing season (Table A7) 
esttab wlevel_preflower80 dfield_preflower80 wlevel_postflower80 dfield_postflower80 using "${latex}tableA7_waterlevel_timesplit80.tex", ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control" "phet p-Value: Treat+Treat*Volumetric"  "N Number of Observations" "r2 R squared") sfmt(%10.2f %10.3f %10.0f %10.3f) ///
mtitles ("Water Level" "Dry" "Water Level" "Dry") align(cccc) width(\hsize) order(treatment t_marg anymarginal) nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = _IUpa*") ///
mgroups("\multicolumn{2}{c}{0-80 Days After Planting} & \multicolumn{2}{c}{80+ Days After Planting} \\ \cline{2-3} \cline{4-5}", pattern(1 0 1 0))

}

/*********************************
		TABLE 3  - TABLE C3
**********************************/
{
*****************************************************************************************************************
*RCT 1 ANALYSIS FROM FOLLOW UP SURVEY
*****************************************************************************************************************
******************** input use *************************
use "data/input_primary/rct1_followup.dta", clear
merge 1:1 farmer_id using "data/input_primary/rct1_baseline.dta", keepusing(farmer_id)
drop if _merge==2 /*drop farmers not in baseline*/
drop _merge

*generate fertilizer expenditures per bigah 
gen purea=16 /*local prices from that season*/
gen ptsp=22
gen pmop=14 
gen pother=17.3333333
foreach j in urea tsp mop other { 
egen a_kg_`j' = rowtotal(a_dose1_`j' a_dose2_`j' a_dose3_`j' a_dose4_`j' a_dose5_`j')
gen a_spend_`j' = a_kg_`j'*p`j'
gen a_spendbg_`j' = a_spend_`j' / a_areacult_bg
}
gen totalfert_bg = a_spendbg_urea+a_spendbg_tsp+a_spendbg_mop+a_spendbg_other
order a_kg_* a_spend*, after(a_dose5_other)
*generate family labor per bigah 
gen a_famhar_bg = (a_famhar*a_harwage)/a_areacult_bg
gen a_famplant_bg = (a_famplant*a_plantwage)/a_areacult_bg 
egen weed_wage = rowmean(a_plantwage a_harwage) 
gen a_famweed_bg = (a_famweed*weed_wage)/a_areacult_bg 
*generate interaction term between treatment and volumetric prices 
gen t_marg = anymarginal*treatment  
*convert everything to per acre (for making units consistent across tables) 
foreach i in a_spendbg_urea a_spendbg_tsp a_spendbg_mop a_spendbg_other a_pesticides_bg a_herbicides_bg a_plantspend_bg a_weedspend_bg a_harspend_bg a_famplant_bg a_famhar_bg a_famweed_bg a_watercost totalfert_bg { 
replace `i'=`i'*3
}

******************** self-reported water use *************************
gen loga_watercost = ln(a_watercost)
*run regressions 
foreach j in a_irrigations a_timedry a_watercost loga_watercost { 
xi: reg `j' treatment i.Upazila, vce(cl village_id) 
sum `j' if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store `j'

xi: reg `j' treatment t_marg anymarginal i.Upazila, vce(cl village_id) 
sum `j' if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment+t_marg=0 
estadd sca phet=r(p)
estimates store `j'_het
}

******************** revenues and profits ****************************
*mean fill on price 
gen a_price_per_kg=a_price*(1/40)
sum a_price, detail
replace a_price = r(mean) if a_price==.
*generate variables 
gen a_yield_kgac = a_yield*120 /*convert yield from monn per bigha to kg per acre*/
gen a_rev_ac = a_yield*a_price*3 /*calculate revenue per acre (yield in monn per bigha, price in taka per monn, and multiply by bigha per acre)*/
egen a_cost_ac = rowtotal(a_spendbg_urea a_spendbg_tsp a_spendbg_mop a_spendbg_other a_pesticides_bg a_herbicides_bg a_plantspend_bg a_weedspend_bg a_harspend_bg a_famplant_bg a_famhar_bg a_famweed_bg a_watercost) /*total cost per acre*/
gen a_profit_ac = a_rev_ac - a_cost_ac 
gen a_nonwater_ac = a_cost_ac - a_watercost 
foreach i in a_yield_kgac a_rev_ac a_profit_ac a_nonwater_ac { 
gen log_`i' = ln(`i')
}

*run regressions 
foreach j in a_yield_kgac a_rev_ac a_cost_ac a_profit_ac log_a_yield_kgac log_a_rev_ac log_a_profit_ac a_price_per_kg a_nonwater_ac log_a_nonwater_ac { 
xi: reg `j' treatment i.Upazila, vce(cl village_id) 
sum `j' if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store `j'

xi: reg `j' treatment t_marg anymarginal i.Upazila, vce(cl village_id) 
sum `j' if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment+t_marg=0 
estadd sca phet=r(p)
estimates store `j'_het
}

*trimmed profits 
cumul a_profit_ac, gen(a_profit_ac_hat)
gen trim = a_profit_ac if a_profit_ac_hat >= 0.015 & a_profit_ac_hat <= 0.985
gen stupid=1 /*put the constant in the regression manually just for making the table easier*/
xi: reg trim treatment anymarginal t_marg stupid i.Upazila, vce(cl village_id) nocons
sum trim if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment + t_marg = 0
estadd sca phet=r(p)
estimates store pi_trim

gen log_trim = log(trim)
xi: reg log_trim treatment anymarginal t_marg stupid i.Upazila, vce(cl village_id) nocons
sum log_trim if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment + t_marg = 0
estadd sca phet=r(p)
estimates store logpi_trim

*profits with controls 
merge 1:1 farmer_id using "data/derived/baseline_covariates.dta", keepusing(farmer_id age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1) 
drop if _merge==2 
drop _merge

merge 1:1 farmer_id using "data/input_secondary/GIS_covariates.dta", keepusing(farmer_id srtm_eleva soilclay soilsand soilcarbon soilwater) 
drop if _merge==2 
drop _merge

foreach k in age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1 srtm_eleva soilclay soilsand soilcarbon soilwater { 
rename `k' bc_`k'
sum bc_`k', detail 
gen dmean_`k' = bc_`k' - r(mean) 
gen t_dmean_`k' = treatment*dmean_`k'
}

xi: reg a_profit_ac treatment anymarginal t_marg bc_* i.Upazila, vce(cl village_id)
sum a_profit_ac if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment + t_marg = 0
estadd sca phet=r(p)
estimates store pi_xbeta

xi: reg log_a_profit_ac treatment anymarginal t_marg bc_* i.Upazila, vce(cl village_id)
sum log_a_profit_ac if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment + t_marg = 0
estadd sca phet=r(p)
estimates store logpi_xbeta

*trimmed profits with controls 
xi: reg trim treatment anymarginal t_marg bc_* i.Upazila, vce(cl village_id)
sum trim if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment + t_marg = 0
estadd sca phet=r(p)
estimates store pi_trim_xbeta

xi: reg log_trim treatment anymarginal t_marg bc_* i.Upazila, vce(cl village_id)
sum log_trim if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment + t_marg = 0
estadd sca phet=r(p)
estimates store logpi_trim_xbeta

*profits with controls interacted with treatment 
xi: reg a_profit_ac treatment anymarginal t_marg bc_* t_dmean_* i.Upazila, vce(cl village_id)
sum a_profit_ac if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment + t_marg = 0
estadd sca phet=r(p)
estimates store pi_xbeta_inter

xi: reg log_a_profit_ac treatment anymarginal t_marg bc_* t_dmean_* i.Upazila, vce(cl village_id)
sum log_a_profit_ac if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment + t_marg = 0
estadd sca phet=r(p)
estimates store logpi_xbeta_inter

******************** heterogeneity by holding own prepaid card ****************************
gen hourcard = (a_paymethod==1)
gen t_hourcard = hourcard*treatment 

xi: reg a_watercost treatment t_hourcard hourcard i.Upazila if a_paymethod!=., vce(cl village_id) 
test treatment+t_hourcard=0
sum a_watercost if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment+t_hourcard=0 
estadd sca phet=r(p)
estimates store cost_card

xi: reg loga_watercost treatment t_hourcard hourcard i.Upazila if a_paymethod!=., vce(cl village_id)
test treatment+t_hourcard=0
sum loga_watercost if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment+t_hourcard=0 
estadd sca phet=r(p)
estimates store logcost_card 

xi: reg a_profit_ac treatment t_hourcard hourcard i.Upazila if a_paymethod!=., vce(cl village_id) 
test treatment+t_hourcard=0
sum a_profit_ac if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment+t_hourcard=0 
estadd sca phet=r(p)
estimates store profit_card

xi: reg log_a_profit_ac treatment t_hourcard hourcard i.Upazila if a_paymethod!=., vce(cl village_id) 
test treatment+t_hourcard=0
sum log_a_profit_ac if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment+t_hourcard=0 
estadd sca phet=r(p)
estimates store logprofit_card

xi: reg a_watercost treatment t_hourcard hourcard dmean_* t_dmean_* i.Upazila if a_paymethod!=., vce(cl village_id) 
test treatment+t_hourcard=0
sum a_watercost if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment+t_hourcard=0 
estadd sca phet=r(p)
estimates store cost_card_h

xi: reg loga_watercost treatment t_hourcard hourcard dmean_* t_dmean_* i.Upazila if a_paymethod!=., vce(cl village_id) 
test treatment+t_hourcard=0
sum loga_watercost if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment+t_hourcard=0 
estadd sca phet=r(p)
estimates store logcost_card_h

xi: reg a_profit_ac treatment t_hourcard hourcard dmean_* t_dmean_* i.Upazila if a_paymethod!=., vce(cl village_id) 
test treatment+t_hourcard=0
sum a_profit_ac if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment+t_hourcard=0 
estadd sca phet=r(p)
estimates store profit_card_h

xi: reg log_a_profit_ac treatment t_hourcard hourcard dmean_* t_dmean_* i.Upazila if a_paymethod!=., vce(cl village_id) 
test treatment+t_hourcard=0
sum log_a_profit_ac if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment+t_hourcard=0 
estadd sca phet=r(p)
estimates store logprofit_card_h

*OUTPUT REGRESSION TABLES 
label var anymarginal "Volumetric Pricing" 
label var treatment "Treatment" 
label var t_marg "Treatment * Volumetric Pricing" 
label var hourcard "Has Card" 
label var t_hourcard "Treatment * Has Card" 
label var treatment "Treatment"

*Table 3: Effects of conservation technology on log costs, revenues, and profits
esttab log_a_profit_ac_het loga_watercost_het log_a_yield_kgac_het log_a_rev_ac_het logcost_card logprofit_card  using "${latex}table3_rev_profit_het.tex", ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("phet p-Value: Treat+Treat*Volumetric" "N Number of Observations" "r2 R squared") sfmt(%10.3f %10.0f %10.3f) ///
mtitles ("Profit" "Water Cost" "Yield" "Revenue" "Water Cost" "Profit") align(cccccc) width(\hsize) ///
nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = _IUpa*") order(treatment t_marg anymarginal t_hourcard hourcard) ///
mgroups("\multicolumn{4}{c}{Full Sample} & \multicolumn{2}{c}{Rajshahi Sample} \\ \cline{2-5} \cline{6-7}", pattern(1 0 0 0 1 0))

*TABLE C3 Robustness of card ownership results to interactions between treatment and covariates
esttab cost_card_h logcost_card_h profit_card_h logprofit_card_h using "${latex}tableC3_profit_cardhet_controls.tex", ///
replace b(a3) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control" "phet p-Value: Treat+Treat*Has Card" "N Number of observations") sfmt(%10.2f %10.3f %10.0f) ///
mtitles ("Water Cost" "Log Water Cost" "Profit" "Log Profit" ) align(cccc) ///
nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = _IUpa*" "Covariates=dmean_*" "Treatment*Covariates = t_dmea*") order(treatment) 

}


/**********************************
		TABLE 4: 
***********************************/
{
******************************************************************************************************************
*RCT 1 DEMAND EXPERIMENT IN BORO 2018
******************************************************************************************************************
*set aside marginal pricing variable from baseline survey
use "data/input_primary/rct1_baseline", clear
keep farmer_id anymarginal
tempfile anym
save "`anym'", replace

use "data/input_primary/rct1_boro2018pipesales.dta", clear
merge 1:1 farmer_id using "`anym'" 
drop if _merge==2
drop _merge
gen t_marg = treatment_phase1*anymarginal

*effects of initial treatment on future demand, separate by marginal pricing
xi: reg adoption treatment_phase1 price anymarginal t_marg i.upazila if farmer_id<6000, vce(cl village_id)
sum adoption if treatment_phase1==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store adopt_rct1

*same regression, but only using the within-upazila variation
xi: reg adoption price anymarginal t_marg i.upazila*treatment_phase1 if farmer_id<6000, vce(cl village_id)
sum adoption if treatment_phase1==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store adopt_rct1_upazilaXtreat

label var treatment_phase1 "Treatment" 
label var anymarginal "Volumetric Pricing" 
label var t_marg "Treatment * Volumetric Pricing"
label var price "Price"

*make table: Table 4 Followup demand for AWD in the 2018 season
esttab adopt_rct1 adopt_rct1_upazilaXtreat using "${latex}table4_pipesales_rct1_2018.tex", ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control"  "N Number of Observations" "r2 R squared") sfmt(%10.2f %10.0f %10.3f) ///
nomtitles align(cc) order(treatment_phase1 anymarginal t_marg price) nonotes gaps drop(_cons) ///
indicate("Upazila Fixed Effects = _Iupaz*" "Upazila FE X Treatment = _IupaX*")

}

/**********************************************
		TABLE 5 - TABLE C4 - TABLE C5
***********************************************/
{
******************************************************************************************************************
*RCT 1: ROBUSTNESS OF INTERACTION TERM TO CONTROLS 
******************************************************************************************************************
use "data/input_primary/rct1_watermeasure.dta", clear 
keep if waterunit != . /*keep only completed measurements*/
merge n:1 farmer_id using "data/input_secondary/GIS_covariates.dta", keepusing(farmer_id srtm_eleva soilclay soilsand soilcarbon soilwater) 
drop if _merge==2 
drop _merge
gen t_marg = treatment*anymarginal
merge n:1 farmer_id using "data/derived/baseline_covariates.dta", keepusing(farmer_id age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1) 
drop if _merge==2 
drop _merge
*demean control variables and create upazila-level mean
foreach k in age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1 { 
rename `k' bc_`k'
sum bc_`k', detail 
gen dmean_`k' = bc_`k' - r(mean) 
gen t_dmean_`k' = treatment*dmean_`k'

egen upamean_`k' = mean(bc_`k'), by(upazila)
sum upamean_`k', detail 
gen dupamean_`k' = upamean_`k' - r(mean) 
gen t_dupamean_`k' = treatment*dupamean_`k'
}
*demean control variables and create upazila-level mean (geographic variables)
foreach k in srtm_eleva soilclay soilsand soilcarbon soilwater { 
rename `k' bc_`k'
sum bc_`k', detail 
gen dmeanGeo_`k' = bc_`k' - r(mean) 
gen t_dmeanGeo_`k' = treatment*dmeanGeo_`k'

egen upameanGeo_`k' = mean(bc_`k'), by(upazila)
sum upameanGeo_`k', detail 
gen dupameanGeo_`k' = upameanGeo_`k' - r(mean) 
gen t_dupameanGeo_`k' = treatment*dupameanGeo_`k'
}

*0-70 days
xi: reg waterlevel treatment anymarginal t_marg dmean_* t_dmean_* i.Upazila if dat<=70, vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_rob70


xi: reg waterlevel treatment anymarginal t_marg t_dupameanGeo_soilwater i.Upazila if dat<=70, vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_rob70_upazila


xi: reg dryfield treatment anymarginal t_marg dmean_* t_dmean_* i.Upazila if dat<=70, vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum dryfield if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store dry_rob70

xi: reg waterlevel treatment anymarginal t_marg dmean_* t_dmean_* dmeanGeo_* t_dmeanGeo_* i.Upazila if dat<=70, vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_rob70_geo

xi: reg dryfield treatment anymarginal t_marg dmean_* t_dmean_* dmeanGeo_* t_dmeanGeo_* i.Upazila if dat<=70, vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum dryfield if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store dry_rob70_geo

*overall
xi: reg waterlevel treatment anymarginal t_marg dmean_* t_dmean_* i.Upazila, vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_rob

xi: reg dryfield treatment anymarginal t_marg dmean_* t_dmean_* i.Upazila, vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum dryfield if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store dry_rob

xi: reg waterlevel treatment anymarginal t_marg dmean_* t_dmean_* dmeanGeo_* t_dmeanGeo_* i.Upazila, vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_rob_geo

xi: reg dryfield treatment anymarginal t_marg dmean_* t_dmean_* dmeanGeo_* t_dmeanGeo_* i.Upazila, vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum dryfield if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store dry_rob_geo

foreach k in srtm_eleva soilclay soilsand soilcarbon soilwater {
xi: reg waterlevel treatment anymarginal t_marg dmean_* t_dmean_* t_dupameanGeo_`k' i.Upazila, vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store upasoil_`k'
}


*mymensingh only
xi: reg waterlevel treatment anymarginal t_marg i.Upazila if district=="Mymensingh", vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_mymensingh

xi: reg dryfield treatment anymarginal t_marg i.Upazila if district=="Mymensingh", vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum dryfield if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store dry_mymensingh

xi: reg waterlevel treatment anymarginal t_marg i.Upazila if dat<=70 & district=="Mymensingh", vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_70_mymensingh

xi: reg dryfield treatment anymarginal t_marg i.Upazila if dat<=70 & district=="Mymensingh", vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum dryfield if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store dry_70_mymensingh

*rajshahi only
xi: reg waterlevel treatment anymarginal t_marg i.Upazila if district=="Rajshahi", vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_rajshahi

xi: reg dryfield treatment anymarginal t_marg i.Upazila if district=="Rajshahi", vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum dryfield if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store dry_rajshahi

xi: reg waterlevel treatment anymarginal t_marg i.Upazila if dat<=70 & district=="Rajshahi", vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum waterlevel if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store wlevel_70_rajshahi

xi: reg dryfield treatment anymarginal t_marg i.Upazila if dat<=70 & district=="Rajshahi", vce(cl village_id)
test treatment+t_marg=0 
estadd sca phet=r(p)
sum dryfield if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store dry_70_rajshahi

*make tables:
label var anymarginal "Volumetric Pricing" 
label var treatment "Treatment" 
label var t_marg "Treatment * Volumetric Pricing" 
label var t_dupameanGeo_soilclay "Soil Clay Content"
label var t_dupameanGeo_soilsand "Soil Sand Content"
label var t_dupameanGeo_soilcarbon "Soil Carbon Content"
label var t_dupameanGeo_soilwater "Soil Water Content"

*Table 5 Robustness of water-usage results to interactions between the AWD treatment and covariates
esttab wlevel_rob wlevel_rob_geo upasoil_soilclay upasoil_soilsand upasoil_soilcarbon upasoil_soilwater using "${latex}table5_water_robust_interaction.tex", ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control" "phet p-Value: Treat+Treat*Volumetric"  "N Number of Observations" "r2 R squared") sfmt(%10.2f %10.3f %10.0f %10.3f) ///
nomtitles align(cccccc) width(\hsize) order(treatment t_marg anymarginal  t_dupameanGeo_soilclay t_dupameanGeo_soilsand t_dupameanGeo_soilcarbon t_dupameanGeo_soilwater) ///
nonotes refcat(t_dupameanGeo_soilclay "\emph{\underline{Treatment Interacted With:}}", nolabel) gaps drop(_cons) indicate("Upazila Fixed Effects = _IUpa*" "Controls = dmean_*" "Controls X Treatment = t_dmean_*" "Geo Controls = dmeanGeo*" "Geo Controls X Treatment = t_dmeanGeo_*") 

*Table C5: 
esttab wlevel_mymensingh dry_mymensingh wlevel_70_mymensingh dry_70_mymensingh  using "${latex}tableC5_water_mymensingh_interaction.tex", ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control" "phet p-Value: Treat+Treat*Volumetric"  "N Number of Observations" "r2 R squared") sfmt(%10.2f %10.3f %10.0f %10.3f) ///
mtitles ("Level" "Dry" "Level" "Dry") align(cccc) width(\hsize) order(treatment t_marg anymarginal) ///
nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = _IUpa*") ///
mgroups("\multicolumn{2}{c}{Overall} & \multicolumn{2}{c}{0-70 Days After Planting} \\ \cline{2-3} \cline{4-5}", pattern(1 0 1 0))

*Table C4: 
esttab wlevel_rajshahi dry_rajshahi wlevel_70_rajshahi dry_70_rajshahi  using "${latex}tableC4_water_rajshahi_interaction.tex", ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control" "phet p-Value: Treat+Treat*Volumetric"  "N Number of Observations" "r2 R squared") sfmt(%10.2f %10.3f %10.0f %10.3f) ///
mtitles ("Level" "Dry" "Level" "Dry") align(cccc) width(\hsize) order(treatment t_marg anymarginal) ///
nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = _IUpa*") ///
mgroups("\multicolumn{2}{c}{Overall} & \multicolumn{2}{c}{0-70 Days After Planting} \\ \cline{2-3} \cline{4-5}", pattern(1 0 1 0))
}

/**********************************
	TABLE 6
***********************************/
estimates clear /*start with cleared estimates to avoid hitting Stata limit of 300*/
{
*******************************************
*RCT 1: LASSO to see which covariates drive heterogeneity the most
*TABLE 6
******************************************************************************************************************
*put together dataset
use "data/input_primary/rct1_watermeasure.dta", clear 
keep if waterunit != . /*keep only completed measurements*/
merge n:1 farmer_id using "data/derived/baseline_covariates.dta", keepusing(farmer_id age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1) 
drop if _merge==2 
drop _merge

keep if dat<=70
foreach j in anymarginal age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1 {
sum `j' 
gen zs_`j' = (`j'-r(mean))/r(sd)
}
keep farmer_id waterlevel treatment zs_* upazila village_id
save "data/derived/rct1_lasso_ds.dta", replace

* Set the seed
set seed 591852 

* Create an empty dataset to house the new variables
tempname memoryhold
postfile `memoryhold' ate het g1 g2 g3 g4 using "data/derived/LASSO_simulate.dta", replace

* Loop over our dataset, pull main/auxiliary sample, and execute calculations 
forvalues t=1(1)100 { 
	use "data/derived/rct1_lasso_ds.dta", clear
	
	* Create indicator to we can split sample into assessment (auxiliary) sample (otherwise in main sample) 
	gen runi=runiform()
	sort runi
	desc 
	gen assess = _n <= (r(N)/2) 
	
	* LASSO Regression in assessment sample (assessment sample to avoid over fitting) ---
	
	* First lasso in Control and ASSESSMENT sample (outcome = ncrops grown, and independent vars = demeaned characteristics)
	preserve 
	keep if treatment==0 & assess == 1
	lasso2 waterlevel zs_*, long lic(aicc) postresults maxiter(50000) 
	/* long = show all models, aicc =  the information criteria supported, 
	postresults =  stores estimation results of the model selected by information criterion in e().  */
	local k = e(selected) /* grab the selected variables */
	if e(s)>0 { /*number of parameters selected*/
	reg waterlevel `k', vce(robust)
	}
	if e(s)==0 { 
	reg waterlevel, vce(robust)
	}
	estimates store beta_control_`t' /* store this control regression */
	restore 
	
	* Second lasso in Treatment and ASSESSMENT sample (outcome = ncrops grown, and independent vars = demeaned characteristics)
	preserve 
	keep if treatment==1 & assess==1
	lasso2 waterlevel zs_*, long lic(aicc) postresults maxiter(50000) 
	local k = e(selected)
	if e(s) > 0 {
	reg waterlevel `k', vce(robust)
	}
	if e(s)==0 { 
	reg waterlevel, vce(robust)
	}
	estimates store beta_treat_`t' /* store this treatment regression */
	restore 
	
	* Calculate treatment effect heterogeneity in MAIN sample to avoid over fitting --------------------------------------------------------------------------------------------
	keep if assess==0
	
	estimates restore beta_control_`t' /* this restores the regression and hence the beta coefficients  */
	predict xb_control_`t', xb /* this produces the predictions using control betas*/
	estimates restore beta_treat_`t' /* this restores the regression and hence the beta coefficients  */
	predict xb_treat_`t', xb  /* this produces the predictions using treatment betas*/

	gen s0_`t' = xb_treat_`t' - xb_control_`t'
	sum s0_`t', detail 
	gen s0dm_`t' = s0_`t' - r(mean)
	gen treat_s0_`t' = treatment*s0dm_`t'
	
	* Regress outcome on treatment interacted with heterogeneity index ----------------------------------------------------------------------
	xi: reg waterlevel treatment xb_control_`t' s0dm_`t' treat_s0_`t' i.upazila, vce(cl village_id) /*looks plausible in validation dataset*/
	loc ate=_b[treatment]
	loc het=_b[treat_s0_`t']
	
	* Regress outcome on treatment interacted with heterogeneity index ---------------------------------------------------------------------
	xtile het_group = s0_`t', nq(4)
	foreach i in 1 2 3 4 { 
	xi: reg waterlevel treatment i.upazila if het_group==`i', vce(cl village_id) /*looks plausible in validation dataset*/
	loc g`i'=_b[treatment] in `t'
	}
	post `memoryhold' (`ate') (`het') (`g1') (`g2') (`g3') (`g4')
}
postclose `memoryhold' 

use "data/derived/LASSO_simulate.dta", clear
sum

****** STEP 3: Predicted heterogeneity for everybody in the sample *********
use "data/derived/baseline_covariates.dta", clear
* Demean variables
foreach j in anymarginal age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1 {
sum `j' 
gen zs_`j' = (`j'-r(mean))/r(sd)
}
forvalues t=1(1)100 { 
* Control betas
estimates restore beta_control_`t' 
predict xb_control_`t', xb
* Treatment betas
estimates restore beta_treat_`t' 
predict xb_treat_`t', xb
* Predicted treatment effect 
gen s0_`t' = xb_treat_`t' - xb_control_`t'
}
egen s0=rowmedian(s0_*) /*median across 100 iterations*/
drop s0_*
save "data/derived/LASSO_phet.dta", replace 

****** STEP 4: Characteristics of least and most affected units *********
*set aside observed water level 
use "data/input_primary/rct1_watermeasure.dta", clear 
keep if waterunit != . & dat<=70
collapse (mean) waterlevel, by(farmer_id) 
tempfile addh20 
save "`addh20'", replace 

*LASSO results
use "data/derived/LASSO_phet.dta", clear 
merge 1:1 farmer_id using "data/input_primary/rct1_baseline.dta", keepusing(farmer_id village_id treatment) nogen keep(match)
merge 1:1 farmer_id using "`addh20'" 
drop if _merge==2
drop _merge
cumul s0, gen(Fhat)
xtile aff_group = Fhat, nq(5)
gen m_most=.
gen m_least=. 
gen r2=.
loc ct=0

foreach j in anymarginal age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1 {
loc ct=`ct'+1
reg s0 `j', vce(cl village_id)
replace r2=e(r2) in `ct'
sum `j' if aff_group==1
replace m_most=r(mean) in `ct'
sum `j' if aff_group==5
replace m_least=r(mean) in `ct'
}

label var age "Age" 
label var edu "Years Education" 
label var hhsize "Household Size" 
label var livestock "Number Livestock Owned" 
label var landhold "Landholdings in Acres" 
label var knowawd "Heard of AWD?" 
label var renter "Plot is Rented or Sharecropped" 
label var area1_acre "Area in Acres" 
label var anymarginal "Volumetric Water Price" 
label var ncrop1 "Number Crops Grown" 
label var rice_rice_sys "Rice-Rice Cropping System" 
label var boro_rvirrig_1 "Number Irrigations in Boro" 
label var boro_revperacre_1 "Revenue per Acre in Boro" 
label var boro_tcost_1 "Cost per Acre in Boro" 
label var boro_watercostpacre_1 "Water Cost per Acre in Boro" 
label var aman_peracre_1 "Revenue per Acre in Aman"
label var assetheld_1 "Owns Television"
label var assetheld_5 "Owns Refrigerator"
label var assetheld_9 "Owns Irrigation Shallow Tubewell"

order r2 m_most m_least
br
drop if m_least==.
mkmat m_most m_least r2, matrix(finalres)
matrix rownames finalres = anymarginal age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1

*make Table 6 Characteristics of farmers most and least affected by conservation technology
frmttable using "${latex}table6_lasso_covariates", statmat(finalres) sdec(3,3,3) ctitle("","Mean Most","Mean Least","Share Variation" \ "","Affected","Affected","Explained") ///
varlabels tex fra replace
}

/**************************************************
		TABLE 7 - TABLE A18 - TABLE A19
***************************************************/
{
******************************************************************************************************************
*RCT 2: TREATMENT EFFECTS ON PIPE PURCHASES AND USAGE 
******************************************************************************************************************
use "data/input_primary/rct2_usage.dta", clear
merge 1:1 farmer_id using "data/input_primary/rct2_pipesales.dta", keepusing(farmer_id adoption) /*merge in pipe sales outcome*/
drop _merge
merge 1:1 farmer_id using "data/input_primary/rct2_baseline.dta", keepusing(farmer_id  waterprice waterirrig waterwhopay nadults nyoung)
drop _merge
gen logprice = ln(price)
gen cardtreat = (treatment=="card")
gen cardtreat_price = cardtreat*price
gen cardtreat_logprice = cardtreat*logprice 
gen awd_use = (awdirri==1 | awdoth==1)
gen dpaba = (Upazila=="PABA") /*creating upazila dummies, mostly to scale graph*/
gen dtanore = (Upazila=="TANORE")
foreach j in paba tanore { /*demean upazila dummies to scale graph correctly*/
sum d`j'
gen d`j'_d = d`j'-r(mean)
}

*linear demand purchasing
xi: reg adoption cardtreat price dpaba_d dtanore_d, vce(cl village_id) 
sum adoption if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
estadd sca epst=_b[price]*(55)/(_b[_cons]+_b[cardtreat]+_b[price]*55) /*demand elasticity at price of 55 taka, treatment*/ 
estadd sca epsc=_b[price]*(55)/(_b[_cons]+_b[price]*55) /*demand elasticity at price of 55 taka, control*/ 
estimates store lindem 

*linear demand using
xi: reg awd_use cardtreat price dpaba_d dtanore_d, vce(cl village_id) 
sum awd_use if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
estadd sca epst=_b[price]*(55)/(_b[_cons]+_b[cardtreat]+_b[price]*55) /*demand elasticity at price of 55 taka, treatment*/ 
estadd sca epsc=_b[price]*(55)/(_b[_cons]+_b[price]*55) /*demand elasticity at price of 55 taka, control*/ 
estimates store lindem_use 

*linear demand purchasing, interaction  
gen pvalue=. 
xi: reg adoption cardtreat price cardtreat_price dpaba_d dtanore_d, vce(cl village_id) 
di -_b[_cons]/_b[price] /*choke price in control*/
di (-_b[_cons]-_b[cardtreat])/(_b[price]+_b[cardtreat_price]) /*choke price in treatment*/
foreach i in 20 30 40 50 60 70 80 90 {
di _b[cardtreat]+`i'*_b[cardtreat_price]
test cardtreat + `i'*cardtreat_price = 0 
replace pvalue = r(p) if price==`i' 
}
sum adoption if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
estadd sca epst=(_b[price]+_b[cardtreat_price])*(55)/(_b[_cons]+_b[cardtreat]+_b[price]*55+_b[cardtreat_price]*55) /*demand elasticity at price of 55 taka, treatment*/ 
estadd sca epsc=_b[price]*(55)/(_b[_cons]+_b[price]*55) /*demand elasticity at price of 55 taka, control*/ 
nlcom ((_b[price]+_b[cardtreat_price])*(55)/(_b[_cons]+_b[cardtreat]+_b[price]*55+_b[cardtreat_price]*55)) - (_b[price]*(55)/(_b[_cons]+_b[price]*55))
mat K=r(V)
mat B=r(b) 
mat D=r(N)
loc st = B[1,1]/sqrt(K[1,1])
loc dfree=D[1,1]
estadd sca pelas = 2*(1-t(`dfree',`st'))
estimates store lindem_i

*linear demand using, interaction  
gen pvalue_u=. 
xi: reg awd_use cardtreat price cardtreat_price dpaba_d dtanore_d, vce(cl village_id) 
di -_b[_cons]/_b[price] /*choke price in control*/
di (-_b[_cons]-_b[cardtreat])/(_b[price]+_b[cardtreat_price]) /*choke price in treatment*/
foreach i in 20 30 40 50 60 70 80 90 {
di _b[cardtreat]+`i'*_b[cardtreat_price]
test cardtreat + `i'*cardtreat_price = 0 
replace pvalue_u = r(p) if price==`i' 
}
sum awd_use if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
estadd sca epst=(_b[price]+_b[cardtreat_price])*(55)/(_b[_cons]+_b[cardtreat]+_b[price]*55+_b[cardtreat_price]*55) /*demand elasticity at price of 55 taka, treatment*/ 
estadd sca epsc=_b[price]*(55)/(_b[_cons]+_b[price]*55) /*demand elasticity at price of 55 taka, control*/ 
nlcom ((_b[price]+_b[cardtreat_price])*(55)/(_b[_cons]+_b[cardtreat]+_b[price]*55+_b[cardtreat_price]*55)) - (_b[price]*(55)/(_b[_cons]+_b[price]*55))
mat K=r(V)
mat B=r(b) 
mat D=r(N)
loc st = B[1,1]/sqrt(K[1,1])
loc dfree=D[1,1]
estadd sca pelas = 2*(1-t(`dfree',`st'))
estimates store lindem_i_use

*log demand 
xi: reg adoption cardtreat logprice dpaba_d dtanore_d, vce(cl village_id)
sum adoption if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
estadd sca epst= _b[logprice] / (_b[_cons]+_b[cardtreat]+_b[logprice]*ln(55)) /*demand elasticity at price of 55 taka, treatment*/
estadd sca epsc= _b[logprice] / (_b[_cons]+_b[logprice]*ln(55)) /*demand elasticity at price of 55 taka, control*/
estimates store logdem

*log demand using 
xi: reg awd_use cardtreat logprice dpaba_d dtanore_d, vce(cl village_id)
sum awd_use if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
estadd sca epst= _b[logprice] / (_b[_cons]+_b[cardtreat]+_b[logprice]*ln(55)) /*demand elasticity at price of 55 taka, treatment*/
estadd sca epsc= _b[logprice] / (_b[_cons]+_b[logprice]*ln(55)) /*demand elasticity at price of 55 taka, control*/
estimates store logdem_use 

*log demand interaction 
xi: reg adoption cardtreat logprice cardtreat_logprice dpaba_d dtanore_d, vce(cl village_id)
foreach i in 20 30 40 50 60 70 80 90 {
loc j=ln(`i')
di _b[cardtreat]+`j'*_b[cardtreat_logprice]
test cardtreat + `j'*cardtreat_logprice = 0 
} 
di exp(-_b[_cons]/_b[logprice]) /*choke price in control*/
di exp((-_b[_cons]-_b[cardtreat])/(_b[logprice]+_b[cardtreat_logprice])) /*choke price in treatment*/
sum adoption if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
estadd sca epst= (_b[logprice]+_b[cardtreat_logprice]) / (_b[_cons]+_b[cardtreat]+_b[logprice]*ln(55)+_b[cardtreat_logprice]*ln(55)) /*demand elasticity at price of 55 taka, treatment*/
estadd sca epsc= _b[logprice] / (_b[_cons]+_b[logprice]*ln(55)) /*demand elasticity at price of 55 taka, control*/
nlcom ((_b[logprice]+_b[cardtreat_logprice]) / (_b[_cons]+_b[cardtreat]+_b[logprice]*ln(55)+_b[cardtreat_logprice]*ln(55))) - (_b[logprice] / (_b[_cons]+_b[logprice]*ln(55)))
mat K=r(V)
mat B=r(b) 
mat D=r(N)
loc st = B[1,1]/sqrt(K[1,1])
loc dfree=D[1,1]
estadd sca pelas = 2*(1-t(`dfree',`st'))
estimates store logdem_i

*log demand using interaction 
xi: reg awd_use cardtreat logprice cardtreat_logprice dpaba_d dtanore_d, vce(cl village_id)
foreach i in 20 30 40 50 60 70 80 90 {
loc j=ln(`i')
di _b[cardtreat]+`j'*_b[cardtreat_logprice]
test cardtreat + `j'*cardtreat_logprice = 0 
} 
di exp(-_b[_cons]/_b[logprice]) /*choke price in control*/
di exp((-_b[_cons]-_b[cardtreat])/(_b[logprice]+_b[cardtreat_logprice])) /*choke price in treatment*/
sum awd_use if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
estadd sca epst= (_b[logprice]+_b[cardtreat_logprice]) / (_b[_cons]+_b[cardtreat]+_b[logprice]*ln(55)+_b[cardtreat_logprice]*ln(55)) /*demand elasticity at price of 55 taka, treatment*/
estadd sca epsc= _b[logprice] / (_b[_cons]+_b[logprice]*ln(55)) /*demand elasticity at price of 55 taka, control*/
nlcom ((_b[logprice]+_b[cardtreat_logprice]) / (_b[_cons]+_b[cardtreat]+_b[logprice]*ln(55)+_b[cardtreat_logprice]*ln(55))) - (_b[logprice] / (_b[_cons]+_b[logprice]*ln(55)))
mat K=r(V)
mat B=r(b) 
mat D=r(N)
loc st = B[1,1]/sqrt(K[1,1])
loc dfree=D[1,1]
estadd sca pelas = 2*(1-t(`dfree',`st'))
estimates store logdem_i_use

*regressions of usage on price (screening regressions)
xi: reg awd_use cardtreat price cardtreat_price dpaba_d dtanore_d if adoption==1, vce(cl village_id) 
sum awd_use if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
test price + cardtreat_price=0
estadd sca pval = r(p)
estimates store use_price_conditional

xi: reg awd_use cardtreat logprice cardtreat_logprice dpaba_d dtanore_d if adoption==1, vce(cl village_id)
sum awd_use if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
test logprice + cardtreat_logprice=0
estadd sca pval = r(p)
estimates store use_logprice_conditional


******************************************************************************************************************
*RCT 2: RELATIONSHIP BETWEEN PREPAID CARD TREATMENT AND OBSERVED WATER MANAGEMENT ON ONE FIELD PER FARMER (TABLE 6)
*uses same data loaded in above
******************************************************************************************************************
gen waterlevel_comp = waterlevel_c_cm 
replace waterlevel_comp = waterlevel_f_cm if waterlevel_c_cm==. /*water level, either far or close*/
gen dryfield = (waterlevel_comp==0) /*1 if no water*/

xi: reg awdmeas cardtreat price dpaba_d dtanore_d, vce(cl village_id)
sum awdmeas if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
estimates store fs_awdmeas_noint

xi: reg awdmeas cardtreat price cardtreat_price dpaba_d dtanore_d, vce(cl village_id)
sum awdmeas if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
test price + cardtreat_price=0
estadd sca pval = r(p)
estimates store fs_awdmeas

xi: reg waterlevel_comp cardtreat price cardtreat_price dpaba_d dtanore_d, vce(cl village_id)
sum waterlevel_comp if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
test price + cardtreat_price=0
estadd sca pval = r(p)
estimates store fs_level

xi: reg waterlevel_comp cardtreat price dpaba_d dtanore_d, vce(cl village_id)
sum waterlevel_comp if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
estimates store fs_level_noint

xi: reg dryfield cardtreat price cardtreat_price dpaba_d dtanore_d, vce(cl village_id)
sum dryfield if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
test price + cardtreat_price=0
estadd sca pval = r(p)
estimates store fs_dry

xi: reg dryfield cardtreat price dpaba_d dtanore_d, vce(cl village_id)
sum dryfield if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
estimates store fs_dry_noint

*Make table: Table 7 Impacts of hourly irrigation cards on water usage and AWD demand
*export table  
label var price "Pipe Price" 
label var logprice "Log Pipe Price"
label var cardtreat "Card Treatment" 
label var cardtreat_price "Pipe Price * Card Treatment" 
label var cardtreat_logprice "Log Pipe Price * Card Treatment" 

esttab fs_awdmeas_noint fs_awdmeas fs_level_noint fs_level lindem lindem_i lindem_use lindem_i_use using "${latex}table7_observe_water_phase2.tex", ///
replace b(%10.4f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control" "epst Elasticity at Price=55 Treat" "epsc Elasticity at Price=55 Control" "pelas P-value: Equal Elasticities" "N Number Obs" "r2 R squared") ///
sfmt(%10.3f %10.2f %10.2f %10.3f %10.0f %10.3f) nomtitles align(cccccccc) nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = d*") ///
mgroups("\multicolumn{2}{c}{AWD Installed} & \multicolumn{2}{c}{Water Level} & \multicolumn{2}{c}{Purchase AWD} & \multicolumn{2}{c}{Use AWD} \\ \cmidrule(l){2-3} \cmidrule(l){4-5} \cmidrule(l){6-7} \cmidrule(l){8-9}", pattern(1 0 1 0 1 0 1 0))


*Table A18 Impacts of hourly irrigation cards on demand with log functional form
esttab logdem logdem_i logdem_use logdem_i_use using "${latex}tableA18_pipeUPTAKE_prepaid_LOG.tex", ///
replace b(%10.4f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control" "epst Elasticity at Price=55 Treat" "epsc Elasticity at Price=55 Control" "pelas P-value: Equal Elasticities" "N Number Obs" "r2 R squared") ///
sfmt(%10.3f %10.2f %10.2f %10.3f %10.0f %10.3f) nomtitles align(cccc) nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = d*") ///
mgroups("\multicolumn{2}{c}{Purchase} & \multicolumn{2}{c}{Usage} \\ \cmidrule(l){2-3} \cmidrule(l){4-5}", pattern(1 0 1 0))

*Table A19 Relationship between price and usage conditional on purchase of conservation technology
esttab use_price_conditional use_logprice_conditional using "${latex}tableA19_pipeusage_price_corr.tex", ///
replace b(%10.4f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control" "pval P-value: Price+Price*Volumetric" "N Number Obs" "r2 R squared") ///
sfmt(%10.3f %10.3f %10.0f %10.3f) nomtitles align(cccc) nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = d*")

}

/**********************************
		FIGURE 3 
***********************************/
{
*****************************************************************************************************************
*Nonparametric estimates of treatment effect as a function of days after planting (Figure 3) 
*Note: uses same dataset as table 1-2
*Note: Needs fanreg.ado to run (provided)
*****************************************************************************************************************
use "data/input_primary/rct1_watermeasure.dta", clear 
keep if waterunit != . /*keep only completed measurements*/

tempfile bloop /*set aside data and then loop through, once for volumetric and then for non-volumetric*/
save "`bloop'", replace 

*make title of graph and subset the data
foreach j in 0 1 { 
if `j' == 1 {
	loc tit "A: Volumetric Price"
}
else if `j' == 0 {
	loc tit "B: Fixed Price"
}
use "`bloop'", clear 
keep if anymarginal==`j'
*bin the data into 10 day bins 
gen dat_bin = . 
forvalues q=10(10)180 { 
replace dat_bin = `q'-5 if dat>=`q'-10 & dat<`q'
}
egen m_waterlevel = mean(waterlevel), by(dat_bin treatment) 
egen m_dryfield = mean(dryfield), by(dat_bin treatment)

fanreg waterlevel dat if treatment==1, xgen(xt) ygen(yt)
fanreg waterlevel dat if treatment==0, xgen(xc) ygen(yc)
egen tot_yt = sum(yt) 
egen tot_yc = sum(yc)
fanreg dryfield dat if treatment==1, xgen(xdt) ygen(ydt)
fanreg dryfield dat if treatment==0, xgen(xdc) ygen(ydc)

*make graphs
twoway (line yt xt if xt<=120, lcolor(blue) lwidth(thick)) (line yc xc if xc<=120, lcolor(black) lwidth(thick)) ///
(scatter m_waterlevel dat_bin if treatment==1 & dat_bin<=120, mcolor(blue)) (scatter m_waterlevel dat_bin if treatment==0 & dat_bin<=120, mcolor(black)), ///
scheme(s1color) xtitle("") ytitle(Water level CM) title(`tit') ylabel(0 1 2 3 4 5) xlabel(0 20 40 60 80 100 120) ///
legend(off) text(4 80 "Treatment", place(e) color(blue) size(medlarge)) text(3.66 80 "Control", place(e) color(black) size(medlarge)) name(t1_`j', replace) 
graph export "${latex}t1_`j'.pdf", as(pdf) replace

twoway (line ydt xdt if xdt<=120, lcolor(blue) lwidth(thick)) (line ydc xdc if xdc<=120, lcolor(black) lwidth(thick)) ///
(scatter m_dryfield dat_bin if treatment==1 & dat_bin<=120, mcolor(blue)) (scatter m_dryfield dat_bin if treatment==0 & dat_bin<=120, mcolor(black)), ///
scheme(s1color) xtitle("") ytitle(Field is dry) ylabel(0 0.2 0.4 0.6 0.8 1) xlabel(0 20 40 60 80 100 120) ///
legend(off) name(t2_`j', replace) 
graph export "${latex}t2_`j'.pdf", as(pdf) replace

histogram dat if dat<=120, xlabel(0 20 40 60 80 100 120) fysize(30) scheme(s1color) lcolor(gs4) fcolor(gs6) name(t3_`j', replace) xtitle("Days after transplanting")
}

*combine graph
graph combine t1_1 t1_0 t2_1 t2_0 t3_1 t3_0, scheme(s1color) rows(3) xsize(9) ysize(12) scale(0.9) 
graph export "${latex}figure3_waterlevel.pdf", as(pdf) replace

erase "${latex}t1_0.pdf"
erase "${latex}t1_1.pdf"
erase "${latex}t2_0.pdf"
erase "${latex}t2_1.pdf"
}

/**********************************
		FIGURE 4
***********************************/
{
********************************************************************************
*COMPARISON BETWEEN IMPACTS FROM THE RCT AND AGRONOMIC EXPERIMENTS (FIGURE 3) 
********************************************************************************
loc per_vol = -18.3 /*FLAG table 1 column 3*/
loc per_novol = 5.12 /*FLAG table 1 column 3*/
use "data/input_secondary/AgronomicLiterature.dta", clear
label var DifferenceinWateruse "Effect of AWD on Irrigation %"
twoway (kdensity DifferenceinWateruse, lcolor(gs10) lwidth(thick)) , legend(off) scheme(s1color) ///
xline(`per_vol', lwidth(thick) lcolor(blue)) xline(`per_novol', lwidth(thick) lcolor(black)) xtitle("Effect of AWD on Irrigation in %") ytitle(Density) ///
xlabel(-60 -40 -20 0 10) text(0.005 -40 "volumetric price", place(e) color(blue) size(medlarge)) ///
text(0.027 -16 "area-based price", place(e) color(black) size(medlarge))
graph export "${latex}figure4_kernel_agrontrial_exp.pdf", as(pdf) replace
sum DifferenceinWateruse, detail /*RCT at the 25th percentile of experimental estimates*/
}
/**********************************
		FIGURE 5 - FIGURE A3
***********************************/
{
use "data/input_primary/rct2_usage.dta", clear
merge 1:1 farmer_id using "data/input_primary/rct2_pipesales.dta", keepusing(farmer_id adoption) /*merge in pipe sales outcome*/
drop _merge
merge 1:1 farmer_id using "data/input_primary/rct2_baseline.dta", keepusing(farmer_id  waterprice waterirrig waterwhopay nadults nyoung)
drop _merge
gen logprice = ln(price)
gen cardtreat = (treatment=="card")
gen cardtreat_price = cardtreat*price
gen cardtreat_logprice = cardtreat*logprice 
gen awd_use = (awdirri==1 | awdoth==1)
gen dpaba = (Upazila=="PABA") /*creating upazila dummies, mostly to scale graph*/
gen dtanore = (Upazila=="TANORE")
foreach j in paba tanore { /*demean upazila dummies to scale graph correctly*/
sum d`j'
gen d`j'_d = d`j'-r(mean)
}

*linear demand purchasing, interaction  
gen pvalue=. 
xi: reg adoption cardtreat price cardtreat_price dpaba_d dtanore_d, vce(cl village_id) 
di -_b[_cons]/_b[price] /*choke price in control*/
di (-_b[_cons]-_b[cardtreat])/(_b[price]+_b[cardtreat_price]) /*choke price in treatment*/
foreach i in 20 30 40 50 60 70 80 90 {
di _b[cardtreat]+`i'*_b[cardtreat_price]
test cardtreat + `i'*cardtreat_price = 0 
replace pvalue = r(p) if price==`i' 
}
sum adoption if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
estadd sca epst=(_b[price]+_b[cardtreat_price])*(55)/(_b[_cons]+_b[cardtreat]+_b[price]*55+_b[cardtreat_price]*55) /*demand elasticity at price of 55 taka, treatment*/ 
estadd sca epsc=_b[price]*(55)/(_b[_cons]+_b[price]*55) /*demand elasticity at price of 55 taka, control*/ 
nlcom ((_b[price]+_b[cardtreat_price])*(55)/(_b[_cons]+_b[cardtreat]+_b[price]*55+_b[cardtreat_price]*55)) - (_b[price]*(55)/(_b[_cons]+_b[price]*55))
mat K=r(V)
mat B=r(b) 
mat D=r(N)
loc st = B[1,1]/sqrt(K[1,1])
loc dfree=D[1,1]
estadd sca pelas = 2*(1-t(`dfree',`st'))
estimates store lindem_i

*linear demand using, interaction  
gen pvalue_u=. 
xi: reg awd_use cardtreat price cardtreat_price dpaba_d dtanore_d, vce(cl village_id) 
di -_b[_cons]/_b[price] /*choke price in control*/
di (-_b[_cons]-_b[cardtreat])/(_b[price]+_b[cardtreat_price]) /*choke price in treatment*/
foreach i in 20 30 40 50 60 70 80 90 {
di _b[cardtreat]+`i'*_b[cardtreat_price]
test cardtreat + `i'*cardtreat_price = 0 
replace pvalue_u = r(p) if price==`i' 
}
sum awd_use if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
estadd sca epst=(_b[price]+_b[cardtreat_price])*(55)/(_b[_cons]+_b[cardtreat]+_b[price]*55+_b[cardtreat_price]*55) /*demand elasticity at price of 55 taka, treatment*/ 
estadd sca epsc=_b[price]*(55)/(_b[_cons]+_b[price]*55) /*demand elasticity at price of 55 taka, control*/ 
nlcom ((_b[price]+_b[cardtreat_price])*(55)/(_b[_cons]+_b[cardtreat]+_b[price]*55+_b[cardtreat_price]*55)) - (_b[price]*(55)/(_b[_cons]+_b[price]*55))
mat K=r(V)
mat B=r(b) 
mat D=r(N)
loc st = B[1,1]/sqrt(K[1,1])
loc dfree=D[1,1]
estadd sca pelas = 2*(1-t(`dfree',`st'))
estimates store lindem_i_use

*preserve 
xi: reg adoption cardtreat price cardtreat_price dpaba_d dtanore_d, vce(cl village_id) 
estimate store lindem_i_gph 
xi: reg awd_use cardtreat price cardtreat_price dpaba_d dtanore_d, vce(cl village_id) 
estimate store lindem_i_use_gph 
replace dpaba_d=0 /*trick to use predict command and make it for average farm*/
replace dtanore_d=0 /*trick to use predict command and make it for average farm*/
estimates restore lindem_i_gph
predict ahat, xb
estimates restore lindem_i_use_gph 
predict ahat_use, xb
collapse (mean) ahat adoption pvalue ahat_use awd_use pvalue_u, by(cardtreat price)
gen asterisk="" 
replace asterisk="*" if pvalue>0.05 & pvalue<=0.1
replace asterisk="**" if pvalue<=0.05 & pvalue>0.01 
replace asterisk="***" if pvalue<=0.01
gen asterisk_u="" 
replace asterisk_u="*" if pvalue_u>0.05 & pvalue_u<=0.1
replace asterisk_u="**" if pvalue_u<=0.05 & pvalue_u>0.01 
replace asterisk_u="***" if pvalue_u<=0.01
gen labspot = ahat+0.05 /*trick to make graph look better*/

*Figure 5 Demand curve for conservation technology by hourly card treatment 
twoway (line price ahat if cardtreat==1, lcolor(blue) lwidth(vthick)) (line price ahat if cardtreat==0, lcolor(gs13) lwidth(vthick)) ///
(scatter price labspot if cardtreat==1, msymbol(none) mlabel(asterisk) mlabsize(large) mlabcolor(black)) ///
(scatter price adoption if cardtreat==1, mcolor(blue) msize(medlarge) msymbol(circle_hollow)) (scatter price adoption if cardtreat==0, mcolor(gs13) msize(medlarge) msymbol(circle_hollow)), ///
scheme(s1color) legend(off) xtitle(Share Purchasing, size(large)) ytitle(Price, size(large)) xlabel(,labsize(large)) ylabel(,labsize(large)) ///
text(90 0.6 "Treatment", place(e) color(blue) size(medlarge)) text(85 0.6 "Control", place(e) color(gs13) size(medlarge))
graph export "${latex}figure5_demandcurve_newvillages.pdf", as(pdf) replace

*Figure A3 Usage of conservation technology as a function of price and hourly card treatment
twoway (line price ahat if cardtreat==1, lcolor(blue) lwidth(vthick)) (line price ahat if cardtreat==0, lcolor(gs13) lwidth(vthick)) ///
(scatter price ahat if cardtreat==1, msymbol(none)) ///
(line price ahat_use if cardtreat==1, lcolor(blue) lwidth(thick) lpattern(shortdash)) (line price ahat_use if cardtreat==0, lcolor(gs13) lwidth(thick) lpattern(shortdash)) ///
(scatter price ahat_use if cardtreat==1, msymbol(none)), ///
scheme(s1color) legend(off) xtitle(Purchase or Use, size(large)) ytitle(Price, size(large)) xlabel(,labsize(large)) ylabel(,labsize(large)) ///
text(90 0.6 "Treatment", place(e) color(blue) size(medlarge)) text(85 0.6 "Control", place(e) color(gs13) size(medlarge)) text(40 0.65 "Purchase", place(e) color(black) size(medlarge)) ///
text(30 0.18 "Use", place(e) color(black) size(medlarge))
graph export "${latex}figureA3_combine_demand_use.pdf", as(pdf) replace
}

/*******************************************************************************************
		TABLE A8- TABLE A9 - TABLE A10 - TABLE A11 - TABLE A12 - TABLE A13  
********************************************************************************************/
{
******************** self-reported water use *************************
******************************************************************************************************************
*RCT 1 ANALYSIS FROM FOLLOW UP SURVEY
******************************************************************************************************************
******************** input use *************************
use "data/input_primary/rct1_followup.dta", clear
merge 1:1 farmer_id using "data/input_primary/rct1_baseline.dta", keepusing(farmer_id)
drop if _merge==2 /*drop farmers not in baseline*/
drop _merge

*generate fertilizer expenditures per bigah 
gen purea=16 /*local prices from that season*/
gen ptsp=22
gen pmop=14 
gen pother=17.3333333
foreach j in urea tsp mop other { 
egen a_kg_`j' = rowtotal(a_dose1_`j' a_dose2_`j' a_dose3_`j' a_dose4_`j' a_dose5_`j')
gen a_spend_`j' = a_kg_`j'*p`j'
gen a_spendbg_`j' = a_spend_`j' / a_areacult_bg
}
gen totalfert_bg = a_spendbg_urea+a_spendbg_tsp+a_spendbg_mop+a_spendbg_other
order a_kg_* a_spend*, after(a_dose5_other)
*generate family labor per bigah 
gen a_famhar_bg = (a_famhar*a_harwage)/a_areacult_bg
gen a_famplant_bg = (a_famplant*a_plantwage)/a_areacult_bg 
egen weed_wage = rowmean(a_plantwage a_harwage) 
gen a_famweed_bg = (a_famweed*weed_wage)/a_areacult_bg 
*generate interaction term between treatment and volumetric prices 
gen t_marg = anymarginal*treatment  
*convert everything to per acre (for making units consistent across tables) 
foreach i in a_spendbg_urea a_spendbg_tsp a_spendbg_mop a_spendbg_other a_pesticides_bg a_herbicides_bg a_plantspend_bg a_weedspend_bg a_harspend_bg a_famplant_bg a_famhar_bg a_famweed_bg a_watercost totalfert_bg { 
replace `i'=`i'*3
}
*run regressions for material and labor inputs
foreach j in a_doses a_spendbg_urea a_spendbg_tsp a_spendbg_mop a_spendbg_other a_pesticides_bg a_herbicides_bg a_plantspend_bg a_weedspend_bg a_harspend_bg a_famplant_bg a_famhar_bg a_famweed_bg totalfert_bg { 
xi: reg `j' treatment i.Upazila, vce(cl village_id) 
sum `j' if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store `j'

xi: reg `j' treatment t_marg anymarginal i.Upazila, vce(cl village_id) 
sum `j' if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment+t_marg=0 
estadd sca phet=r(p)
estimates store `j'_het
}
*make tables 
label var anymarginal "Volumetric Pricing" 
label var treatment "Treatment" 
label var t_marg "Treatment * Volumetric Pricing" 

gen loga_watercost = ln(a_watercost)
*run regressions 
foreach j in a_irrigations a_timedry a_watercost loga_watercost { 
xi: reg `j' treatment i.Upazila, vce(cl village_id) 
sum `j' if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store `j'

xi: reg `j' treatment t_marg anymarginal i.Upazila, vce(cl village_id) 
sum `j' if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment+t_marg=0 
estadd sca phet=r(p)
estimates store `j'_het
}

*make table: Effects on self-reported water use (Table A8)
esttab a_irrigations a_irrigations_het a_timedry a_timedry_het using "${latex}tableA8_self_report_wateruse.tex", ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control" "phet p-Value: Treat+Treat*Volumetric" "N Number of Observations" "r2 R squared") sfmt(%10.2f %10.3f %10.0f %10.3f) ///
nomtitles align(cccc) order(treatment t_marg anymarginal) ///
nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = _IUpa*") ///
mgroups("\multicolumn{2}{c}{Number Irrigations} & \multicolumn{2}{c}{Times Drained} \\ \cline{2-3} \cline{4-5}", pattern(1 0 1 0))

*Effects on Material Input Expenditure (Table A9)
esttab a_doses a_spendbg_urea a_spendbg_tsp a_spendbg_mop a_spendbg_other a_pesticides_bg a_herbicides_bg using "${latex}tableA9_materialinputs.tex", ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control" "N Number of Observations" "r2 R squared") sfmt(%10.2f %10.0f %10.3f) ///
mtitles ("N apps" "Urea" "TSP" "Potash" "Other" "Pesticide" "Herbicide") align(ccccccc) width(\hsize) order(treatment) ///
nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = _IUpa*") ///
mgroups("\multicolumn{5}{c}{Fertilizer} & \multicolumn{2}{c}{Chemicals} \\ \cline{2-6} \cline{7-8}", pattern(1 0 0 0 0 1 0))

*Effects on Labor Expenditure (Table A10) 
esttab a_plantspend_bg a_weedspend_bg a_harspend_bg a_famplant_bg a_famweed_bg a_famhar_bg using "${latex}tableA10_laborinputs.tex", ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control" "N Number of Observations" "r2 R squared") sfmt(%10.2f %10.0f %10.3f) ///
mtitles ("Plant" "Weed" "Harvest" "Plant" "Weed" "Harvest") align(cccccc) width(\hsize) order(treatment) ///
nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = _IUpa*") ///
mgroups("\multicolumn{3}{c}{Hired} & \multicolumn{3}{c}{Family} \\ \cline{2-4} \cline{5-7}", pattern(1 0 0 1 0 0))

*Heterogeneous effects on material input expenditure (Table A11) 
esttab a_doses_het a_spendbg_urea_het a_spendbg_tsp_het a_spendbg_mop_het a_spendbg_other_het a_pesticides_bg_het a_herbicides_bg_het using "${latex}tableA11_materialinputs_het.tex", ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control" "phet p-Value: Treat+Treat*Volumetric" "N Number of Observations" "r2 R squared") sfmt(%10.2f %10.3f %10.0f %10.3f) ///
mtitles ("N apps" "Urea" "TSP" "Potash" "Other" "Pesticide" "Herbicide") align(ccccccc) width(\hsize) ///
nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = _IUpa*") order(treatment t_marg anymarginal) ///
mgroups("\multicolumn{5}{c}{Fertilizer} & \multicolumn{2}{c}{Chemicals} \\ \cline{2-6} \cline{7-8}", pattern(1 0 0 0 0 1 0))

*Heterogeneous effects on labor expenditure (Table A12)
esttab a_plantspend_bg_het a_weedspend_bg_het a_harspend_bg_het a_famplant_bg_het a_famweed_bg_het a_famhar_bg_het using "${latex}tableA12_laborinputs_het.tex", ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control" "phet p-Value: Treat+Treat*Volumetric" "N Number of Observations" "r2 R squared") sfmt(%10.2f %10.3f %10.0f %10.3f) ///
mtitles ("Plant" "Weed" "Harvest" "Plant" "Weed" "Harvest") align(cccccc) width(\hsize) ///
nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = _IUpa*") order(treatment t_marg anymarginal) ///
mgroups("\multicolumn{3}{c}{Hired} & \multicolumn{3}{c}{Family} \\ \cline{2-4} \cline{5-7}", pattern(1 0 0 1 0 0))

******************** revenues and profits ****************************
*mean fill on price 
gen a_price_per_kg=a_price*(1/40)
sum a_price, detail
replace a_price = r(mean) if a_price==.
*generate variables 
gen a_yield_kgac = a_yield*120 /*convert yield from monn per bigha to kg per acre*/
gen a_rev_ac = a_yield*a_price*3 /*calculate revenue per acre (yield in monn per bigha, price in taka per monn, and multiply by bigha per acre)*/
egen a_cost_ac = rowtotal(a_spendbg_urea a_spendbg_tsp a_spendbg_mop a_spendbg_other a_pesticides_bg a_herbicides_bg a_plantspend_bg a_weedspend_bg a_harspend_bg a_famplant_bg a_famhar_bg a_famweed_bg a_watercost) /*total cost per acre*/
gen a_profit_ac = a_rev_ac - a_cost_ac 
gen a_nonwater_ac = a_cost_ac - a_watercost 
foreach i in a_yield_kgac a_rev_ac a_profit_ac a_nonwater_ac { 
gen log_`i' = ln(`i')
}

*run regressions 
foreach j in a_yield_kgac a_rev_ac a_cost_ac a_profit_ac log_a_yield_kgac log_a_rev_ac log_a_profit_ac a_price_per_kg a_nonwater_ac log_a_nonwater_ac { 
xi: reg `j' treatment i.Upazila, vce(cl village_id) 
sum `j' if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
estimates store `j'

xi: reg `j' treatment t_marg anymarginal i.Upazila, vce(cl village_id) 
sum `j' if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment+t_marg=0 
estadd sca phet=r(p)
estimates store `j'_het
}

*trimmed profits 
cumul a_profit_ac, gen(a_profit_ac_hat)
gen trim = a_profit_ac if a_profit_ac_hat >= 0.015 & a_profit_ac_hat <= 0.985
gen stupid=1 /*put the constant in the regression manually just for making the table easier*/
xi: reg trim treatment anymarginal t_marg stupid i.Upazila, vce(cl village_id) nocons
sum trim if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment + t_marg = 0
estadd sca phet=r(p)
estimates store pi_trim

gen log_trim = log(trim)
xi: reg log_trim treatment anymarginal t_marg stupid i.Upazila, vce(cl village_id) nocons
sum log_trim if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment + t_marg = 0
estadd sca phet=r(p)
estimates store logpi_trim

*profits with controls 
merge 1:1 farmer_id using "data/derived/baseline_covariates.dta", keepusing(farmer_id age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1) 
drop if _merge==2 
drop _merge

merge 1:1 farmer_id using "data/input_secondary/GIS_covariates.dta", keepusing(farmer_id srtm_eleva soilclay soilsand soilcarbon soilwater) 
drop if _merge==2 
drop _merge

foreach k in age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1 srtm_eleva soilclay soilsand soilcarbon soilwater { 
rename `k' bc_`k'
sum bc_`k', detail 
gen dmean_`k' = bc_`k' - r(mean) 
gen t_dmean_`k' = treatment*dmean_`k'
}

xi: reg a_profit_ac treatment anymarginal t_marg bc_* i.Upazila, vce(cl village_id)
sum a_profit_ac if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment + t_marg = 0
estadd sca phet=r(p)
estimates store pi_xbeta

xi: reg log_a_profit_ac treatment anymarginal t_marg bc_* i.Upazila, vce(cl village_id)
sum log_a_profit_ac if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment + t_marg = 0
estadd sca phet=r(p)
estimates store logpi_xbeta

*trimmed profits with controls 
xi: reg trim treatment anymarginal t_marg bc_* i.Upazila, vce(cl village_id)
sum trim if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment + t_marg = 0
estadd sca phet=r(p)
estimates store pi_trim_xbeta

xi: reg log_trim treatment anymarginal t_marg bc_* i.Upazila, vce(cl village_id)
sum log_trim if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment + t_marg = 0
estadd sca phet=r(p)
estimates store logpi_trim_xbeta

*profits with controls interacted with treatment 
xi: reg a_profit_ac treatment anymarginal t_marg bc_* t_dmean_* i.Upazila, vce(cl village_id)
sum a_profit_ac if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment + t_marg = 0
estadd sca phet=r(p)
estimates store pi_xbeta_inter

xi: reg log_a_profit_ac treatment anymarginal t_marg bc_* t_dmean_* i.Upazila, vce(cl village_id)
sum log_a_profit_ac if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment + t_marg = 0
estadd sca phet=r(p)
estimates store logpi_xbeta_inter


*MAKE TABLES FOR REVENUES AND PROFITS (Table A13)
*Effects of conservation technology on costs, revenues, and profits (Table A13)
esttab a_yield_kgac a_rev_ac a_profit_ac log_a_yield_kgac log_a_rev_ac log_a_profit_ac using "${latex}tableA13_rev_profit.tex", ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control" "N Number of Observations" "r2 R squared") sfmt(%10.2f %10.0f %10.3f) ///
mtitles ("Yield" "Revenue" "Profit" "Yield" "Revenue" "Profit") align(cccccc) width(\hsize) ///
nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = _IUpa*") order(treatment) ///
mgroups("&&& \multicolumn{3}{c}{Log:} \\ \cline{5-7}", pattern(1 0 0 1 0 0))
}


/**********************************
		TABLE A14 
***********************************/
estimates clear /*start with cleared estimates to avoid hitting Stata limit of 300*/
{
*****************************************************************************************************************
*RCT 1 ANALYSIS FROM FOLLOW UP SURVEY ON NON-STUDY PLOT (PLOT B IN THE SURVEY)
************************************************************************************************************************************** input use *************************
use "data/input_primary/rct1_followup.dta", clear
merge 1:1 farmer_id using "data/input_primary/rct1_baseline.dta", keepusing(farmer_id)
drop if _merge==2 /*drop farmers not in baseline*/
drop _merge

*generate fertilizer expenditures per bigah 
gen purea=16 /*local prices from that season*/
gen ptsp=22
gen pmop=14 
gen pother=17.3333333
foreach j in urea tsp mop other { 
egen b_kg_`j' = rowtotal(b_dose1_`j' b_dose2_`j' b_dose3_`j' b_dose4_`j' b_dose5_`j')
gen b_spend_`j' = b_kg_`j'*p`j'
gen b_spendbg_`j' = b_spend_`j' / b_areacult_bg
}
gen totalfert_bg = b_spendbg_urea+b_spendbg_tsp+b_spendbg_mop+b_spendbg_other
order b_kg_* b_spend*, after(b_dose5_other)
*generate family labor per bigah 
gen b_famhar_bg = (b_famhar*a_harwage)/b_areacult_bg
gen b_famplant_bg = (b_famplant*a_plantwage)/b_areacult_bg 
egen weed_wage = rowmean(a_plantwage a_harwage) 
gen b_famweed_bg = (b_famweed*weed_wage)/b_areacult_bg 
*generate interaction term between treatment and volumetric prices 
gen t_marg = anymarginal*treatment  
*convert everything to per acre (for making units consistent across tables) 
foreach i in b_spendbg_urea b_spendbg_tsp b_spendbg_mop b_spendbg_other b_pesticides_bg b_herbicides_bg b_plantspend_bg b_weedspend_bg b_harspend_bg b_famplant_bg b_famhar_bg b_famweed_bg b_watercost totalfert_bg { 
replace `i'=`i'*3
}
*mean fill on price 
gen b_price_per_kg=b_price*(1/40)
sum b_price, detail
replace b_price = r(mean) if b_price==.
*generate variables 
gen b_yield_kgac = b_yield*120 /*convert yield from monn per bigha to kg per acre*/
gen b_rev_ac = b_yield*b_price*3 /*calculate revenue per acre (yield in monn per bigha, price in taka per monn, and multiply by bigha per acre)*/
egen b_cost_ac = rowtotal(b_spendbg_urea b_spendbg_tsp b_spendbg_mop b_spendbg_other b_pesticides_bg b_herbicides_bg b_plantspend_bg b_weedspend_bg b_harspend_bg b_famplant_bg b_famhar_bg b_famweed_bg b_watercost) /*total cost per acre*/
replace b_cost_ac = . if b_yield_kgac==.
gen b_profit_ac = b_rev_ac - b_cost_ac 
gen b_nonwater_ac = b_cost_ac - b_watercost 
foreach i in b_yield_kgac b_rev_ac b_profit_ac b_nonwater_ac { 
gen log_`i' = ln(`i')
}
gen no_b = (b_yield_kgac==.)


*run regressions for material and labor inputs
foreach j in b_doses b_spendbg_urea b_spendbg_tsp b_spendbg_mop b_spendbg_other b_pesticides_bg b_herbicides_bg b_plantspend_bg b_weedspend_bg b_harspend_bg b_famplant_bg b_famhar_bg b_famweed_bg totalfert_bg b_watercost b_irrigations b_timedry b_yield_kgac b_rev_ac b_cost_ac b_nonwater_ac b_profit_ac no_b { 

xi: reg `j' treatment t_marg anymarginal i.Upazila, vce(cl village_id) 
sum `j' if treatment==0 & e(sample)==1
estadd sca ym = r(mean)
test treatment+t_marg=0 
estadd sca phet=r(p)
estimates store `j'_het
}


*make tables 
label var anymarginal "Volumetric Pricing" 
label var treatment "Treatment" 
label var t_marg "Treatment * Volumetric Pricing" 

*Table A14 Treatment effects on a randomly selected non-study plot
esttab b_profit_ac_het b_rev_ac_het b_watercost_het b_nonwater_ac_het using "${latex}tableA14_plotB_basic_result.tex", ///
replace b(%10.3f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control" "phet p-Value: Treat+Treat*Volumetric" "N Number of Observations" "r2 R squared") sfmt(%10.2f %10.3f %10.0f %10.3f) ///
mtitles ("Profit" "Revenue" "Water Cost" "Other Input Cost") align(cccc) width(\hsize) order(treatment t_marg anymarginal) ///
nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = _IUpa*") 
}

/********************************************************************
		TABLE A15: REPLICATION IS IN FILE REPLICATE_RESULTS_A15.DO
*********************************************************************/

/**********************************
		TABLE A16
***********************************/
{
*******************************************************************************************************************RCT 2 BALANCE TABLE (TABLE A16 BALANCE OF BASELINE CHARACTERISTICS FOR VOLUMETRIC PRICING EXPERIMENT)
*****************************************************************************************************************
use "data/input_primary/rct2_baseline.dta", clear 

*generate variables 
gen hhsize = nadults+nyoung
gen landhold = totarea_bg/3
foreach i in 1 2 3 4 5 6 7 8 9 { 
gen assetheld_`i' = (regex(assets,"`i'")>0 & regex(assets,"10")==0)
}
gen paydeep = (waterwhopay==1) 
replace paydeep=. if waterwhopay==.
gen cardtreat = (treatment=="card")

*run balance test 
loc nvar=11
loc vlist "age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 waterprice waterirrig paydeep" 
*set up vectors for results 
foreach k in c1 c2 p s1 s2 {  
mat `k'=J(1,`nvar',0)
mat colnames `k' = `vlist'
}
*run regressions and store means, and p values in those vectors 
loc k=1
foreach var in `vlist' { 
xi: reg `var' cardtreat i.Upazila, vce(cl village_id) 
test cardtreat
mat p[1,`k']=r(p)
sum `var' if cardtreat==0  & e(sample)==1 
mat c1[1,`k']=r(mean)
mat s1[1,`k']=r(sd)
sum `var' if cardtreat==1  & e(sample)==1 
mat c2[1,`k']=r(mean)
mat s2[1,`k']=r(sd)

loc k = `k'+1 /*add to counter*/
}

*run espost to make table look better
foreach j in c1 c2 p {
estpost ttest `vlist', by(cardtreat)
matrix rename `j' kyl, replace
estadd matrix kyl, replace /*overwrite existing e(p)*/ 
estimates store col_`j'
}
foreach k in 1 2 { 
estimates restore col_c`k' 
matrix rename s`k' sdev, replace 
estadd matrix sdev, replace 
estimates store col_c`k'
}
*label variables better
label var age "Age" 
label var edu "Years Education" 
label var hhsize "Household Size" 
label var livestock "Number Livestock Owned" 
label var landhold "Landholdings in Acres" 
label var assetheld_1 "Owns Television"
label var assetheld_5 "Owns Refrigerator"
label var assetheld_9 "Owns Irrigation Shallow Tubewell"
label var waterprice "Seasonal Water Price (taka per bigah)" 
label var waterirrig "Usual Number Irrigations"
label var paydeep "Pays Deep Driver for Irrigation" 

*export table A16 Balance of baseline characteristics for volumetric pricing experiment
esttab col_c1 col_c2 col_p using "${latex}tableA16_phase2card_balance.tex", cells("kyl(fmt(a3))" "sdev(par)") notes replace ///
noobs mlabels("Control" "Hourly Card" "p-value") nonumbers collabels(,none) label alignment(ccc) ///
gaps width(\hsize) mgroups("\multicolumn{2}{c}{Means} \\ \cline{2-3} ", pattern(1 1 0))
}

/**********************************
		TABLE A17 - FIGURE A2
***********************************/
{
****************************************************************************************************
*RCT 2 DATA ON CARD USAGE FROM PABA UPAZILA

*FIGURE A2: CORRELATION BETWEEN CARD USAGE AND WATER LEVELS
*TABLE A17: COMPLIER CHARACTERISTICS FOR  PREPAID CARD USAGE
******************************************************************************************************************
*collapse data on card reloading 
use "data/input_secondary/rct2_cardreload_paba.dta", clear
gen n=1
collapse (mean) Chargemoney (sum) sum_recharge = Charge n, by(CardNo farmer_id)
*merge in water usage 
merge 1:1 farmer_id using "data/input_primary/rct2_usage.dta"
drop if inlist(upazila,5,12) | treatment=="mobile load"
drop _merge
*merge in baseline characteristics 
merge 1:1 farmer_id using  "data/input_primary/rct2_baseline.dta", keepusing(farmer_id  waterprice waterirrig waterwhopay assets nplot boroarea_bg totarea_bg age edu livestock nadults nyoung)
drop if _merge==2
drop _merge
*generate variables
gen usecard = (n>0 & n!=.)
gen water_amt = waterlevel_c_cm 
replace water_amt = waterlevel_f_cm if waterlevel_f_cm!=.
gen dryfield = (water_amt==0)
gen television = regexm(assets, "1 ")
gen motorbike = regexm(assets, "2")
gen toilet = regexm(assets, "3")
gen fan = regexm(assets, "4")
gen fridge = regexm(assets, "5")
gen washingmachine = regexm(assets, "6")
gen electricity = regexm(assets, "7")
gen tractor = regexm(assets, "8")
gen shallow = regexm(assets, "9")
gen nassets = television + motorbike + toilet + fan + fridge + washingmachine + electricity + tractor + shallow
gen young = age<40 
gen highirrig = waterirrig>30 
gen highprice = waterprice>=2000
gen highland = boroarea_bg>=4
*table with "complier characteristics" of who uses card
gen z=.
gen fs = . 
loc j = 1
foreach k in young highirrig highprice highland {
sum `k', detail
replace z = r(mean) in `j' /*share of sample with characteristic==1*/
sum usecard if `k'==1 
replace fs = r(mean) in `j' /*share with characteristic==1 that used prepaid card*/
loc j = `j'+1
}
sum usecard 
gen fsratio = fs / r(mean) /*ratio of share with characteristic==1 that used card to overall share used card*/

preserve
drop if z==.
mkmat z fs fsratio, matrix(finalres)
label var young "Younger than 40" 
label var highirrig "More than 30 irrigations" 
label var highprice "Water Price at least 2000 BDT" 
label var highland "Dry-season area 1.33 acres or more" 
matrix rownames finalres = young highirrig highprice highland

frmttable using "${latex}tableA17_complier_chars", statmat(finalres) sdec(3,3,3) ctitle("","Share of Sample","Share that", "Ratio of Usage to" \ "","w/ Characteristic","Used Card","Overall Usage") ///
varlabels tex fra replace 
restore
drop z

*make graph to display correlation between water use and using prepaid card
gen dat_bin = . 
forvalues q=10(10)180 { 
replace dat_bin = `q'-5 if dat>=`q'-10 & dat<`q'
}
egen m_water_amt = mean(water_amt), by(dat_bin usecard) 
egen m_dryfield = mean(dryfield), by(dat_bin usecard)
*Fan regressions 
fanreg dryfield dat if usecard==1, xgen(xcard) ygen(ycard)
fanreg dryfield dat if usecard==0, xgen(xn) ygen(yn)
twoway (line ycard xcard if xcard<=90, lcolor(blue) lwidth(thick)) (line yn xn if xn<=90, lcolor(black) lwidth(thick)) ///
(scatter m_dryfield dat_bin if usecard==1 & dat_bin<=90, mcolor(blue)) (scatter m_dryfield dat_bin if usecard==0 & dat_bin<=90, mcolor(black)), ///
xlabel(10 20 30 40 50 60 70 80 90) scheme(s1color) ytitle(Field is dry) xtitle() legend(off) ///
text(0.75 20 "User", place(e) color(blue) size(medlarge)) text(0.71 20 "Nonuser", place(e) color(black) size(medlarge)) name(df, replace) 

drop xcard ycard xn yn
fanreg water_amt dat if usecard==1, xgen(xcard) ygen(ycard)
fanreg water_amt dat if usecard==0, xgen(xn) ygen(yn)
twoway (line ycard xcard if xcard<=90, lcolor(blue) lwidth(thick)) (line yn xn if xn<=90, lcolor(black) lwidth(thick)) ///
(scatter m_water_amt dat_bin if usecard==1 & dat_bin<=90, mcolor(blue)) (scatter m_water_amt dat_bin if usecard==0 & dat_bin<=90, mcolor(black)), ///
xlabel(10 20 30 40 50 60 70 80 90) scheme(s1color) ytitle(Water level) xtitle() legend(off) ///
text(3.5 70 "User", place(e) color(blue) size(medlarge)) text(3.32 70 "Nonuser", place(e) color(black) size(medlarge)) name(wl, replace) 

histogram dat if dat<=90, xlabel(10 20 30 40 50 60 70 80 90) fysize(30) scheme(s1color) lcolor(gs4) fcolor(gs6) name(histo, replace) xtitle("Days after transplanting")

graph combine df wl histo histo, scheme(s1color) rows(3) xsize(8) ysize(6) scale(0.9) 
graph export "${latex}figureA2_carduse_water_paba.pdf", as(pdf) replace
}


/**********************************
	FIGURE B1 
***********************************/
{
*****************************************************CORRELATION BETWEEN DAILY PUMPING AND WITH WATER USAGE ON THE FIELD
***************************************************/
use "data/input_primary/rct2_dailyusagefarmer.dta", clear
preserve 
use "data/input_primary/rct2_usage.dta", clear
keep farmer_id village_id Upazila waterlevel_c_cm waterlevel_f_cm waterunit visit_day
tempfile ob_level 
save "`ob_level'", replace
restore 

merge n:1 farmer_id visit_day using "`ob_level'" 
sort farmer_id visit_day 
format visit_day %td
order farmer_id visit_day pump_start pump_end Chargemoney AccumulativeTime waterlevel_c_cm waterlevel_f_cm
br if n_pumps>0 & n_pumps!=.

keep if n_pumps>0 & n_pumps!=.
xtset farmer_id visit_day
replace AccumulativeTime=AccumulativeTime/60 /*in hours*/
forvalues j=1(1)10 { 
gen ltime`j' = l`j'.AccumulativeTime
}

gen waterlevel_comp = waterlevel_c_cm 
replace waterlevel_comp = waterlevel_f_cm if waterlevel_c_cm==. /*water level, either far or close*/
xi: reg waterlevel_comp ltime* if visit_day>=pump_start & visit_day<=pump_end, vce(cl village_id)
regsave ltime* using "data/derived/pump_meas_val.dta", replace

*Figure B1: Relationship between observed water levels and daily pumping hours
use "data/derived/pump_meas_val.dta", clear
gen day_before = substr(var,6,2)
destring day_before, replace
gen clow = coef - 1.96*stderr 
gen chigh = coef + 1.96*stderr

twoway (rspike clow chigh day_before, lcolor(black) lwidth(thick)) (scatter coef day_before, mcolor(black) msize(vlarge)), ///
legend(off) scheme(s1color) ytitle("Coefficient", size(medlarge)) xtitle("Days Before Water Observation", size(medlarge)) ///
name(right, replace) yline(0, lcolor(gs7) lpattern(dash)) xlabel(1 2 3 4 5 6 7 8 9 10)
graph export "${latex}figureB1_check_minutes_wlevel.pdf", as(pdf) replace 

}
/**********************************
	TABLE C1-C2
***********************************/
{
use "data/input_primary/rct1_followup.dta", clear
merge 1:1 farmer_id using "data/input_primary/rct1_baseline.dta", keepusing(farmer_id)
drop if _merge==2 /*drop farmers not in baseline*/
drop _merge

*baseline HH covariates
merge 1:1 farmer_id using "data/derived/baseline_covariates.dta", keepusing(farmer_id age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1) 
drop if _merge==2 
drop _merge
*GIS covariates
merge 1:1 farmer_id using "data/input_secondary/GIS_covariates.dta", keepusing(farmer_id srtm_eleva soilclay soilsand soilcarbon soilwater) 
drop if _merge==2 
drop _merge
*generate dummy for having own card
gen hourcard = (a_paymethod==1)
*label variables better
label var age "Age" 
label var edu "Years Education" 
label var hhsize "Household Size" 
label var livestock "Number Livestock Owned" 
label var landhold "Landholdings in Acres" 
label var knowawd "Heard of AWD?" 
label var renter "Plot is Rented or Sharecropped" 
label var area1_acre "Area in Acres" 
label var anymarginal "Volumetric Water Price" 
label var ncrop1 "Number Crops Grown" 
label var rice_rice_sys "Rice-Rice Cropping System" 
label var boro_rvirrig_1 "Number Irrigations in Boro" 
label var boro_revperacre_1 "Revenue per Acre in Boro" 
label var boro_tcost_1 "Cost per Acre in Boro" 
label var boro_watercostpacre_1 "Water Cost per Acre in Boro" 
label var aman_peracre_1 "Revenue per Acre in Aman"
label var assetheld_1 "Owns Television"
label var assetheld_5 "Owns Refrigerator"
label var assetheld_9 "Owns Irrigation Shallow Tubewell"
label var srtm_eleva "Elevation (m)"
label var soilclay "Soil Clay Content (\%) at 10 cm"
label var soilsand "Soil Sand Content (\%) at 10 cm"
label var soilcarbon "Soil Carbon Content at 10cm (g/kg)"
label var soilwater "Soil Water Content (\%) at 10 cm"

loc nvar=23
loc vlist "age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1 srtm_eleva soilclay soilsand soilcarbon soilwater"
/*local all the dependent variables as models*/
forvalues i=1(1)2 { 
mat b`i' = J(1,`nvar',0)
mat s`i' = J(1,`nvar',0)
mat beta`i' = J(1,`nvar',0)
mat serror`i' = J(1,`nvar',0)
mat bf`i' = J(1,`nvar',0)
mat sf`i' = J(1,`nvar',0)
mat betaf`i' = J(1,`nvar',0)
mat serrorf`i' = J(1,`nvar',0)
}/*set up matrix for result report*/
loc j=0

foreach i of local vlist { 
loc j=`j'+1
*characteristics on having own prepaid card
xi: reg `i' hourcard if a_paymethod!=., vce(cl village_id) 
mat b1[1,`j']=_b[hourcard] /*save coefficient of hourcard*/
mat s1[1,`j']=_se[hourcard] /*save standard error of hourcard*/
xi: reg `i' hourcard i.Upazila if a_paymethod!=., vce(cl village_id) 
mat bf1[1,`j']=_b[hourcard] /*save coefficient of hourcard upazila FE*/
mat sf1[1,`j']=_se[hourcard] /*save standard error of hourcard upazila FE*/
sum `i' if a_paymethod!=. & hourcard==0
mat b2[1,`j']=r(mean) /*save constant term*/
mat s2[1,`j']= r(sd) /*standard error of constant term*/
*characteristics on marginal price
xi: reg `i' anymarginal, vce(cl village_id) 
mat beta1[1,`j']=_b[anymarginal] /*save coefficient of hourcard*/
mat serror1[1,`j']=_se[anymarginal] /*save standard error of hourcard*/
xi: reg `i' anymarginal i.Upazila, vce(cl village_id) 
mat betaf1[1,`j']=_b[anymarginal] /*save coefficient of hourcard*/
mat serrorf1[1,`j']=_se[anymarginal] /*save standard error of hourcard*/
sum `i' if anymarginal==0
mat beta2[1,`j']=r(mean) /*save constant term*/
mat serror2[1,`j']= r(sd) /*standard error of constant term*/
}
*rename results vectors for table appearance (has own card)
foreach i in 1 f1 { 
mat b = b`i'
mat se = s`i'
matrix colnames b = `vlist'
matrix colnames se = `vlist'
ereturn post b 
estadd matrix se
eststo mod`i'  /*store results of regressions(coefficients and standard error) */
}
mat b=b2
matrix colnames b = `vlist'
ereturn post b 
eststo mod2
*rename results vectors for table appearance (volumetric pricing)
foreach i in 1 f1  { 
mat b = beta`i'
mat se = serror`i'
matrix colnames b = `vlist'
matrix colnames se = `vlist'
ereturn post b 
estadd matrix se
eststo modvol`i'  /*store results of regressions(coefficients and standard error) */
}
mat b=beta2
matrix colnames b = `vlist'
ereturn post b 
eststo mod2vol

*TABLE C1: Household, plot, and geographic characteristics by volumetric pricing
esttab mod2vol modvol1 modvolf1 using "${latex}tableC1_balance_anymarginal.tex", ///
replace b(%10.3f) se  star(* 0.10 ** 0.05 *** 0.01) noobs label mtitles("Mean No Volumetric" "Coef. Volumetric" "Coef. Volumetric, Upazila FE") ///
align(ccc) nonotes gaps ///
refcat(age "\emph{\underline{Panel A: Household Characteristics}}" renter "\emph{\underline{Panel B: Characteristics of Study Plot}}" srtm_eleva "\emph{\underline{Panel C: Geographic Variables}}", nolabel)

*TABLE C2 Household, plot, and geographic characteristics by individual card ownership
esttab mod2 mod1 modf1 using "${latex}tableC2_balance_holdsown.tex", ///
replace b(%10.3f) se  star(* 0.10 ** 0.05 *** 0.01) noobs label mtitles("No Card Mean" "Own Card" "Own Card, Upazila FE") ///
align(ccc) nonotes gaps ///
refcat(age "\emph{\underline{Panel A: Household Characteristics}}" renter "\emph{\underline{Panel B: Characteristics of Study Plot}}" srtm_eleva "\emph{\underline{Panel C: Geographic Variables}}", nolabel)
}


/**********************************
	TABLE C6
***********************************/
estimates clear /*start with cleared estimates to avoid hitting Stata limit of 300*/
{
**************************
*LASSO WITH OWNING OWN CARD
*******************************************************
use "data/input_primary/rct1_followup.dta", clear
merge 1:1 farmer_id using "data/input_primary/rct1_baseline.dta", keepusing(farmer_id)
drop if _merge==2 /*drop farmers not in baseline*/
drop _merge
*generate fertilizer expenditures per bigah 
gen purea=16 /*local prices from that season*/
gen ptsp=22
gen pmop=14 
gen pother=17.3333333
foreach j in urea tsp mop other { 
egen a_kg_`j' = rowtotal(a_dose1_`j' a_dose2_`j' a_dose3_`j' a_dose4_`j' a_dose5_`j')
gen a_spend_`j' = a_kg_`j'*p`j'
gen a_spendbg_`j' = a_spend_`j' / a_areacult_bg
}
gen totalfert_bg = a_spendbg_urea+a_spendbg_tsp+a_spendbg_mop+a_spendbg_other
order a_kg_* a_spend*, after(a_dose5_other)
*generate family labor per bigah 
gen a_famhar_bg = (a_famhar*a_harwage)/a_areacult_bg
gen a_famplant_bg = (a_famplant*a_plantwage)/a_areacult_bg 
egen weed_wage = rowmean(a_plantwage a_harwage) 
gen a_famweed_bg = (a_famweed*weed_wage)/a_areacult_bg 
*generate interaction term between treatment and volumetric prices 
gen t_marg = anymarginal*treatment  
*convert everything to per acre (for making units consistent across tables) 
foreach i in a_spendbg_urea a_spendbg_tsp a_spendbg_mop a_spendbg_other a_pesticides_bg a_herbicides_bg a_plantspend_bg a_weedspend_bg a_harspend_bg a_famplant_bg a_famhar_bg a_famweed_bg a_watercost totalfert_bg { 
replace `i'=`i'*3
}
gen loga_watercost = ln(a_watercost)
*mean fill on price 
gen a_price_per_kg=a_price*(1/40)
sum a_price, detail
replace a_price = r(mean) if a_price==.
*generate variables 
gen a_yield_kgac = a_yield*120 /*convert yield from monn per bigha to kg per acre*/
gen a_rev_ac = a_yield*a_price*3 /*calculate revenue per acre (yield in monn per bigha, price in taka per monn, and multiply by bigha per acre)*/
egen a_cost_ac = rowtotal(a_spendbg_urea a_spendbg_tsp a_spendbg_mop a_spendbg_other a_pesticides_bg a_herbicides_bg a_plantspend_bg a_weedspend_bg a_harspend_bg a_famplant_bg a_famhar_bg a_famweed_bg a_watercost) /*total cost per acre*/
gen a_profit_ac = a_rev_ac - a_cost_ac 
gen a_nonwater_ac = a_cost_ac - a_watercost 
foreach i in a_yield_kgac a_rev_ac a_profit_ac a_nonwater_ac { 
gen log_`i' = ln(`i')
}
*profits with controls 
merge 1:1 farmer_id using "data/derived/baseline_covariates.dta", keepusing(farmer_id age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1) 
drop if _merge==2 
drop _merge
gen hourcard = (a_paymethod==1)
foreach j in hourcard age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1 {
sum `j' 
gen zs_`j' = (`j'-r(mean))/r(sd)
}
keep if a_paymethod!=.
keep farmer_id a_watercost treatment zs_* Upazila village_id
save "data/derived/rct1_lasso_own.dta", replace

* Set the seed
set seed 67184

* Create an empty dataset to house the new variables
tempname memoryhold
postfile `memoryhold' ate het g1 g2 g3 g4 using "data/derived/LASSO_simulate_own.dta", replace

* Loop over our dataset, pull main/auxiliary sample, and execute calculations 
forvalues t=1(1)100 { 
	use "data/derived/rct1_lasso_own.dta", clear
	
	* Create indicator to we can split sample into assessment (auxiliary) sample (otherwise in main sample) 
	gen runi=runiform()
	sort runi
	desc 
	gen assess = _n <= (r(N)/2) 
	
	* LASSO Regression in assessment sample (assessment sample to avoid over fitting) ---
	
	* First lasso in Control and ASSESSMENT sample (outcome = ncrops grown, and independent vars = demeaned characteristics)
	preserve 
	keep if treatment==0 & assess == 1
	lasso2 a_watercost zs_*, long lic(aicc) postresults maxiter(50000) 
	/* long = show all models, aicc =  the information criteria supported, 
	postresults =  stores estimation results of the model selected by information criterion in e().  */
	local k = e(selected) /* grab the selected variables */
	if e(s)>0 { /*number of parameters selected*/
	reg a_watercost `k', vce(robust)
	}
	if e(s)==0 { 
	reg a_watercost, vce(robust)
	}
	estimates store beta_control_`t' /* store this control regression */
	restore 
	
	* Second lasso in Treatment and ASSESSMENT sample (outcome = ncrops grown, and independent vars = demeaned characteristics)
	preserve 
	keep if treatment==1 & assess==1
	lasso2 a_watercost zs_*, long lic(aicc) postresults maxiter(50000) 
	local k = e(selected)
	if e(s) > 0 {
	reg a_watercost `k', vce(robust)
	}
	if e(s)==0 { 
	reg a_watercost, vce(robust)
	}
	estimates store beta_treat_`t' /* store this treatment regression */
	restore 
	
	* Calculate treatment effect heterogeneity in MAIN sample to avoid over fitting --------------------------------------------------------------------------------------------
	keep if assess==0
	
	estimates restore beta_control_`t' /* this restores the regression and hence the beta coefficients  */
	predict xb_control_`t', xb /* this produces the predictions using control betas*/
	estimates restore beta_treat_`t' /* this restores the regression and hence the beta coefficients  */
	predict xb_treat_`t', xb  /* this produces the predictions using treatment betas*/

	gen s0_`t' = xb_treat_`t' - xb_control_`t'
	sum s0_`t', detail 
	gen s0dm_`t' = s0_`t' - r(mean)
	gen treat_s0_`t' = treatment*s0dm_`t'
	
	* Regress outcome on treatment interacted with heterogeneity index ----------------------------------------------------------------------
	xi: reg a_watercost treatment xb_control_`t' s0dm_`t' treat_s0_`t' i.Upazila, vce(cl village_id) /*looks plausible in validation dataset*/
	loc ate=_b[treatment]
	loc het=_b[treat_s0_`t']
	
	* Regress outcome on treatment interacted with heterogeneity index ---------------------------------------------------------------------
	xtile het_group = s0_`t', nq(4)
	foreach i in 1 2 3 4 { 
	xi: reg a_watercost treatment i.Upazila if het_group==`i', vce(cl village_id) /*looks plausible in validation dataset*/
	loc g`i'=_b[treatment] in `t'
	}
	post `memoryhold' (`ate') (`het') (`g1') (`g2') (`g3') (`g4')
}
postclose `memoryhold' 

use "data/derived/LASSO_simulate_own.dta", clear
sum

****** STEP 3: Predicted heterogeneity for everybody in the sample *********
use "data/derived/baseline_covariates.dta", clear
merge 1:1 farmer_id using "data/input_primary/rct1_followup.dta", keepusing(farmer_id a_paymethod)
keep if _merge==3 & a_paymethod!=.
gen hourcard = (a_paymethod==1)
* Demean variables
foreach j in hourcard age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1 {
sum `j' 
gen zs_`j' = (`j'-r(mean))/r(sd)
}
forvalues t=1(1)100 { 
* Control betas
estimates restore beta_control_`t' 
predict xb_control_`t', xb
* Treatment betas
estimates restore beta_treat_`t' 
predict xb_treat_`t', xb
* Predicted treatment effect 
gen s0_`t' = xb_treat_`t' - xb_control_`t'
}
egen s0=rowmedian(s0_*) /*median across 100 iterations*/
drop s0_*
save "data/derived/LASSO_phet_own.dta", replace 

****** STEP 4: Characteristics of least and most affected units *********
*LASSO results
use "data/derived/LASSO_phet_own.dta", clear 
merge 1:1 farmer_id using "data/input_primary/rct1_baseline.dta", keepusing(farmer_id village_id treatment) nogen keep(match)
drop _merge
merge 1:1 farmer_id using "data/input_primary/rct1_followup.dta", keepusing(farmer_id a_watercost) 
drop if _merge==2
drop _merge
cumul s0, gen(Fhat)
xtile aff_group = Fhat, nq(5)
gen m_most=.
gen m_least=. 
gen r2=.
loc ct=0

foreach j in hourcard age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1 {
loc ct=`ct'+1
reg s0 `j', vce(cl village_id)
replace r2=e(r2) in `ct'
sum `j' if aff_group==1
replace m_most=r(mean) in `ct'
sum `j' if aff_group==5
replace m_least=r(mean) in `ct'
}

label var age "Age" 
label var edu "Years Education" 
label var hhsize "Household Size" 
label var livestock "Number Livestock Owned" 
label var landhold "Landholdings in Acres" 
label var knowawd "Heard of AWD?" 
label var renter "Plot is Rented or Sharecropped" 
label var area1_acre "Area in Acres" 
label var hourcard "Has Card" 
label var ncrop1 "Number Crops Grown" 
label var rice_rice_sys "Rice-Rice Cropping System" 
label var boro_rvirrig_1 "Number Irrigations in Boro" 
label var boro_revperacre_1 "Revenue per Acre in Boro" 
label var boro_tcost_1 "Cost per Acre in Boro" 
label var boro_watercostpacre_1 "Water Cost per Acre in Boro" 
label var aman_peracre_1 "Revenue per Acre in Aman"
label var assetheld_1 "Owns Television"
label var assetheld_5 "Owns Refrigerator"
label var assetheld_9 "Owns Irrigation Shallow Tubewell"

order r2 m_most m_least
br
drop if m_least==.
mkmat m_most m_least r2, matrix(finalres)
matrix rownames finalres = hourcard age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1

*Table C6: Characteristics of farmers most and least affected by conservation technology for Rajshahi sample only
frmttable using "${latex}tableC6_lasso_covariates_own", statmat(finalres) sdec(3,3,3) ctitle("","Mean Most","Mean Least","Share Variation" \ "","Affected","Affected","Explained") ///
varlabels tex fra replace
}


/**********************************
	FIGURE C1-C2
***********************************/
{
estimates clear
*********************************************************************************************************
*RCT 1: ROBUSTNESS TO INTERACTIONS WITH UPAZILA-LEVEL AVERAGES 
*FIGURE C1-C2
******************************************************************************************************************
use "data/input_primary/rct1_watermeasure.dta", clear 
keep if waterunit != . /*keep only completed measurements*/
merge n:1 farmer_id using "data/input_secondary/GIS_covariates.dta", keepusing(farmer_id srtm_eleva soilclay soilsand soilcarbon soilwater) 
drop if _merge==2 
drop _merge
gen t_marg = treatment*anymarginal
merge n:1 farmer_id using "data/derived/baseline_covariates.dta", keepusing(farmer_id age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre ncrop1 rice_rice_sys boro_rvirrig_1 boro_revperacre_1 boro_tcost_1 boro_watercostpacre_1 aman_peracre_1) 
drop if _merge==2 
drop _merge
*demean control variables and create upazila-level mean
foreach k in age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre srtm_eleva soilclay soilsand soilcarbon soilwater { 
egen upamean_`k' = mean(`k'), by(upazila)
sum upamean_`k', detail 
gen dupamean_`k' = upamean_`k' - r(mean) 
gen t_dupamean_`k' = treatment*dupamean_`k'
}
*generate upazila-level interaction term
egen upamean_marg = mean(anymarginal), by(upazila) 
gen t_upamean_marg = treatment*upamean_marg
*dry field 0-70 days
xi: reg dryfield treatment t_upamean_marg i.Upazila if dat<=70, vce(cl village_id)
regsave t_upamean_marg using maindry, ci replace
foreach k in age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre srtm_eleva soilclay soilsand soilcarbon soilwater { 
xi: reg dryfield treatment t_upamean_marg t_dupamean_`k' i.Upazila if dat<=70, vce(cl village_id)
regsave t_upamean_marg using `k', ci replace
}
*water level 0-70 days
xi: reg waterlevel treatment t_upamean_marg i.Upazila if dat<=70, vce(cl village_id)
regsave t_upamean_marg using mainwater, ci replace
foreach k in age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre srtm_eleva soilclay soilsand soilcarbon soilwater { 
xi: reg waterlevel treatment t_upamean_marg t_dupamean_`k' i.Upazila if dat<=70, vce(cl village_id)
regsave t_upamean_marg using `k'_water, ci replace
}

*make graphs
*compile dry field regressions
use maindry, clear
gen cont=""
loc j=1
foreach k in age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre srtm_eleva soilclay soilsand soilcarbon soilwater {
loc j=`j'+1
append using `k', force
replace cont="`k'" in `j'
erase `k'.dta
}
tempfile md 
save "`md'", replace
*compile water level regressions
use mainwater, clear
gen cont=""
loc j=1
foreach k in age edu hhsize livestock landhold assetheld_1 assetheld_5 assetheld_9 knowawd renter area1_acre srtm_eleva soilclay soilsand soilcarbon soilwater { 
loc j=`j'+1
append using `k'_water, force
replace cont="`k'" in `j'
erase `k'_water.dta
}
keep coef ci_lower ci_upper cont 
foreach v in coef ci_lower ci_upper { 
rename `v' `v'_w
}
*merge in dry field regressions
merge 1:1 cont using "`md'", nogen keep(match)
erase mainwater.dta 
erase maindry.dta

*make graph for dry field
sort coef
gen index=_n
gen labvar = "None" /*for making labels in graph*/
replace labvar="Age" if cont=="age"  
replace labvar="Years Education"  if cont=="edu" 
replace labvar="Household Size"  if cont=="hhsize" 
replace labvar="Number Livestock Owned"  if cont=="livestock" 
replace labvar="Landholdings in Acres" if cont=="landhold"  
replace labvar="Heard of AWD?"  if cont=="knowawd" 
replace labvar="Plot is Rented or Sharecropped"  if cont=="renter" 
replace labvar="Area in Acres" if cont=="area1_acre"  
replace labvar="Owns Television" if cont=="assetheld_1" 
replace labvar="Owns Refrigerator" if cont=="assetheld_5" 
replace labvar="Owns Irrigation Shallow Tubewell" if cont=="assetheld_9" 
replace labvar="Soil Clay Content (%) at 10 cm" if cont=="soilclay" 
replace labvar="Soil Sand Content (%) at 10 cm" if cont=="soilsand" 
replace labvar="Soil Carbon Content at 10cm (g/kg)" if cont=="soilcarbon" 
replace labvar="Soil Water Content (%) at 10 cm" if cont=="soilwater" 
replace labvar="Elevation (m)" if cont=="srtm_eleva" 
labmask index, values(labvar)

*Figure C1 Coefficients on treatment-pricing interaction for dry fields when including upazila-level covariates interacted with treatment

twoway (rspike ci_lower ci_upper index, horizontal lcolor(gs6) lwidth(medthin)) ///
(scatter index coef, mcolor(black) msize(mlarge) ///
ylabel(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17, valuelabel angle(0) tstyle(major_notick) labcolor(gs6) labsize(vsmall)) ///
ytitle("") xlabel(-0.1 0 0.1 0.2 0.3 0.4, grid gmin gmax) plotregion(style(none)) ///
yscale(lstyle(none)) xtitle("Coefficient on Treatment * Volumetric Pricing", color(gs6))), ///
name(dry_int, replace) scheme(s1color) xsize(8) ysize(6) legend(off) xline(0, lcolor(gs6) lpattern(dash) lwidth(thin))
graph export "${latex}figureC1_control_upazila_mean_dry.pdf", as(pdf) replace


*Figure C2 Coefficients on treatment-pricing interaction for water level when including upazila-level covariates interacted with treatment
drop index
sort coef_w
gen index=_n
labmask index, values(labvar)

twoway (rspike ci_lower_w ci_upper_w index, horizontal lcolor(gs6) lwidth(medthin)) ///
(scatter index coef_w, mcolor(black) msize(mlarge) ///
ylabel(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17, valuelabel angle(0) tstyle(major_notick) labcolor(gs6) labsize(vsmall)) ///
ytitle("") xlabel(-1.9 -1.4 -0.9 -0.4 0.1 0.6, grid gmin gmax) plotregion(style(none)) ///
yscale(lstyle(none)) xtitle("Coefficient on Treatment * Volumetric Pricing", color(gs6))), ///
name(water_int, replace) scheme(s1color) xsize(8) ysize(6) legend(off) xline(0, lcolor(gs6) lpattern(dash) lwidth(thin))
graph export "${latex}figureC2_control_upazila_mean_water.pdf", as(pdf) replace
}

/**********************************
	FIGURE C3 
***********************************/
{
****************************************************************************************
*RCT 1: Treatment effects by heterogeneity score
*Figure C3 Heterogeneous treatment effect by quartiles of predicted heterogeneity score
****************************************************************************************
use "data/derived/LASSO_simulate.dta", clear
keep g*
gen iter=_n
reshape long g, i(iter) j(row)
collapse (mean) mean_g=g (p5) p5_g=g (p95) p95_g=g, by(row)
twoway (rspike p5_g p95_g row, lcolor(black) lwidth(thick)) (scatter mean_g row, mcolor(black) msize(vlarge)), ///
legend(off) scheme(s1color) ytitle("Treatment Effect", size(medlarge)) xtitle("Quartile", size(medlarge)) ///
name(right, replace) yline(0, lcolor(gs7) lpattern(dash))
graph export "${latex}figureC3_lasso_hetero.pdf", as(pdf) replace 
}

/**********************************
	TABLE D1
***********************************/
{
*HETEROGENEOUS EFFECTS OF THE PREPAID CARD TREATMENT BY A PREDICTED MEASURE OF LIQUIDITY CONSTRAINTS
*TABLE D1
******************************************************************************************************************

*collapse data on card reloading 
use "data/input_secondary/rct2_cardreload_paba.dta", clear
gen n=1
collapse (mean) Chargemoney (sum) sum_recharge = Charge n, by(CardNo farmer_id)
*merge in baseline characteristics 
merge 1:1 farmer_id using "data/input_primary/rct2_baseline.dta", keepusing(farmer_id  waterprice waterirrig waterwhopay assets nplot boroarea_bg totarea_bg age edu livestock nadults nyoung)
keep if _merge==3 /*& both==1 is both is farmers located in both recharge and usage data*/
drop _merge
*regress number of recharges on total spending, get residual as measure of liquidity constraint 
reg n sum_recharge
predict uhat if e(sample), residuals
*split into training and validation 
sort CardNo
set seed 567123
gen runi=runiform()
sort runi
desc
gen train = _n<=(r(N)/2)
*append data on pipe purchases so liquidity constraint can be predicted for entire sample
preserve
use "data/input_primary/rct2_usage.dta", clear
merge 1:1 farmer_id using "data/input_primary/rct2_pipesales.dta", keepusing(farmer_id adoption)
drop _merge
merge 1:1 farmer_id using "data/input_primary/rct2_baseline.dta", keepusing(farmer_id  waterprice waterirrig waterwhopay assets nplot boroarea_bg totarea_bg age edu livestock nadults nyoung)
drop _merge
gen estim=1
tempfile edata 
save "`edata'", replace  
restore 
append using "`edata'", force
*generate variables for liquidity constraint prediction
gen television = regexm(assets, "1 ")
gen motorbike = regexm(assets, "2")
gen toilet = regexm(assets, "3")
gen fan = regexm(assets, "4")
gen fridge = regexm(assets, "5")
gen washingmachine = regexm(assets, "6")
gen electricity = regexm(assets, "7")
gen tractor = regexm(assets, "8")
gen shallow = regexm(assets, "9")
gen non = regexm(assets, "10")
gen deep_driver = (waterwhopay==1)
gen plotsize = boroarea_bg / nplot
gen asset_i = motorbike + toilet + fan + television + fridge + washingmachine /*number of assets*/
*lasso and OLS of covariates selected by LASSO 	
lasso2 uhat age totarea_bg edu livestock nadults nyoung waterirrig waterprice asset_i electricity tractor shallow deep_driver if train==1, long lic(aicc) postest
loc k=e(selected)
reg uhat `k' if train==1, vce(robust)
predict plasso, xb 
corr uhat plasso if train==0
*random forest  
replace uhat=5 if estim==1 /*just trick, otherwise the predict command doesn't work because can't have missing values in dep var*/
rforest uhat age totarea_bg edu livestock nadults nyoung waterirrig waterprice asset_i electricity tractor shallow deep_driver if train==1, type(reg)
ereturn list
predict pranfor /*prediction*/
corr uhat pranfor if train==0
scatter uhat pranfor if train==0
save "data/derived/liq_ml_res.dta", replace
*interaction between predicted liquidity constraint and prepaid card treatment 
gen logprice = ln(price)
gen cardtreat = (treatment=="card")
gen cardtreat_price = cardtreat*price
gen cardtreat_logprice = cardtreat*logprice 
gen awd_use = (awdirri==1 | awdoth==1)
gen dpaba = (Upazila=="PABA") /*creating upazila dummies, mostly to scale graph*/
gen dtanore = (Upazila=="TANORE")
foreach j in paba tanore { /*demean upazila dummies to scale graph correctly*/
sum d`j'
gen d`j'_d = d`j'-r(mean)
}
gen cardtreat_plasso = plasso*cardtreat
xi: reg adoption cardtreat plasso cardtreat_plasso dpaba_d dtanore_d if estim==1, vce(cl village_id)
sum adoption if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
estimates store lasso_interaction

drop plasso cardtreat_plasso 
rename pranfor plasso /*just renaming to make the table look better*/
gen cardtreat_plasso = plasso*cardtreat
xi: reg adoption cardtreat plasso cardtreat_plasso dpaba_d dtanore_d if estim==1, vce(cl village_id)
sum adoption if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
estimates store rf_interaction

foreach k in age totarea_bg edu livestock nadults nyoung waterirrig waterprice asset_i electricity tractor shallow deep_driver { 
gen card_int_`k' = cardtreat*`k' 
}
xi: reg adoption cardtreat card_int_totarea_bg card_int_livestock card_int_asset_i totarea_bg livestock asset_i dpaba_d dtanore_d if estim==1, vce(cl village_id)
sum adoption if e(sample)==1 & cardtreat==0
estadd sca ym=r(mean)
estimates store all_interaction


label var cardtreat "Card Treatment" 
label var plasso "Liquidity Constraint" 
label var cardtreat_plass "Card Treatment * Liquidity Constraint"
label var totarea_bg "Landholdings" 
label var livestock "Number Livestock" 
label var asset_i "Number Assets"
label var card_int_totarea_bg "Card Treatment * Landholdings" 
label var card_int_livestock "Card Treatment * Number Livestock" 
label var card_int_asset_i "Card Treatment * Number Assets"

esttab all_interaction lasso_interaction rf_interaction using "${latex}tableD1_pipedemand_liquidity_test2.tex", ///
replace b(%10.4f) se wrap star(* 0.10 ** 0.05 *** 0.01) noobs label ///
scalars("ym Mean in Control" "N Number Obs" "r2 R squared") ///
sfmt(%10.3f %10.0f %10.3f) order(cardtreat card_int_* totarea_bg livestock asset_i cardtreat_plasso) mtitles("Interactions" "Lasso" "Random Forest") align(ccc) nonotes gaps drop(_cons) indicate("Upazila Fixed Effects = d*") 

}
