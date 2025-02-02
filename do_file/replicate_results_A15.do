*uses working directory from "replicate_results.do"

/**********************************
		TABLE A15 
***********************************/

	
*Note to replicator: 
*The creation of this table requires that one manually inputs p-values to run a command below. To do so, I've included a "stop" after a browse command so that you can copy & paste these values and store them in memory. Then, proceed to run the other part of the code and it will pause opening a data browse window. Then, use the edit command so you can paste the p-values in the column. To continue running the program type "q" in the command window. 	

*The code relies on code available from Michael Anderson. See Anderson (2008) Journal of the American Statistical Association (cited in main text of the paper)

*Directions: 
*1. First run up to line 136
*2. The data window opens. Copy the variable normal_p
*3. Paste that variable into the data editor for the variable pval when prompted
*4. Type q in the command window to complete the code
	
*******************************************************************************************************************MULTIPLE HYPOTHESIS TESTING
************************************************************************************************************************************** set aside outcomes from followup *************************
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
*generate family labor per bigah 
gen a_famhar_bg = (a_famhar*a_harwage)/a_areacult_bg
gen a_famplant_bg = (a_famplant*a_plantwage)/a_areacult_bg 
egen weed_wage = rowmean(a_plantwage a_harwage) 
gen a_famweed_bg = (a_famweed*weed_wage)/a_areacult_bg 
*convert everything to per acre (for making units consistent across tables) 
foreach i in a_spendbg_urea a_spendbg_tsp a_spendbg_mop a_spendbg_other a_pesticides_bg a_herbicides_bg a_plantspend_bg a_weedspend_bg a_harspend_bg a_famplant_bg a_famhar_bg a_famweed_bg a_watercost totalfert_bg { 
replace `i'=`i'*3
}
*mean fill on price 
gen a_price_per_kg=a_price*(1/40)
sum a_price, detail
replace a_price = r(mean) if a_price==.
*generate variables 
gen a_yield_kgac = a_yield*120 /*convert yield from monn per bigha to kg per acre*/
gen a_rev_ac = a_yield*a_price*3 /*calculate revenue per acre (yield in monn per bigha, price in taka per monn, and multiply by bigha per acre)*/
egen a_cost_ac = rowtotal(a_spendbg_urea a_spendbg_tsp a_spendbg_mop a_spendbg_other a_pesticides_bg a_herbicides_bg a_plantspend_bg a_weedspend_bg a_harspend_bg a_famplant_bg a_famhar_bg a_famweed_bg a_watercost) /*total cost per acre*/
gen a_nonwater_ac = a_cost_ac - a_watercost
gen a_profit_ac = a_rev_ac - a_cost_ac
*separate for volumetric 
foreach j in a_doses a_spendbg_urea a_spendbg_tsp a_spendbg_mop a_spendbg_other a_pesticides_bg a_herbicides_bg a_plantspend_bg a_weedspend_bg a_harspend_bg a_famplant_bg a_famhar_bg a_famweed_bg totalfert_bg a_irrigations a_timedry anymarginal a_yield_kgac a_rev_ac a_cost_ac a_profit_ac a_watercost a_nonwater_ac { 
gen `j'_VM = `j' if anymarginal==1
}
*save traditional p-values and point estimates
preserve
gen t_marg = treatment*anymarginal
gen outcome="" 
gen normal_p=.
gen beta=.
loc i=1
foreach j in a_irrigations a_timedry a_rev_ac a_watercost a_nonwater_ac a_profit_ac { 
xi: reg `j' treatment anymarginal t_marg i.Upazila, vce(cl village_id)
test treatment + t_marg=0 
replace beta=_b[treatment]+_b[t_marg] in `i' 
replace normal_p = r(p) in `i' 
replace outcome="`j'" in `i'
loc i=`i'+1
}
keep if beta!=.
keep outcome beta normal_p
tempfile p1 
save "`p1'", replace /*for calling in later*/
restore 
*save dataset at village level for List et al correction
gen ct=1
egen vwt = sum(ct), by(village_id)
collapse (mean) *_VM [aweight=vwt], by(village_id treatment) 
tempfile outfollow
save "`outfollow'", replace


******************** set aside outcomes from objective water measurements *************************
use "data/input_primary/rct1_watermeasure.dta", clear 
keep if waterunit != . /*keep only completed measurements*/
gen waterlevel70 = waterlevel if dat<=70 
gen dryfield70 = dryfield if dat<=70
*save traditional p-values and point estimates
preserve
gen t_marg = treatment*anymarginal
gen outcome="" 
gen normal_p=.
gen beta=.
loc i=1
foreach j in waterlevel dryfield waterlevel70 dryfield70 { 
xi: reg `j' treatment anymarginal t_marg i.Upazila, vce(cl village_id)
test treatment + t_marg=0 
replace beta=_b[treatment]+_b[t_marg] in `i' 
replace normal_p = r(p) in `i' 
replace outcome="`j'" in `i'
loc i=`i'+1
}
keep if beta!=.
keep outcome beta normal_p
tempfile p2 
save "`p2'", replace /*for calling in later*/
restore 
*save dataset at village level for List et al correction
foreach j in waterlevel dryfield waterlevel70 dryfield70 { 
gen `j'_VM = `j' if anymarginal==1
}
gen ct=1
egen vwt = sum(ct), by(village_id)
collapse (mean) *_VM [aweight=vwt], by(village_id) 
merge 1:1 village_id using "`outfollow'" 

******************** multiple inference correction (List, Sheikh, and Xu) *************************
mhtexp a_irrigations_VM a_timedry_VM a_rev_ac_VM a_nonwater_ac_VM a_watercost_VM a_profit_ac_VM waterlevel_VM dryfield_VM waterlevel70_VM dryfield70_VM, treatment(treatment)
matlist results 
mata st_matrix("output",results)
svmat output, names(col)
keep c1 c5 c7
rename c7 lsh_p 
tempfile lsh_p 
save "`lsh_p'", replace
*pull together traditional and LSX p values 
use "`p1'", clear 
append using "`p2'" 
gen c1 = _n 
merge 1:n c1 using "`lsh_p'", nogen keep(match)
save rsofar, replace /*set aside data to merge back after getting p values from Anderson (2008)*/
br
*After this browse, you should be able to see the p-values (normal_p) you need to copy in the data editor window



******************** multiple inference correction (Anderson 2008 JASA from his website) ***************************
* This code generates standard BH (1995) q-values as described in Anderson (2008), "Multiple Inference and Gender Differences in the Effects of Early Intervention: A Reevaluation of the Abecedarian, Perry Preschool, and Early Training Projects", Journal of the American Statistical Association, 103(484), 1481-1495

* BH (1995) q-values are introduced in Benjamini and Hochberg (1995), "Controlling the False Discovery Rate", Journal of the Royal Statistical Society: Series B, 57(1), 289-300

* Last modified: M. Anderson, 11/20/07
* Test Platform: Stata/MP 10.0 for Macintosh (Intel 32-bit), Mac OS X 10.5.1
* Should be compatible with Stata 10 or greater on all platforms
* Likely compatible with with Stata 9 or earlier on all platforms (remove "version 10" line below)

clear
version 10

****  INSTRUCTIONS:
****    Please start with a clear data set
****	When prompted, paste the vector of p-values you are testing into the "pval" variable
****	Please use the "do" button rather than the "run" button to run this file (if you use "run", you will miss the instructions at the prompts

pause on
set more off

if _N>0 {
	display "Please clear data set before proceeding"
	display "After clearing, type 'q' to resume"
	pause
	}	

quietly gen float pval = .

display "***********************************"
display "Using the edit command, please paste the vector of p-values that you wish to test into the variable 'pval'"
display	"After pasting, type 'q' to resume"
display "***********************************"

pause



* Collect the total number of p-values tested

quietly sum pval
local totalpvals = r(N)

* Sort the p-values in ascending order and generate a variable that codes each p-value's rank

quietly gen int original_sorting_order = _n
quietly sort pval
quietly gen int rank = _n if pval~=.

* Set the initial counter to 1 

local qval = 1

* Generate the variable that will contain the BH (1995) q-values

gen bh95_qval = 1 if pval~=.

* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.

while `qval' > 0 {
	* Generate value qr/M
	quietly gen fdr_temp = `qval'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= qr/M
	quietly gen reject_temp = (fdr_temp>=pval) if fdr_temp~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	quietly gen reject_rank = reject_temp*rank
	* Record the rank of the largest p-value that meets above condition
	quietly egen total_rejected = max(reject_rank)
	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	replace bh95_qval = `qval' if rank <= total_rejected & rank~=.
	* Reduce q by 0.001 and repeat loop
	quietly drop fdr_temp reject_temp reject_rank total_rejected
	local qval = `qval' - .001
}
	
quietly sort original_sorting_order
pause off
set more on

display "Code has completed."
display "Benjamini Hochberg (1995) q-vals are in variable 'bh95_qval'"
display	"Sorting order is the same as the original vector of p-values"
*Merge back with other results 
rename original_sorting_order c1 
keep c1 bh95_qval 
merge 1:1 c1 using rsofar
*Erase unneeded data 
erase rsofar.dta 
*Sort data 
gen ordering = . 
replace ordering=1 if outcome=="a_irrigations"
replace ordering=2 if outcome=="a_timedry"
replace ordering=9 if outcome=="a_rev_ac"
replace ordering=7 if outcome=="a_watercost"
replace ordering=8 if outcome=="a_nonwater_ac"
replace ordering=10 if outcome=="a_profit_ac"
replace ordering=3 if outcome=="waterlevel"
replace ordering=4 if outcome=="dryfield"
replace ordering=5 if outcome=="waterlevel70"
replace ordering=6 if outcome=="dryfield70"
sort ordering
*make latex table A15 Multiple inference corrections for effects of conservation technology with volu- metric pricing
mkmat beta normal_p bh95_qval lsh_p, matrix(finalres)
matrix list finalres
frmttable using "${latex}tableA15_mht", statmat(finalres) sdec(2,3,3,3) ctitle("","Effect","Unadjusted","FDR q value","FWER adjusted" \ "","","p-value","Anderson (2008)","List et al. (2016)") ///
rtitle("Irrigations"\"Times Dried"\"Water Level"\"Dry Field"\"Water Level 0-70 days"\"Dry Field 0-70 days"\"Water Cost"\"Other Input Cost"\"Revenue"\"Profit") ///
tex fra replace
