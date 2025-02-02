*******************************************************************************************************************************
*Code to install packages for replicating "Inefficient Water Prices and Incentives for Conservation" 
*Run the commands in this do file to install all necessary packages before running "replicate_results.do"
*Authors: Ujjayant Chakravorty, Manzoor Dar, and Kyle Emerick
*Stata Version: 17
*Contact Kyle Emerick (kyle.emerick@tufts.edu) with any questions
*****************************************************************************************************************

********************PRELIMINARY***********************************
clear all
set more off, permanently

*********install packages from SSC***************************************
foreach k in estout mhtexp lassopack rforest boottest regsave { 
cap ssc install `k'
}

*********net install packages********************************************
net install gr0034, from("http://www.stata-journal.com/software/sj8-2") replace
net install sg97_5, from("http://www.stata-journal.com/software/sj12-4") replace

*********install fan regression ADO file**********************************
run "do_file/fanreg.ado"
