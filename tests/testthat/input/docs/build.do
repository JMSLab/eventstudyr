
est clear

local indir "examples/source/raw/eventstudy_illustration_data/orig"
local outdir "tests/testthat/input"

use "`indir'/simulation_data_dynamic.dta", clear


local G 1
local LG 2
local M 1
local LM 2
local furthest_lead = `G' + `LG'
local furthest_lag  = `M' + `LM'
local M_minus1 = `M' - 1
local furthest_lag_minus1 = `furthest_lag' - 1

xtset id t
gen zfd = d.z

local leads
	forvalues i = 2/`furthest_lead' {
		local leads `leads' f`i'.zfd
	}
	display "`leads'"	
	local lags
	forvalues i = 1/`furthest_lag_minus1' {
		local lags `lags' l`i'.zfd
	}
		
eststo: reg y_base zfd x_r `lags' `leads' f`furthest_lead'.z l`furthest_lag'.z i.t i.id, vce(cluster id)

esttab * using "`outdir'/df_test_base_STATA.csv", csv plain wide noobs ///
keep(x_r zfd L.zfd L2.zfd F2.zfd F3.zfd F3.z L3.z) ///
replace se nomtitles collabels("coef" "std_error") 

eststo clear 
eststo: reg y_base z i.t i.id, vce(cluster id)

esttab * using "`outdir'/df_test_base_STATA_allzero.csv", csv plain wide noobs ///
keep(z) ///
replace se nomtitles collabels("coef" "std_error") 

