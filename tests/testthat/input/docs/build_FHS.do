cd C:\Users\emwang\Documents\GitHub\eventstudyr
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
gen z_fd = d.z

forvalues i = 2/`furthest_lead' {
	gen z_fd_lead`i' = f`i'.z_fd
}
forvalues i = 1/`furthest_lag_minus1' {
	gen z_fd_lag`i' = l`i'.z_fd
}
gen z_lead`furthest_lead' = f`furthest_lead'.z
gen z_lag`furthest_lag' = l`furthest_lag'.z
	
* Case 2: FE=TRUE , TFE=TRUE , cluster=TRUE
eststo: xtivreg y_base x_r (eta_m = z_fd_lead3) z_lead3 z_fd_lead2 z_fd z_fd_lag1 z_fd_lag2 z_lag3 i.t, fe vce(cluster id)

* Case 4: FE=TRUE , TFE=FALSE, cluster=TRUE
eststo: xtivreg y_base x_r (eta_m = z_fd_lead3) z_lead3 z_fd_lead2 z_fd z_fd_lag1 z_fd_lag2 z_lag3, fe vce(cluster id)

* Case 5: FE=FALSE, TFE=TRUE , cluster=FALSE
eststo: ivregress 2sls y_base x_r (eta_m = z_fd_lead3) z_lead3 z_fd_lead2 z_fd z_fd_lag1 z_fd_lag2 z_lag3 i.t, small vce(robust)

* Case 6: FE=FALSE, TFE=TRUE , cluster=TRUE
eststo: ivregress 2sls y_base x_r (eta_m = z_fd_lead3) z_lead3 z_fd_lead2 z_fd z_fd_lag1 z_fd_lag2 z_lag3 i.t, small vce(cluster id)

* Case 7: FE=FALSE, TFE=FALSE, cluster=FALSE
eststo: ivregress 2sls y_base x_r (eta_m = z_fd_lead3) z_lead3 z_fd_lead2 z_fd z_fd_lag1 z_fd_lag2 z_lag3, small vce(robust)

* Case 8: FE=FALSE, TFE=FALSE, cluster=TRUE
eststo: ivregress 2sls y_base x_r (eta_m = z_fd_lead3) z_lead3 z_fd_lead2 z_fd z_fd_lag1 z_fd_lag2 z_lag3, small vce(cluster id)

esttab * using "`outdir'/df_test_base_STATA_FHS.csv", csv plain wide noobs ///
keep(z_lead3 z_fd_lead2 z_fd z_fd_lag1 z_fd_lag2 z_lag3 x_r eta_m) ///
replace se nomtitles collabels("coef" "std_error") 

