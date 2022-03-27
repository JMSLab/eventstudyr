clear

use "examples/source/raw/eventstudy_illustration_data/orig/simulation_data_dynamic.dta"

by id: gen policylead0 = z[_n+1]

reg y_base z i.t i.id, vce(cluster id)

program drop comparisons

program comparisons
	local G 3
	local LG 3
	local M 4
	local LM 4
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
	
	local lags
	forvalues i = 1/`furthest_lag_minus1' {
		local lags `lags' l`i'.zfd
	}
	
	reg y_base zfd `lags' `leads' f`furthest_lead'.z l`furthest_lag'.z i.t i.id, vce(cluster id)
end

comparisons

by id: gen zlag1 = z[_n -1]
by id: gen zlead1 = z[_n + 1]

* M = 1 case

reg y_base zfd zlag1 i.t i.id

* G = 1 case

reg y_base z f1.zfd i.t i.id

* LG = 1 case

reg y_base z f1.z i.t i.id

* LG = 2

reg y_base z f2.z f2.zfd i.t i.id

* M = 3

reg y_base zfd l1.zfd l2.zfd l3.z i.t i.id