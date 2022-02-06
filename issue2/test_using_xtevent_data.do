version 14
set more off
preliminaries

program main	
	local indir "examples/source/raw/eventstudy_illustration_data/orig"
	local outdir "issue2"
	
	log using "`outdir'/comparisons.log", replace
	
	use "`indir'/simulation_data_dynamic.dta", clear

	comparisons
	
	log close
end

program comparisons
	local G 0
	local LG 5
	local M 2
	local LM 2
	local furthest_lead = `G' + `LG'
	local furthest_lag  = `M' + `LM'
	local M_minus1 = `M' - 1
	local furthest_lag_minus1 = `furthest_lag' - 1
	
	xtevent y_base, pol(z) panelvar(id) timevar(t) pre(`G') post(`M_minus1') overidpre(`LG') overidpost(`LM') cluster(id)

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
	
	xtreg y_base zfd `lags' `leads' f`furthest_lead'.z l`furthest_lag'.z i.t, fe vce(cluster id)
end

main






