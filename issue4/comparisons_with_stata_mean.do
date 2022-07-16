version 14
set more off
preliminaries
program main	
	local indir "examples/source/raw/eventstudy_illustration_data/orig"
	local outdir "issue4"
	
	log using "`outdir'/comparisons.log", replace
	
	use "`indir'/simulation_data_dynamic.dta", clear

	comparisons
	
	compute_mean	
	
	log close
end

program comparisons
	local G 2
	local LG 2
	local M 2
	local LM 2
	local furthest_lead = `G' + `LG'
	local furthest_lag  = `M' + `LM'
	local furthest_lag_minus1 = `furthest_lag' - 1

	xtset id t
	gen zfd = d.z
	
	local leads
	forvalues i = 2/`furthest_lead' {
		by id: gen zfd_lead`i' = f`i'.zfd
	}
	display "`leads'"	
	local lags
	forvalues i = 1/`furthest_lag_minus1' {
		by id: gen zfd_lag`i'  = l`i'.zfd
	}
end

program compute_mean
	summarize y_base if zfd_lead3 != 0 & zfd_lead3 != .
end

main





