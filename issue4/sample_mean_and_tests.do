version 14
set more off
preliminaries
program main	
	local indir "examples/source/raw/eventstudy_illustration_data/orig"
	local outdir "issue4"
	
	log using "`outdir'/comparisons.log", replace
	
	use "`indir'/simulation_data_dynamic.dta", clear

	gen_vars
	sample_regression
	do_tests	
	compute_mean	
	
	log close
end

program gen_vars
	local G 2
	local LG 2
	local M 2
	local LM 2
	local furthest_lead = `G' + `LG'
	local furthest_lag  = `M' + `LM'
	local furthest_lag_minus1 = `furthest_lag' - 1

	xtset id t
	gen zfd = d.z
	
	forvalues i = 2/`furthest_lead' {
		by id: gen zfd_lead`i' = f`i'.zfd
	}
	
	forvalues i = 1/`furthest_lag_minus1' {
		by id: gen zfd_lag`i'  = l`i'.zfd
	}
	display "hello"	
	by id: gen furthest_lead = 1 - f`furthest_lead'.z 
	by id: gen furthest_lag  = l`furthest_lag'.z 
end

program sample_regression
	reg y_base zfd zfd_lead* zfd_lag* furthest_lead furthest_lag i.t i.id, vce(cluster id)
end

program do_tests
	* pre-trends test
	test furthest_lead = zfd_lead4 = zfd_lead3 = 0
	display r(p)
	file open pvalues using "tests/testthat/input/pvalues.txt", write replace
	file write pvalues "`r(p)'" _n
	* leveling-off test
	test zfd_lag2 = zfd_lag3 = furthest_lag
	file write pvalues "`r(p)'" _n
	file close pvalues
end

program compute_mean
	by id: gen zfd_lead1 = f1.zfd
	summarize y_base if zfd_lead1 != 0 & zfd_lead1 != .
end

main




