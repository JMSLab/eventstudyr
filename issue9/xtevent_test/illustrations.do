clear all
set more off

adopath + "issue9/xtevent_test"
adopath + "issue9/xtevent_test"
global dir_graph "issue9/xtevent_test"
global plot_options   ytitle("Coefficient") xtitle("Event time")
global xtplot_options ${plot_options} smplotopts(lcol(gs7)) ciplotopts(lcol(gs7)) suptciplotopts(lcol(gs7))
set scheme s1color_horizontal

program main
    cartoon_figures

    use "examples/source/raw/eventstudy_illustration_data/orig/simulation_data_dynamic", clear

	*foreach stub in "smooth_m" "jump_m"
    foreach stub in "smooth_m"  {
        illustrate_plot_options, stub(`stub')
    }

    *foreach stub in "jump_m" "r" {
    *    linear_extrapolation, stub(`stub')       
    *}
	
	*fhs_iv, stub(r)
	*fhs_iv, stub(jump_m) offset(12.1)

    *static_overlay
end

program cartoon_figures
    local colnames "t_6 t_5 t_4 t_3 t_2 t_1 t0 t1 t2 t3 t4 t5"

    mat y = (-0.02, -0.03, -0.01, 0.03, 0.02, 0, 0.13, 0.16, 0.18, 0.27, 0.31, 0.29)
    mat colnames y = `colnames'

    mat CI_y = (-0.22 , -0.23, -0.21, -0.17, -0.18, 0, -0.07, -0.04, -0.02, 0.07, 0.11, 0.09 \ ///
                0.18, 0.17, 0.19, 0.23, 0.22, 0, 0.33, 0.36, 0.38, 0.47, 0.51, 0.49)

    coefplot (matrix(y), ciopts(recast(rcap) lcol(gs7)) mc(dkgreen) ci(CI_y)), ${plot_options} yscale(titlegap(*0)) ///
        vertical nooffset legend(off) order(t_6 t_5 t_4 t_3 t_2 t_1 t0 t1 t2 t3 t4 t5) yline(0, lpattern(dash) lc(gs10)) ///
        rename(t_6="-6+" t_5="-5" t_4="-4" t_3="-3" t_2="-2" t_1="-1" t0="0" t1="1" t2="2" t3="3" t4="4" t5="5+") 
    graph export "$dir_graph/multiple_paths_outcome.eps", replace
    graph export "$dir_graph/multiple_paths_outcome.png", replace
end

program illustrate_plot_options
    syntax, stub(str)

    xtevent y_smooth_m, panelvar(id) timevar(t) policyvar(z) window(5)

    loc lab: di %-9.2f `=e(y1)'
    loc lab=strtrim("`lab'")

    file open value_label using "$dir_graph/value_label_`stub'.txt", text write replace
    file write value_label "`lab'"
    file close value_label

    mat coefs = e(b)
    local coefcol = colnumb(coefs,"_k_eq_p3")
    scalar coef = coefs[1, `coefcol']
    loc coef: di %-9.2f coef
    loc coef=strtrim("`coef'")

    *file open value_coef using "$dir_graph/value_coef_`stub'.txt", text write replace
    *file write value_coef "`coef'"
    *file close value_coef

    * Base event study plot
    *xteventplot, graphregion(color(white)) noprepval nopostpval ///
    *    nosupt ylab(-1.5 -.8 0 .8 1.5) ${xtplot_options}
    *graph export "$dir_graph/base_plot_`stub'.eps", replace
    *graph export "$dir_graph/base_plot_`stub'.png", replace

    * Add label of average of dependent variable at event-time -1
    *xteventplot, graphregion(color(white)) noprepval nopostpval ///
    *    nosupt ylab(-1.5 -.8 0 `"0 (`lab')"' .8 1.5) ${xtplot_options}
    *graph export "$dir_graph/add_label_average_`stub'.eps", replace
    *graph export "$dir_graph/add_label_average_`stub'.png", replace

    * Add sup-t CIs
    *xteventplot, graphregion(color(white)) noprepval nopostpval ///
    *    ylab(-1.5 -.8 0 `"0 (`lab')"' .8 1.5) ${xtplot_options}
    *graph export "$dir_graph/add_supt_ci_`stub'.eps", replace
    *graph export "$dir_graph/add_supt_ci_`stub'.png", replace

    * Add p-value for pre-trends and dynamics leveling off
    *xteventplot, graphregion(color(white)) ///
    *    ylab(-1.5 -.8 0 `"0 (`lab')"' .8 1.5) ${xtplot_options}
    *graph export "$dir_graph/add_pval_pretrend_`stub'.eps", replace
    *graph export "$dir_graph/add_pval_pretrend_`stub'.png", replace

    * Add smoothest line
	xteventplot, graphregion(color(white)) smpath(line) ylab(-1.5 -.8 0 `"0 (`lab')"' .8 1.5) ${xtplot_options}

    graph export "$dir_graph/add_smoothest_line_`stub'.eps", replace
    graph export "$dir_graph/add_smoothest_line_`stub'.png", replace
end

program linear_extrapolation
    syntax, stub(str)

    xtevent y_`stub', panelvar(id) timevar(t) policyvar(z) window(5)

    loc lab: di %-9.2f `=e(y1)'
    loc lab=strtrim("`lab'")

    * P-value but no smoothest line
    xteventplot, graphregion(color(white)) ylab(-1.5 0 `"0 (`lab')"' 1.5 3) ${xtplot_options}
    graph export "$dir_graph/pval_noline_`stub'.eps", replace
    graph export "$dir_graph/pval_noline_`stub'.png", replace

    xtevent y_`stub', panelvar(id) timevar(t) policyvar(z) window(5) trend(-3)

    xteventplot, overlay(trend) graphregion(color(white)) ///
        ylab(-1.5 0 `"0 (`lab')"' 1.5 3) ${xtplot_options}
    graph export "$dir_graph/trend_`stub'.eps", replace
    graph export "$dir_graph/trend_`stub'.png", replace

    xteventplot, graphregion(color(white)) ///
        ylab(-1.5 0 `"0 (`lab')"' 3 6) ${xtplot_options}
    graph export "$dir_graph/trend_extrapolation_`stub'.eps", replace
    graph export "$dir_graph/trend_extrapolation_`stub'.png", replace
end

program fhs_iv
    syntax, stub(str) [offset(real 0)]
	
	replace y_`stub' = y_`stub' + `offset'

    xtevent y_`stub', panelvar(id) timevar(t) policyvar(z) window(5) ///
        proxy(x_`stub') proxyiv(select)

    loc lab_y: di %-9.2f `=e(y1)'
    loc lab_y=strtrim("`lab_y'")

    qui sum x_`stub' if t == eventtime - 1
    loc lab_x: di %-9.2f `=r(mean)'
    loc lab_x=strtrim("`lab_x'")
	
	xteventplot, y graphregion(color(white)) ///
        ylab(-1.5 0 `"0 (`lab_y')"' 1.5 3) ${xtplot_options}
    graph export "$dir_graph/fhs_outcome_`stub'.eps", replace
    graph export "$dir_graph/fhs_outcome_`stub'.png", replace

    xteventplot, proxy graphregion(color(white)) ///
        ylab(-1.5 -.8 0 `"0 (`lab_x')"' .8 1.5) ${xtplot_options}
    graph export "$dir_graph/fhs_proxy_`stub'.eps", replace
    graph export "$dir_graph/fhs_proxy_`stub'.png", replace

    xteventplot, overlay(iv) graphregion(color(white)) ///
        ylab(-1.5 0 `"0 (`lab_y')"' 1.5 3) ${xtplot_options}
    graph export "$dir_graph/fhs_align_proxy_outcome_`stub'.eps", replace
    graph export "$dir_graph/fhs_align_proxy_outcome_`stub'.png", replace

    xteventplot, graphregion(color(white)) ///
        ylab(-1.5 0 `"0 (`lab_y')"' 1.5 3) ${xtplot_options}
    graph export "$dir_graph/fhs_iv_`stub'.eps", replace
    graph export "$dir_graph/fhs_iv_`stub'.png", replace
end

program static_overlay

    use "release/exampleplots/simulation_data_static", clear

    xtevent y_static, panelvar(id) timevar(t) policyvar(z) window(5)

    xteventplot, overlay(static) staticovplotopts(lwidth(thick)) graphregion(color(white)) ${xtplot_options}
    graph export "$dir_graph/static_overlay.eps", replace
    graph export "$dir_graph/static_overlay.png", replace
end

* Execute
main

