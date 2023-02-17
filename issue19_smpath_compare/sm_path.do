clear all

import delimited "data_dynamic.csv"

xtset id t

foreach yvar in y_base y_smooth_m y_jump_m {
  
  xtevent `yvar' x_r, policyvar("z") panelvar("id") timevar("t") ///
    post(3) overidpost(1) pre(0) overidpre(3) norm(-1) ///
    reghdfe

  xteventplot, smpath(line) ///
    graphregion(color(white)) bgcolor(white)

  graph export STATA/`yvar'.png, replace width(1700) height(1214)
}
