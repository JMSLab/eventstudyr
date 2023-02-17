
# Compare `eventstudyr` with `xtevent`

We estimate the same event studies with `xtevent` and `eventstudyr` and compare the results.

## Code

### R

The code is in [`sm_path.R`](sm_path.R).

For any `yvar`, the key event study plot is constructed as follows:

``` r
estimates_ols <- EventStudy(
    outcomevar = yvar,
    estimator = "OLS", data = df_sample_dynamic,
    policyvar = "z", idvar = "id", timevar = "t", controls = "x_r",
    post = 3, overidpost = 1,
    pre  = 0, overidpre  = 3,
    normalize = - 1)
EventStudyPlot(estimates = estimates_ols,
               Smpath    = TRUE)
```
### STATA

The code is in [`sm_path.do`](sm_path.do).

For any `yvar`, the key event study plot is constructed as follows:

``` stata
xtevent `yvar' x_r, policyvar("z") panelvar("id") timevar("t") ///
    post(3) overidpost(1) pre(0) overidpre(3) norm(-1) ///
    reghdfe

xteventplot, smpath(line)
```

## Graph

### Outcome `y_base`

| R | STATA |
| --- | --- |
| ![y_base_R](R/y_base.png) | ![y_base_STATA](STATA/y_base.png) |

### Outcome `y_smooth_m`

| R | STATA |
| --- | --- |
| ![y_smooth_m_R](R/y_smooth_m.png) | ![y_smooth_m_STATA](STATA/y_smooth_m.png) |

### Outcome `y_jump_m`

| R | STATA |
| --- | --- |
| ![y_jump_m_R](R/y_jump_m.png) | ![y_jump_m_STATA](STATA/y_jump_m.png) |
