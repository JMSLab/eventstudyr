
# Compare `eventstudyr` with `xtevent`

We estimate the same event studies with `xtevent` and `eventstudyr` and compare the results.

## Data

We use the dataset `df_sample_dynamic` from `eventstudyr`.

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

The smoothest path is a flat line at `y=0` for both `R` and `STATA`.
No numerical optimization is used to find the optimal path in this case.

### Outcome `y_smooth_m`

| R | STATA |
| --- | --- |
| ![y_smooth_m_R](R/y_smooth_m.png) | ![y_smooth_m_STATA](STATA/y_smooth_m.png) |

The smoothest paths are slightly different in this case, which probably reflects different estimated covariance matrices.

- The exact values of the optimal path in `R` are [here](sm_path_R.log#L64-L74)
- The exact values of the optimal path in `STATA` are [here](sm_path.log#L142-L160)

No numerical optimization is used to find the optimal path in this case.

### Outcome `y_jump_m`

| R | STATA |
| --- | --- |
| ![y_jump_m_R](R/y_jump_m.png) | ![y_jump_m_STATA](STATA/y_jump_m.png) |

The smoothest paths are somewhat different in this case as well.

- The exact values of the optimal path in `R` are [here](sm_path_R.log#L114-L122)
- The exact values of the optimal path in `STATA` are [here](sm_path.log#L357-L365)

In this case there is a numerical optimization involved to find the optimal path.

- In `R` the solver seems to converge without throwing any errors.
- In `STATA` the function returns is programmed to return a warning.
  - The full output of the solver is [here](sm_path.log#L216-L351).
  - The error in the last lines reads `could not calculate numerical derivatives -- discontinuous region with missing values encountered`.

