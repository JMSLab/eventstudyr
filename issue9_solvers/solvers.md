Solvers
================
2023-01-28

## Overview

In this document we explore the behavior of two solvers for the
smoothest path problem.

- `Rsolnp`: General Non-linear Optimization Using Augmented Lagrange
  Multiplier Method. Vignette
  [here](https://cran.r-project.org/web/packages/Rsolnp/Rsolnp.pdf).

- `nloptr`: Solve optimization problems using an R interface to
  open-source NLopt. Vignette
  [here](https://cran.r-project.org/web/packages/nloptr/nloptr.pdf).
  Useful pdf with explainers
  [here](https://arxiv.org/pdf/2101.02912.pdf).

## Loading packages

``` r
# Load package
library(devtools)

setwd("..")
load_all()
setwd("issue9_solvers")

# Load solvers
library(nloptr)
library(Rsolnp)

# Load other
library(data.table)  # Data manipulation
library(ggplot2)     # Plotting
library(knitr)       # Priting nice tables
library(pracma)      # Matrix inverses
```

## Data

We use the sample data of the package, specifically the one where the y
variable jumps.

``` r
dt <- fread("data_jump.csv")
kable(dt[, .(term, label, estimate, std.error)])
```

| term       | label |   estimate | std.error |
|:-----------|:------|-----------:|----------:|
| z_lead6    | -7+   |  0.0631927 | 0.1396882 |
| z_fd_lead6 | -6    | -0.0307564 | 0.1719871 |
| z_fd_lead5 | -5    |  0.1187085 | 0.1974440 |
| z_fd_lead4 | -4    | -0.1254036 | 0.1639341 |
| z_fd_lead3 | -3    |  0.0000000 | 0.0000000 |
| z_fd_lead2 | -2    | -0.0085515 | 0.2001933 |
| z_fd_lead1 | -1    | -0.2090427 | 0.1740237 |
| z_fd       | 0     |  0.6067985 | 0.1688490 |
| z_fd_lag1  | 1     |  0.6047815 | 0.2083604 |
| z_fd_lag2  | 2     |  0.3983893 | 0.1941487 |
| z_fd_lag3  | 3     | -0.0690363 | 0.2360809 |
| z_fd_lag4  | 4     | -0.1483994 | 0.1952580 |
| z_fd_lag5  | 5     | -0.1561979 | 0.2342532 |
| z_fd_lag6  | 6     | -0.0603482 | 0.1898868 |
| z_fd_lag7  | 7     |  0.1620751 | 0.2442110 |
| z_lag8     | 8+    | -0.0709703 | 0.1874676 |

We also load the covariance data. We show only the first 7 rows and
columns below.

``` r
covar <- read.csv("covar.csv", row.names = 1)
dim(covar)
```

    ## [1] 16 16

``` r
kable(covar[1:7,1:7])
```

|            |   z_lead6 | z_fd_lead6 | z_fd_lead5 | z_fd_lead4 | z_fd_lead3 | z_fd_lead2 | z_fd_lead1 |
|:-----------|----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|
| z_lead6    | 0.0195128 |  0.0135527 |  0.0195855 |  0.0146371 |          0 |  0.0193603 |  0.0114426 |
| z_fd_lead6 | 0.0135527 |  0.0295796 |  0.0195584 |  0.0124986 |          0 |  0.0170163 |  0.0125072 |
| z_fd_lead5 | 0.0195855 |  0.0195584 |  0.0389841 |  0.0163861 |          0 |  0.0296992 |  0.0140001 |
| z_fd_lead4 | 0.0146371 |  0.0124986 |  0.0163861 |  0.0268744 |          0 |  0.0150419 |  0.0152545 |
| z_fd_lead3 | 0.0000000 |  0.0000000 |  0.0000000 |  0.0000000 |          0 |  0.0000000 |  0.0000000 |
| z_fd_lead2 | 0.0193603 |  0.0170163 |  0.0296992 |  0.0150419 |          0 |  0.0400774 |  0.0129153 |
| z_fd_lead1 | 0.0114426 |  0.0125072 |  0.0140001 |  0.0152545 |          0 |  0.0129153 |  0.0302842 |

## Find order of polynomial

We find the minimum order such that a polynomial is in the Wald region
of the coefficients. We use a function already incorporated in the
package.

``` r
coefficients <- dt$estimate

covar     <- data.matrix(covar)
inv_covar <- pracma::pinv(covar)

Wcritic   <- qchisq(.95, length(coefficients))

Sol1 <- FindOrder(coefficients, inv_covar, Wcritic, maxorder = 10)

order <- Sol1$order
poly_coeffs <- Sol1$results$vhat
poly_path   <- Sol1$results$trfit
```

The resulting order is 8.

A plot of the resulting path is below. This is *not* the smoothest path.

``` r
dt[, poly_path := poly_path]
dt[, label := factor(label, levels = dt$label)]

ggplot(dt, aes(x = label)) +
    geom_point(aes(y = estimate), size = 2) +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error,
                      ymax = estimate + 1.96*std.error),
                  width = 0.2) +
    geom_point(aes(y = poly_path), size = 2, color = "red")
```

<img src="solvers_files/figure-gfm/plot-1.png" style="display: block; margin: auto;" />

## Searching for the smoothest path

Now that we now the order of the desired polynomial, we want to find the
smoothest path by solving the following problem:

1.  Choose coefficients for the desired order to minimize the squared of
    the coefficient on the highest order polynomial term.
2.  Constraint the minimization to paths inside the Wald confidence
    region.
3.  Constraint the minimization to paths where the normalized
    coefficients are zero.

### Solver `Rsolnp`

We define the functions we use to implement this problem below.

``` r
Objective <- function(v, d, invV) {
  p = length(v)
  return(v[p]^2)
}

IneqConstraint <- function(v, d, invV) {
  p <- length(v)
  r <- length(d)

  k    = seq(0, r-1)/(r-1)
  Fmat = sapply(seq(1, p),
                   function(j) {k^(j-1)})
  trfit <- Fmat %*% v

  W     <- (t(d-trfit)%*%invV)%*%(d-trfit)
  return(W)
}

EqConstraint <- function(v, d, invV) {

  norm_index <- which(d == 0)
    
  p <- length(v)
  r <- length(d)

  k    = seq(0, r-1)/(r-1)
  Fmat = sapply(seq(1, p),
                   function(j) {k^(j-1)})
  trfit <- Fmat %*% v

  norm_sm_path <- trfit[norm_index]

  return(norm_sm_path)
}
```

Next, we run the solver.

``` r
norm_idxs <- which(coefficients == 0)

optim <- Rsolnp::solnp(pars      = rep(0, order),
                       fun       = Objective,
                       eqfun     = EqConstraint,
                       eqB       = rep(0, length(norm_idxs)),
                       ineqfun   = IneqConstraint,
                       ineqUB    = Wcritic,
                       ineqLB    = -Inf,
                       d         = coefficients,
                       invV      = inv_covar)
```

    ## 
    ## Iter: 1 fn: 1.768e+14     Pars:  -24099440.50957  90906237.35481  12412914.26399 -34603155.52079 -53756068.07190 -84895558.35832  45321774.26364  13295230.98600
    ## Iter: 2 fn: 1.603e+15     Pars:   -20154582.25786  102374632.71135  -73969996.11513 -138709796.81952   38084726.88490  131276031.72551   -6498493.36523  -40038389.60642
    ## Iter: 3 fn: 1.409e+15     Pars:  -11540624.80675  62959366.01102 -59282298.28487 -93969791.14945  73843292.10378  67720294.94511  -7640874.01334 -37535407.60122
    ## solnp--> Solution not reliable....Problem Inverting Hessian.

We obtain the error “Problem inverting Hessian”. Can we invert the
hessian manually?

``` r
pinv(optim$hessian) # Pseudo-inverse
```

    ##                [,1]          [,2]          [,3]          [,4]          [,5]
    ##  [1,]  6.645016e-59  2.831658e-48  2.975792e-48 -8.343456e-49  3.083509e-49
    ##  [2,] -5.595221e-33 -2.384305e-22 -2.505670e-22  7.025335e-23 -2.596371e-23
    ##  [3,] -5.880023e-33 -2.505670e-22 -2.633210e-22  7.382935e-23 -2.728526e-23
    ##  [4,]  1.648627e-33  7.025335e-23  7.382935e-23 -2.070001e-23  7.650189e-24
    ##  [5,] -6.092868e-34 -2.596372e-23 -2.728526e-23  7.650190e-24 -2.827286e-24
    ##  [6,]  5.968213e-33  2.543251e-22  2.672704e-22 -7.493670e-23  2.769449e-23
    ##  [7,]  4.990001e-33  2.126402e-22  2.234638e-22 -6.265429e-23  2.315527e-23
    ##  [8,] -2.583095e-33 -1.100741e-22 -1.156770e-22  3.243322e-23 -1.198642e-23
    ##  [9,] -4.346501e-33 -1.852186e-22 -1.946464e-22  5.457450e-23 -2.016922e-23
    ##                [,6]          [,7]          [,8]          [,9]
    ##  [1,] -3.020423e-48 -2.525365e-48  1.307266e-48  2.199699e-48
    ##  [2,]  2.543251e-22  2.126402e-22 -1.100741e-22 -1.852186e-22
    ##  [3,]  2.672704e-22  2.234638e-22 -1.156770e-22 -1.946464e-22
    ##  [4,] -7.493670e-23 -6.265429e-23  3.243323e-23  5.457450e-23
    ##  [5,]  2.769449e-23  2.315527e-23 -1.198643e-23 -2.016922e-23
    ##  [6,] -2.712789e-22 -2.268153e-22  1.174119e-22  1.975657e-22
    ##  [7,] -2.268153e-22 -1.896395e-22  9.816769e-23  1.651840e-22
    ##  [8,]  1.174119e-22  9.816769e-23 -5.081690e-23 -8.550817e-23
    ##  [9,]  1.975657e-22  1.651840e-22 -8.550817e-23 -1.438822e-22

``` r
inv(optim$hessian)  # Inverse
```

    ## Warning in inv(optim$hessian): Matrix appears to be singular.

    ##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
    ##  [1,]  Inf  Inf  Inf  Inf  Inf  Inf  Inf  Inf  Inf
    ##  [2,]  Inf  Inf  Inf  Inf  Inf  Inf  Inf  Inf  Inf
    ##  [3,]  Inf  Inf  Inf  Inf  Inf  Inf  Inf  Inf  Inf
    ##  [4,]  Inf  Inf  Inf  Inf  Inf  Inf  Inf  Inf  Inf
    ##  [5,]  Inf  Inf  Inf  Inf  Inf  Inf  Inf  Inf  Inf
    ##  [6,]  Inf  Inf  Inf  Inf  Inf  Inf  Inf  Inf  Inf
    ##  [7,]  Inf  Inf  Inf  Inf  Inf  Inf  Inf  Inf  Inf
    ##  [8,]  Inf  Inf  Inf  Inf  Inf  Inf  Inf  Inf  Inf
    ##  [9,]  Inf  Inf  Inf  Inf  Inf  Inf  Inf  Inf  Inf

### Solver `nloptr`

We define the functions we use to implement this problem below. We
changed them a bit because we need to rewrite the inequality constraint
as
![W(v)-W\_{critic}\leq 0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;W%28v%29-W_%7Bcritic%7D%5Cleq%200 "W(v)-W_{critic}\leq 0"),
when before we could do
![W(v)\leq W\_{critic}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;W%28v%29%5Cleq%20W_%7Bcritic%7D "W(v)\leq W_{critic}")

``` r
Objective <- function(v, d, invV, Wcritic) {
  p = length(v)
  return(v[p]^2)
}

IneqConstraint <- function(v, d, invV, Wcritic) {
  p <- length(v)
  r <- length(d)

  k    = seq(0, r-1)/(r-1)
  Fmat = sapply(seq(1, p),
                   function(j) {k^(j-1)})
  trfit <- Fmat %*% v

  W     <- (t(d-trfit)%*%invV)%*%(d-trfit)
  return(W - Wcritic)
}

EqConstraint <- function(v, d, invV, Wcritic) {

  norm_index <- which(d == 0)
    
  p <- length(v)
  r <- length(d)

  k    = seq(0, r-1)/(r-1)
  Fmat = sapply(seq(1, p),
                   function(j) {k^(j-1)})
  trfit <- Fmat %*% v

  norm_sm_path <- trfit[norm_index]

  return(norm_sm_path)
}
```

Next, we run the solver. It appears that, for some reason, the initial
guess of
![(0,..,0)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%280%2C..%2C0%29 "(0,..,0)")
does not change. The solver, however, works in a simpler example with
the options. (See file [`test_nloptr.R`](test_nloptr.R))

``` r
local_opts <- list( "algorithm" = "NLOPT_LD_MMA", "xtol_rel" = 1.0e-8)
opts <- list("algorithm"   = "NLOPT_GN_ISRES",
             "xtol_rel"    = 1.0e-10,
             "maxeval"     = 100*1e3,
              "local_opts" = local_opts,
             "print_level" = 0)

# Bounds on vector of coefficients (for some reason they are required)
lb_ <- sapply(1:order, function(j) {-(100^j)})
ub_ <- sapply(1:order, function(j) {(100^j)})

res <- nloptr ( x0 = rep(0, order),
                eval_f = Objective,      # Objective function
                lb = lb_, ub = ub_,
                eval_g_ineq = IneqConstraint,  # Ineq constraint <= 0
                eval_g_eq = EqConstraint,      # Eq contraint    = 0
                opts = opts,
                d = coefficients, invV = inv_covar, Wcritic = Wcritic)
print(res)
```

    ## 
    ## Call:
    ## nloptr(x0 = rep(0, order), eval_f = Objective, lb = lb_, ub = ub_, 
    ##     eval_g_ineq = IneqConstraint, eval_g_eq = EqConstraint, opts = opts, 
    ##     d = coefficients, invV = inv_covar, Wcritic = Wcritic)
    ## 
    ## 
    ## Minimization using NLopt version 2.7.1 
    ## 
    ## NLopt solver status: 5 ( NLOPT_MAXEVAL_REACHED: Optimization stopped because 
    ## maxeval (above) was reached. )
    ## 
    ## Number of Iterations....: 100000 
    ## Termination conditions:  xtol_rel: 1e-10 maxeval: 1e+05 
    ## Number of inequality constraints:  1 
    ## Number of equality constraints:    1 
    ## Current value of objective function:  0 
    ## Current value of controls: 0 0 0 0 0 0 0 0
