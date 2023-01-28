remove(list=ls())
# install.packages(nloptr)
library(nloptr)

### Eq and ineq constraints

#### Without gradients
####  RELEVANT EXAMPLE

eval_f <- function(x)
{
    return (x[1]*x[4]*(x[1] +x[2] + x[3] ) + x[3] )
}
eval_g_ineq <- function(x)
{
    return (25 - x[1]*x[2]*x[3]*x[4])
}
eval_g_eq <- function(x)
{
    return ( x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2 - 40 )
}

lb <- c(1,1,1,1)
ub <- c(5,5,5,5)

x0 <- c(1,5,5,1)

local_opts <- list( "algorithm" = "NLOPT_LD_MMA", "xtol_rel" = 1.0e-15 )
opts <- list( "algorithm"= "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-15,
              "maxeval"= 160000,
              "local_opts" = local_opts,
              "print_level" = 0 )



res <- nloptr ( x0 = x0,
                eval_f = eval_f,
                lb = lb,
                ub = ub,
                eval_g_ineq = eval_g_ineq,
                eval_g_eq = eval_g_eq,
                opts = opts
)
print(res)


#### With gradients

eval_f <- function( x ) {
    return( list( "objective" = x[1]*x[4]*(x[1] + x[2] + x[3]) + x[3],
                  "gradient" = c( x[1] * x[4] + x[4] * (x[1] + x[2] + x[3]),
                                  x[1] * x[4],
                                  x[1] * x[4] + 1.0,
                                  x[1] * (x[1] + x[2] + x[3]) ) ) )
}

eval_g_ineq <- function( x ) {
    constr <- c( 25 - x[1] * x[2] * x[3] * x[4] )
    grad <- c( -x[2]*x[3]*x[4],
               -x[1]*x[3]*x[4],
               -x[1]*x[2]*x[4],
               -x[1]*x[2]*x[3] )
    return( list( "constraints"=constr, "jacobian"=grad ) )
}

eval_g_eq <- function( x ) {
    constr <- c( x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2 - 40 )
    grad <- c( 2.0*x[1],
               2.0*x[2],
               2.0*x[3],
               2.0*x[4] )
    return( list( "constraints"=constr, "jacobian"=grad ) )
}

x0 <- c( 1, 5, 5, 1 )

lb <- c( 1, 1, 1, 1 )
ub <- c( 5, 5, 5, 5 )

local_opts <- list( "algorithm" = "NLOPT_LD_MMA",
                    "xtol_rel" = 1.0e-7 )
opts <- list( "algorithm" = "NLOPT_LD_AUGLAG",
              "xtol_rel" = 1.0e-7,
              "maxeval" = 1000,
              "local_opts" = local_opts,
              "print_level" = 0 )

res <- nloptr( x0 = x0,
               eval_f = eval_f,
               lb = lb,
               ub = ub,
               eval_g_ineq = eval_g_ineq,
               eval_g_eq = eval_g_eq,
               opts = opts )
print(res)


