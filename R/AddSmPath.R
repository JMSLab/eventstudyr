#' Calculates smoothest path
#'
#' @description
#'
#' @param df Dataset with coefficients prepared for plotting (must include normalized coefficients)
#' @param inv_covar Inverse of covariance matrix of coefficients (must include row and column of 0s for normalized coefficients)
#' @param conf_level Confidence level to define critical value of Wald region.
#' @param maxorder Sets a maximum polynomial order that will be used when calculating
#' lowest possible polynomial order. Should be a whole number. Defaults to 10.
#'
#' @return sm_path
#' @import pracma, Rsolnp
#' @export
#'
#' @examples
#'
#'

AddSmPath <- function(df, inv_covar, normalization_column,
                      conf_level = 0.95, maxorder = 10){

    if (!is.numeric(conf_level) | conf_level < 0 | conf_level > 1) {
        stop("Argument 'conf_level' should be a real number between 0 and 1, inclusive.")
    }
    if (!(maxorder%%1 == 0) | maxorder < 0) {
        stop("Argument 'maxorder' should be an integer greater than zero.")
    }

    coefficients <- df$estimate
    coeff_length <- length(coefficients)
    Wcritic      <- qchisq(conf_level, coeff_length)

    # First step: Find lowest possible polynomial order
    order <- FindOrder(coefficients, inv_covar, Wcritic, maxorder)

    # Second step: Find minimum coefficient on highest-order term
    if (order != 0) {
        optim <- Rsolnp::solnp(pars      = rep(1, order),
                               fun       = Objective,
                               ineqfun   = IneqConstraint,
                               ineqUB    = Wcritic,
                               ineqLB    = -1e6,
                               coeffs    = coefficients,
                               inv_covar = inv_covar)

        if (optim$convergence != 0) {
            stop("The search for parameters for the polynomial did not converge. Please unselect the smoothest path option by setting Smpath to FALSE.")
        }

        vstar <- optim$pars

        p <- coeff_length
        r <- order

        k    <- seq(0, p-1)/(p-1)
        Fmat <- sapply(seq(0, r-1),
                       function(j) {k^(j)})
    } else if (order == 0) {

        p     <- coeff_length
        Fmat  <- rep(1, p)
        vstar <- matrix(0)
    }

    df["smoothest_path"] = Fmat %*% vstar

    return(df)
}
