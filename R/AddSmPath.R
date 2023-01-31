#' Calculates smoothest path
#'
#' @description Function to add smoothest path to dataframe with coefficients
#'
#' @param df Dataset with coefficients prepared for plotting (must include normalized coefficients)
#' @param inv_covar Inverse of covariance matrix of coefficients (must include row and column of 0s for normalized coefficients)
#' @param conf_level Confidence level to define critical value of Wald region. Should be a real number between 0 and 1, inclusively. Defaults to 0.95.
#' @param maxorder Sets a maximum polynomial order that will be used when calculating lowest possible polynomial order. Should be a whole number. Defaults to 10.
#'
#' @return df with smoothest path add as a new column
#' @import pracma
#' @import Rsolnp
#' @export
#'
#' @examples

AddSmPath <- function(df, coefficients, inv_covar,
                      conf_level = 0.95, maxorder = 10){

    if (!is.numeric(conf_level) | conf_level < 0 | conf_level > 1) {
        stop("Argument 'conf_level' should be a real number between 0 and 1, inclusive.")
    }
    if (!(maxorder%%1 == 0) | maxorder < 0 | maxorder > 10) {
        stop("Argument 'maxorder' should be an integer between zero and ten.")
    }
    if (!is.data.frame(df)) {
        stop("Argument 'df' should be a dataframe.")
    }
    if (!is.matrix(inv_covar)) {
        stop("Argument 'inv_covar' should be a matrix.")
    }

    coeff_length <- length(coefficients)
    Wcritic      <- qchisq(conf_level, coeff_length)
    norm_idxs    <- which(coefficients == 0)

    # First step: Find lowest possible polynomial order
    res_order <- FindOrder(coefficients, inv_covar, Wcritic, maxorder)
    order     <- res_order$order
    res_order <- res_order$results

    # Second step: Find minimum coefficient on highest-order term

    if (order != 0) {

        # Commented out momentarily

        #optim <- Rsolnp::solnp(pars      = rep(0, order),
        #                       fun       = Objective,
        #                       eqfun     = EqConstraint,
        #                       eqB       = rep(0, length(norm_idxs)),
        #                       ineqfun   = IneqConstraint,
        #                       ineqUB    = Wcritic,
        #                       ineqLB    = -Inf,
        #                       coeffs    = coefficients,
        #                       inv_covar = inv_covar)
        #                     # control   = list("tol" = 1e-5)


        #if (optim$convergence != 0) {
        #    stop("The search for parameters for the polynomial did not converge. Please unselect the smoothest path option by setting Smpath to FALSE.")
        #}

        #vstar <- matrix(optim$pars)


        # TEMP: Use coefficients found in first step
        vstar <- res_order$vhat

        p <- coeff_length
        r <- order

        k    <- seq(0, p-1)/(p-1)
        Fmat <- sapply(seq(1, r),
                       function(j) {k^(j-1)})

    } else if (order == 0) {

        p     <- coeff_length
        Fmat  <- rep(1, p)
        vstar <- matrix(0)
    }

    df["smoothest_path"] = Fmat %*% vstar

    return(df)
}
