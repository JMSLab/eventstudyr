#' Calculates smoothest path
#'
#' @description
#'
#' @param estimates Output from the EventStudy function. Should be a list of 2.
#' @param conf_level Desired confidence level for Wald critical value. Should be a real number between
#' 0 and 1, inclusive. Defaults to .95.
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

AddSmPath <- function(df_estimates_tidy, eventstudy_coefficients,
                      conf_level = 0.95, maxorder = 10){

    if (!is.numeric(conf_level) | conf_level < 0 | conf_level > 1) {
        stop("Argument 'conf_level' should be a real number between 0 and 1, inclusive.")
    }
    if (!(maxorder%%1 == 0) | maxorder < 0) {
        stop("Argument 'maxorder' should be an integer greater than zero.")
    }

    coefficients            <- df_estimates_tidy$estimate[df_estimates_tidy$term %in% eventstudy_coefficients]
    coeff_length            <- length(coefficients)

    vcov_matrix_all    <- estimates[[1]]$vcov
    v_terms_to_keep    <- colnames(vcov_matrix_all) %in% eventstudy_coefficients
    covar_matrix       <- vcov_matrix_all[v_terms_to_keep, v_terms_to_keep]
    inv_covar_matrix   <- pracma::pinv(covar_matrix)

    Wcritic            <- qchisq(conf_level, coeff_length)

    # First step: Find lowest possible polynomial order
    order <- FindOrder(coefficients, inv_covar_matrix, Wcritic, maxorder)

    if (order == "Error") {
        stop("Error computing lowest possible polynomial order.")
    }

    # Second step: Find minimum coefficient on highest-order term

    if (order != 0) {
        optim <- Rsolnp::solnp(pars      = rep(0, order),
                               fun       = Objective,
                               ineqfun   = IneqConstraint,
                               ineqUB    = Wcritic,
                               ineqLB    = -1e6,
                               coeffs    = coefficients,
                               inv_covar = inv_covar_matrix)

        if (optim$convergence != 0) {
            stop("The search for parameters for the polynomial did not converge. Please unselect the smoothest path option.")
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


    df_smpath <- data.frame(term           = eventstudy_coefficients,
                            smoothest_path = Fmat %*% vstar)
    df_estimates_tidy <- merge(df_estimates_tidy, df_smpath, by = "term", all.x = T)

    return(df_estimates_tidy)
}
