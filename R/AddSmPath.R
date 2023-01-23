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

AddSmPath <- function(estimates, conf_level = 0.95, maxorder = 10){
    if (class(estimates) != "list" | length(estimates) != 2){
        stop("Argument 'estimates' should be a list of length two, an output of EventStudy()")
    }
    if ((class(estimates[[1]]) != "lm_robust") | (typeof(estimates[[1]]) != "list")) {
        stop("The first element of 'estimates' should be a list of class 'lm_robust' with coefficient estimates and standard errors")
    }
    if (class(estimates[[2]]) != "list" | typeof(estimates[[2]]) != "list") {
        stop("The second 'element' of estimates should be a list with argument definitions, an output of EventStudy()")
    }
    if (! is.numeric(conf_level) | conf_level < 0 | conf_level > 1) {
        stop("Argument 'conf_level' should be a real number between 0 and 1, inclusive.")
    }
    if (! is.integer(maxorder) | maxorder >= 0) {
        stop("Argument 'maxorder' should be a whole number.")
    }

    df_estimates_tidy       <- estimatr::tidy(estimates[[1]])
    eventstudy_coefficients <- estimates[[2]]$eventstudy_coefficients
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

    sm_path <- Fmat %*% vstar

    return(sm_path)
}
