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
        stop("Argument 'maxorder' should be an integer between 0 and 10.")
    }
    if (!is.data.frame(df)) {
        stop("Argument 'df' should be a dataframe.")
    }
    if (!is.matrix(inv_covar)) {
        stop("Argument 'inv_covar' should be a matrix.")
    }
    unselect_message <- "Please unselect the 'Addsmpath' option for plotting."

    coeff_length <- length(coefficients)
    Wcritic      <- qchisq(conf_level, coeff_length)
    norm_idxs    <- which(coefficients == 0)
    pN           <- length(norm_idxs)

    # First step: Find lowest possible polynomial order
    res_order <- FindOrder(coefficients, inv_covar, Wcritic, maxorder)
    order     <- res_order$order
    res_order <- res_order$results

    # Second step: Find minimum coefficient on highest-order term
    if (order == 0) {

        Fmat  <- rep(1, coeff_length)
        vstar <- matrix(0)
    } else if (order == maxorder) {

        stop(paste0("Smoothest path reached the maximum order. ", unselect_message))
    } else {

        Fmat <- GetFmat(coeff_length, order)

        if (pN <= order) {

            vstar <- FindCoeffs(res_order, coefficients, inv_covar, Wcritic, pN, order, norm_idxs, Fmat)

            # Should we try the case pN == order differently?

        } else {
            stop(paste0("The smoothest path cannot be found because the number of normalized coefficients is larger than the minimum order. ", unselect_message))
        }

    }

    df["smoothest_path"] = Fmat %*% vstar

    return(df)
}
