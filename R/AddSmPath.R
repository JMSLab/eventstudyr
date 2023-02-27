#' Calculates smoothest path
#'
#' @description Function to add smoothest path to dataframe with coefficients
#'
#' @param df Dataset with coefficients prepared for plotting (must include coefficients normalized in estimation).
#' @param coefficients Event-study coefficients (must include coefficients normalized in estimation).
#' @param inv_covar Inverse of covariance matrix of coefficients (must include row and column of zeros for normalized coefficients).
#' @param conf_level Confidence level to define critical value of Wald region. Should be a real number between 0 and 1, inclusive. Defaults to 0.95.
#' @param maxorder Sets a maximum polynomial order that will be used when calculating lowest possible polynomial order. Should be a whole number. Defaults to 7.
#' @param maxiter_solver Sets the maximum number of iterations when searching for the smoothest path with minimum squared term in highest order coefficient. Should be a positive whole number. Defaults to 1e6.
#'
#' @return df with smoothest path added as a new column
#' @import pracma
#' @export

AddSmPath <- function(df, coefficients, inv_covar,
                      conf_level = 0.95, maxorder = 10, maxiter_solver = 2e6){

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
    if (!(maxiter_solver%%1 == 0) | maxiter_solver < 0) {
        stop("Argument 'maxiter_solver' should be a positive integer.")
    }
    unselect_message <- "Please change the 'Smpath' argument in 'EventStudyPlot' to FALSE."

    coeff_length <- length(coefficients)
    norm_idxs    <- which(coefficients == 0)
    Wcritic      <- qchisq(conf_level, coeff_length - length(norm_idxs))
    pN           <- length(norm_idxs)

    # First step: Find lowest possible polynomial order
    res_order <- FindOrder(coefficients, inv_covar, Wcritic, maxorder)
    order     <- res_order$order
    res_order <- res_order$results
   
    print(sprintf("Critical Wald value: %s", Wcritic))
    print(sprintf("Polynomial order: %s", order))

    # Second step: Find minimum coefficient on highest-order term
    if (order == 0) {

        Fmat  <- GetFmat(coeff_length, 0)
        vstar <- matrix(0)
    } else if (order == maxorder) {

        stop(paste0("Search for smoothest path reached the maximum order of ", maxorder,". ", unselect_message))
    } else {

        Fmat <- GetFmat(coeff_length, order)

        if (pN < order) {
            vstar <- FindCoeffs(res_order, coefficients, inv_covar, Wcritic, pN, order, norm_idxs, Fmat, maxiter_solver)

        } else if (pN == order) {
            vstar <- FindCoeffsEq(res_order, coefficients, inv_covar, Wcritic, pN, order, norm_idxs, Fmat, maxiter_solver)

        } else {
            stop(paste0("The smoothest path cannot be found because the number of normalized coefficients is larger than the minimum order. ",
                        unselect_message))
        }
    }

    sm_path = Fmat %*% vstar
    Woptim  = (t(sm_path - coefficients)%*%inv_covar)%*%(sm_path - coefficients)
     
    print("Smoothest path:")
    print(sm_path)
    print(sprintf("Wald value of optimal path: %s", Woptim))

    df["smoothest_path"] = sm_path
    
    return(list("df" = df, "order" = order, 
                "Wcritic" = Wcritic, "Woptim" = Woptim))
}
