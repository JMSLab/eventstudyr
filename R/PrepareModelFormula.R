
#' Prepares a formula object for use in [EventStudyOLS()] or [EventStudyFHS()]

#' @param estimator Accepts one of "OLS" or "FHS". If "FHS" is specified, implements IV estimator in Freyaldenhoven et al. 2019.
#' @param outcomevar Character indicating column of outcome variable.
#' @param str_policy_vars Character vector indicating event-study variables.
#' @param controls Character indicating optional vector of controls.
#' @param proxy Character indicating column of variable that is thought to be affected by the confound but not by the policy.
#' Should be specified if and only if estimator is specified as "FHS".
#' @param proxyIV Character of column to be used as an instrument. Should be specified if and only if estimator is specified as "FHS".
#' If NULL, defaults to the strongest lead of the policy variable based on the first stage.
#' @param static Indicates whether the model to be estimated is static. Defaults to FALSE.
#' @param kernel Character indicating the estimation kernel. Accepts "estimatr" or "fixest". Defaults to "estimatr".
#' @param idvar Character indicating the identifier variable for fixed effects. Required when kernel = "fixest".
#' @param timevar Character indicating the time variable for fixed effects. Required when kernel = "fixest".
#' @param FE Logical indicating whether to include unit fixed effects. Defaults to FALSE.
#' @param TFE Logical indicating whether to include time fixed effects. Defaults to FALSE.
#' @return A formula object to be passed to EventStudy
#'
#' @importFrom stats reformulate as.formula
#' @keywords internal
#' @noRd
#'
#' @examples
#' # For a static model:
#' PrepareModelFormula(estimator = "OLS", outcomevar = "y_base",
#'                     str_policy_vars = "z", static = T)
#' 
#' # For a dynamic model:
#' PrepareModelFormula(estimator = "OLS", outcomevar = "y_base",
#'                     str_policy_vars = c("z_lead3", "z_fd_lead3", "z_fd_lead2",
#'                                         "z_fd_lag1", "z_fd_lag2", "z_lag3"),
#'                     controls = "x_r")
#'
#' # If you would like to use IV regression:
#' PrepareModelFormula(estimator = "FHS",
#'                     outcomevar = "y_base",
#'                     str_policy_vars = c("z_lead3", "z_fd_lead3", "z_fd_lead2",
#'                                         "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3"),
#'                     controls = "x_r",
#'                     proxy = "eta_m",
#'                     proxyIV = "z_fd_lead3")
#'

PrepareModelFormula <- function(estimator, outcomevar,
                                str_policy_vars, static = FALSE,
                                controls = NULL, proxy = NULL, proxyIV = NULL,
                                kernel = "estimatr", idvar = NULL, timevar = NULL,
                                FE = FALSE, TFE = FALSE) {

    if (! estimator %in% c("OLS", "FHS"))      {stop("estimator should be either 'OLS' or 'FHS'.")}
    if (! is.character(outcomevar))            {stop("outcomevar should be a character.")}
    if (! is.character(str_policy_vars))       {stop("str_policy_vars should be a character.")}
    if (! (is.null(controls) | is.character(controls))) {stop("controls should be either NULL or a character.")}
    if (is.null(proxyIV) & estimator == "FHS") {stop("proxyIV must be specified when estimator is FHS.")}
    if (is.null(proxy) & estimator == "FHS")   {stop("proxy must be specified when estimator is FHS.")}

    if (! is.logical(static) )                   {stop("static should be a logical.")}
    if (  static & length(str_policy_vars) > 1)  {stop("str_policy_vars must have one variable with static = TRUE.")}
    if (! static & length(str_policy_vars) <= 1) {stop("str_policy_vars must have more than one variable with static = FALSE.")}
    if (  static & !is.null(proxyIV))            {stop("static model is not compatible with FHS estimator.")}

    if (! kernel %in% c("estimatr", "fixest")) {stop("kernel should be either 'estimatr' or 'fixest'.")}
    if (kernel == "fixest") {
        if (is.null(idvar) | !is.character(idvar)) {stop("idvar must be specified as a character when kernel is 'fixest'.")}
        if (is.null(timevar) | !is.character(timevar)) {stop("timevar must be specified as a character when kernel is 'fixest'.")}
        if (! is.logical(FE)) {stop("FE should be a logical.")}
        if (! is.logical(TFE)) {stop("TFE should be a logical.")}
    }

    if (kernel == "estimatr") {
        if (estimator == "OLS") {
            reg_formula <- stats::reformulate(
                termlabels = c(str_policy_vars, controls),
                response = outcomevar,
                intercept = FALSE
            )
        }

        if (estimator == "FHS") {
            exogenous <- c(str_policy_vars, controls)
            exogenous <- exogenous[exogenous != proxy]
            exogenous <- exogenous[exogenous != proxyIV]

            reg_formula <- stats::as.formula(
                paste(outcomevar, "~",
                paste(c(exogenous, proxy), collapse="+"),
                "|",
                paste(c(exogenous, proxyIV), collapse="+"))
            )
        }
    } else if (kernel == "fixest") {
        regressors <- c(str_policy_vars, controls)

        if (estimator == "OLS") {
            if (FE | TFE) {
                fes <- c()
                if (FE) {
                    fes <- c(fes, idvar)
                }
                if (TFE) {
                    fes <- c(fes, timevar)
                }

                formula_str <- paste(
                    outcomevar,
                    "~",
                    paste(regressors, collapse = " + "),
                    "|",
                    paste(fes, collapse = " + ")
                )
                reg_formula <- stats::as.formula(formula_str)
            } else {
                reg_formula <- stats::reformulate(
                    termlabels = regressors,
                    response = outcomevar,
                    intercept = TRUE
                )
            }
        } else if (estimator == "FHS") {
            exogenous <- c(str_policy_vars, controls)
            exogenous <- exogenous[exogenous != proxy]
            exogenous <- exogenous[exogenous != proxyIV]

            if (FE | TFE) {
                fes <- c()
                if (FE) {
                    fes <- c(fes, idvar)
                }
                if (TFE) {
                    fes <- c(fes, timevar)
                }

                formula_str <- paste(
                    outcomevar,
                    "~",
                    paste(exogenous, collapse = " + "),
                    "|",
                    paste(fes, collapse = " + "),
                    "|",
                    proxy,
                    "~",
                    paste(c(exogenous, proxyIV), collapse = " + ")
                )
                reg_formula <- stats::as.formula(formula_str)
            } else {
                formula_str <- paste(
                    outcomevar,
                    "~",
                    paste(exogenous, collapse = " + "),
                    "|",
                    proxy,
                    "~",
                    paste(c(exogenous, proxyIV), collapse = " + ")
                )
                reg_formula <- stats::as.formula(formula_str)
            }
        }
    }

    return(reg_formula)
}


