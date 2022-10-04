
#' Prepares a formula object for use in EventStudyOLS or EventStudyFHS
#'

#' @param estimator Accepts one of "OLS" or "FHS". If "FHS" is specified, implements IV estimator in Freyaldenhoven et al. 2019.
#' @param outcomevar Variable indicating outcome variable y, should be a character.
#' @param str_policy_fd Vector indicating leads and lags of first differenced policy variable z with the (pre +1)th term omitted for normalization, should be a string.
#' @param str_policy_lead Variable indicating the (pre + overidpre)th lead of the policy variable z, should be a character.
#' @param str_policy_lag Variable indicating the (post + overidpost)th lag of the policy variable z, should be a character.
#' @param controls Optional vector of controls q, should be a character.
#' @param proxy Variable that is thought to be affected by the confound but not by the policy.
#' Should be specified if and only if estimator is specified as "FHS". Should be a character.
#' @param proxyIV Variables to be used as an instrument. Should be specified if and only if estimator is specified as "FHS". Should be a character.
#' @return A formula object to be passed to EventStudy
#' @rawNamespace import(stats, except=c(lag, filter))
#' @export
#'
#' @examples
#' PrepareModelFormula(estimator = "OLS", outcomevar = "y_base",
#' str_policy_fd = c("z_fd_lead1", "z_fd_lead2", "z_fd_lead4",
#' "z_fd_lead5", "z_fd_lead6", "z_fd_lag1", "z_fd_lag2",
#' "z_fd_lag3", "z_fd_lag4", "z_fd_lag5", "z_fd_lag6", "z_fd_lag7"),
#' str_policy_lead = "z_lead6",
#' str_policy_lag = "z_lag8",
#' controls = "x_r")
#'
#' # If you would like to use IV regression:
#' PrepareModelFormula(estimator = "FHS",
#' outcomevar = "y_base",
#' str_policy_fd = c("z_fd", "z_fd_lead2", "z_fd_lead3", "z_fd_lag1", "z_fd_lag2"),
#' str_policy_lead = "z_lead3",
#' str_policy_lag = "z_lag3",
#' controls = "x_r",
#' proxy = "eta_m",
#' proxyIV = "z_fd_lead3")

PrepareModelFormula <- function(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls = NULL, proxy = NULL, proxyIV = NULL) {

    if (! estimator %in% c("OLS", "FHS")) {stop("estimator should be either 'OLS' or 'FHS'.")}
    if (! is.character(outcomevar)) {stop("outcomevar should be a character.")}
    if (! is.character(str_policy_fd)) {stop("str_policy_fd should be a character.")}
    if (! is.character(str_policy_lead)) {stop("str_policy_lead should be a character.")}
    if (! is.character(str_policy_lag)) {stop("str_policy_lag should be a character.")}
    if (! (is.null(controls) | is.character(controls))) {stop("controls should be either NULL or a character.")}


    if (estimator == "OLS") {


        reg_formula <- stats::reformulate(termlabels = c(str_policy_fd, str_policy_lead, str_policy_lag, controls),
                               response = outcomevar,
                               intercept = FALSE)
    }

    if (estimator == "FHS") {

        if (is.null(proxyIV)) {stop("proxyIV must be specified with estimator=FHS")}

        exogenous <- c(str_policy_fd, str_policy_lead, str_policy_lag, controls)
        exogenous <- exogenous[exogenous != proxy]
        exogenous <- exogenous[exogenous != proxyIV]
        reg_formula <- as.formula(paste(outcomevar, "~", paste(c(exogenous, proxy), collapse="+"), "|", paste(c(exogenous, proxyIV), collapse="+")))

    }

    return(reg_formula)

}
