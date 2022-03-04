
#' Prepares a formula object for use in EventStudyOLS or EventStudyFHS
#'

#' @param estimator Accepts one of "OLS" or "FHS". If "FHS" is specified, implements IV estimator in Freyaldenhoven et al. 2019.
#' @param outcomevar Variable indicating outcomme variable y, should be a character.
#' @param str_policy_fd Vector indicating leads and lags of first differenced policy variable z with the (pre +1)th term omitted for normalization, should be a string.
#' @param str_policy_lead Variable indicating the (pre + overidpre)th lead of the policy variable z, should be a character.
#' @param str_policy_lag Variable indicating the (post + overidpost)th lag of the policy variable z, should be a character.
#' @param controls Optional vector of controls q, should be a character.

#'
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

PrepareModelFormula <- function(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls = NULL) {

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

    return(reg_formula)

}
