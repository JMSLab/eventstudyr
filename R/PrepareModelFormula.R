
#' Prepares a formula object for use in EventStudyOLS or EventStudyFHS
#'

#' @param outcomevar Variable indicating outcomme variable y, should be a character.
#' @param policyvar Variable indicating policy variable z, should be a character.
#' @param str_policy_fd Vector indicating leads and lags of first differenced policy variable z with the (G +1)th term omitted for normalization, should be a string.
#' @param str_policy_lead Variable indicating the (G + LG)th lead of the policy variable z, should be a character.
#' @param str_policy_lag Variable indicating the (M + LM)th lag of the policy variable z, should be a character.
#' @param controls Optional vector of controls q, should be a character.

#'
#' @return A formula object to be passed to EventStudy
#' @export
#'
#' @examples
#' PrepareModelFormula(outcomevar = "lifeExp",
#' policyvar = "pop",
#' str_policy_fd = c("pop_fd_lead1", "pop_fd_lead2", "pop_fd_lead4",
#' "pop_fd_lead5", "pop_fd_lead6", "pop_fd_lag1", "pop_fd_lag2",
#' "pop_fd_lag3", "pop_fd_lag4", "pop_fd_lag5", "pop_fd_lag6", "pop_fd_lag7"),
#' str_policy_lead = "pop_lead6",
#' str_policy_lag = "pop_lag8",
#' controls = "gdpPercap")

PrepareModelFormula <- function(outcomevar, policyvar, str_policy_fd, str_policy_lead, str_policy_lag, controls) {

    reg_formula <- reformulate(termlabels = c(policyvar, controls, str_policy_fd, str_policy_lead, str_policy_lag),
                               response = outcomevar,
                               intercept = FALSE)

    return(reg_formula)

}
