
#' Prepares a formula object for use in EventStudyOLS or EventStudyFHS
#'
#' @param estimator Accepts one of "OLS" or "FHS". If "FHS" is specified, implements IV.
#' @param data The data frame that contains the variables of interest.
#' @param outcomevar Variable indicating outcomme variable y, should be a character.
#' @param policyvar Variable indicating policy variable z, should be a character.
#' @param idvar Variable indicating units, should be a character.
#' @param timevar Variable indicating time periods, should be a character.
#' @param controls Optional vector of controls q, should be a character.
#' @param proxy Variable that is thought to be affected by the confoud but not by the policy.
#' Should be specified if and only if estimator is specified as "FHS". Should be a character.
#' @param proxyIV Variables to be used as an instrument. For the case of a single proxy,
#' defaults to the strongest lead of the policy variable based on the first stage.
#' Should be specified if an only if estimator is specified as "FHS".
#' Should be a character
#' @param M The number of periods in the past before which the past values of the policy
#' are not supposed to affect the value of the outcome. Should be a positive integer.
#' @param LM Optional number of event times after M to be included in estimation. Defaults to 1.
#' Should be a positive integer.
#' @param G Number of periods in the future after which the future values of the policy are
#' not supposed to affect the value of the outcome today. Should be a positive integer.
#' @param LG Optional number of event times earlier than -G to be included in estimation. Defaults to M + G.
#' Should be a positive integer.
#' @param normalize Specifies the event-time coefficient to be normalized. Defaults to -1.
#' Should be an integer. Should be one of TRUE or FALSE.
#'
#' @return A formula object to be passed to EventStudy
#' @import estimatr
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' PrepareModelFormula("OLS", gapminder::gapminder, outcomevar = "lifeExp",
#' policyvar = "pop", idvar = "continent", timevar = "year",
#' controls = "gdpPercap", M = 3, G = 2, LG = 5, LM = 4)
#'
#' }


PrepareModelFormula <- function(policyvar, controls, str_policy_fd, str_policy_lead, str_policy_lag, outcomevar) {

    reg_formula <- reformulate(termlabels = c(policyvar, controls, str_policy_fd, str_policy_lead, str_policy_lag),
                               response = outcomevar)

    return(reg_formula)

    # as.formula(
    #     estimatr::lm_robust(
    #     formula = reg_formula,
    #     data = df_leads_lags,
    #     fixed_effects = get(idvar) ~ get(timevar)
    #     )


}
