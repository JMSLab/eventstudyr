
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
#' @param G Optional number of periods in the future after which the future values of the policy are
#' not supposed to affect the value of the outcome today. Should be a positive integer.
#' @param LG Optional number of event times earlier than -G to be included in estimation. Should be an integer
#' greater than G.
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
#' controls = "gdpPercap", M = 3, G = 2, LG = 3, LM = 4)
#'
#' }


PrepareModelFormula <- function(estimator, data, outcomevar, policyvar, idvar, timevar, controls = NULL,
                                proxy = NULL, proxyIV = NULL, M, LM = 1, G, LG = 1,
                                normalize = -1) {

    if (! estimator %in% c("OLS", "FHS")) {stop("estimator should be either 'OLS' or 'FHS'.")}
    if (! is.data.frame(data)) {stop("data should be a data frame.")}
    if (! is.character(outcomevar)) {stop("outcomevar should be a character.")}
    if (! is.character(policyvar)) {stop("policyvar should be a character.")}
    if (! is.character(idvar)) {stop("idvar should be a character.")}
    if (! is.character(timevar)) {stop("timevar should be a character.")}
    if (! (is.null(controls) | is.character(controls))) {stop("controls should be either NULL or a character.")}
    if ((estimator == "OLS" & ! is.null(proxy))) {stop("proxy should only be specified when estimator = 'FHS'.")}
    if ((estimator == "FHS" & ! is.character(proxy))) {stop("proxy should be a character.")}
    if ((estimator == "OLS" & ! is.null(proxyIV))) {stop("proxyIV should only be specified when estimator = 'FHS'.")}
    if ((estimator == "FHS" & ! is.character(proxyIV))) {stop("proxyIV should be a character.")}
    if (! (is.numeric(M) & M > 0)) {stop("M should be a positive integer.")}
    # should LM be > 0?
    if (! (is.numeric(LM) & LM > 0)) {stop("LM should be a positive integer.")}
    # should G be > 0?
    if (! (is.numeric(G) & G > 0)) {stop("G should be a positive integer.")}
    # should LG be > 0?
    if (! (is.numeric(LG) & LG > 0)) {stop("LG should be a positive integer.")}
    if (LG < G) {stop("LG should be greater than G")}
    # should normalize be a negative integer?
    if (!is.numeric(normalize)) {stop("normalize should be numeric.")}

    df_policy_leads <- PrepareLeads(data, groupvar = idvar, timevar = timevar, leadvar = policyvar, leads = 1:M)
    df_policy_lags <- PrepareLags(data, groupvar = idvar, timevar = timevar, lagvar = policyvar, lags = 1:G)

    str_policy_leads <- names(dplyr::select(df_policy_leads, dplyr::matches("_lead[1-9]+[0-9]*$")))
    str_policy_lags <- names(dplyr::select(df_policy_lags, dplyr::matches("_lag[1-9]+[0-9]*$")))

    reg_formula <- reformulate(termlabels = c(policyvar, controls, str_policy_leads, str_policy_lags),
                               response = outcomevar)

    df_leads_lags <- cbind(data, df_policy_leads, df_policy_lags)

    as.formula(
        estimatr::lm_robust(
        formula = reg_formula,
        data = df_leads_lags,
        fixed_effects = get(idvar) ~ get(timevar)
        )
    )

}
