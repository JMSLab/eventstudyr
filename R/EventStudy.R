
#' Estimates Equation (2) in Freyaldenhoven et al. (2021)
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
#' @return A list that contains the estimation output and an object containing the arguments passed to the function
#' @import estimatr
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' EventStudy("OLS", gapminder::gapminder, outcomevar = "lifeExp",
#' policyvar = "pop", idvar = "continent", timevar = "year",
#' controls = "gdpPercap", M = 3, G = 2, LG = 4, LM = 5)
#'
#' }


EventStudy <- function(estimator, data, outcomevar, policyvar, idvar, timevar, controls = NULL,
                       proxy = NULL, proxyIV = NULL, FE = TRUE, TFE = TRUE, M, LM = 1, G, LG = M + G,
                       normalize = -1, cluster = TRUE) {

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
    if (! (is.numeric(LM) & LM > 0)) {stop("LM should be a positive integer.")}
    if (! (is.numeric(G) & G > 0)) {stop("G should be a positive integer.")}
    if (! (is.numeric(LG) & LG > 0)) {stop("LG should be a positive integer.")}
    if (!is.numeric(normalize)) {stop("normalize should be numeric.")}

    num_lead_periods <- G + LG + 1
    num_lag_periods <- M + LM

    names_lead_periods <- paste0("_lead", 1:(num_lead_periods - 1))
    names_lag_periods <- paste0("_lag", 1:(num_lag_periods - 1))

    df_policy_leads <- PrepareLeads(data, groupvar = idvar, timevar = timevar, leadvar = policyvar, leads = 1:num_lead_periods)
    df_policy_lags <- PrepareLags(data, groupvar = idvar, timevar = timevar, lagvar = policyvar, lags = 1:num_lag_periods)


    df_leads_lags <- cbind(data, df_policy_leads, df_policy_lags)

    for (lead_var in names_lead_periods) {

        df_leads_lags <- GetFirstDifferences(df = df_leads_lags, groupvar = NULL, timevar = timevar, diffvar = paste0(policyvar, lead_var))

    }

    for (lag_var in names_lag_periods) {

        df_leads_lags <- GetFirstDifferences(df = df_leads_lags, groupvar = NULL, timevar = timevar, diffvar = paste0(policyvar, lag_var))

    }

    str_policy_fd <- names(dplyr::select(df_leads_lags, dplyr::ends_with("_fd")))
    str_policy_lead <- names(dplyr::select(df_leads_lags, dplyr::ends_with(paste0("_lead", num_lead_periods))))
    str_policy_lag <- names(dplyr::select(df_leads_lags, dplyr::ends_with(paste0("_lag", num_lag_periods))))

    PrepareModelFormula(policyvar, controls, str_policy_fd, str_policy_lead, str_policy_lag, outcomevar)


    # if (estimator == "OLS") {
    #
    #
    #
    #     PrepareModelFormula(reg_formula)
    #
    #
    #
    #
    # }

}
