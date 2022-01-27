
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
#' @param FE Specifies if unit fixed-effects should be included. Defaults to TRUE.
#' @param TFE Specifies if time fixed-effects should be included. Defaults to TRUE.
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
#' @param cluster Specifies whether to use clustered errors by units. If FALSE, will use
#' unclustered heteroskedasticity-robust standard errors. Defaults to TRUE. Should be one of TRUE or FALSE.
#'
#' @return A formula object to be passed to EventStudy
#' @import estimatr
#' @export
#'
#' @examples
#' \dontrun{
#' PrepareModelFormula <- function(estimator = "OLS", data, outcomevar = "employment rate",
#' policyvar = "minimum wage", idvar = "state", timevar = "year", controls = NULL,
#' proxy = NULL, proxyIV = NULL, FE = TRUE, TFE = TRUE, M = 3, LM = 3, G = 4, LG = 5,
#' normalize = -1, cluster = TRUE)
#'
#' }


PrepareModelFormula <- function(estimator, data, outcomevar, policyvar, idvar, timevar, controls = NULL,
                                proxy = NULL, proxyIV = NULL, FE = TRUE, TFE = TRUE, M, LM = 1, G, LG = 1,
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
    if (! is.logical(FE)) {stop("FE should be TRUE or FALSE.")}
    if (! is.logical(TFE)) {stop("TFE should be TRUE or FALSE.")}
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
    if (! is.logical(cluster)) {stop("cluster should be TRUE or FALSE")}

    v_policy_leads <- PrepareLeads(data, groupvar = idvar, timevar = timevar, leadvar = policyvar, leads = 1:M)
    v_policy_lags <- PrepareLags(data, groupvar = idvar, timevar = timevar, lagvar = policyvar, lags = 1:G)

    return(v_policy_leads)

    # reg_formula <- reformulate(termlabels = c(policyvar, controls),
    #                            response = outcomevar)
    #
    # estimatr::lm_robust(
    #     formula = reg_formula,
    #     data = data,
    #     fixed_effects = get(idvar) ~ get(timevar)
    # )




}
