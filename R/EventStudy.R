
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
#' @param FE Specifies if unit fixed-effects should be included. Defaults to TRUE.
#' @param TFE Specifies if time fixed-effects should be included. Defaults to TRUE.
#' @param cluster Specifies whether to use clustered errors by units. If FALSE, will use unclustered
#' heteroskedasticity-robust standard errors. Defaults to TRUE.
#' @param M The number of periods in the past before which the past values of the policy
#' are not supposed to affect the value of the outcome. Should be a positive integer.
#' @param LM Optional number of event times after M to be included in estimation. Defaults to 1.
#' Should be a positive integer.
#' @param G Number of periods in the future after which the future values of the policy are
#' not supposed to affect the value of the outcome today. Should be a positive integer.
#' @param LG Optional number of event times earlier than -G to be included in estimation. Defaults to M + G.
#' Should be a positive integer.
#' @param normalize Specifies the event-time coefficient to be normalized. Defaults to - 1.
#' Should be a negative integer.
#'
#' @return A list that contains the estimation output and an object containing the arguments passed to the function
#' @import dplyr
#' @export
#'
#' @examples
#' EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
#' policyvar = "z", idvar = "id", timevar = "t",
#' controls = "x_r", FE = TRUE, TFE = TRUE,
#' M = 3, G = 2, LG = 4, LM = 5, normalize = - 1, cluster = TRUE)

EventStudy <- function(estimator, data, outcomevar, policyvar, idvar, timevar, controls = NULL,
                       proxy = NULL, proxyIV = NULL, FE = TRUE, TFE = TRUE, M, LM = 1, G, LG = M + G,
                       normalize = - 1, cluster = TRUE) {

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
    if (! is.logical(FE)) {stop("FE should be either TRUE or FALSE.")}
    if (! is.logical(TFE)) {stop("TFE should be either TRUE or FALSE.")}
    if (! (is.numeric(M) & M > 0)) {stop("M should be a positive integer.")}
    if (! (is.numeric(LM) & LM > 0)) {stop("LM should be a positive integer.")}
    if (! (is.numeric(G) & G >= 0)) {stop("G should be a whole number.")}
    if (! (is.numeric(LG) & LG > 0)) {stop("LG should be a positive integer.")}
    if (! (is.numeric(normalize) & normalize < 0)) {stop("normalize should be a negative integer.")}
    if (! is.logical(cluster)) {stop("cluster should be either TRUE or FALSE.")}

    df_first_diff <- GetFirstDifferences(df = data, groupvar = idvar, timevar, diffvar = policyvar)

    num_fd_lead_periods <- M + LM - 1
    num_fd_lag_periods  <- G + LG

    first_lag_period    <- num_fd_lag_periods + 1

    df_first_diff_leads      <- PrepareLeads(df_first_diff, groupvar = idvar, timevar,
                                             leadvar = paste0(policyvar, "_fd"), leads = 1:num_fd_lead_periods)
    df_first_diff_leads_lags <- PrepareLags(df_first_diff_leads, groupvar = idvar, timevar,
                                             lagvar = paste0(policyvar, "_fd"), lags = 1:num_fd_lag_periods)

    df_lead     <- PrepareLeads(df_first_diff_leads_lags, groupvar = idvar, timevar,
                                leadvar = policyvar, leads = num_fd_lead_periods)
    df_lead_lag <- PrepareLags(df_lead, groupvar = idvar, timevar,
                               lagvar = policyvar, lags = first_lag_period)

    column_subtract_1              <- paste0(policyvar, "_lead", num_fd_lead_periods)
    df_lead_lag[column_subtract_1] <- 1 - df_lead_lag[column_subtract_1]

    normalization_column <- paste0(policyvar, "_fd_lag", (-1 * normalize))

    str_policy_fd   <- names(dplyr::select(df_lead_lag, dplyr::starts_with(paste0(policyvar, "_fd")), -normalization_column))
    str_policy_lead <- names(dplyr::select(df_lead_lag, dplyr::starts_with(paste0(policyvar, "_lead"))))
    str_policy_lag  <- names(dplyr::select(df_lead_lag, dplyr::starts_with(paste0(policyvar, "_lag"))))

    event_study_formula <- PrepareModelFormula(outcomevar, str_policy_fd, str_policy_lead, str_policy_lag)

    if (estimator == "OLS") {

        OLS_model        <- EventStudyOLS(event_study_formula, df_lead_lag, idvar, timevar, FE, TFE, cluster)
        event_study_args <- list("estimator" = estimator,
                              "data" = data,
                              "outcomevar" = outcomevar,
                              "policyvar" = policyvar,
                              "idvar" = idvar,
                              "timevar" = timevar,
                              "controls" = controls,
                              "proxy" = proxy,
                              "proxyIV" = proxyIV,
                              "FE" = FE,
                              "TFE" = TFE,
                              "M" = M,
                              "LM" = LM,
                              "G" = G,
                              "LG" = LG,
                              "normalize" = normalize,
                              "cluster" = cluster)

        return(list(OLS_model, event_study_args))

    }

}





