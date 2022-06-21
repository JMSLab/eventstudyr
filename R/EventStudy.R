#' Estimates Equation (2) in Freyaldenhoven et al. (forthcoming)
#'
#' @param estimator Accepts one of "OLS" or "FHS". If "FHS" is specified, implements IV.
#' @param data The data frame that contains the variables of interest.
#' @param outcomevar Variable indicating outcome variable y, should be a character.
#' @param policyvar Variable indicating policy variable z, should be a character.
#' @param idvar Variable indicating units, should be a character.
#' @param timevar Variable indicating time periods, should be a character.
#' @param controls Optional vector of controls q, should be a character.
#' @param proxy Variable that is thought to be affected by the confound but not by the policy.
#' Should be specified if and only if estimator is specified as "FHS". Should be a character.
#' @param proxyIV Variables to be used as an instrument. For the case of a single proxy,
#' defaults to the strongest lead of the policy variable based on the first stage.
#' Should be specified if and only if estimator is specified as "FHS".
#' Should be a character
#' @param FE Specifies if unit fixed-effects should be included. Defaults to TRUE.
#' @param TFE Specifies if time fixed-effects should be included. Defaults to TRUE.
#' @param cluster Specifies whether to use clustered errors by units. If FALSE, will use unclustered
#' heteroskedasticity-robust standard errors. Defaults to TRUE.
#' @param post The number of periods in the past before which the past values of the policy
#' are not supposed to affect the value of the outcome. Should be a whole number. Corresponds to M in equation (2) of
#' Freyaldenhoven et al. (forthcoming).
#' @param overidpost Optional number of event times after "post" to be included in estimation. Defaults to 1.
#' Should be a whole number. Corresponds to L_M in equation (2) of Freyaldenhoven et al. (forthcoming).
#' @param pre Number of periods in the future after which the future values of the policy are
#' not supposed to affect the value of the outcome today. Should be a whole number. Corresponds to G in equation (2) of
#' Freyaldenhoven et al. (forthcoming).
#' @param overidpre Optional number of event times earlier than -"pre" to be included in estimation. Defaults to "post" + "pre".
#' Should be a whole number. Corresponds to L_G in equation (2) of Freyaldenhoven et al. (forthcoming).
#' @param normalize Specifies the event-time coefficient to be normalized. Defaults to - pre - 1.
#'
#' @return A list that contains the estimation output and an object containing the arguments passed to the function
#' @import dplyr
#' @export
#'
#' @examples
#' EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
#' policyvar = "z", idvar = "id", timevar = "t",
#' controls = "x_r", FE = TRUE, TFE = TRUE,
#' post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)
#'
#' #If you would like to estimate a static model:
#' EventStudy(estimator = "OLS", data = df_sample_static, outcomevar = "y_static",
#' policyvar = "z", idvar = "id", timevar = "t",
#' FE = TRUE, TFE = TRUE,
#' post = 0, pre = 0, overidpre = 0, overidpost = 0, cluster = TRUE)

EventStudy <- function(estimator, data, outcomevar, policyvar, idvar, timevar, controls = NULL,
                       proxy = NULL, proxyIV = NULL, FE = TRUE, TFE = TRUE, post, overidpost = 1, pre, overidpre = post + pre,
                       normalize = -1 * (pre + 1), cluster = TRUE) {

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
    if (! (is.numeric(post) & post >= 0 & post %% 1 == 0)) {stop("post should be a whole number.")}
    if (! (is.numeric(overidpost) & overidpost >= 0 & overidpost %% 1 == 0)) {stop("overidpost should be a whole number.")}
    if (! (is.numeric(pre) & pre >= 0 & pre %% 1 == 0)) {stop("pre should be a whole number.")}
    if (! (is.numeric(overidpre) & overidpre >= 0 & overidpre %% 1 == 0)) {stop("overidpre should be a whole number.")}
    if (! (is.numeric(normalize) & normalize %% 1 == 0 & normalize >= -(pre + overidpre + 1) &
           normalize <= post + overidpost)) {stop("normalize should be an integer between - (pre + overidpre + 1) and (post + overidpost).")}
    if (! is.logical(cluster)) {stop("cluster should be either TRUE or FALSE.")}
    max_period <- max(data[[timevar]], na.rm = T)
    min_period <- min(data[[timevar]], na.rm = T)
    if  (overidpost + pre + post + overidpost > max_period - min_period - 1) {stop("overidpost + pre + post + overidpost can not exceed the data window")}
    if  (sum(grepl(paste0(policyvar, "_fd"), colnames(data))) > 0) {warning(paste0("Variables starting with ", policyvar,
                                                                                   "_fd should be reserved for eventstudyr"))}
    if  (sum(grepl(paste0(policyvar, "_lead"), colnames(data))) > 0) {warning(paste0("Variables starting with ", policyvar,
                                                                                   "_lead should be reserved for eventstudyr"))}
    if  (sum(grepl(paste0(policyvar, "_lag"), colnames(data))) > 0) {warning(paste0("Variables starting with ", policyvar,
                                                                                   "_lag should be reserved for eventstudyr"))}

    num_fd_lag_periods   <- post + overidpost - 1
    num_fd_lead_periods  <- pre + overidpre

    furthest_lag_period  <- num_fd_lag_periods + 1

    if (post + overidpost + pre + overidpre > 0) {
        data <- GetFirstDifferences(df = data, groupvar = idvar, timevar, diffvar = policyvar)
    }

    if (post + overidpost - 1 >= 1) {
        data <- PrepareLags(data, groupvar = idvar, timevar,
                            lagvar = paste0(policyvar, "_fd"), lags = 1:num_fd_lag_periods)
    }

    if (pre + overidpre >= 1) {
        data <- PrepareLeads(data, groupvar = idvar, timevar, leadvar = paste0(policyvar, "_fd"),
                             leads = 1:num_fd_lead_periods)
    }


    if (post == 0 & overidpost == 0 & pre == 0 & overidpre == 0) {
        data      <- PrepareLeads(data, groupvar = idvar, timevar,
                                         leadvar = policyvar, leads = num_fd_lead_periods)
    } else {
        data             <- PrepareLags(data, groupvar = idvar, timevar,
                                    lagvar = policyvar, lags = furthest_lag_period)
        data             <- PrepareLeads(data, groupvar = idvar, timevar,
                                    leadvar = policyvar, leads = num_fd_lead_periods)
        column_subtract_1              <- paste0(policyvar, "_lead", num_fd_lead_periods)
        data[column_subtract_1] <- 1 - data[column_subtract_1]
    }

    if (normalize < 0) {
        if (normalize == -(pre + overidpre + 1)) {
           normalization_column <- paste0(policyvar, "_lead", (-1 * (normalize + 1)))
        } else {
            normalization_column <- paste0(policyvar, "_fd_lead", (-1 * normalize))
        }
    } else if (normalize == 0){
        if (normalize == post + overidpost) {
            normalization_column <- paste0(policyvar, "_lag", (normalize))
        } else {
            normalization_column <- paste0(policyvar, "_fd")
        }
    } else {
        if (normalize == post + overidpost) {
            normalization_column <- paste0(policyvar, "_lag", (normalize))
        } else {
            normalization_column <- paste0(policyvar, "_fd_lag", (normalize))
        }
    }

    if (normalize == -(pre + overidpre + 1) &  (post + overidpost + pre + overidpre > 0)) {
        # the latter condition is to avoid removing all variables in the case where M = L_M = G = L_G = 0
        str_policy_fd   <- names(dplyr::select(data, dplyr::starts_with(paste0(policyvar, "_fd"))))
        str_policy_lead <- names(dplyr::select(data, dplyr::starts_with(paste0(policyvar, "_lead")), - dplyr::all_of(normalization_column)))
        str_policy_lag  <- names(dplyr::select(data, dplyr::starts_with(paste0(policyvar, "_lag"))))
    } else if (normalize == (post + overidpost)) {
        str_policy_fd   <- names(dplyr::select(data, dplyr::starts_with(paste0(policyvar, "_fd"))))
        str_policy_lead <- names(dplyr::select(data, dplyr::starts_with(paste0(policyvar, "_lead"))))
        str_policy_lag  <- names(dplyr::select(data, dplyr::starts_with(paste0(policyvar, "_lag")), - dplyr::all_of(normalization_column)))
    } else {
        str_policy_fd   <- names(dplyr::select(data, dplyr::starts_with(paste0(policyvar, "_fd")), - dplyr::all_of(normalization_column)))
        str_policy_lead <- names(dplyr::select(data, dplyr::starts_with(paste0(policyvar, "_lead"))))
        str_policy_lag  <- names(dplyr::select(data, dplyr::starts_with(paste0(policyvar, "_lag"))))
    }

    if (post + overidpost - 1 < 0) {
        str_policy_fd <- str_policy_fd[str_policy_fd != paste0(policyvar, "_fd")]
    }

    if (estimator == "OLS") {

        event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls)

        OLS_model        <- EventStudyOLS(event_study_formula, data, idvar, timevar, FE, TFE, cluster)
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
                              "post" = post,
                              "overidpost" = overidpost,
                              "pre" = pre,
                              "overidpre" = overidpre,
                              "normalize" = normalize,
                              "normalization_column" = normalization_column,
                              "cluster" = cluster,
                              "eventstudy_coefficients" = c(str_policy_fd, str_policy_lead, str_policy_lag)
                              )

        return(list(OLS_model, event_study_args))

    }

}

