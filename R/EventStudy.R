#' Estimates Equation (2) in [Freyaldenhoven et al. (2021)](https://www.nber.org/system/files/working_papers/w29170/w29170.pdf).
#'
#' @param estimator Accepts one of "OLS" or "FHS". If "FHS" is specified, implements IV estimator in Freyaldenhoven et al. 2019.
#' @param data Data frame that contains the variables of interest.
#' @param outcomevar Character indicating column of outcome variable y.
#' @param policyvar Character indicating column of policy variable z.
#' @param idvar Character indicating column of units.
#' @param timevar Character indicating column of time periods.
#' @param controls Character indicating optional vector of controls q.
#' @param proxy Character indicating column of variable that is thought to be affected by the confound but not by the policy.
#' Should be specified if and only if estimator is specified as "FHS".
#' @param proxyIV Character of column to be used as an instrument. Should be specified if and only if estimator is specified as "FHS".
#' If NULL, defaults to the strongest lead of the policy variable based on the first stage.
#' @param FE Specifies if unit fixed-effects should be included. Defaults to TRUE.
#' @param TFE Specifies if time fixed-effects should be included. Defaults to TRUE.
#' @param cluster Specifies whether to use clustered errors by units. If FALSE, will use unclustered
#' heteroskedasticity-robust standard errors. Defaults to TRUE. Must be TRUE if FE is TRUE.
#' @param post Whole number indicating the number of periods in the past before which the past values of the policy
#' are not supposed to affect the value of the outcome. Corresponds to M in equation (2) of
#' [Freyaldenhoven et al. (2021)](https://www.nber.org/system/files/working_papers/w29170/w29170.pdf).
#' @param overidpost Optional whole number indicating the number of event times after "post" to be included in estimation. 
#' Defaults to 1.
#' Corresponds to L_M in equation (2) of [Freyaldenhoven et al. (2021)](https://www.nber.org/system/files/working_papers/w29170/w29170.pdf).
#' @param pre Whole number indicating the number of periods in the future after which the future values of the policy are
#' not supposed to affect the value of the outcome today. Corresponds to G in equation (2) of
#' [Freyaldenhoven et al. (2021)](https://www.nber.org/system/files/working_papers/w29170/w29170.pdf).
#' @param overidpre Optional whole number indicating the number of event times earlier than -"pre" to be included in estimation. 
#' Defaults to "post" + "pre".
#' Corresponds to L_G in equation (2) of [Freyaldenhoven et al. (2021)](https://www.nber.org/system/files/working_papers/w29170/w29170.pdf).
#' @param normalize Specifies the event-time coefficient to be normalized. Defaults to - pre - 1.
#' @param anticipation_effects_normalization If set to TRUE, runs default process and switches coefficient to be normalized to 0
#' when there are anticipation effects. If set to FALSE, does not make switch. Defaults to TRUE.
#'
#' @return A list that contains the estimation output and an object containing the arguments passed to the function
#' @import dplyr
#' @export
#'
#' @examples
#'
#' # A minimal example
#' eventstudy_model <-
#'   EventStudy(
#'     estimator = "OLS",
#'     data = df_sample_dynamic,
#'     outcomevar = "y_base",
#'     policyvar = "z",
#'     idvar = "id",
#'     timevar = "t",
#'     pre = 0, post = 3,
#'     normalize = -1
#'   )
#'
#' ### Access estimates
#' eventstudy_model$output
#'
#' ### Access arguments
#' eventstudy_model$arguments
#'
#' # A dynamic OLS model with anticipation effects and controls
#' eventstudy_model_dyn <-
#'   EventStudy(
#'     estimator = "OLS",
#'     data = df_sample_dynamic,
#'     outcomevar = "y_base",
#'     policyvar = "z",
#'     idvar = "id",
#'     timevar = "t",
#'     controls = "x_r",
#'     FE = TRUE, TFE = TRUE,
#'     post = 3, overidpost = 5,
#'     pre  = 2, overidpre  = 4,
#'     normalize = - 3,
#'     cluster = TRUE,
#'     anticipation_effects_normalization = TRUE
#'   )
#'
#' eventstudy_model_dyn$output
#'
#' # A static model
#' eventstudy_model_static <-
#'   EventStudy(
#'     estimator = "OLS",
#'     data = df_sample_static,
#'     outcomevar = "y_static",
#'     policyvar = "z",
#'     idvar = "id",
#'     timevar = "t",
#'     FE = TRUE, TFE = TRUE,
#'     post = 0, overidpost = 0,
#'     pre  = 0, overidpre  = 0,
#'     cluster = TRUE
#'   )
#'
#' eventstudy_model_static$output
#' 
#' ### Output as data.frame
#' estimatr::tidy(eventstudy_model_static$output)
#'
#' # A dynamic model estimated using IV
#' eventstudy_model_iv <-
#'   EventStudy(
#'     estimator = "FHS",
#'     data = df_sample_dynamic,
#'     outcomevar = "y_base",
#'     policyvar = "z",
#'     idvar = "id",
#'     timevar = "t",
#'     controls = "x_r",
#'     proxy = "eta_m",
#'     FE = TRUE, TFE = TRUE,
#'     post = 2, overidpost = 1,
#'     pre  = 0, overidpre  = 3,
#'     normalize = -1,
#'     cluster = TRUE
#'   )
#'
#' eventstudy_model_iv$output
#'

EventStudy <- function(estimator, data, outcomevar, policyvar, idvar, timevar, controls = NULL,
                       proxy = NULL, proxyIV = NULL, FE = TRUE, TFE = TRUE, post, overidpost = 1, pre, overidpre = post + pre,
                       normalize = -1 * (pre + 1), cluster = TRUE, anticipation_effects_normalization = TRUE) { #There is no default values for post and pre arguments. It breaks if I don't specify them

    # Check for errors in arguments
    if (! estimator %in% c("OLS", "FHS")) {stop("estimator should be either 'OLS' or 'FHS'.")}
    if (! is.data.frame(data))            {stop("data should be a data frame.")}
    if (! is.character(outcomevar))       {stop("outcomevar should be a character.")}
    if (! is.character(policyvar))        {stop("policyvar should be a character.")}
    if (! is.character(idvar))            {stop("idvar should be a character.")}
    if (! is.character(timevar))          {stop("timevar should be a character.")}
    if (! (is.null(controls) | is.character(controls))) {stop("controls should be either NULL or a character.")}

    if ((estimator == "OLS" & ! is.null(proxy)))       {stop("proxy should only be specified when estimator = 'FHS'.")}
    if ((estimator == "FHS" & ! is.character(proxy)))  {stop("proxy should be a character.")}
    if ((estimator == "OLS" & ! is.null(proxyIV)))     {stop("proxyIV should only be specified when estimator = 'FHS'.")}
    if ((estimator == "FHS" & 
        ! is.null(proxyIV) & ! is.character(proxyIV))) {stop("proxyIV should be a character.")}

    if (! is.logical(FE))      {stop("FE should be either TRUE or FALSE.")}
    if (! is.logical(TFE))     {stop("TFE should be either TRUE or FALSE.")}
    if (! is.logical(cluster)) {stop("cluster should be either TRUE or FALSE.")}
    if (FE & !cluster)         {stop("cluster=TRUE is required when FE=TRUE.")}
    if (! is.logical(anticipation_effects_normalization)) {stop("anticipation_effects_normalization should be either TRUE or FALSE.")}

    if (! (is.numeric(post)       &  post >= 0      &  post %% 1 == 0))           {stop("post should be a whole number.")}
    if (! (is.numeric(overidpost) & overidpost >= 0 & overidpost %% 1 == 0))      {stop("overidpost should be a whole number.")}
    if (! (is.numeric(pre)        &  pre >= 0       &  pre %% 1 == 0))            {stop("pre should be a whole number.")}
    if (! (is.numeric(overidpre)  & overidpre >= 0  & overidpre %% 1 == 0))       {stop("overidpre should be a whole number.")}
    if (normalize == 0 & post == 0 & overidpost == 0 & pre == 0 & overidpre == 0) {stop("normalize cannot be zero when post = overidpost = pre = overidpre = 0.")}
    if (! (is.numeric(normalize) & normalize %% 1 == 0 
           & normalize >= -(pre + overidpre + 1) & normalize <= post + overidpost)) {
        stop("normalize should be an integer between -(pre + overidpre + 1) and (post + overidpost).")
    }

    # Check for errors in data
    if (! is.numeric(data[[timevar]])) {stop("timevar column in dataset should be numeric.")}
    
    data_ids <- as.data.frame(data)[, c(idvar, timevar)]

    n_units       <- length(base::unique(data[[idvar]]))
    n_periods     <- length(base::unique(data[[timevar]]))
    n_unique_rows <- nrow(data[!base::duplicated(data_ids),])
    if (n_unique_rows != n_units*n_periods) {
        warning("Dataset is unbalanced.")
    }

    num_evenstudy_coeffs <- overidpre + pre + post + overidpost
    num_periods          <- max(data[[timevar]], na.rm = T) - min(data[[timevar]], na.rm = T)
    if  (num_evenstudy_coeffs > num_periods - 1) {stop("overidpre + pre + post + overidpost cannot exceed the data window")} 

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
        data <- PrepareLags(data, groupvar = idvar, timevar,
                            lagvar = policyvar, lags = furthest_lag_period)
        data <- PrepareLeads(data, groupvar = idvar, timevar,
                             leadvar = policyvar, leads = num_fd_lead_periods)

        column_subtract_1 <- paste0(policyvar, "_lead", num_fd_lead_periods)
        data[column_subtract_1] <- 1 - data[column_subtract_1]
    }

    if (pre !=0 & normalize == -1 & anticipation_effects_normalization) {
        normalize <- -pre - 1
        warning(paste("You allowed for anticipation effects", pre,
                      "periods before the event, so the coefficient at", normalize,
                      "was selected to be normalized to zero. To override this, change anticipation_effects_normalization to FALSE."))
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
        event_study_formula <- PrepareModelFormula(estimator, outcomevar,
                                                   str_policy_fd, str_policy_lead, str_policy_lag,
                                                   controls, proxy, proxyIV)

        output       <- EventStudyOLS(event_study_formula, data, idvar, timevar, FE, TFE, cluster)
        coefficients <- c(str_policy_fd, str_policy_lead, str_policy_lag)
    }
    if (estimator == "FHS") {

        if (is.null(proxyIV)) { #No default proxyIV when M=G=LM=LG=0, probably it is fine that it does not default to anything, but there is no warning message that the code is breaking because it will not default any value of proxyIV in this case (ES)
            Fstart <- 0
            z_fd_lead_indicator <- grepl("^z_fd_lead", str_policy_fd)
            str_policy_fd_lead <- str_policy_fd[z_fd_lead_indicator]
            for (var in str_policy_fd_lead) {
                lm <- lm(data = data, formula = stats::reformulate(termlabels = var, response = proxy))
                Floop <- summary(lm)$fstatistic["value"]
                if (Floop > Fstart) {
                    Fstart <- Floop
                    proxyIV <- var
                }
            }
            message(paste0("Defaulting to strongest lead of differenced policy variable: proxyIV = ", proxyIV,
                           ". To specify a different proxyIV use the proxyIV argument."))
        }

        event_study_formula <- PrepareModelFormula(estimator, outcomevar,
                                                   str_policy_fd, str_policy_lead, str_policy_lag,
                                                   controls, proxy, proxyIV)

        output       <- EventStudyFHS(event_study_formula, data, idvar, timevar, FE, TFE, cluster)
        coefficients <- dplyr::setdiff(c(str_policy_fd, str_policy_lead, str_policy_lag), proxyIV)
    }

    event_study_args <- list("estimator"  = estimator,
                             "data"       = data,
                             "outcomevar" = outcomevar,
                             "policyvar"  = policyvar,
                             "idvar"      = idvar,
                             "timevar"    = timevar,
                             "controls"   = controls,
                             "proxy"      = proxy,
                             "proxyIV"    = proxyIV,
                             "FE"         = FE,
                             "TFE"        = TFE,
                             "post"       = post,
                             "overidpost" = overidpost,
                             "pre"        = pre,
                             "overidpre"  = overidpre,
                             "normalize"  = normalize,
                             "normalization_column"    = normalization_column,
                             "cluster"                 = cluster,
                             "eventstudy_coefficients" = coefficients)

    return(list("output"    = output,
                "arguments" = event_study_args)) #Note: If M=G=LM=LG=0 the name of the coefficient is "z_lead0". In other cases are fine (ES)
}
