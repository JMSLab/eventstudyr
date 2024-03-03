#' Estimates Equation (2) in Freyaldenhoven et al. (2021)
#'
#' @description `EventStudy` uses regression methods to estimate the effect of a policy on a given outcome.
#'
#' @param estimator Accepts one of "OLS" or "FHS". If "OLS" is specified, implements Ordinary Least Squares. If "FHS" is specified, implements Instrumental Variables (IV) estimator proposed in [Freyaldenhoven Hansen Shapiro (FHS, 2019)](https://www.aeaweb.org/articles?id=10.1257/aer.20180609).
#' @param data Data frame containing the variables of interest.
#' @param outcomevar Character indicating column of outcome variable y.
#' @param policyvar Character indicating column of policy variable z.
#' @param idvar Character indicating column of units.
#' @param timevar Character indicating column of time periods.
#' @param controls Optional character vector indicating a set of control variables q.
#' @param proxy Character indicating column of variable that is thought to be affected by the confound but not by the policy.
#' Should be specified if and only if estimator is specified as "FHS".
#' @param proxyIV Character of column to be used as an instrument. Should be specified if and only if estimator is specified as "FHS".
#' If NULL, defaults to the strongest lead of the policy variable based on the first stage.
#' @param FE Logical indicating whether unit fixed-effects should be included. Defaults to TRUE.
#' @param TFE Logical indicating whether time fixed-effects should be included. Defaults to TRUE.
#' @param cluster Logical indicating whether to use clustered errors by units. If FALSE, will use unclustered heteroskedasticity-robust standard errors.
#' Defaults to TRUE. Must be TRUE if FE is TRUE.
#' @param post Whole number indicating the number of periods in the past before which the past values of the policy
#' are not supposed to affect the value of the outcome. Corresponds to M in equation (2) of
#' [Freyaldenhoven et al. (2021)](https://www.nber.org/papers/w29170).
#' @param overidpost Optional whole number indicating the number of event times after "post" to be included in estimation.
#' Defaults to 1.
#' Corresponds to L_M in equation (2) of [Freyaldenhoven et al. (2021)](https://www.nber.org/papers/w29170).
#' @param pre Whole number indicating the number of periods in the future after which the future values of the policy are
#' not supposed to affect the value of the outcome today. Corresponds to G in equation (2) of
#' [Freyaldenhoven et al. (2021)](https://www.nber.org/papers/w29170).
#' @param overidpre Optional whole number indicating the number of event times earlier than -"pre" to be included in estimation.
#' Defaults to "post" + "pre".
#' Corresponds to L_G in equation (2) of [Freyaldenhoven et al. (2021)](https://www.nber.org/papers/w29170).
#' @param normalize Specifies the event-time coefficient to be normalized. Defaults to - pre - 1.
#' @param anticipation_effects_normalization If set to TRUE, runs the default process and switches coefficient to be normalized to 0
#' when there are anticipation effects. If set to FALSE, does not make the switch. Defaults to TRUE.
#'
#' @return A list that contains, under "output", the estimation output as an lm_robust object, and under "arguments", the arguments passed to the function.
#' @import dplyr
#' @import estimatr
#' @importFrom stats reformulate
#' @importFrom data.table setorderv as.data.table .SD
#' @export
#'
#' @examples
#'
#' # A minimal example
#' eventstudy_model <-
#'   EventStudy(
#'     estimator = "OLS",
#'     data = example_data,
#'     outcomevar = "y_base",
#'     policyvar = "z",
#'     idvar = "id",
#'     timevar = "t",
#'     pre = 0, post = 3,
#'     normalize = -1
#'   )
#'
#' ### Access estimated model
#' eventstudy_model$output
#'
#' summary(eventstudy_model$output)
#'
#' ### data.frame of estimates
#' estimatr::tidy(eventstudy_model$output)
#'
#' ### Access arguments
#' eventstudy_model$arguments
#'
#' # A dynamic OLS model with anticipation effects and controls
#' eventstudy_model_dyn <-
#'   EventStudy(
#'     estimator = "OLS",
#'     data = example_data,
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
#' summary(eventstudy_model_dyn$output)
#'
#' # A static model
#' eventstudy_model_static <-
#'   EventStudy(
#'     estimator = "OLS",
#'     data = example_data,
#'     outcomevar = "y_jump_m",
#'     policyvar = "z",
#'     idvar = "id",
#'     timevar = "t",
#'     FE = TRUE, TFE = TRUE,
#'     post = 0, overidpost = 0,
#'     pre  = 0, overidpre  = 0,
#'     cluster = TRUE
#'   )
#'
#' summary(eventstudy_model_static$output)
#'
#' # A dynamic model with an unbalanced panel
#' data_unbal <- example_data[1:(nrow(example_data)-1),]  # drop last row to make unbalanced
#'
#' eventstudy_model_unbal <-
#'  EventStudy(
#'     estimator = "OLS",
#'     data = data_unbal,
#'     outcomevar = "y_base",
#'     policyvar = "z",
#'     idvar = "id",
#'     timevar = "t",
#'     pre = 0, post = 3,
#'     normalize = -1
#'   )
#'
#' summary(eventstudy_model_unbal$output)
#'
#' # A dynamic model estimated using IV
#' eventstudy_model_iv <-
#'   EventStudy(
#'     estimator = "FHS",
#'     data = example_data,
#'     outcomevar = "y_base",
#'     policyvar = "z",
#'     idvar = "id",
#'     timevar = "t",
#'     proxy = "x_r",
#'     FE = TRUE, TFE = TRUE,
#'     post = 2, overidpost = 1,
#'     pre  = 0, overidpre  = 3,
#'     normalize = -1,
#'     cluster = TRUE
#'   )
#'
#' summary(eventstudy_model_iv$output)
#'

EventStudy <- function(estimator, data, outcomevar, policyvar, idvar, timevar, controls = NULL,
                       proxy = NULL, proxyIV = NULL, FE = TRUE, TFE = TRUE, post, overidpost = 1, pre, overidpre = post + pre,
                       normalize = -1 * (pre + 1), cluster = TRUE, anticipation_effects_normalization = TRUE) {

    # Check for errors in arguments
    if (! estimator %in% c("OLS", "FHS")) {stop("estimator should be either 'OLS' or 'FHS'.")}
    if (! is.data.frame(data))            {stop("data should be a data frame.")}
    if (! is.character(outcomevar))       {stop("outcomevar should be a character.")}
    if (! is.character(policyvar))        {stop("policyvar should be a character.")}
    if (! is.character(idvar))            {stop("idvar should be a character.")}
    if (! is.character(timevar))          {stop("timevar should be a character.")}
    if (! (is.null(controls) | is.character(controls))) {stop("controls should be either NULL or a character.")}

    if ((estimator == "OLS" & ! is.null(proxy)))        {stop("proxy should only be specified when estimator = 'FHS'.")}
    if ((estimator == "FHS" & ! is.character(proxy)))   {stop("proxy should be a character.")}
    if ((estimator == "OLS" & ! is.null(proxyIV)))      {stop("proxyIV should only be specified when estimator = 'FHS'.")}
    if ((estimator == "FHS" &
        ! is.null(proxyIV) & ! is.character(proxyIV)))  {stop("proxyIV should be a character.")}
    if (estimator == "FHS" & pre == 0 & overidpre == 0 & is.null(proxyIV)) {
        stop("When estimator is 'FHS' and there are no leads in the model, proxyIV must be specified explicitly.")
    }

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
    if (! all(data[[timevar]] %% 1 == 0)) {
        stop("timevar column in dataset should be a vector of integers.")
    }

    data_ids <- as.data.frame(data)[, c(idvar, timevar)]

    n_units       <- length(base::unique(data[[idvar]]))
    n_periods     <- length(base::unique(data[[timevar]]))
    n_unique_rows <- nrow(data[!base::duplicated(data_ids),])
    if (n_unique_rows != n_units*n_periods) {
        warning("Dataset is unbalanced.")
        unbalanced <- TRUE
    } else {
        unbalanced <- FALSE
    }

    data.table::setorderv(data, c(idvar, timevar))

    detect_holes <- function(dt, idvar, timevar) {
        dt <- data.table::as.data.table(dt)
        holes_per_id <- dt[, .SD[!is.na(base::get(timevar))], by = c(idvar)
                         ][, list(holes = any(base::diff(base::get(timevar)) != 1)),
                            by = c(idvar)]

        return(any(holes_per_id$holes))
    }

    if (detect_holes(data, idvar, timevar)) {
        warning(paste0("Note: gaps of more than one unit in the time variable '", timevar, "' were detected. ",
                       "Treating these as gaps in the panel dimension."))
        timevar_holes <- TRUE
    } else {
        timevar_holes <- FALSE
    }

    if (post == 0 & overidpost == 0 & pre == 0 & overidpre == 0) {
        static <- TRUE
    } else {
        static <- FALSE
    }

    num_evenstudy_coeffs <- overidpre + pre + post + overidpost
    num_periods          <- max(data[[timevar]], na.rm = T) - min(data[[timevar]], na.rm = T)
    if  (num_evenstudy_coeffs > num_periods - 1) {stop("overidpre + pre + post + overidpost cannot exceed the data window.")}

    for (tag in c("_fd", "_lead", "_lag")) {
        if  (sum(grepl(paste0(policyvar, tag), colnames(data))) > 0) {
            warning(paste0("Variables starting with ", policyvar, tag,
                           " should be reserved for usage by eventstudyr."))
        }
    }

    # Compute shifts in policy variable
    num_fd_lags  <- post + overidpost - 1
    num_fd_leads <- pre  + overidpre

    furthest_lag_period <- num_fd_lags + 1

    if (static) {
        message("post, overidpost, pre, and overidpre are set to 0. A static model will be estimated.")
    } else {
        data <- ComputeFirstDifferences(data, idvar, timevar, policyvar, timevar_holes)

        if ((post + overidpost - 1 >= 1) & (pre + overidpre >= 1)) {
            shift_values = c(-num_fd_leads:-1, 1:num_fd_lags)
        } else if (pre + overidpre < 1) {
            shift_values = 1:num_fd_lags
        } else if (post + overidpost - 1 < 1) {
            shift_values = -num_fd_leads:-1
        }

        data <- ComputeShifts(data, idvar, timevar,
                              shiftvar    = paste0(policyvar, "_fd"),
                              shiftvalues = shift_values,
                              timevar_holes = timevar_holes)
    }

    if (!static) {
        data <- ComputeShifts(data, idvar, timevar,
                              shiftvar    = policyvar,
                              shiftvalues = c(-num_fd_leads, furthest_lag_period),
                              timevar_holes = timevar_holes)

        lead_endpoint_var <- paste0(policyvar, "_lead", num_fd_leads)
        data[lead_endpoint_var] <- 1 - data[lead_endpoint_var]
    }

    if (pre != 0 & normalize == -1 & anticipation_effects_normalization) {
        normalize <- -pre - 1
        warning(paste("You allowed for anticipation effects", pre,
                      "periods before the event, so the coefficient at", normalize,
                      "was selected to be normalized to zero.",
                      "To override this, change anticipation_effects_normalization to FALSE."))
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

    if (static) {
        str_policy_vars = policyvar
    } else {
        all_vars <- names(data)[grepl(policyvar, names(data))]

        lead_endpoint_var <- all_vars[grepl(paste0("^", policyvar, "_lead"), all_vars)]
        lead_fd_vars      <- all_vars[grepl(paste0("^", policyvar, "_fd_lead"), all_vars)]
        fd_var            <- paste0(policyvar, "_fd")
        lag_fd_vars       <- all_vars[grepl(paste0("^", policyvar, "_fd_lag"), all_vars)]
        lag_endpoint_var  <- all_vars[grepl(paste0("^", policyvar, "_lag"), all_vars)]

        str_policy_vars <- c(lead_endpoint_var, lead_fd_vars, fd_var, lag_fd_vars, lag_endpoint_var)
        str_policy_vars <- str_policy_vars[!(str_policy_vars %in% normalization_column)]
    }

    if (estimator == "OLS") {
        event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_vars,
                                                   static, controls, proxy, proxyIV)

        output       <- EventStudyOLS(event_study_formula, data, idvar, timevar, FE, TFE, cluster)
        coefficients <- str_policy_vars
    }
    if (estimator == "FHS") {

        if (is.null(proxyIV)) {
            Fstart <- 0
            str_fd_leads <- str_policy_vars[grepl("^z_fd_lead", str_policy_vars)]

            for (var in str_fd_leads) {
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

        event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_vars,
                                                   static, controls, proxy, proxyIV)

        output       <- EventStudyFHS(event_study_formula, data, idvar, timevar, FE, TFE, cluster)
        coefficients <- dplyr::setdiff(str_policy_vars, proxyIV)
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
                "arguments" = event_study_args))
}
