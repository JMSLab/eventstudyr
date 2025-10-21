#' Runs Ordinary Least Squares (OLS) with optional fixed effects and clustering
#'
#' @param prepared_model_formula A formula object created in [PrepareModelFormula()] that is passed to [EventStudy()].
#' @param prepared_data Data frame containing all of the parameters required for [EventStudy()] plus leads and
#' lags of the first differenced policy variable and leads and lags of the policy variable.
#' @param idvar Character indicating column of units.
#' @param timevar Character indicating column of time periods.
#' @param FE Specifies if unit fixed-effects should be included. Defaults to TRUE.
#' @param TFE Specifies if time fixed-effects should be included. Defaults to TRUE.
#' @param cluster Specifies whether to use clustered errors by units. If FALSE, will use unclustered
#' heteroskedasticity-robust standard errors. Defaults to TRUE. Must be TRUE if FE is TRUE.
#'
#' @return A data.frame that contains the estimates for the event study coefficients.
#' @import estimatr
#' @keywords internal
#' @noRd
#'
#' @examples
#' model_formula <-  PrepareModelFormula(
#'    estimator = "OLS",
#'    outcomevar = "y_base",
#'    str_policy_fd = c("z_fd", "z_fd_lead2", "z_fd_lead3", "z_fd_lag1", "z_fd_lag2"),
#'    str_policy_lead = "z_lead3",
#'    str_policy_lag = "z_lag3",
#'    controls = "x_r"
#' )
#'
#' EventStudyOLS(
#'    prepared_model_formula = model_formula,
#'    prepared_data = df_EventStudyOLS_example,
#'    idvar = "id",
#'    timevar = "t",
#'    FE = TRUE,
#'    TFE = TRUE,
#'    cluster = TRUE
#')

EventStudyOLS <- function(prepared_model_formula, prepared_data,
                          idvar, timevar, FE, TFE, cluster) {

    if (! inherits(prepared_model_formula, "formula")) {stop("prepared_model_formula should be a formula")}
    if (! is.data.frame(prepared_data)) {stop("data should be a data frame.")}
    if (! is.character(idvar)) {stop("idvar should be a character.")}
    if (! is.character(timevar)) {stop("timevar should be a character.")}
    if (! is.logical(FE)) {stop("FE should be either TRUE or FALSE.")}
    if (! is.logical(TFE)) {stop("TFE should be either TRUE or FALSE.")}
    if (! is.logical(cluster)) {stop("cluster should be either TRUE or FALSE.")}

    if (FE & TFE & cluster) {

        ols_output <- estimatr::lm_robust(
            formula = prepared_model_formula,
            data = prepared_data,
            fixed_effects = ~ get(idvar) + get(timevar),
            clusters = get(idvar),
            se_type = "stata"
        )

    } else if ((!FE) & TFE & cluster) {

        ols_output <- estimatr::lm_robust(
            formula = prepared_model_formula,
            data = prepared_data,
            fixed_effects = ~ get(timevar),
            clusters = get(idvar),
            se_type = "stata"
        )

    } else if (FE & (!TFE) & cluster) {

        ols_output <- estimatr::lm_robust(
            formula = prepared_model_formula,
            data = prepared_data,
            fixed_effects = ~ get(idvar),
            clusters = get(idvar),
            se_type = "stata"
        )

    } else if ((!FE) & (!TFE) & cluster) {

        ols_output <- estimatr::lm_robust(
            formula = prepared_model_formula,
            data = prepared_data,
            clusters = get(idvar),
            se_type = "stata"
        )

    } else if (FE & TFE & (!cluster)) {

        ols_output <- estimatr::lm_robust(
            formula = prepared_model_formula,
            data = prepared_data,
            fixed_effects = ~ get(idvar) + get(timevar),
            se_type = "stata"
        )

    } else if ((!FE) & TFE & (!cluster)) {

        ols_output <- estimatr::lm_robust(
            formula = prepared_model_formula,
            data = prepared_data,
            fixed_effects = ~ get(timevar),
            se_type = "stata"
        )

    } else if (FE & (!TFE) & (!cluster)) {

        ols_output <- estimatr::lm_robust(
            formula = prepared_model_formula,
            data = prepared_data,
            fixed_effects = ~ get(idvar),
            se_type = "stata"
        )

    } else if ((!FE) & (!TFE) & (!cluster)) {

        ols_output <- estimatr::lm_robust(
            formula = prepared_model_formula,
            data = prepared_data,
            se_type = "stata"
        )
    }

    return(ols_output)
}

EventStudyFEOLS <- function(formula, prepared_data,
                          idvar, timevar, FE, TFE, cluster) {

    if (cluster) {
        vcov_spec <- as.formula(paste0("~", idvar))
    } else {
        vcov_spec <- "iid"
    }

    ols_output <- fixest::feols(
        fml = formula,
        data = prepared_data,
        vcov = vcov_spec
    )
    return(ols_output)
}

EventStudyFEOLS_FHS <- function(formula, prepared_data,
                                idvar, timevar, FE, TFE, cluster) {

    if (cluster) {
        vcov_spec <- as.formula(paste0("~", idvar))
    } else {
        vcov_spec <- "iid"
    }

    fhs_output <- fixest::feols(
        fml = formula,
        data = prepared_data,
        vcov = vcov_spec
    )

    # Apply the same standard error adjustments as EventStudyFHS
    if (FE & TFE & cluster) {
        N <- fhs_output$nobs
        n <- fhs_output$fixef_sizes[1]  # number of clusters (unique id values)
        # For FE & TFE, K = number of time FEs + number of structural parameters
        # But we need to be careful - fixest nparams includes the fitted endogenous variable
        # Let's count the actual event study coefficients (excluding fit_xxx)
        n_event_coefs <- length(grep("^z_", names(coef(fhs_output)))) - 1  # -1 for fit_eta_m
        K <- fhs_output$fixef_sizes[2] + n_event_coefs  # time FEs + structural params

        adjustment <- sqrt((N - K) / (N - n - K + 1))
        fhs_output$se <- fhs_output$se / adjustment
        fhs_output$cov.scaled <- fhs_output$cov.scaled / (adjustment^2)

    } else if (FE & (!TFE) & cluster) {
        N <- fhs_output$nobs
        n <- fhs_output$fixef_sizes[1]  # number of clusters
        # For FE only, K = 1 (for the FE intercept?) + structural parameters
        n_event_coefs <- length(grep("^z_", names(coef(fhs_output)))) - 1  # -1 for fit_eta_m
        K <- 1 + n_event_coefs

        adjustment <- sqrt((N - K) / (N - n - K + 1))
        fhs_output$se <- fhs_output$se / adjustment
        fhs_output$cov.scaled <- fhs_output$cov.scaled / (adjustment^2)
    }

    return(fhs_output)
}

