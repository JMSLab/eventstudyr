#' Adds confidence intervals around estimations in new columns
#'
#' @param estimates, A list containing the outputs of an estimation containing, at least,
#' coefficient estimates and standard errors.
#' @param CI, Confidence interval expressed as a rational number between 0 and 1, inclusively. Defaults to 0.95.
#'
#' @import estimatr
#' @export
#'
#' @examples
#' AddCIs(OLS_estimates, .95)
#'
#'

AddCIs <- function(Estimates, CI = 0.95) {
    if ((class(Estimates) != "lm_robust") | (typeof(Estimates) != "list")) {
        stop("Estimates should be a list with coefficient estimates and standard errors")
    }
    if (!is.numeric(CI) | CI < 0 | CI > 1) {stop("CI should be a rational number between 0 and 1, inclusive.")}

    df <- tidy(Estimates)

    percentile <- CI + ((1 - CI)/2)


    df <- df %>%
          mutate(ci_lower = ifelse(grepl("_lead",term, fixed = T), estimate - std.error * qnorm(percentile),
                                   ifelse(grepl("_lag", term, fixed = T), estimate - std.error * qnorm(percentile), NA)),
                 ci_upper = ifelse(grepl("_lead",term, fixed = T), estimate + std.error * qnorm(percentile),
                                   ifelse(grepl("_lag", term, fixed = T), estimate + std.error * qnorm(percentile), NA)),)

    return(df)
}
