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
    if ((class(Estimates) != "list") | typeof(Estimates) != "list" |
         class(Estimates[[1]]) != "lm_robust" | class(Estimates[[2]]) != "list")
         {stop("Estimates should be a list of length 2 with Estimates[[1]] an 'lm_robust' list estimates
                and standard errors and Estimates[[2]] a list of arguments. Should be an output from EventStudy().")}
    if (!is.numeric(CI) | CI < 0 | CI > 1) {stop("CI should be a rational number between 0 and 1, inclusive.")}

    if (normalize < 0) {
        normalization_column <- paste0(policyvar, "_fd_lead", (-1 * normalize))
    } else if (normalize == 0){
        normalization_column <- paste0(policyvar, "_fd")
    } else {
        normalization_column <- paste0(policyvar, "_fd_lag", (normalize))
    }

    terms <- Estimates[[1]]$term[startsWith(Estimates[[1]]$term, paste0(policyvar, "_fd")) |
                                 startsWith(Estimates[[1]]$term, paste0(policyvar, "_lead")) |
                                 startsWith(Estimates[[1]]$term, paste0(policyvar, "_lag")) &
                                 Estimates[[1]]$term != normalization_column]

    percentile <- CI + ((1 - CI)/2)

    df_CI <- df %>%
          filter(term %in% terms) %>%
          mutate(ci_lower = estimate - std.error * qnorm(percentile),
                 ci_upper = estimate + std.error * qnorm(percentile)) %>%
          select(term, ci_lower, ci_upper)

    df <- left_join(df, df_CI, by = "term")

    return(df)
}
