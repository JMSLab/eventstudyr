#' Adds confidence intervals around estimations in new columns
#'
#' @param df_estimates, A list containing the outputs of an estimation containing, at least,
#' coefficient estimates and standard errors.
#' @param policyvar, A string with the name of the policy variable used in EventStudy()
#' @param normalize, Specifies the event-time coefficient used to normalize in EventStudy(). Defaults to - pre - 1.
#' @param CI, Confidence interval expressed as a rational number between 0 and 1, inclusively. Defaults to 0.95.
#'
#'
#' @import estimatr, dplyr
#' @export
#'
#' @examples
#' AddCIs(df_estimates, policyvar = "z", normalize = -3, CI = .95)
#'
#'

AddCIs <- function(df_estimates, policyvar, normalize, CI = 0.95) {
    if (class(df_estimates) != "data.frame") {stop("df_estimates should be a data frame")}
    if (! "term" %in% colnames(df_estimates) | ! "estimate" %in% colnames(df_estimates) |
        ! "std.error" %in% colnames(df_estimates)) {stop("df_estimates should include columns 'term', 'estimate', and 'std.error'")}
    if (! is.character(policyvar)) {stop("policyvar should be a character.")}
    if (! is.numeric(normalize)) {stop("normalize should be an integer.")}
    if (! is.numeric(CI) | CI < 0 | CI > 1) {stop("CI should be a rational number between 0 and 1, inclusive.")}

    if (normalize < 0) {
        normalization_column <- paste0(policyvar, "_fd_lead", (-1 * normalize))
    } else if (normalize == 0){
        normalization_column <- paste0(policyvar, "_fd")
    } else {
        normalization_column <- paste0(policyvar, "_fd_lag", (normalize))
    }

    terms <- df_estimates$term[startsWith(df_estimates$term, paste0(policyvar, "_fd")) |
                               startsWith(df_estimates$term, paste0(policyvar, "_lead")) |
                               startsWith(df_estimates$term, paste0(policyvar, "_lag")) &
                               df_estimates$term != normalization_column]

    percentile <- CI + ((1 - CI)/2)

    df_CI <- dplyr::filter(df_estimates, term %in% terms)
    df_CI <- dplyr::mutate(df_CI, ci_lower = estimate - std.error * qnorm(percentile), ci_upper = estimate + std.error * qnorm(percentile))
    df_CI <- dplyr::select(df_CI, c("term", "ci_lower", "ci_upper"))

    df_estimates <- dplyr::left_join(df_estimates, df_CI, by = "term")

    return(df_estimates)
}
