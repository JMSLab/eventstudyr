#' Adds columns to data frame containing confidence intervals around provided estimates.
#'
#' @param df_estimates, A data frame with columns for term, estimate, and standard error.
#' @param eventstudy_coefficients, A list specifying the names of the columns that were not normalized in EventStudy().
#' @param conf_level, Confidence level used for confidence interval
#' expressed as a real number between 0 and 1, inclusively. Defaults to 0.95.
#'
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom stats qnorm
#' @import estimatr
#' @keywords internal
#' @noRd
#'
#' @examples
#' estimates <- EventStudy(
#'    estimator = "OLS",
#'    data = example_data,
#'    outcomevar = "y_base",
#'    policyvar = "z",
#'    idvar = "id",
#'    timevar = "t",
#'    controls = "x_r",
#'    FE = TRUE,
#'    TFE = TRUE,
#'    post = 3,
#'    pre = 2,
#'    overidpre = 4,
#'    overidpost = 5,
#'    normalize = - 3,
#'    cluster = TRUE,
#'    anticipation_effects_normalization = TRUE
#' )
#'
#' df_estimates_tidy <- estimatr::tidy(estimates$output)
#'
#' df_estimates_tidy <- AddCIs(
#'    df_estimates_tidy,
#'    eventstudy_coefficients = estimates$arguments$eventstudy_coefficients,
#'    conf_level = 0.95
#')
#'
#'

AddCIs <- function(df_estimates, eventstudy_coefficients, conf_level = 0.95) {
    if (! inherits(df_estimates, "data.frame")) {stop("df_estimates should be a data frame")}
    if (!all(c("term", "estimate", "std.error") %in% colnames(df_estimates))) {
        stop("df_estimates should include columns 'term', 'estimate', and 'std.error'.")
    }
    if (! is.character(eventstudy_coefficients)) {stop("eventstudy_coefficients should be a character vector.")}
    if (! is.numeric(conf_level) | conf_level < 0 | conf_level > 1) {stop("conf_level should be a real number between 0 and 1, inclusive.")}

    terms <- eventstudy_coefficients

    percentile <- conf_level + ((1 - conf_level)/2)

    df_CI <- dplyr::filter(df_estimates, .data$term %in% terms)
    df_CI <- dplyr::mutate(df_CI, ci_lower = .data$estimate - .data$std.error * stats::qnorm(percentile),
                                  ci_upper = .data$estimate + .data$std.error * stats::qnorm(percentile))
    df_CI <- dplyr::select(df_CI, c("term", "ci_lower", "ci_upper"))
    df_estimates <- dplyr::left_join(df_estimates, df_CI, by = "term")

    return(df_estimates)
}
