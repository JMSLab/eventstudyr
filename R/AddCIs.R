#' Adds columns to data frame containing confidence intervals around provided estimates.
#'
#' @param df_estimates, A data frame with columns for term, estimate, and standard error.
#' @param policyvar, A string with the name of the policy variable used in EventStudy().  # policyvar is not used in this function - MZW
#' @param eventstudy_coefficients, A list specifying the names of the columns that were not normalized in EventStudy().
#' @param conf_level, Confidence level used for confidence interval expressed as a real number between 0 and 1, inclusively. Defaults to 0.95.
#'
#'
#' @import dplyr
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' estimates <- EventStudy(
#'    estimator = "OLS",
#'    data = df_sample_dynamic,
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
#' df_estimates_tidy <- estimatr::tidy(estimates[[1]])
#'
#' df_estimates_tidy <- AddCIs(
#'    df_estimates_tidy,
#'    policyvar = "z",
#'    eventstudy_coefficients = estimates[[2]]$eventstudy_coefficients,
#'    conf_level = 0.95
#')
#'
#'

AddCIs <- function(df_estimates, policyvar, eventstudy_coefficients, conf_level = 0.95) {
    if (! inherits(df_estimates, "data.frame")) {stop("df_estimates should be a data frame")}
    if (! "term" %in% colnames(df_estimates) | ! "estimate" %in% colnames(df_estimates) |
        ! "std.error" %in% colnames(df_estimates)) {stop("df_estimates should include columns 'term', 'estimate', and 'std.error'")}
    if (! is.character(policyvar)) {stop("policyvar should be a character.")}
    if (! is.character(eventstudy_coefficients)) {stop("eventstudy_coefficients should be a character.")}
    if (! is.numeric(conf_level) | conf_level < 0 | conf_level > 1) {stop("conf_level should be a real number between 0 and 1, inclusive.")}

    terms <- eventstudy_coefficients

    percentile <- conf_level + ((1 - conf_level)/2)

    df_CI <- dplyr::filter(df_estimates, .data$term %in% terms)
    df_CI <- dplyr::mutate(df_CI, ci_lower = .data$estimate - .data$std.error * qnorm(percentile),
                                  ci_upper = .data$estimate + .data$std.error * qnorm(percentile))
    df_CI <- dplyr::select(df_CI, c("term", "ci_lower", "ci_upper"))
    # When running the example, the last term (`x_r`) has NAs for the lower and upper CI bounds - is this expected? - MZW
    df_estimates <- dplyr::left_join(df_estimates, df_CI, by = "term")

    return(df_estimates)
}
