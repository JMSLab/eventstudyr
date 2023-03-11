#' Creates an Event-Study Plot following the suggestions in [Freyaldenhoven et al. (2021)](https://www.nber.org/system/files/working_papers/w29170/w29170.pdf).
#'
#' @param estimates The output from calling [EventStudy()]. It should be a list of length 2.
#' @param xtitle The title for the x-axis. It should be a string. Defaults to "Event time".
#' @param ytitle The title for the y-axis. It should be a string. Defaults to "Coefficient".
#' @param ybreaks A vector containing the desired breaks for the y-axis.
#' Defaults to NULL, which means the breaks are computed automatically.
#' If custom breaks are selected with the `add_mean` argument set to TRUE, then the breaks must include zero.
#' @param conf_level Confidence level used for the confidence interval
#' expressed as a real number between 0 and 1, inclusive. Defaults to 0.95.
#' @param supt The confidence level used for obtaining the sup-t bands critical value.
#' It should be a real number between 0 and 1, inclusive. Defaults to .95.
#' @param num_sim The number of simulations used in generating the sup-t bands.
#' It should be a natural number. Defaults to 1000.
#' @param seed The pseudorandom state used to make drawing "random" numbers reproducible.
#' It should be a natural number. Defaults to 1234.
#' @param add_mean Adds the mean of the dependent variable in the period used for normalization.
#' It should be TRUE or FALSE. Defaults to FALSE.
#' @param pre_event_coeffs If TRUE, uses pre and overidpre from estimates to test for pre-trends.
#' It should be TRUE or FALSE. Defaults to TRUE.
#' @param post_event_coeffs If TRUE, uses post and overidpost from estimates to test for leveling-off.
#' It should be TRUE or FALSE. Defaults to TRUE.
#' @param add_zero_line Whether or not to plot a dashed horizontal line at y = 0.
#' It should be TRUE or FALSE. Defaults to TRUE, meaning the line is plotted.
#' @param smpath Plot smoothest path of confounder that rationalizes event study coefficients.
#' It should be TRUE or FALSE. Defaults to FALSE.
#'
#' @return The Event-Study plot as a ggplot2 object
#' @import ggplot2 dplyr
#' @importFrom rlang .data
#' @importFrom data.table setorder
#' @export

#' @examples
#'
#' # Minimal examples
#' ### OLS
#'
#' estimates_ols <- EventStudy(
#'    estimator = "OLS",
#'    data = df_sample_dynamic,
#'    outcomevar = "y_smooth_m",
#'    policyvar = "z",
#'    idvar = "id",
#'    timevar = "t",
#'    controls = "x_r",
#'    FE = TRUE, TFE = TRUE,
#'    post = 3, overidpost = 5,
#'    pre = 2,  overidpre = 4,
#'    normalize = - 3
#' )
#'
#' plt_ols <- EventStudyPlot(estimates = estimates_ols)
#' plt_ols
#'
#' ### IV
#'
#' estimates_fhs <- EventStudy(
#'    estimator = "FHS",
#'    data = df_sample_dynamic,
#'    outcomevar = "y_smooth_m",
#'    policyvar = "z",
#'    idvar = "id",
#'    timevar = "t",
#'    controls = "x_r",
#'    proxy = "eta_m",
#'    post = 2, overidpost = 1,
#'    pre = 0,  overidpre = 3,
#'    normalize = -1
#' )
#'
#' plt_fhs <- EventStudyPlot(estimates = estimates_fhs)
#' plt_fhs
#'
#' # Optional arguments
#'
#' ### Change x- and y-axis titles and set ybreaks
#' EventStudyPlot(estimates = estimates_ols,
#'                xtitle = "Relative time", ytitle = "",
#'                ybreaks = seq(-2, 1, 0.5))
#'
#' ### Add smoothest path
#' EventStudyPlot(estimates = estimates_ols, smpath = T)
#'
#' ### Add y-mean to y-axis and line y = 0
#' EventStudyPlot(estimates = estimates_ols, add_mean = T,
#'                add_zero_line = T)
#'
#' ### Do not plot supt bands
#' EventStudyPlot(estimates = estimates_ols, supt = NULL)
#'
#' # Modify plots using ggplot2 functions
#'
#' ### Change color of dots and theme
#' plt_ols +
#'   geom_point(color = "red") +
#'   geom_hline(color = "gray", yintercept = 0) +
#'   theme_light() +
#'   theme(panel.grid.minor.x = element_blank())
#'