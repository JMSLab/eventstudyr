
#'  Creates an Event-Study Plot Following the Suggestions in Freyaldenhoven et al. (forthcoming)
#'
#' @param estimates The output from calling EventStudy(). Should be a list of length 2
#' @param xtitle The title for the x-axis. Should be a string. Defaults to "Event time".
#' @param ytitle The title for the y-axis. Should be a string. Defaults to "Coefficient".
#' @param ybreaks A vector containing the desired breaks for the y-axis. Should be a numeric vector that contains 0.
#' @param conf_level Confidence level used for confidence interval expressed as a real number between 0 and 1, inclusively. Defaults to 0.95.
#' @param Supt The confidence level used for obtaining the sup-t bands critical value. Should be a real number between
#' 0 and 1, inclusive. Defaults to .95
#' @param seed The pseudorandom state used to make drawing "random" numbers reproducible. Should be a natural number.
#' Defaults to 1234
#' @param Addmean Adds the mean of the dependent variable in the period used for normalization. Should be TRUE or FALSE. Defaults to FALSE.
#' @param Preeventcoeffs If TRUE, uses pre and overidpre from estimates to test for pre-trends. Should be TRUE or FALSE. Defaults to TRUE.
#' @param Posteventcoeffs If TRUE, uses post and overidpost from estimates to test for leveling-off. Should be TRUE or FALSE. Defaults to TRUE.
#' @param Nozeroline Whether or not to plot a dashed horizontal line at y = 0. Should be TRUE or FALSE. Defaults to FALSE, meaning the line is plotted.
#' @param Smpath PLACE HOLDER
#'
#' @return The Event-Study plot as a gpplot2 object
#' @import ggplot2 dplyr
#' @export
#'
#' @examples
#'eventstudy_estimates <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
#'policyvar = "z", idvar = "id", timevar = "t",
#'controls = "x_r", FE = TRUE, TFE = TRUE,
#'post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)
#'
#'EventStudyPlot(estimates = eventstudy_estimates,
#'xtitle = "Event time",
#'ytitle = "Coefficient",
#'ybreaks = c(-1.5, -.5, 0, .5, 1.5),
#'conf_level = .95,
#'Supt = .95,
#'seed = 1234,
#'Addmean = FALSE,
#'Preeventcoeffs = TRUE,
#'Posteventcoeffs = TRUE,
#'Nozeroline = FALSE,
#'Smpath = NULL)

EventStudyPlot <- function(estimates, xtitle = "Event time", ytitle = "Coefficient", ybreaks, conf_level = .95, Supt = .95, seed = 1234,
                           Addmean = FALSE, Preeventcoeffs = TRUE, Posteventcoeffs = TRUE, Nozeroline = FALSE, Smpath) {

    if (!is.character(xtitle)) {stop("xtitle should be a character.")}
    if (!is.character(ytitle)) {stop("ytitle should be a character.")}
    if (!is.logical(Nozeroline)) {stop("Nozeroline should be either TRUE or FALSE.")}
    if (class(ybreaks) != "numeric") {stop("ybreaks should be a numeric vector.")}
    if (! 0 %in% ybreaks) {stop("0 needs to be one of the specified breaks.")}

    df_estimates <- estimates[[1]]
    df_estimates_tidy <- estimatr::tidy(estimates[[1]])

    df_data <- estimates[[2]]$data
    outcomevar <- estimates[[2]]$outcomevar
    policyvar <- estimates[[2]]$policyvar
    post <- estimates[[2]]$post
    overidpost <- estimates[[2]]$overidpost
    pre <- estimates[[2]]$pre
    overidpre <- estimates[[2]]$overidpre
    normalize <- estimates[[2]]$normalize
    normalization_column <- estimates[[2]]$normalization_column
    eventstudy_coefficients <- estimates[[2]]$eventstudy_coefficients

    plot_Supt <- if(!is.null(Supt)) TRUE else FALSE

    if (plot_Supt) {

        df_estimates_tidy <- AddSuptBand(df_estimates, 1000, conf_level = Supt,
                                         seed = seed, eventstudy_coefficients = eventstudy_coefficients)
    }

    plot_CI <- if(!is.null(conf_level)) TRUE else FALSE

    if (plot_CI) {

        df_estimates_tidy <- AddCIs(df_estimates_tidy, policyvar, eventstudy_coefficients, conf_level)
    }

    df_test_linear <- TestLinear(estimates = estimates, pretrends = Preeventcoeffs, leveling_off = Posteventcoeffs)

    if (Preeventcoeffs & Posteventcoeffs) {

        pretrends_p_value <- df_test_linear[df_test_linear["Test"] == "Pre-Trends", "p.value"]
        levelingoff_p_value <- df_test_linear[df_test_linear["Test"] == "Leveling-Off", "p.value"]

        text_pretrends <- paste0("Pretrends p-value = ", round(pretrends_p_value, 2))
        text_levelingoff <- paste0("Leveling off p-value = ", round(levelingoff_p_value, 2))
        text_caption <- paste0(text_pretrends, " -- ", text_levelingoff)
    }

    else if (Preeventcoeffs & !Posteventcoeffs) {

        pretrends_p_value <- df_test_linear[df_test_linear["Test"] == "Pre-Trends", "p.value"]

        text_caption <- paste0("Pretrends p-value = ", round(pretrends_p_value, 2))

    }

    else if (!Preeventcoeffs & Posteventcoeffs) {

        levelingoff_p_value <- df_test_linear[df_test_linear["Test"] == "Leveling-Off", "p.value"]

        text_caption <- paste0("Leveling off p-value = ", round(levelingoff_p_value, 2))

    } else {

        text_caption <- NULL
    }

    df_plotting <- PreparePlottingData(df_estimates_tidy, policyvar, post, overidpost, pre, overidpre, normalization_column)

    y_axis_labels <- ybreaks

    if (Addmean) {

        y_mean <- AddMeans(df_data, normalization_column, policyvar, outcomevar)

        index_zero <- which(ybreaks == 0)
        y_axis_labels[index_zero] <- paste0(y_axis_labels[index_zero], " (", round(y_mean, 2), ")")

    }

    p_Nozeroline <- if(Nozeroline) NULL else ggplot2::geom_hline(yintercept = 0, color = "green", linetype = "dashed")
    p_Supt <- if(plot_Supt) ggplot2::geom_linerange(data = df_plotting, ggplot2::aes(ymin = suptband_lower, ymax = suptband_upper)) else NULL
    p_CI <- if(plot_CI) ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), width = .2) else NULL

    ggplot2::ggplot(df_plotting, ggplot2::aes(x = label, y = estimate)) +
        p_Nozeroline +
        p_Supt +
        p_CI +
        ggplot2::geom_point(color = "#006600", size = 3) +
        ggplot2::labs(
            x = xtitle,
            y = ytitle,
            caption = text_caption
            ) +
        ggplot2::scale_y_continuous(breaks = ybreaks, labels = y_axis_labels,
                                    limits = c(min(ybreaks), max(ybreaks))) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank()
            ) +
        ggplot2::theme(
            plot.caption = ggplot2::element_text(hjust = 0)
        )
}
