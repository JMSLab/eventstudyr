
#'  Creates an Event-Study Plot Following the Suggestions in Freyaldenhoven et al. (forthcoming)
#'
#' @param estimates The output from calling EventStudy(). Should be a list of length 2
#' @param CI Confidence interval expressed as a real number between 0 and 1, inclusively. Defaults to 0.95.
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
#' @importFrom tidyr pivot_longer
#' @export
#'
#' @examples EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
#'policyvar = "z", idvar = "id", timevar = "t",
#'controls = "x_r", FE = TRUE, TFE = TRUE,
#'post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
#'CI = .95,
#'Supt = .95,
#'seed = 1234,
#'Addmean = FALSE,
#'Preeventcoeffs = TRUE,
#'Posteventcoeffs = TRUE,
#'Nozeroline = FALSE,
#'Smpath = NULL)

EventStudyPlot <- function(estimates, CI = .95, Supt = .95, seed = 1234, Addmean = FALSE, Preeventcoeffs = TRUE, Posteventcoeffs = TRUE, Nozeroline = FALSE, Smpath) {

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
    v_eventstudy_coefficients <- estimates[[2]]$v_eventstudy_coefficients

    plot_Supt <- if(!is.null(Supt)) TRUE else FALSE

    if (plot_Supt) {

        df_estimates_tidy <- AddSuptBand(df_estimates, 1000, conf_level = Supt,
                                         seed = seed, eventstudy_coefficients = v_eventstudy_coefficients)
    }

    plot_CI <- if(!is.null(CI)) TRUE else FALSE

    if (plot_CI) {

        df_estimates_tidy <-  df_CI <- AddCIs(df_estimates_tidy, policyvar, normalization_column, CI)
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

    p_AddMeans <- if (Addmean) {

        y_mean <- AddMeans(df_data, normalization_column, policyvar, outcomevar)
        df_estimate_columns <- dplyr::select(df_plotting, -term, -std.error:-outcome, -label)
        df_estimates_longer <- tidyr::pivot_longer(df_estimate_columns, dplyr::everything(), names_to = "term", values_to = "value")

        largest_y_value <- max(abs(df_estimates_longer$value))
        largest_rounded_y_value <- ceiling(largest_y_value)

        y_axis_breaks <- seq(largest_rounded_y_value, - largest_rounded_y_value, length.out = 5)
        y_axis_labels <- c(y_axis_breaks[1:2],
                           paste0(y_axis_breaks[3], " (", round(y_mean, 2), ")"),
                           y_axis_breaks[4:5])

        ggplot2::scale_y_continuous(breaks = y_axis_breaks, labels = y_axis_labels)

    } else NULL

    p_Nozeroline <- if(Nozeroline) NULL else ggplot2::geom_hline(yintercept = 0, color = "green", linetype = "dashed")
    p_Supt <- if(plot_Supt) ggplot2::geom_linerange(data = df_plotting, ggplot2::aes(ymin = suptband_lower, ymax = suptband_upper)) else NULL
    p_CI <- if(plot_CI) ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), width = .2) else NULL

    ggplot2::ggplot(df_plotting, ggplot2::aes(x = label, y = estimate)) +
        p_Nozeroline +
        p_Supt +
        p_CI +
        p_AddMeans +
        ggplot2::geom_point(color = "#006600", size = 3) +
        ggplot2::labs(
            x = "Event time",
            y = "Coefficient",
            caption = text_caption
            ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank()
            ) +
        ggplot2::theme(
            plot.caption = ggplot2::element_text(hjust = 0)
        )
}
