
#'  Creates an Event-Study Plot Following the Suggestions in Freyaldenhoven et al. (forthcoming)
#'
#' @param estimates The output from calling EventStudy(). Should be a list of length 2.
#' @param xtitle The title for the x-axis. Should be a string. Defaults to "Event time".
#' @param ytitle The title for the y-axis. Should be a string. Defaults to "Coefficient".
#' @param ybreaks A vector containing the desired breaks for the y-axis.
#' Defaults to NULL, which means the breaks are computed automatically.
#' If custom breaks are selected with the `Addmean` argument set to TRUE, then the breaks must include zero.
#' @param conf_level Confidence level used for confidence interval
#' expressed as a real number between 0 and 1, inclusively. Defaults to 0.95.
#' @param Supt The confidence level used for obtaining the sup-t bands critical value.
#' Should be a real number between 0 and 1, inclusive. Defaults to .95.
#' @param num_sim The number of simulations used in generating the sup-t bands.
#' Should be a natural number. Defaults to 1000.
#' @param seed The pseudorandom state used to make drawing "random" numbers reproducible.
#' Should be a natural number. Defaults to 1234.
#' @param Addmean Adds the mean of the dependent variable in the period used for normalization.
#' Should be TRUE or FALSE. Defaults to FALSE.
#' @param Preeventcoeffs If TRUE, uses pre and overidpre from estimates to test for pre-trends.
#' Should be TRUE or FALSE. Defaults to TRUE.
#' @param Posteventcoeffs If TRUE, uses post and overidpost from estimates to test for leveling-off.
#' Should be TRUE or FALSE. Defaults to TRUE.
#' @param Addzeroline Whether or not to plot a dashed horizontal line at y = 0.
#' Should be TRUE or FALSE. Defaults to TRUE, meaning the line is plotted.
#' @param Smpath Plot smoothest path of confounder that rationalizes event study coefficients.
#' Should be TRUE or FALSE. Defaults to FALSE.
#'
#' @return The Event-Study plot as a gpplot2 object
#' @import ggplot2 dplyr
#' @importFrom rlang .data
#' @importFrom data.table setorder
#' @export
#'
#' @examples
#'
#' # OLS
#'
#'eventstudy_estimates_ols <- EventStudy(
#'   estimator = "OLS",
#'   data = df_sample_dynamic,
#'   outcomevar = "y_base",
#'   policyvar = "z",
#'   idvar = "id",
#'   timevar = "t",
#'   controls = "x_r",
#'   FE = TRUE,
#'   TFE = TRUE,
#'   post = 3,
#'   pre = 2,
#'   overidpre = 4,
#'   overidpost = 5,
#'   normalize = - 3,
#'   cluster = TRUE,
#'   anticipation_effects_normalization = TRUE
#')
#'
#'EventStudyPlot(
#'   estimates = eventstudy_estimates_ols,
#'   xtitle = "Event time",
#'   ytitle = "Coefficient",
#'   ybreaks = c(-1.5, -.5, 0, .5, 1.5),
#'   conf_level = .95,
#'   Supt = .95,
#'   num_sim = 1000,
#'   seed = 1234,
#'   Addmean = FALSE,
#'   Preeventcoeffs = TRUE,
#'   Posteventcoeffs = TRUE,
#'   Addzeroline = TRUE,
#'   Smpath = FALSE
#')
#'
#' # IV
#'
#'eventstudy_estimates_fhs <- EventStudy(
#'   estimator = "FHS",
#'   data = df_sample_dynamic,
#'   outcomevar = "y_base",
#'   policyvar = "z",
#'   idvar = "id",
#'   timevar = "t",
#'   controls = "x_r",
#'   FE = TRUE,
#'   TFE = TRUE,
#'   post = 3,
#'   pre = 0,
#'   overidpre = 3,
#'   overidpost = 1,
#'   normalize = - 1,
#'   cluster = TRUE,
#'   proxy = "eta_m",
#'   anticipation_effects_normalization = TRUE
#')
#'
#'EventStudyPlot(
#'   estimates = eventstudy_estimates_fhs,
#'   xtitle = "Event time",
#'   ytitle = "Coefficient",
#'   ybreaks = seq(-5, 10, 5),
#'   conf_level = .95,
#'   Supt = .95,
#'   num_sim = 1000,
#'   seed = 1234,
#'   Addmean = FALSE,
#'   Preeventcoeffs = TRUE,
#'   Posteventcoeffs = TRUE,
#'   Addzeroline = TRUE,
#'   Smpath = FALSE
#')

EventStudyPlot <- function(estimates,
                           xtitle = "Event time", ytitle = "Coefficient", ybreaks = NULL,
                           conf_level = .95, Supt = .95, num_sim = 1000, seed = 1234,
                           Addmean = FALSE, Preeventcoeffs = TRUE, Posteventcoeffs = TRUE,
                           Addzeroline = TRUE, Smpath = FALSE) {

    if (!is.character(xtitle))    {stop("Argument 'xtitle' should be a character.")}
    if (!is.character(ytitle))    {stop("Argument 'ytitle' should be a character.")}
    if (!is.logical(Addzeroline)) {stop("Argument 'Addzeroline' should be either TRUE or FALSE.")}
    if (!is.null(ybreaks) &
        !is.numeric(ybreaks))     {stop("Argument 'ybreaks' should be NULL or a numeric vector.")}

# Estimation Elements -----------------------------------------------------

    df_estimates      <- estimates[[1]]
    df_estimates_tidy <- estimatr::tidy(estimates[[1]])

    df_data                 <- estimates[[2]]$data
    outcomevar              <- estimates[[2]]$outcomevar
    policyvar               <- estimates[[2]]$policyvar
    post                    <- estimates[[2]]$post
    overidpost              <- estimates[[2]]$overidpost
    pre                     <- estimates[[2]]$pre
    overidpre               <- estimates[[2]]$overidpre
    normalize               <- estimates[[2]]$normalize
    normalization_column    <- estimates[[2]]$normalization_column
    eventstudy_coefficients <- estimates[[2]]$eventstudy_coefficients
    proxyIV                 <- estimates[[2]]$proxyIV

# Optionally Add Suptbands/Confidence Intervals ---------------------------

    plot_Supt <- if(!is.null(Supt)) TRUE else FALSE

    if (plot_Supt) {
        df_estimates_tidy <- AddSuptBand(df_estimates, num_sim = 1000, conf_level = Supt,
                                         seed = seed, eventstudy_coefficients = eventstudy_coefficients)
    }

    plot_CI <- if(!is.null(conf_level)) TRUE else FALSE

    if (plot_CI) {

        df_estimates_tidy <- AddCIs(df_estimates_tidy, policyvar, eventstudy_coefficients, conf_level)
    }

# Optionally Test For Pretrends/Levelling-Off -------------------------------

    df_test_linear <- TestLinear(estimates = estimates, pretrends = Preeventcoeffs, leveling_off = Posteventcoeffs)

    if (Preeventcoeffs & Posteventcoeffs) {

        pretrends_p_value   <- df_test_linear[df_test_linear["Test"] == "Pre-Trends",   "p.value"]
        levelingoff_p_value <- df_test_linear[df_test_linear["Test"] == "Leveling-Off", "p.value"]

        text_pretrends   <- paste0("Pretrends p-value = ", round(pretrends_p_value, 2))
        text_levelingoff <- paste0("Leveling off p-value = ", round(levelingoff_p_value, 2))
        text_caption     <- paste0(text_pretrends, " -- ", text_levelingoff)

    } else if (Preeventcoeffs & !Posteventcoeffs) {

        pretrends_p_value <- df_test_linear[df_test_linear["Test"] == "Pre-Trends", "p.value"]

        text_caption <- paste0("Pretrends p-value = ", round(pretrends_p_value, 2))

    } else if (!Preeventcoeffs & Posteventcoeffs) {

        levelingoff_p_value <- df_test_linear[df_test_linear["Test"] == "Leveling-Off", "p.value"]

        text_caption <- paste0("Leveling off p-value = ", round(levelingoff_p_value, 2))

    } else {

        text_caption <- NULL
    }

    df_plt <- PreparePlottingData(df_estimates_tidy, policyvar,
                                  post, overidpost, pre, overidpre, normalization_column, proxyIV)

# Construct y breaks ------------------------------------------------------

    if (!is.null(ybreaks)) {
        if (!(0 %in% ybreaks) & Addmean) {
            stop("If you want to add the mean of y in the y-axis then 'ybreaks' must include 0.")
        }

        ylabels <- ybreaks
        ylims   <- c(min(ybreaks), max(ybreaks))
    } else {
        min_value <- min(c(df_plt$estimate, df_plt$ci_lower, df_plt$suptband_lower), na.rm = T)
        max_value <- max(c(df_plt$estimate, df_plt$ci_upper, df_plt$suptband_upper), na.rm = T)
        max_abs   <- max(abs(min_value), abs(max_value))

        magnitude <- 10^floor(log10(max_abs))

        # Determine step depending on how far the endpoints are from the magnitude
        mean_ratio <- mean(c(abs(min_value)/magnitude, max_value/magnitude))
        if (mean_ratio > 6.67) {
            step = 3*magnitude
        } else if (mean_ratio > 3.33) {
            step = 2*magnitude
        } else {
            step = magnitude
        }

        # Pick multiples of step to ensure zero is included
        close_to_min <- floor(min_value/step)*step
        close_to_max <- ceiling(max_value/step)*step

        ybreaks <- seq(close_to_min, close_to_max, step)
        ylims   <- c(min(ybreaks), max(ybreaks))

        if (length(ybreaks) >= 9) {
            # Too many breaks, double step size
            step         <- step*2
            close_to_min <- floor(min_value/step)*step
            close_to_max <- ceiling(max_value/step)*step

            ybreaks <- seq(close_to_min, close_to_max, step)
        } else if (length(ybreaks) <= 3) {
            # Too few breaks, halve step size
            step         <- step/2
            close_to_min <- floor(min_value/step)*step
            close_to_max <- ceiling(max_value/step)*step

            ybreaks <- seq(close_to_min, close_to_max, step)
        }
        ylabels <- ybreaks
    }

# Optionally Adds Mean ----------------------------------------------------

    if (Addmean) {

        y_mean <- AddMeans(df_data, normalization_column, policyvar, outcomevar)

        index_zero <- which(ybreaks == 0)
        ylabels[index_zero] <- paste0(ylabels[index_zero], " (", round(y_mean, 2), ")")

    }

# Optionally Add smooth path ----------------------------------------------

    # Order coefficients
    data.table::setorder(df_plt, label)

    if (Smpath) {
        coefficients <- df_plt$estimate

        # Add column and row in matrix of coefficients in index of norm columns
        covar <- AddZerosCovar(estimates[[1]]$vcov,
                               eventstudy_coefficients,
                               df_plt[df_plt$estimate==0, ]$term,
                               df_plt$term)

        inv_covar <- pracma::pinv(covar)

        df_plt <- AddSmPath(df_plt, coefficients, inv_covar)
    }

# Construct Plot ----------------------------------------------------------

    plt <- ggplot(df_plt,
                  aes(x = .data$label, y = .data$estimate))

    if (Addzeroline) {
        plt <- plt +
            geom_hline(yintercept = 0,
                       color = "green", linetype = "dashed")
    }
    if (plot_Supt) {
        plt <- plt +
            geom_linerange(aes(ymin = .data$suptband_lower,
                               ymax = .data$suptband_upper))
    }
    if (plot_CI) {
        plt <- plt +
            geom_errorbar(aes(ymin = .data$ci_lower,
                              ymax = .data$ci_upper), width = .2)
    }
    if (Smpath) {
        plt <- plt +
            geom_line(aes(y = smoothest_path, group = 1),
                      color = "black")
    }

    plt <- plt  +
        geom_point(color = "#006600") +
        scale_y_continuous(breaks = ybreaks,
                           labels = ylabels,
                           limits = ylims) +
        labs(x = xtitle, y = ytitle,
             caption = text_caption) +
        theme_bw() +
        theme(panel.grid   = element_blank(),
              plot.caption = element_text(hjust = 0))

    return(plt)
}
