
#' Title
#'
#' @param data
#' @param estimates
#' @param CI
#' @param Supt
#' @param Preeventcoeffs
#' @param Posteventcoeffs
#' @param Nozeroline
#' @param Smpath
#'
#' @return
#' @import ggplot2
#' @export
#'
#' @examples

EventStudyPlot <- function(data, estimates, CI = .95, Supt = .95, Preeventcoeffs, Posteventcoeffs, Nozeroline = FALSE, Smpath) {

    df_estimates <- estimates[[1]]
    df_estimates_tidy <- estimatr::tidy(estimates[[1]])

    policyvar <- estimates[[2]]$policyvar
    post <- estimates[[2]]$post
    overidpost <- estimates[[2]]$overidpost
    pre <- estimates[[2]]$pre
    overidpre <- estimates[[2]]$overidpre
    normalize <- estimates[[2]]$normalize
    normalization_column <- estimates[[2]]$normalization_column


    plot_Supt <- if(!is.null(Supt)) TRUE else FALSE

    if (plot_Supt) {

        df_estimates_tidy <- AddSuptBand(df_estimates, 1000, conf_level = Supt)
    }

    plot_CI <- if(!is.null(CI)) TRUE else FALSE

    if (plot_CI) {

        df_estimates_tidy <-  df_CI <- AddCIs(df_estimates_tidy, policyvar, normalization_column, CI)
    }

    df_plotting <- PreparePlottingData(df_estimates_tidy, policyvar, post, overidpost, pre, overidpre, normalization_column)


    p_Nozeroline <- if(Nozeroline) NULL else ggplot2::geom_hline(yintercept = 0, color = "green", linetype = "dashed")
    p_Supt <- if(plot_Supt) ggplot2::geom_linerange(data = df_plotting, ggplot2::aes(ymin = suptband_lower, ymax = suptband_upper)) else NULL
    p_CI <- if(plot_CI) ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), width = .2) else NULL

    ggplot2::ggplot(df_plotting, ggplot2::aes(x = label, y = estimate)) +
        p_Nozeroline +
        p_Supt +
        p_CI +
        ggplot2::geom_point(color = "#006600", size = 3) +
        ggplot2::labs(
            x = "Event time",
            y = "Coefficient"
            ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank()
            )
}
