
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

EventStudyPlot <- function(data, estimates, CI, Supt = .95, Preeventcoeffs, Posteventcoeffs, Nozeroline = FALSE, Smpath) {

    df_plotting <- PreparePlottingData(data)

    plot_Supt <- if(!is.null(Supt)) TRUE else FALSE

    if (plot_Supt) {

        df_Supt <- AddSuptBand(data, 1000, conf_level = Supt)
        df_plotting <- merge(df_plotting, df_Supt, by = "term")
    }


    p_Nozeroline <- if(Nozeroline) NULL else ggplot2::geom_hline(yintercept = 0, color = "green", linetype = "dashed")
    p_Supt <- if(plot_Supt) ggplot2::geom_linerange(data = df_plotting, ggplot2::aes(ymin = lower, ymax = upper)) else NULL

    ggplot2::ggplot(df_plotting, ggplot2::aes(x = term, y = estimate)) +
        p_Supt +
        ggplot2::geom_point(color = "#006600", size = 3) +
        ggplot2::labs(
            x = "Event time",
            y = "Coefficient"
            ) +
        p_Nozeroline +
        ggplot2::theme_bw() +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank()
            )

}
