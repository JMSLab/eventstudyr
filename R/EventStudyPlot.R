
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

EventStudyPlot <- function(data, estimates, CI, Supt, Preeventcoeffs, Posteventcoeffs, Nozeroline, Smpath) {

    df_plotting <- PreparePlottingData(data)

    ggplot2::ggplot(df_plotting, ggplot2::aes(x = term, y = estimate)) +
        ggplot2::geom_point() +
        ggplot2::labs(
            x = "Event time",
            y = "Coefficient"
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank()
        )



}
