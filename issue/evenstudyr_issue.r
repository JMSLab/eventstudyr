remove(list=ls())
library(dplyr)
library(ggplot2)
library(ggthemes)
library(data.table)
set.seed(23)
source("R/EventStudy.R")
source("R/EventStudyPlot.R")
source("R/ComputeFirstDifferences.R")
source("R/ComputeShifts.R")
source("R/PrepareModelFormula.R")
source("R/EventStudyOLS.R")
source("R/AddSuptBand.R")
source("R/AddCIs.R")
source("R/PreparePlottingData.R")
source("R/AddSmPath.R")
source("R/AddMeans.R")
source("R/data.R")
source("R/EventStudyFHS.R")
source("R/EventStudyOLS.R")
source("R/SmPathHelpers.R")

main <- function() {
    outstub <- "C:/Users/Moses/Downloads/eventstudyr_issue"
    estimates_ols <- EventStudy(
        estimator = "OLS",
        data = example_data,
        outcomevar = "y_jump_m", policyvar = "z",
        idvar      = "id",       timevar   = "t",
        controls = "x_r",
        post = 5, overidpost = 2,
        pre  = 0,  overidpre = 6
    )

    y_breaks = seq(-1, 2, 1)
    plts <- list()

    plts[["smooth_path"]] <- EventStudyPlot(estimates         = estimates_ols,
                                            pre_event_coeffs  = FALSE,
                                            post_event_coeffs = FALSE,
                                            ybreaks           = y_breaks,
                                            smpath            = TRUE) +
                                theme_plots()
    lapply(names(plts), function(p) {
        hh = 6
        if (p %in% c("p_values", "all_features")) {
            hh = hh + .28
        }

        ggsave(plts[[p]], filename = file.path(outstub, paste0(p, ".png")),
               width = 10, height = hh)
    })
}

theme_plots <- function() {
  theme(text             = element_text(size=20),
        plot.background  = element_rect(color = "#FAFAFA", fill = "#FAFAFA"),
        panel.background = element_rect(fill = "#FAFAFA"))
}

main()
