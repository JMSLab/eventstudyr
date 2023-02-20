remove(list=ls())
library(devtools)

setwd("..")
load_all()
setwd("issue19_smpath_compare/")

#write.csv(df_sample_dynamic,
#          "data_dynamic.csv", row.names = T)

for (yvar in c("y_base", "y_smooth_m", "y_jump_m")) {

    print(sprintf("*******/// Plotting %s ///*******", yvar))

    if (yvar == "y_base")     y_breaks = c(-.5, 0, .5, 1)
    if (yvar == "y_smooth_m") y_breaks = c(-1.3, -1, -.5, 0, .5, .7)
    if (yvar == "y_jump_m")   y_breaks = c(-.5, 0, .5, 1, 1.5)


    print("Estimating event study with OLS")

    estimates_ols <- EventStudy(
        outcomevar = yvar,
        estimator = "OLS", data = df_sample_dynamic,
        policyvar = "z", idvar = "id", timevar = "t", controls = "x_r",
        post = 3, overidpost = 1,
        pre  = 0, overidpre  = 3,
        normalize = - 1
    )
    
    print("Creating event study plot")
    EventStudyPlot(
        estimates = estimates_ols,
        ybreaks   = y_breaks,
        Smpath    = TRUE
    ) -> p

    p <- p + geom_point(size = 2.5)

    ggsave(sprintf("R/%s.png", yvar), dpi = 250,
           width = 7, height = 5)

    print("Smoothest path:")
    print(p$data$smoothest_path)
    print("-")
    print("-")
    print("-")
}

