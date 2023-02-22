remove(list=ls())
library(devtools)
library(data.table)

setwd("..")
load_all()
setwd("issue19_paths")

all_data = data.table()

for (yvar in c("y_smooth_m", "y_jump_m")) {

    print(sprintf("*******/// Plotting %s ///*******", yvar))

    if (yvar == "y_smooth_m") y_breaks = seq(-1.5, 1, .5)
    if (yvar == "y_jump_m")   y_breaks = seq(-2, 3, 1)

    for (post_ in 2:7) {
        for (pre_ in 2:7) {

            print(sprintf("Creating event study for post = %s, pre = %s",
                  post_, pre_))

            estimates_ols <- EventStudy(
                outcomevar = yvar,
                estimator = "OLS", data = df_sample_dynamic,
                policyvar = "z", idvar = "id", timevar = "t", controls = "x_r",
                post = post_, overidpost = 1,
                pre  = 0,     overidpre  = pre_,
                normalize = - 1
            )

            print("Creating event study plot")
            result <- tryCatch({
                EventStudyPlot(
                    estimates = estimates_ols,
                    ybreaks   = y_breaks,
                    Smpath    = TRUE
                ) -> result

                p     <- result$plt
                order <- result$order
                err   <- 0

                p <- p + geom_point(size = 2.5)

                ggsave(sprintf("R/%s_post%s_pre%s.png", yvar, post_, pre_), dpi = 250,
                    width = 7, height = 5)

            }, error = function(e) {
                # Handle error here
                message("Caught an error: ", conditionMessage(e))
                order <<- NA
                err   <<- 1
            })

            if (err == 0) print("Smoothest path found successfully")
            if (err == 1) print("Smoothest path not found")

            all_data <- rbindlist(list(all_data, data.table(
                yvar = yvar, post = post_, pre = pre_,
                order = order, error = err
            )))
        }
    }
}

fwrite(all_data, "data_paths.csv")
