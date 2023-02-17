remove(list=ls())
library(devtools)

setwd("..")
load_all()
setwd("issue19_smpath_compare/")

write.csv(df_sample_dynamic,
          "data_dynamic.csv", row.names = T)

for (yvar in c("y_base", "y_smooth_m", "y_jump_m")) {

    estimates_ols <- EventStudy(
        outcomevar = yvar,
        estimator = "OLS", data = df_sample_dynamic,
        policyvar = "z", idvar = "id", timevar = "t", controls = "x_r",
        post = 3, overidpost = 1,
        pre  = 0, overidpre  = 3,
        normalize = - 1,
    )
    EventStudyPlot(
        estimates = estimates_ols,
        Smpath    = TRUE
    ) -> p

    ggsave(sprintf("R/%s.png", yvar), dpi = 250,
           width = 7, height = 5)
}


