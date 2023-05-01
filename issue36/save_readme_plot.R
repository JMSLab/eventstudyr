
pacman::p_load(
    tidyverse,
    eventstudyr,
    here
)

estimates_ols <- EventStudy(
    estimator = "OLS",
    data = example_data,   # Use package sample data
    outcomevar = "y_smooth_m",
    policyvar = "z",
    idvar = "id",
    timevar = "t",
    controls = "x_r",
    pre = 0,  post = 4
)

plt <- EventStudyPlot(estimates = estimates_ols)

ggsave(
    plot = plt,
    filename = here("man/figures/readme_plot.png")
)
