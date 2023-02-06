remove(list=ls())
# Run with working directory in ROOT
library(devtools)

load_all()

estimates <- EventStudy(
    estimator = "OLS",
    data = df_sample_dynamic,
    outcomevar = "y_jump_m",
    policyvar = "z",
    idvar = "id",
    timevar = "t",
    controls = "x_r",
    FE = TRUE,
    TFE = TRUE,
    post = 3,
    pre = 2,
    overidpre = 4,
    overidpost = 5,
    normalize = - 3,
    cluster = TRUE,
    anticipation_effects_normalization = TRUE
)

EventStudyPlot(
    estimates = estimates,
    xtitle = "Event time",
    ytitle = "Coefficient",
    conf_level = .95,
    Supt = .95,
    num_sim = 1000,
    seed = 1234,
    Addmean = FALSE,
    Preeventcoeffs = TRUE,
    Posteventcoeffs = TRUE,
    Addzeroline = T,
    Smpath = T
)

ggsave("issue9/smpath.png", width = 7, height = 5)

