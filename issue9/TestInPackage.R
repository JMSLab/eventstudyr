# Run with working directory in ROOT
library(devtools)

load_all()

eventstudy_estimates_ols <- EventStudy(
    estimator = "OLS",
    data = df_sample_dynamic,
    outcomevar = "y_smooth_m",
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
    estimates = eventstudy_estimates_ols,
    xtitle = "Event time",
    ytitle = "Coefficient",
    ybreaks = c(-1.5, -.5, 0, .5, 1.5),
    conf_level = .95,
    Supt = .95,
    num_sim = 1000,
    seed = 1234,
    Addmean = FALSE,
    Preeventcoeffs = TRUE,
    Posteventcoeffs = TRUE,
    Nozeroline = FALSE,
    Smpath = T
)

ggsave("issue9/smpath_base.png")

