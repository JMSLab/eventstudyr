setwd('~/GitHub/EventStudyR')

devtools::load_all()

eventstudy_estimates <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
policyvar = "z", idvar = "id", timevar = "t",
controls = "x_r", FE = TRUE, TFE = TRUE,
post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

EventStudyPlot(estimates = eventstudy_estimates,
xtitle = "Event time",
ytitle = "Coefficient",
ybreaks = c(-1.5, -.5, 0, .5, 1.5),
conf_level = .95,
Supt = .95,
num_sim = 1000,
seed = 1234,
Addmean = TRUE,
Preeventcoeffs = TRUE,
Posteventcoeffs = TRUE,
Nozeroline = FALSE,
Smpath = NULL)

# Regression with fewer variables, normalizing the first lead

estimates <- EventStudy(estimator = "OLS", data = df_sample_dynamic,
                        outcomevar = "y_base", policyvar = "z",
                        idvar = "id", timevar = "t",
                        FE = TRUE, TFE = TRUE,
                        post = 2, pre = 2, overidpre = 2,
                        overidpost = 2, normalize = - 1, cluster = TRUE)


EventStudyPlot(estimates = estimates,
               xtitle = "Event time",
               ytitle = "Coefficient",
               ybreaks = c(-1.5, -.5, 0, .5, 1.5),
               conf_level = .95,
               Supt = .95,
               num_sim = 1000,
               seed = 1234,
               Addmean = TRUE,
               Preeventcoeffs = TRUE,
               Posteventcoeffs = TRUE,
               Nozeroline = FALSE,
               Smpath = NULL)
