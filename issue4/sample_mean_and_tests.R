setwd('~/GitHub/EventStudyR')

devtools::load_all()
devtools::test()
estimates <- EventStudy(estimator = "OLS", data = df_sample_dynamic,
                        outcomevar = "y_base", policyvar = "z",
                        idvar = "id", timevar = "t",
                        FE = TRUE, TFE = TRUE,
                        post = 2, pre = 2, overidpre = 2,
                        overidpost = 2, normalize = - 1,
                        cluster = TRUE)

es_plot   <- EventStudyPlot(estimates = estimates,
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

ggsave('issue4/sample_mean_tests_plot.png', es_plot)
