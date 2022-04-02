setwd('~/GitHub/eventstudyr')
library(devtools)
library(broom)

load_all()

estimation_output <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
    policyvar = "z", idvar = "id", timevar = "t", FE = TRUE, TFE = TRUE,
    post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 1, cluster = TRUE)

estimates_regular <- tidy(estimation_output[[1]])

View(estimates_regular)

estimation_allzero <- EventStudy(estimator = "OLS", data = df_sample_dynamic,
                                outcomevar = "y_base", policyvar = "z",
                                idvar = "id", timevar = "t", FE = T, TFE = T,
                                post = 0, pre = 0, overidpre = 0, overidpost = 0,
                                cluster = TRUE)

estimates_allzero <- tidy(estimation_allzero[[1]])

View(estimates_allzero)

estimation_m1 <- EventStudy(estimator = "OLS", data = df_sample_dynamic,
                                outcomevar = "y_base", policyvar = "z",
                                idvar = "id", timevar = "t", FE = T, TFE = T,
                                post = 1, pre = 0, overidpre = 0, overidpost = 0,
                                cluster = TRUE)

estimates_m1 <- tidy(estimation_m1[[1]])


View(estimates_m1)

estimation_lm1 <- EventStudy(estimator = "OLS", data = df_sample_dynamic,
                                outcomevar = "y_base", policyvar = "z",
                                idvar = "id", timevar = "t", FE = T, TFE = T,
                                post = 0, pre = 0, overidpre = 0, overidpost = 1,
                                cluster = TRUE)

estimates_lm1 <- tidy(estimation_lm1[[1]])


View(estimates_lm1)

estimation_g1 <- EventStudy(estimator = "OLS", data = df_sample_dynamic,
                                outcomevar = "y_base", policyvar = "z",
                                idvar = "id", timevar = "t", FE = T, TFE = T,
                                post = 0, pre = 1, overidpre = 0, overidpost = 0,
                                cluster = TRUE)

estimates_g1 <- tidy(estimation_g1[[1]])
View(estimates_g1)

estimation_lg1 <- EventStudy(estimator = "OLS", data = df_sample_dynamic,
                                outcomevar = "y_base", policyvar = "z",
                                idvar = "id", timevar = "t", FE = T, TFE = T,
                                post = 0, pre = 0, overidpre = 1, overidpost = 0,
                                cluster = TRUE)

estimates_lg1 <- tidy(estimation_lg1[[1]])
View(estimates_lg1)

estimation_lg2 <- EventStudy(estimator = "OLS", data = df_sample_dynamic,
                                outcomevar = "y_base", policyvar = "z",
                                idvar = "id", timevar = "t", FE = T, TFE = T,
                                post = 0, pre = 0, overidpre = 2, overidpost = 0,
                                cluster = TRUE)

estimates_lg2 <- tidy(estimation_lg2[[1]])
View(estimates_lg2)

estimation_m3 <- EventStudy(estimator = "OLS", data = df_sample_dynamic,
                                outcomevar = "y_base", policyvar = "z",
                                idvar = "id", timevar = "t", FE = T, TFE = T,
                                post = 3, pre = 0, overidpre = 0, overidpost = 0,
                                cluster = TRUE)

estimates_m3 <- tidy(estimation_m3[[1]])
