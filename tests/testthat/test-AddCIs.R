test_that("correctly recognizes wrong variable type for Estimate variable", {
    load("./../../data/df_sample_dynamic.RData")
    df_test <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    expect_error(AddCIs(df_test, .95))
})

test_that("correctly recognizes wrong variable input for CI variable", {
    load("./../../data/df_sample_dynamic.RData")
    df_test <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                     policyvar = "z", idvar = "id", timevar = "t",
                     controls = "x_r", FE = TRUE, TFE = TRUE,
                     post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    expect_error(AddCIs(df_test[[1]], "95"))
    expect_error(AddCIs(df_test[[1]], 95))
    expect_error(AddCIs(df_test[[1]], -.95))
})
