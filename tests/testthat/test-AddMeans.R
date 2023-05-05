test_that("means are computed correctly when a first differenced variable is normalized", {
    suppressWarnings(
        sample_estimation <- EventStudy(estimator = "OLS", data = example_data,
                                    outcomevar = "y_base", policyvar = "z",
                                    idvar = "id", timevar = "t", FE = TRUE,
                                    TFE = TRUE, post = 2, pre = 2, overidpre = 2,
                                    overidpost = 2, normalize = -1, cluster = TRUE, anticipation_effects_normalization = TRUE)
    )

    df_estimation <- sample_estimation[[2]]$data

    mean_function <- AddMeans(df_estimation, "z_fd_lead1",
                              sample_estimation[[2]]$policyvar,
                              sample_estimation[[2]]$outcomevar)

    mean_manual   <- mean(df_estimation[df_estimation[,"z_fd_lead1"] != 0, ]$y_base,
                          na.rm = T)

    expect_equal(mean_function, mean_manual)

})

test_that("means are computed correctly when the furthest lead is normalized", {
    sample_estimation <- EventStudy(estimator = "OLS", data = example_data,
                                    outcomevar = "y_base", policyvar = "z",
                                    idvar = "id", timevar = "t", FE = TRUE,
                                    TFE = TRUE, post = 2, pre = 2, overidpre = 2,
                                    overidpost = 2, normalize = -5, cluster = TRUE, anticipation_effects_normalization = TRUE)

    df_estimation <- sample_estimation[[2]]$data

    mean_function <- AddMeans(df_estimation, "z_lead4",
                              sample_estimation[[2]]$policyvar,
                              sample_estimation[[2]]$outcomevar)

    mean_manual   <- mean(df_estimation[df_estimation[,"z_lead4"] == 0, ]$y_base,
                          na.rm = T)

    expect_equal(mean_function, mean_manual)
})


