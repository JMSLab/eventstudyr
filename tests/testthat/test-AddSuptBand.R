test_that("check that the correct part of vcov matrix is used", {
    sample_estimation <- EventStudy(estimator = "OLS", data = df_sample_dynamic,
                                    outcomevar = "y_base", policyvar = "z",
                                    idvar = "id", timevar = "t", FE = TRUE, control = "x_r",
                                    TFE = TRUE, post = 2, pre = 2, overidpre = 2,
                                    overidpost = 2, normalize = -1, cluster = TRUE)

    eventstudy_coefficients <- sample_estimation[[2]]$eventstudy_coefficients
    vcov_matrix_all <- sample_estimation[[1]]$vcov
    v_terms_to_keep <- colnames(vcov_matrix_all) %in% eventstudy_coefficients
    vcov_matrix     <- vcov_matrix_all[v_terms_to_keep, v_terms_to_keep]

    expect_true(isSymmetric(vcov_matrix))
    expect_true(all(eventstudy_coefficients %in% colnames(vcov_matrix)))
    expect_true(! "x_r" %in% colnames(vcov_matrix))
})


