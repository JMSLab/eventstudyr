
test_that("The x-axis and y-axis labels are correct", {

    p <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                               policyvar = "z", idvar = "id", timevar = "t",
                                               controls = "x_r", FE = TRUE, TFE = TRUE,
                                               post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
                        CI = .95,
                        Supt = .95,
                        seed = 1234,
                        Preeventcoeffs = TRUE,
                        Posteventcoeffs = TRUE,
                        Nozeroline = FALSE,
                        Smpath = NULL)

    expect_equal(p$labels$x, "Event time")
    expect_equal(p$labels$y, "Coefficient")


})

test_that("Sup-t bands are appropriately present or absent", {

    p_with_supt <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                                         policyvar = "z", idvar = "id", timevar = "t",
                                                         controls = "x_r", FE = TRUE, TFE = TRUE,
                                                         post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
                                  CI = .95,
                                  Supt = .95,
                                  seed = 1234,
                                  Preeventcoeffs = TRUE,
                                  Posteventcoeffs = TRUE,
                                  Nozeroline = FALSE,
                                  Smpath = NULL)

    p_without_supt <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                                            policyvar = "z", idvar = "id", timevar = "t",
                                                            controls = "x_r", FE = TRUE, TFE = TRUE,
                                                            post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
                                     CI = .95,
                                     Supt = NULL,
                                     seed = 1234,
                                     Preeventcoeffs = TRUE,
                                     Posteventcoeffs = TRUE,
                                     Nozeroline = FALSE,
                                     Smpath = NULL)

    expect_equal(p_with_supt$labels$ymin, "suptband_lower")
    expect_equal(p_with_supt$labels$ymax, "suptband_upper")
    expect_true(p_without_supt$labels$ymin != "suptband_lower")
    expect_true(p_without_supt$labels$ymax != "suptband_upper")

})

test_that("Confidence intervals are appropriately present or absent", {

    p_with_ci <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                                         policyvar = "z", idvar = "id", timevar = "t",
                                                         controls = "x_r", FE = TRUE, TFE = TRUE,
                                                         post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
                                  CI = .95,
                                  Supt = NULL,
                                  seed = 1234,
                                  Preeventcoeffs = TRUE,
                                  Posteventcoeffs = TRUE,
                                  Nozeroline = FALSE,
                                  Smpath = NULL)

    p_without_ci <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                                            policyvar = "z", idvar = "id", timevar = "t",
                                                            controls = "x_r", FE = TRUE, TFE = TRUE,
                                                            post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
                                     CI = NULL,
                                     Supt = NULL,
                                     seed = 1234,
                                     Preeventcoeffs = TRUE,
                                     Posteventcoeffs = TRUE,
                                     Nozeroline = FALSE,
                                     Smpath = NULL)

    expect_equal(p_with_ci$labels$ymin, "ci_lower")
    expect_equal(p_with_ci$labels$ymax, "ci_upper")
    expect_null(p_without_ci$labels$ymin)
    expect_null(p_without_ci$labels$ymax)

})

test_that("Preevent Coeffs and Postevent Coeffs are appropriately present or absent", {

    p_with_pre_and_post_caption <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                                       policyvar = "z", idvar = "id", timevar = "t",
                                                       controls = "x_r", FE = TRUE, TFE = TRUE,
                                                       post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
                                CI = .95,
                                Supt = NULL,
                                seed = 1234,
                                Preeventcoeffs = TRUE,
                                Posteventcoeffs = TRUE,
                                Nozeroline = FALSE,
                                Smpath = NULL)$labels$caption

    p_with_pre_caption <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                                          policyvar = "z", idvar = "id", timevar = "t",
                                                          controls = "x_r", FE = TRUE, TFE = TRUE,
                                                          post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
                                   CI = NULL,
                                   Supt = NULL,
                                   seed = 1234,
                                   Preeventcoeffs = TRUE,
                                   Posteventcoeffs = FALSE,
                                   Nozeroline = FALSE,
                                   Smpath = NULL)$labels$caption

    p_with_post_caption <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                                                policyvar = "z", idvar = "id", timevar = "t",
                                                                controls = "x_r", FE = TRUE, TFE = TRUE,
                                                                post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
                                         CI = NULL,
                                         Supt = NULL,
                                         seed = 1234,
                                         Preeventcoeffs = FALSE,
                                         Posteventcoeffs = TRUE,
                                         Nozeroline = FALSE,
                                         Smpath = NULL)$labels$caption

    p_neither_caption <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                                                 policyvar = "z", idvar = "id", timevar = "t",
                                                                 controls = "x_r", FE = TRUE, TFE = TRUE,
                                                                 post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
                                          CI = NULL,
                                          Supt = NULL,
                                          seed = 1234,
                                          Preeventcoeffs = FALSE,
                                          Posteventcoeffs = FALSE,
                                          Nozeroline = FALSE,
                                          Smpath = NULL)$labels$caption

    regex_for_p_value <- "1\\.0*$|0\\.\\d+" # 1 followed by . and then zero or more 0's or 0 then . then any number
    regex_pretrends <- "Pretrends p-value = "
    regex_posttrends <- "Leveling off p-value = "

    expect_true(
        stringr::str_detect(p_with_pre_and_post_caption, regex_for_p_value) &
        stringr::str_detect(p_with_pre_and_post_caption, regex_pretrends)
    )

    expect_true(
        stringr::str_detect(p_with_pre_and_post_caption, regex_for_p_value) &
        stringr::str_detect(p_with_pre_and_post_caption, regex_posttrends)
    )

    expect_true(
        stringr::str_detect(p_with_pre_caption, regex_for_p_value) &
        stringr::str_detect(p_with_pre_caption, regex_pretrends)
    )

    expect_false(
        stringr::str_detect(p_with_pre_caption, regex_for_p_value) &
        stringr::str_detect(p_with_pre_caption, regex_posttrends)
    )

    expect_false(
        stringr::str_detect(p_with_post_caption, regex_for_p_value) &
        stringr::str_detect(p_with_post_caption, regex_pretrends)
    )

    expect_true(
        stringr::str_detect(p_with_post_caption, regex_for_p_value) &
        stringr::str_detect(p_with_post_caption, regex_posttrends)
    )

    expect_null(p_neither_caption)




})

test_that("Sup-t bands are wider than confidence intervals", {

    p <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                               policyvar = "z", idvar = "id", timevar = "t",
                                               controls = "x_r", FE = TRUE, TFE = TRUE,
                                               post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
                        CI = .95,
                        Supt = .95,
                        seed = 1234,
                        Preeventcoeffs = TRUE,
                        Posteventcoeffs = TRUE,
                        Nozeroline = FALSE,
                        Smpath = NULL)

    ci_lower <- p$data$ci_lower
    ci_upper <- p$data$ci_upper
    suptband_lower <- p$data$suptband_lower
    suptband_upper <- p$data$suptband_upper
    num_terms <- nrow(p$data)

    v_lower_comparison <- suptband_lower <= ci_lower
    v_upper_comparison <- suptband_upper >= ci_upper

    lower_check <- num_terms == sum(v_lower_comparison)
    upper_check <- num_terms == sum(v_upper_comparison)

    expect_true(lower_check)
    expect_true(upper_check)


})
