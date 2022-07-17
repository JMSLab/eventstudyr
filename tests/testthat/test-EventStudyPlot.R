
test_that("The x-axis and y-axis labels are correct", {

    p <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                               policyvar = "z", idvar = "id", timevar = "t",
                                               controls = "x_r", FE = TRUE, TFE = TRUE,
                                               post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
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

    p_labels <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                               policyvar = "z", idvar = "id", timevar = "t",
                                               controls = "x_r", FE = TRUE, TFE = TRUE,
                                               post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
                        ybreaks = c(-1.5, -.5, 0, .5, 1.5),
                        conf_level = .95,
                        xtitle = "Event Time",
                        ytitle = "Event-Study Coefficients",
                        Supt = .95,
                        num_sim = 1000,
                        seed = 1234,
                        Addmean = TRUE,
                        Preeventcoeffs = TRUE,
                        Posteventcoeffs = TRUE,
                        Nozeroline = FALSE,
                        Smpath = NULL)

    expect_equal(p_labels$labels$x, "Event Time")
    expect_equal(p_labels$labels$y, "Event-Study Coefficients")


})

test_that("The x and y axis breaks and limits are correct", {

    p_Addmean <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                          policyvar = "z", idvar = "id", timevar = "t",
                                          controls = "x_r", FE = TRUE, TFE = TRUE,
                                          post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
                   xtitle = "Event Time",
                   ytitle = "Event-Study Coefficients",
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

    p_without_Addmean <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                                       policyvar = "z", idvar = "id", timevar = "t",
                                                       controls = "x_r", FE = TRUE, TFE = TRUE,
                                                       post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
                                xtitle = "Event Time",
                                ytitle = "Event-Study Coefficients",
                                ybreaks = c(-1.5, -.5, 0, .5, 1.5),
                                conf_level = .95,
                                Supt = .95,
                                num_sim = 1000,
                                seed = 1234,
                                Addmean = FALSE,
                                Preeventcoeffs = TRUE,
                                Posteventcoeffs = TRUE,
                                Nozeroline = FALSE,
                                Smpath = NULL)

    v_limits_addmeans <- p_Addmean$scales$scales[[1]]$limits
    v_limits_without_addmeans <- p_without_Addmean$scales$scales[[1]]$limits
    v_breaks_addmeans <- p_Addmean$scales$scales[[1]]$breaks
    v_breaks_without_addmeans <- p_without_Addmean$scales$scales[[1]]$breaks

    expect_equal(v_limits_addmeans, c(-1.5, 1.5))
    expect_equal(v_limits_without_addmeans, c(-1.5, 1.5))
    expect_equal(v_breaks_addmeans, c(-1.5, -.5, 0, .5, 1.5))
    expect_equal(v_breaks_without_addmeans, c(-1.5, -.5, 0, .5, 1.5))
})

test_that("Sup-t bands are appropriately present or absent", {

    p_with_supt <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                                         policyvar = "z", idvar = "id", timevar = "t",
                                                         controls = "x_r", FE = TRUE, TFE = TRUE,
                                                         post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
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

    p_without_supt <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                                            policyvar = "z", idvar = "id", timevar = "t",
                                                            controls = "x_r", FE = TRUE, TFE = TRUE,
                                                            post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
                                     ybreaks = c(-1.5, -.5, 0, .5, 1.5),
                                     conf_level = .95,
                                     Supt = NULL,
                                     seed = 1234,
                                     Addmean = TRUE,
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
                                  ybreaks = c(-1.5, -.5, 0, .5, 1.5),
                                  conf_level = .95,
                                  Supt = NULL,
                                  seed = 1234,
                                  Addmean = TRUE,
                                  Preeventcoeffs = TRUE,
                                  Posteventcoeffs = TRUE,
                                  Nozeroline = FALSE,
                                  Smpath = NULL)

    p_without_ci <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                                            policyvar = "z", idvar = "id", timevar = "t",
                                                            controls = "x_r", FE = TRUE, TFE = TRUE,
                                                            post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
                                     ybreaks = c(-1.5, -.5, 0, .5, 1.5),
                                     conf_level = NULL,
                                     Supt = NULL,
                                     seed = 1234,
                                     Addmean = TRUE,
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
                                ybreaks = c(-1.5, -.5, 0, .5, 1.5),
                                conf_level = .95,
                                Supt = NULL,
                                seed = 1234,
                                Addmean = TRUE,
                                Preeventcoeffs = TRUE,
                                Posteventcoeffs = TRUE,
                                Nozeroline = FALSE,
                                Smpath = NULL)$labels$caption

    p_with_pre_caption <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                                          policyvar = "z", idvar = "id", timevar = "t",
                                                          controls = "x_r", FE = TRUE, TFE = TRUE,
                                                          post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
                                   ybreaks = c(-1.5, -.5, 0, .5, 1.5),
                                   conf_level = NULL,
                                   Supt = NULL,
                                   seed = 1234,
                                   Addmean = TRUE,
                                   Preeventcoeffs = TRUE,
                                   Posteventcoeffs = FALSE,
                                   Nozeroline = FALSE,
                                   Smpath = NULL)$labels$caption

    p_with_post_caption <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                                                policyvar = "z", idvar = "id", timevar = "t",
                                                                controls = "x_r", FE = TRUE, TFE = TRUE,
                                                                post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
                                         ybreaks = c(-1.5, -.5, 0, .5, 1.5),
                                         conf_level = NULL,
                                         Supt = NULL,
                                         seed = 1234,
                                         Addmean = TRUE,
                                         Preeventcoeffs = FALSE,
                                         Posteventcoeffs = TRUE,
                                         Nozeroline = FALSE,
                                         Smpath = NULL)$labels$caption

    p_neither_caption <- EventStudyPlot(estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                                                 policyvar = "z", idvar = "id", timevar = "t",
                                                                 controls = "x_r", FE = TRUE, TFE = TRUE,
                                                                 post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE),
                                          ybreaks = c(-1.5, -.5, 0, .5, 1.5),
                                          conf_level = NULL,
                                          Supt = NULL,
                                          seed = 1234,
                                          Addmean = TRUE,
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
