
# OLS ---------------------------------------------------------------------

test_that("correctly changes x-axis and y-axis labels", {

    estimates <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                            policyvar = "z", idvar = "id", timevar = "t", controls = "x_r",
                            post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3)

    p_labels <- EventStudyPlot(estimates = estimates,
                               conf_level = .95,
                               xtitle     = "Event Time",
                               ytitle     = "Event-study Coefficients",)

    expect_equal(p_labels$labels$x, "Event Time")
    expect_equal(p_labels$labels$y, "Event-study Coefficients")

})

test_that("x- and y-axis breaks and limits are correct", {

    estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                           policyvar = "z", idvar = "id", timevar = "t", controls = "x_r",
                           post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3)

    p_Addmean <- EventStudyPlot(estimates = estimates,
                                ybreaks   = c(-1.5, -.5, 0, .5, 1.5),
                                Addmean   = TRUE)

    p_noAddmean <- EventStudyPlot(estimates = estimates,
                                  ybreaks   = c(-1.5, -.5, 0, .5, 1.5),
                                  Addmean   = FALSE)

    v_limits_addmeans    <- p_Addmean$scales$scales[[1]]$limits
    v_limits_no_addmeans <- p_noAddmean$scales$scales[[1]]$limits
    v_breaks_addmeans    <- p_Addmean$scales$scales[[1]]$breaks
    v_breaks_no_addmeans <- p_noAddmean$scales$scales[[1]]$breaks

    expect_equal(v_limits_addmeans,    c(-1.5, 1.5))
    expect_equal(v_limits_no_addmeans, c(-1.5, 1.5))

    expect_equal(v_breaks_addmeans,    c(-1.5, -.5, 0, .5, 1.5))
    expect_equal(v_breaks_no_addmeans, c(-1.5, -.5, 0, .5, 1.5))
})

test_that("correctly adds mean of outcome var", {

    estimates = EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                           policyvar = "z", idvar = "id", timevar = "t", controls = "x_r",
                           post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3)

    p_Addmean <- EventStudyPlot(estimates = estimates,
                                ybreaks   = c(-1.5, -.5, 0, .5, 1.5),
                                Addmean   = TRUE)

    p_noAddmean <- EventStudyPlot(estimates = estimates,
                                  ybreaks   = c(-1.5, -.5, 0, .5, 1.5),
                                  Addmean   = FALSE)

    y_mean <- AddMeans(estimates[[2]]$data, estimates[[2]]$normalization_column,
                       "z", "y_base")
    y_mean <- round(y_mean, 2)

    v_labels_addmeans    <- p_Addmean$scales$scales[[1]]$labels
    v_labels_no_addmeans <- p_noAddmean$scales$scales[[1]]$labels

    expect_equal(v_labels_addmeans,    c("-1.5", "-0.5", sprintf("0 (%s)", y_mean), "0.5", "1.5"))
    expect_equal(v_labels_no_addmeans, c(-1.5, -.5, 0, .5, 1.5))
})

test_that("sup-t bands are appropriately present or absent", {

    estimates <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                            policyvar = "z", idvar = "id", timevar = "t",
                            controls = "x_r", post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3)

    p_supt <- EventStudyPlot(estimates = estimates,
                             Supt = .95)

    p_no_supt <- EventStudyPlot(estimates = estimates,
                                Supt = NULL)

    expect_true(p_supt$labels$ymin    == "suptband_lower")
    expect_true(p_no_supt$labels$ymin != "suptband_lower")

    expect_true(p_supt$labels$ymax    == "suptband_upper")
    expect_true(p_no_supt$labels$ymax != "suptband_upper")
})

test_that("confidence intervals are appropriately present or absent", {

    estimates <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                            policyvar = "z", idvar = "id", timevar = "t",
                            controls = "x_r", post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3)

    p_ci <- EventStudyPlot(estimates = estimates,
                           conf_level = .95, Supt = NULL)

    p_no_ci <- EventStudyPlot(estimates = estimates,
                              conf_level = NULL, Supt = NULL)

    expect_equal(p_ci$labels$ymin, "ci_lower")
    expect_equal(p_ci$labels$ymax, "ci_upper")
    expect_null(p_no_ci$labels$ymin)
    expect_null(p_no_ci$labels$ymax)
})

test_that("Preevent Coeffs and Postevent Coeffs are appropriately present or absent", {

    estimates <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                            policyvar = "z", idvar = "id", timevar = "t", controls = "x_r",
                            post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3)

    p_pre_post_caption <- EventStudyPlot(estimates       = estimates,
                                         ybreaks         = c(-1.5, -.5, 0, .5, 1.5),
                                         Preeventcoeffs  = TRUE,
                                         Posteventcoeffs = TRUE)$labels$caption

    p_pre_caption      <- EventStudyPlot(estimates       = estimates,
                                         ybreaks         = c(-1.5, -.5, 0, .5, 1.5),
                                         Preeventcoeffs  = TRUE,
                                         Posteventcoeffs = FALSE)$labels$caption

    p_post_caption     <- EventStudyPlot(estimates       = estimates,
                                         ybreaks         = c(-1.5, -.5, 0, .5, 1.5),
                                         Preeventcoeffs  = FALSE,
                                         Posteventcoeffs = TRUE)$labels$caption

    p_neither_caption   <- EventStudyPlot(estimates       = estimates,
                                          ybreaks         = c(-1.5, -.5, 0, .5, 1.5),
                                          Preeventcoeffs  = FALSE,
                                          Posteventcoeffs = FALSE)$labels$caption

    regex_for_p_value <- "1\\.0*$|0\\.\\d+" # 1 followed by . and then zero or more 0's or 0 then . then any number
    regex_pretrends   <- "Pretrends p-value = "
    regex_posttrends  <- "Leveling off p-value = "

    expect_true(
        stringr::str_detect(p_pre_post_caption, regex_for_p_value) &
        stringr::str_detect(p_pre_post_caption, regex_pretrends)   &
        stringr::str_detect(p_pre_post_caption, regex_posttrends)
    )

    expect_true(
        stringr::str_detect(p_pre_caption, regex_for_p_value) &
        stringr::str_detect(p_pre_caption, regex_pretrends)
    )

    expect_false(
        stringr::str_detect(p_pre_caption, regex_for_p_value) &
        stringr::str_detect(p_pre_caption, regex_posttrends)
    )

    expect_false(
        stringr::str_detect(p_post_caption, regex_for_p_value) &
        stringr::str_detect(p_post_caption, regex_pretrends)
    )

    expect_true(
        stringr::str_detect(p_post_caption, regex_for_p_value) &
        stringr::str_detect(p_post_caption, regex_posttrends)
    )

    expect_null(p_neither_caption)
})

test_that("Sup-t bands are wider than confidence intervals", {

    estimates <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                            policyvar = "z", idvar = "id", timevar = "t", controls = "x_r",
                            post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3)

    p <- EventStudyPlot(estimates = estimates,
                        conf_level = .95,
                        Supt = .95)

    ci_lower       <- na.omit(p$data$ci_lower)
    ci_upper       <- na.omit(p$data$ci_upper)
    suptband_lower <- na.omit(p$data$suptband_lower)
    suptband_upper <- na.omit(p$data$suptband_upper)
    num_terms      <- nrow(na.omit(p$data))

    v_lower_comparison <- (suptband_lower <= ci_lower)
    v_upper_comparison <- (suptband_upper >= ci_upper)

    expect_equal(num_terms, sum(v_lower_comparison))
    expect_equal(num_terms, sum(v_upper_comparison))
})
