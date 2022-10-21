test_that("correctly calculates conf_level at 0.95", {
    df_test <- as.data.frame(data.table::fread("./input/df_test_AddCI.csv"))

    policyvar <- "z"
    eventstudy_coefficients <- c("z_fd_lag1", "z_fd_lead1")

    df_test_CI <- AddCIs(df_test, policyvar, eventstudy_coefficients, 0.95)

    expected_lower <- 2 - 1.959964
    expected_upper <- 2 + 1.959964

    expect_equal(df_test_CI$ci_lower[df_test_CI$term == "z_fd_lead1"], expected_lower, tolerance = 1e-6)
    expect_equal(df_test_CI$ci_upper[df_test_CI$term == "z_fd_lead1"], expected_upper, tolerance = 1e-6)
})

# OLS ------------------------------------------

test_that("correctly recognizes wrong class for estimate argument", {
    estimates <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    policyvar <- "z"
    eventstudy_coefficients <- estimates[[2]]$eventstudy_coefficients

    expect_error(AddCIs(df_test, policyvar, eventstudy_coefficients, 0.95))
})

test_that("correctly recognizes missing columns in estimates argument", {
    estimates <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_test <- estimatr::tidy(estimates[[1]])

    policyvar <- "z"
    eventstudy_coefficients <- estimates[[2]]$eventstudy_coefficients

    df_test_noterm <- df_test %>% rename(wrongname = term)
    df_test_noest  <- df_test %>% rename(wrongname = estimate)
    df_test_nostd  <- df_test %>% rename(wrongname = std.error)

    expect_error(AddCIs(df_test_noterm, policyvar, eventstudy_coefficients, 0.95))
    expect_error(AddCIs(df_test_noest , policyvar, eventstudy_coefficients, 0.95))
    expect_error(AddCIs(df_test_nostd , policyvar, eventstudy_coefficients, 0.95))
})

test_that("correctly recognizes wrong inputs for policyvar argument", {
    estimates <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_test <- estimatr::tidy(estimates[[1]])

    policyvar <- 1
    eventstudy_coefficients <- estimates[[2]]$eventstudy_coefficients

    expect_error(AddCIs(df_test, policyvar, eventstudy_coefficients, 0.95))
})

test_that("correctly recognizes wrong inputs for conf_level argument", {
    estimates <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                     policyvar = "z", idvar = "id", timevar = "t",
                     controls = "x_r", FE = TRUE, TFE = TRUE,
                     post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_test <- estimatr::tidy(estimates[[1]])

    policyvar <- "z"
    eventstudy_coefficients <- estimates[[2]]$eventstudy_coefficients

    expect_error(AddCIs(df_test, policyvar, eventstudy_coefficients, "95"))
    expect_error(AddCIs(df_test, policyvar, eventstudy_coefficients,  95))
    expect_error(AddCIs(df_test, policyvar, eventstudy_coefficients,-.95))
})

# FHS ------------------------------------------

test_that("correctly recognizes wrong class for estimate argument", {
    data <- df_sample_dynamic[, c("y_base", "z", "id", "t", "x_r", "eta_m")]
    estimates <- EventStudy(estimator = "FHS", data = data, outcomevar = "y_base", policyvar = "z", idvar = "id",
                            timevar = "t", controls = "x_r", proxy = "eta_m", FE = TRUE, TFE = TRUE, post = 1,
                            overidpost = 2, pre = 1, overidpre = 2, normalize = -1, cluster = TRUE)

    policyvar <- "z"
    eventstudy_coefficients <- estimates[[2]]$eventstudy_coefficients

    expect_error(AddCIs(df_test, policyvar, eventstudy_coefficients, 0.95))
})

test_that("correctly recognizes missing columns in estimates argument", {
    data <- df_sample_dynamic[, c("y_base", "z", "id", "t", "x_r", "eta_m")]
    estimates <- EventStudy(estimator = "FHS", data = data, outcomevar = "y_base", policyvar = "z", idvar = "id",
                            timevar = "t", controls = "x_r", proxy = "eta_m", FE = TRUE, TFE = TRUE, post = 1,
                            overidpost = 2, pre = 1, overidpre = 2, normalize = -1, cluster = TRUE)

    df_test <- estimatr::tidy(estimates[[1]])

    policyvar <- "z"
    eventstudy_coefficients <- estimates[[2]]$eventstudy_coefficients

    df_test_noterm <- df_test %>% rename(wrongname = term)
    df_test_noest  <- df_test %>% rename(wrongname = estimate)
    df_test_nostd  <- df_test %>% rename(wrongname = std.error)

    expect_error(AddCIs(df_test_noterm, policyvar, eventstudy_coefficients, 0.95))
    expect_error(AddCIs(df_test_noest , policyvar, eventstudy_coefficients, 0.95))
    expect_error(AddCIs(df_test_nostd , policyvar, eventstudy_coefficients, 0.95))
})

test_that("correctly recognizes wrong inputs for policyvar argument", {
    data <- df_sample_dynamic[, c("y_base", "z", "id", "t", "x_r", "eta_m")]
    estimates <- EventStudy(estimator = "FHS", data = data, outcomevar = "y_base", policyvar = "z", idvar = "id",
                            timevar = "t", controls = "x_r", proxy = "eta_m", FE = TRUE, TFE = TRUE, post = 1,
                            overidpost = 2, pre = 1, overidpre = 2, normalize = -1, cluster = TRUE)

    df_test <- estimatr::tidy(estimates[[1]])

    policyvar <- 1
    eventstudy_coefficients <- estimates[[2]]$eventstudy_coefficients

    expect_error(AddCIs(df_test, policyvar, eventstudy_coefficients, 0.95))
})

test_that("correctly recognizes wrong inputs for conf_level argument", {
    data <- df_sample_dynamic[, c("y_base", "z", "id", "t", "x_r", "eta_m")]
    estimates <- EventStudy(estimator = "FHS", data = data, outcomevar = "y_base", policyvar = "z", idvar = "id",
                            timevar = "t", controls = "x_r", proxy = "eta_m", FE = TRUE, TFE = TRUE, post = 1,
                            overidpost = 2, pre = 1, overidpre = 2, normalize = -1, cluster = TRUE)

    df_test <- estimatr::tidy(estimates[[1]])

    policyvar <- "z"
    eventstudy_coefficients <- estimates[[2]]$eventstudy_coefficients

    expect_error(AddCIs(df_test, policyvar, eventstudy_coefficients, "95"))
    expect_error(AddCIs(df_test, policyvar, eventstudy_coefficients,  95))
    expect_error(AddCIs(df_test, policyvar, eventstudy_coefficients,-.95))
})



