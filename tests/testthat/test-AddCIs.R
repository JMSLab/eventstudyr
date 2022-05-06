test_that("correctly recognizes wrong class for estimate argument", {
    load("./../../data/df_sample_dynamic.RData")
    df_test <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    policyvar <- "z"
    normalization_column <- "z_fd_lead3"

    expect_error(AddCIs(df_test, policyvar, normalization_column, 0.95))
})

test_that("correctly recognizes missing columns in estimates argument", {
    load("./../../data/df_sample_dynamic.RData")
    df_test <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_test <- estimatr::tidy(df_test[[1]])

    policyvar <- "z"
    normalization_column <- "z_fd_lead3"

    df_test_noterm <- df_test %>% rename(wrongname = term)
    df_test_noest  <- df_test %>% rename(wrongname = estimate)
    df_test_nostd  <- df_test %>% rename(wrongname = std.error)

    expect_error(AddCIs(df_test_noterm, policyvar, normalization_column, 0.95))
    expect_error(AddCIs(df_test_noest , policyvar, normalization_column, 0.95))
    expect_error(AddCIs(df_test_nostd , policyvar, normalization_column, 0.95))
})

test_that("correctly recognizes wrong inputs for policyvar argument", {
    load("./../../data/df_sample_dynamic.RData")
    df_test <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_test <- estimatr::tidy(df_test[[1]])

    policyvar <- 1
    normalization_column <- "z_fd_lead3"

    expect_error(AddCIs(df_test, policyvar, normalization_column, 0.95))
})

test_that("correctly recognizes wrong inputs for normalization_column argument", {
    load("./../../data/df_sample_dynamic.RData")
    df_test <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_test <- estimatr::tidy(df_test[[1]])

    policyvar <- "z"
    normalization_column <- 3

    expect_error(AddCIs(df_test, policyvar, normalization_column, 0.95))
})

test_that("correctly recognizes wrong inputs for CI argument", {
    load("./../../data/df_sample_dynamic.RData")
    df_test <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                     policyvar = "z", idvar = "id", timevar = "t",
                     controls = "x_r", FE = TRUE, TFE = TRUE,
                     post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    expect_error(AddCIs(df_test[[1]], "95"))
    expect_error(AddCIs(df_test[[1]], 95))
    expect_error(AddCIs(df_test[[1]], -.95))
})

test_that("correctly calculates CI at 0.95", {
    df_test <- read.csv("./input/df_test_AddCI.csv")

    policyvar <- "z"
    normalization_column <- "z_fd"

    df_test_CI <- AddCIs(df_test, policyvar, normalization_column, 0.95)

    expected_lower <- 2 - 1.959964
    expected_upper <- 2 + 1.959964

    expect_equal(df_test_CI$ci_lower[df_test_CI$term == "z_fd_lead1"], expected_lower, tolerance = 1e-6)
    expect_equal(df_test_CI$ci_upper[df_test_CI$term == "z_fd_lead1"], expected_upper, tolerance = 1e-6)
})

test_that("correctly ignores normalization_column", {
    df_test <- read.csv("./input/df_test_AddCI.csv")

    policyvar <- "z"
    normalization_column <- "z_fd"

    df_test_CI <- AddCIs(df_test, policyvar, normalization_column, 0.95)

    expect_true(is.na(df_test_CI$ci_lower[df_test_CI$term == "z_fd"]))
    expect_true(is.na(df_test_CI$ci_upper[df_test_CI$term == "z_fd"]))

})



