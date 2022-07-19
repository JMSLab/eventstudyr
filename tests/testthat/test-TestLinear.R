test_that("correctly recognizes wrong variable type for estimate argument", {
    estimate <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_estimate <- estimatr::tidy(estimate[[1]])

    test = "z_fd_lag1 = z_fd"

    expect_error(TestLinear(df_estimate, test, pretrends = T, leveling_off =  T))
    expect_error(TestLinear(estimate[[1]], test, pretrends = T, leveling_off =  T))
    expect_error(TestLinear(estimate[[2]], test, pretrends = T, leveling_off =  T))
})

test_that("correctly recognizes wrong variable type for pretrends", {
    estimate <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                           policyvar = "z", idvar = "id", timevar = "t",
                           controls = "x_r", FE = TRUE, TFE = TRUE,
                           post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    expect_error(TestLinear(df_estimate, pretrends = "pretrends"))
    expect_error(TestLinear(df_estimate, pretrends = 1))
})

test_that("correctly recognizes wrong variable type for leveling_off", {
    estimate <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                           policyvar = "z", idvar = "id", timevar = "t",
                           controls = "x_r", FE = TRUE, TFE = TRUE,
                           post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    expect_error(TestLinear(df_estimate, leveling_off = "leveling_off"))
    expect_error(TestLinear(df_estimate, leveling_off = 1))
})

test_that("produces only functions that are specified", {
    estimate <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                           policyvar = "z", idvar = "id", timevar = "t",
                           controls = "x_r", FE = TRUE, TFE = TRUE,
                           post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    test = "z_fd_lag1 = z_fd"

    df <- TestLinear(estimate, pretrends = F, leveling_off = F)
    expect_equal(length(df$Test), 0)

    df <- TestLinear(estimate, test, pretrends = F, leveling_off = F)
    expect_equal(length(df$Test), 1)
    expect_equal(df$Test, "User Test")

    df <- TestLinear(estimate, pretrends = T, leveling_off = F)
    expect_equal(length(df$Test), 1)
    expect_equal(df$Test, "Pre-Trends")

    df <- TestLinear(estimate, pretrends = F, leveling_off = T)
    expect_equal(length(df$Test), 1)
    expect_equal(df$Test, "Leveling-Off")

    df <- TestLinear(estimate, test, pretrends = T, leveling_off = F)
    expect_equal(length(df$Test), 2)
    tests <- c("User Test", "Pre-Trends")
    expect_equal(df$Test, tests)

    df <- TestLinear(estimate, test, pretrends = F, leveling_off = T)
    expect_equal(length(df$Test), 2)
    tests <- c("User Test", "Leveling-Off")
    expect_equal(df$Test, tests)

    df <- TestLinear(estimate, pretrends = T, leveling_off = T)
    expect_equal(length(df$Test), 2)
    tests <- c("Pre-Trends", "Leveling-Off")
    expect_equal(df$Test, tests)

    df <- TestLinear(estimate, test, pretrends = T, leveling_off = T)
    expect_equal(length(df$Test), 3)
    tests <- c("User Test", "Pre-Trends", "Leveling-Off")
    expect_equal(df$Test, tests)
})


test_that("checks equality with STATA", {
    estimate <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                           policyvar = "z", idvar = "id", timevar = "t",
                           FE = TRUE, TFE = TRUE,
                           post = 2, pre = 2, overidpre = 2,
                           overidpost = 2, normalize = - 1,
                           cluster = TRUE)

    codes <- read.csv('input/pvalues.txt', header =F)
    
    pretrends_stata_p <- codes[1,1]
    leveloff_stata_p  <- codes[2,1]

    df_test_linear <- TestLinear(estimate, pretrends = T, leveling_off = T)
    pretrends_p_value   <- df_test_linear[df_test_linear["Test"] == "Pre-Trends",   "p.value"]
    levelingoff_p_value <- df_test_linear[df_test_linear["Test"] == "Leveling-Off", "p.value"]
    
    expect_true(abs(pretrends_stata_p - pretrends_p_value) < 0.0001)
    expect_true(abs(leveloff_stata_p  - levelingoff_p_value) < 0.0001)    
})
