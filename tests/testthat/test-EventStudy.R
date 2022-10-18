
# OLS ---------------------------------------------------------------------

test_that("correctly creates highest order leads and lags", {

    post  <- 2
    pre  <- 3
    overidpre <- 4
    overidpost <- 11

    outputs <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = - 1, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term
    largest_fd_lag  <- as.double(stringr::str_extract(leads_lags, "(?<=fd_lag)[0-9]+"))
    largest_fd_lead <- as.double(stringr::str_extract(leads_lags, "(?<=fd_lead)[0-9]+"))
    largest_lag     <- as.double(stringr::str_extract(leads_lags, "(?<=lag)[0-9]+"))
    largest_lead    <- as.double(stringr::str_extract(leads_lags, "(?<=lead)[0-9]+"))

    expect_equal(max(largest_fd_lag, na.rm = TRUE), post + overidpost - 1)
    expect_equal(max(largest_fd_lead, na.rm = TRUE), pre + overidpre)
    expect_equal(max(largest_lag, na.rm = TRUE), post + overidpost)
    expect_equal(max(largest_lead, na.rm = TRUE), pre + overidpre)
})

test_that("correctly throws an error when normalized coefficient is outside event-study window", {

    post  <- 2
    pre  <- 3
    overidpre <- 4
    overidpost <- 7
    normalize <- 15

    expect_error(EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = normalize, cluster = TRUE))
})

test_that("throws an error when post + pre + overidpre + overidpost exceeds the data window", {

    post  <- 10
    pre  <- 15
    overidpre <- 20
    overidpost <- 25
    normalize <- 2

    expect_error(EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = normalize, cluster = TRUE))
})

test_that("removes the correct column when normalize < 0", {

    post  <- 2
    pre  <- 3
    overidpre <- 4
    overidpost <- 7
    normalize <- -2

    outputs <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                          normalize = normalize, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term

    normalization_column <- paste0("z", "_fd_lead", (-1 * normalize))

    expect_equal(stringr::str_extract(normalization_column, "lead"), "lead")
    expect_true(!normalization_column %in% leads_lags)
    expect_true(-1 * normalize > 0)

})

test_that("removes the correct column when normalize = 0", {

    post  <- 2
    pre  <- 3
    overidpre <- 4
    overidpost <- 7
    normalize <- 0

    outputs <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = normalize, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term

    normalization_column <- paste0("z", "_fd")
    expect_equal(stringr::str_extract(normalization_column, "fd"), "fd")
    expect_true(!normalization_column %in% leads_lags)
    expect_true(normalize == 0)
})

test_that("does not create a first differenced variable when post, overidpost, pre, overidpre are all zero", {

    post  <- 0
    pre  <- 0
    overidpre <- 0
    overidpost <- 0
    normalize <- -1

    outputs <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = normalize, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term

    expect_true(! "z_fd" %in% leads_lags)
})

test_that("tests that package and STATA output agree when post, overidpost, pre, overidpre are zero", {

    post  <- 0
    pre  <- 0
    overidpre <- 0
    overidpost <- 0
    normalize <- -1

    outputs <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = normalize, cluster = TRUE)

    coef_package <- outputs[[1]]$coefficients[[1]]
    std_package  <- outputs[[1]]$std.error[[1]]

    STATA_output <- read.csv('./input/df_test_base_STATA_allzero.csv')
    coef_STATA <- STATA_output$coef[[1]]
    std_STATA  <- STATA_output$std_error[[1]]

    epsilon <- 10e-7
    expect_equal(coef_package, coef_STATA, tolerance = epsilon)
    expect_equal(std_package, std_STATA, tolerance = epsilon)
})

test_that("does not create lags of differenced variable when post + overidpost - 1 < 1", {

    post  <- 1
    pre  <- 0
    overidpre <- 0
    overidpost <- 0
    normalize <- -1

    outputs <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = normalize, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term

    n_true <- sum(grepl("fd_lags", leads_lags))

    expect_equal(n_true, 0)
})

test_that("does not create leads of differenced variable when pre + overidpre < 1", {

    post  <- 1
    pre  <- 0
    overidpre <- 0
    overidpost <- 0
    normalize <- -1

    outputs <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = normalize, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term

    n_true <- sum(grepl("fd_leads", leads_lags))

    expect_equal(n_true, 0)
})

test_that("removes the correct column when normalize > 0", {

    post  <- 2
    pre  <- 3
    overidpre <- 4
    overidpost <- 7
    normalize <- 2

    outputs <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = normalize, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term

    normalization_column <- paste0("z", "_fd_lag", normalize)
    expect_equal(stringr::str_extract(normalization_column, "lag"), "lag")
    expect_true(!normalization_column %in% leads_lags)
    expect_true(normalize > 0)
})

test_that("removes the correct column when normalize = - (pre + overidpre + 1)", {

    post  <- 3
    pre  <- 2
    overidpre <- 1
    overidpost <- 4
    normalize <- -4

    outputs <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = normalize, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term

    normalization_column <- paste0("z", "_lead", -1 * (normalize + 1))
    expect_equal(stringr::str_extract(normalization_column, "lead"), "lead")
    expect_true(!normalization_column %in% leads_lags)
})

test_that("removes the correct column when normalize = post + overidpost", {

    post  <- 3
    pre  <- 2
    overidpre <- 1
    overidpost <- 4
    normalize <- 5

    outputs <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = normalize, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term

    normalization_column <- paste0("z", "_lag", normalize )
    expect_equal(stringr::str_extract(normalization_column, "lag"), "lag")
    expect_true(!normalization_column %in% leads_lags)
})

test_that("subtraction is peformed on the correct column", {

    post  <- 1
    pre  <- 1
    overidpre <- 2
    overidpost <- 2

    df_first_diff <- GetFirstDifferences(df = df_sample_static, groupvar = "id", timevar = "t", diffvar = "z")

    num_fd_lag_periods   <- post + overidpost - 1
    num_fd_lead_periods  <- pre + overidpre

    furthest_lag_period    <- num_fd_lag_periods + 1

    df_first_diff_leads      <- PrepareLeads(df_first_diff, groupvar = "id", timevar = "t",
                                             leadvar = paste0("z", "_fd"), leads = 1:num_fd_lead_periods)
    df_first_diff_leads_lags <- PrepareLags(df_first_diff_leads, groupvar = "id", timevar = "t",
                                            lagvar = paste0("z", "_fd"), lags = 1:num_fd_lag_periods)


    df_lag           <- PrepareLags(df_first_diff_leads_lags, groupvar = "id", timevar = "t",
                                    lagvar = "z", lags = furthest_lag_period)
    df_lag_lead      <- PrepareLeads(df_lag, groupvar = "id", timevar = "t",
                                     leadvar = "z", leads = num_fd_lead_periods)


    column_subtract_1 <- paste0("z", "_lead", num_fd_lead_periods)
    df_lag_lead_subtracted_1 <- 1 - df_lag_lead[column_subtract_1]

    num_equal <- sum(df_lag_lead_subtracted_1[column_subtract_1] == 1 - df_lag_lead[column_subtract_1], na.rm = TRUE)
    num_na <- sum(is.na(df_lag_lead_subtracted_1[column_subtract_1]))
    column_subtract_degree <- as.double(stringr::str_extract(column_subtract_1, "(?<=lead)[0-9]+"))

    expect_equal(num_equal + num_na, nrow(df_lag_lead))
    expect_equal(column_subtract_degree, pre + overidpre)
})

# FHS ---------------------------------------------------------------------

test_that("correctly creates highest order leads and lags", {

    post  <- 2
    pre  <- 3
    overidpre <- 4
    overidpost <- 11

    outputs <- EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = - 1, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term
    largest_fd_lag  <- as.double(stringr::str_extract(leads_lags, "(?<=fd_lag)[0-9]+"))
    largest_fd_lead <- as.double(stringr::str_extract(leads_lags, "(?<=fd_lead)[0-9]+"))
    largest_lag     <- as.double(stringr::str_extract(leads_lags, "(?<=lag)[0-9]+"))
    largest_lead    <- as.double(stringr::str_extract(leads_lags, "(?<=lead)[0-9]+"))

    expect_equal(max(largest_fd_lag, na.rm = TRUE), post + overidpost - 1)
    expect_equal(max(largest_fd_lead, na.rm = TRUE), pre + overidpre)
    expect_equal(max(largest_lag, na.rm = TRUE), post + overidpost)
    expect_equal(max(largest_lead, na.rm = TRUE), pre + overidpre)
})

test_that("correctly throws an error when normalized coefficient is outside event-study window", {

    post  <- 2
    pre  <- 3
    overidpre <- 4
    overidpost <- 7
    normalize <- 15

    expect_error(EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = normalize, cluster = TRUE))
})

test_that("throws an error when post + pre + overidpre + overidpost exceeds the data window", {

    post  <- 10
    pre  <- 15
    overidpre <- 20
    overidpost <- 25
    normalize <- 2

    expect_error(EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = normalize, cluster = TRUE))
})

test_that("removes the correct column when normalize < 0", {

    post  <- 2
    pre  <- 3
    overidpre <- 4
    overidpost <- 7
    normalize <- -2

    outputs <- EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                          normalize = normalize, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term

    normalization_column <- paste0("z", "_fd_lead", (-1 * normalize))

    expect_equal(stringr::str_extract(normalization_column, "lead"), "lead")
    expect_true(!normalization_column %in% leads_lags)
    expect_true(-1 * normalize > 0)

})

test_that("removes the correct column when normalize = 0", {

    post  <- 2
    pre  <- 3
    overidpre <- 4
    overidpost <- 7
    normalize <- 0

    outputs <- EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = normalize, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term

    normalization_column <- paste0("z", "_fd")
    expect_equal(stringr::str_extract(normalization_column, "fd"), "fd")
    expect_true(!normalization_column %in% leads_lags)
    expect_true(normalize == 0)
})

test_that("FHS does not run when post, pre, overidpre, and overidpost are all 0", {

    post  <- 0
    pre  <- 0
    overidpre <- 0
    overidpost <- 0
    normalize <- -1

    expect_error(outputs <- EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = normalize, cluster = TRUE))


})

test_that("removes the correct column when normalize > 0", {

    post  <- 2
    pre  <- 3
    overidpre <- 4
    overidpost <- 7
    normalize <- 2

    outputs <- EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = normalize, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term

    normalization_column <- paste0("z", "_fd_lag", normalize)
    expect_equal(stringr::str_extract(normalization_column, "lag"), "lag")
    expect_true(!normalization_column %in% leads_lags)
    expect_true(normalize > 0)
})

test_that("removes the correct column when normalize = - (pre + overidpre + 1)", {

    post  <- 3
    pre  <- 2
    overidpre <- 1
    overidpost <- 4
    normalize <- -4

    outputs <- EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = normalize, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term

    normalization_column <- paste0("z", "_lead", -1 * (normalize + 1))
    expect_equal(stringr::str_extract(normalization_column, "lead"), "lead")
    expect_true(!normalization_column %in% leads_lags)
})

test_that("removes the correct column when normalize = post + overidpost", {

    post  <- 3
    pre  <- 2
    overidpre <- 1
    overidpost <- 4
    normalize <- 5

    outputs <- EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE, "eta_m",
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = normalize, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term

    normalization_column <- paste0("z", "_lag", normalize )
    expect_equal(stringr::str_extract(normalization_column, "lag"), "lag")
    expect_true(!normalization_column %in% leads_lags)
})

test_that("subtraction is peformed on the correct column", {

    post  <- 1
    pre  <- 1
    overidpre <- 2
    overidpost <- 2

    df_first_diff <- GetFirstDifferences(df = df_sample_static, groupvar = "id", timevar = "t", diffvar = "z")

    num_fd_lag_periods   <- post + overidpost - 1
    num_fd_lead_periods  <- pre + overidpre

    furthest_lag_period    <- num_fd_lag_periods + 1

    df_first_diff_leads      <- PrepareLeads(df_first_diff, groupvar = "id", timevar = "t",
                                             leadvar = paste0("z", "_fd"), leads = 1:num_fd_lead_periods)
    df_first_diff_leads_lags <- PrepareLags(df_first_diff_leads, groupvar = "id", timevar = "t",
                                            lagvar = paste0("z", "_fd"), lags = 1:num_fd_lag_periods)


    df_lag           <- PrepareLags(df_first_diff_leads_lags, groupvar = "id", timevar = "t",
                                    lagvar = "z", lags = furthest_lag_period)
    df_lag_lead      <- PrepareLeads(df_lag, groupvar = "id", timevar = "t",
                                     leadvar = "z", leads = num_fd_lead_periods)


    column_subtract_1 <- paste0("z", "_lead", num_fd_lead_periods)
    df_lag_lead_subtracted_1 <- 1 - df_lag_lead[column_subtract_1]

    num_equal <- sum(df_lag_lead_subtracted_1[column_subtract_1] == 1 - df_lag_lead[column_subtract_1], na.rm = TRUE)
    num_na <- sum(is.na(df_lag_lead_subtracted_1[column_subtract_1]))
    column_subtract_degree <- as.double(stringr::str_extract(column_subtract_1, "(?<=lead)[0-9]+"))

    expect_equal(num_equal + num_na, nrow(df_lag_lead))
    expect_equal(column_subtract_degree, pre + overidpre)
})

test_that("proxyIV selection works", {

    expect_message(
        EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base", policyvar = "z", idvar = "id",
                   timevar = "t", controls = "x_r", proxy = "eta_m", FE = TRUE, TFE = TRUE, post = 2,
                   overidpost = 2, pre = 1, overidpre = 2, normalize = -1, cluster = TRUE),
        "Defaulting to strongest lead of differenced policy variable: proxyIV = z_fd_lead3"
    )

    expect_message(
        EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base", policyvar = "z", idvar = "id",
                   timevar = "t", controls = "x_r", proxy = "eta_m", FE = TRUE, TFE = TRUE, post = 1,
                   overidpost = 2, pre = 2, overidpre = 2, normalize = -1, cluster = TRUE),
        "Defaulting to strongest lead of differenced policy variable: proxyIV = z_fd_lead4"
    )

    expect_message(
        EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base", policyvar = "z", idvar = "id",
                   timevar = "t", controls = "x_r", proxy = "eta_m", FE = TRUE, TFE = TRUE, post = 1,
                   overidpost = 2, pre = 6, overidpre = 2, normalize = -1, cluster = TRUE),
        "Defaulting to strongest lead of differenced policy variable: proxyIV = z_fd_lead5"
    )
})
