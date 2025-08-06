
# Housekeeping ------------------------------------------------------------

test_that("does not modify input data (even if input is data.table) when avoid_internal_copy = FALSE", {

    example_dt <- data.table::as.data.table(example_data)
    example_dt_copy <- data.table::copy(example_dt)

    outputs <- suppressWarnings(
        EventStudy(
            estimator = "OLS", data = example_dt, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = 2, pre = 3, overidpre = 4,
                          overidpost = 11, normalize = - 1,
                          cluster = TRUE, anticipation_effects_normalization = TRUE)
    )

    expect_true(isTRUE(all.equal(example_dt, example_dt_copy, check.attributes = FALSE)))
})

test_that("input dt IS modified in-place when avoid_internal_copy = TRUE", {

    example_dt <- data.table::as.data.table(example_data)
    example_dt_copy <- data.table::copy(example_dt)
    address_before  <- rlang::obj_address(example_dt)

    outputs <- suppressWarnings(
        EventStudy(
            estimator = "OLS", data = example_dt, outcomevar = "y_base",
            policyvar = "z", idvar = "id", timevar = "t",
            controls = "x_r", FE = TRUE, TFE = TRUE,
            post = 2, pre = 3, overidpre = 4,
            overidpost = 11, normalize = - 1,
            cluster = TRUE, anticipation_effects_normalization = TRUE,
            avoid_internal_copy = TRUE)
    )
    address_after <- rlang::obj_address(example_dt)

    expect_equal(address_before, address_after)
    expect_true(isFALSE(identical(example_dt, example_dt_copy)))
})

# OLS ---------------------------------------------------------------------

test_that("correctly creates highest order shiftvalues", {

    post  <- 2
    pre  <- 3
    overidpre <- 4
    overidpost <- 11

    outputs <- suppressWarnings(
        EventStudy(estimator = "OLS", data = example_data, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = - 1, cluster = TRUE, anticipation_effects_normalization = TRUE)
    )

    shiftvalues      <- outputs$output$term
    largest_fd_lag  <- as.double(stringr::str_extract(shiftvalues, "(?<=fd_lag)[0-9]+"))
    largest_fd_lead <- as.double(stringr::str_extract(shiftvalues, "(?<=fd_lead)[0-9]+"))
    largest_lag     <- as.double(stringr::str_extract(shiftvalues, "(?<=lag)[0-9]+"))
    largest_lead    <- as.double(stringr::str_extract(shiftvalues, "(?<=lead)[0-9]+"))

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

    expect_error(EventStudy(estimator = "OLS", data = example_data, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                          normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE))
})

test_that("throws an error when post + pre + overidpre + overidpost exceeds the data window", {

    post  <- 10
    pre  <- 15
    overidpre <- 20
    overidpost <- 25
    normalize <- 2

    expect_error(EventStudy(estimator = "OLS", data = example_data, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                          normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE))
})

test_that("removes the correct column when normalize < 0", {

    post       <- 2
    pre        <- 3
    overidpre  <- 4
    overidpost <- 7
    normalize  <- -2

    outputs <- EventStudy(estimator = "OLS", data = example_data, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                          normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE)

    shiftvalues <- outputs$output$term

    normalization_column <- paste0("z", "_fd_lead", (-1 * normalize))

    expect_equal(stringr::str_extract(normalization_column, "lead"), "lead")
    expect_true(!normalization_column %in% shiftvalues)
    expect_true(-1 * normalize > 0)

})

test_that("removes the correct column when normalize = 0", {

    post       <- 2
    pre        <- 3
    overidpre  <- 4
    overidpost <- 7
    normalize  <- 0

    outputs <- EventStudy(estimator = "OLS", data = example_data, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                          normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE)

    shiftvalues <- outputs$output$term

    normalization_column <- paste0("z", "_fd")
    expect_equal(stringr::str_extract(normalization_column, "fd"), "fd")
    expect_true(!normalization_column %in% shiftvalues)
    expect_true(normalize == 0)
})

test_that("does not create a first differenced variable when post, overidpost, pre, overidpre are all zero", {

    post       <- 0
    pre        <- 0
    overidpre  <- 0
    overidpost <- 0
    normalize  <- -1

    outputs <- EventStudy(estimator = "OLS", data = example_data, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                          normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE)

    shiftvalues <- outputs$output$term

    expect_true(! "z_fd" %in% shiftvalues)
})

test_that("tests that package and STATA output agree when post, overidpost, pre, overidpre are zero", {

    post       <- 0
    pre        <- 0
    overidpre  <- 0
    overidpost <- 0
    normalize  <- -1

    outputs <- EventStudy(estimator = "OLS", data = example_data, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                          normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE)

    coef_package <- outputs$output$coefficients[[1]]
    std_package  <- outputs$output$std.error[[1]]

    STATA_output <- read.csv('./input/df_test_base_STATA_allzero.csv')
    coef_STATA <- STATA_output$coef[[1]]
    std_STATA  <- STATA_output$std_error[[1]]

    epsilon <- 10e-7
    expect_equal(coef_package, coef_STATA, tolerance = epsilon)
    expect_equal(std_package, std_STATA, tolerance = epsilon)
})

test_that("does not create shiftvalues of differenced variable when post + overidpost - 1 < 1", {

    post       <- 1
    pre        <- 0
    overidpre  <- 0
    overidpost <- 0
    normalize  <- -1

    outputs <- EventStudy(estimator = "OLS", data = example_data, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                          normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE)

    shiftvalues <- outputs$output$term

    n_true <- sum(grepl("fd_shiftvalues", shiftvalues))

    expect_equal(n_true, 0)
})

test_that("does not create leads of differenced variable when pre + overidpre < 1", {

    post       <- 1
    pre        <- 0
    overidpre  <- 0
    overidpost <- 0
    normalize  <- -1

    outputs <- EventStudy(estimator = "OLS", data = example_data, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                          normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE)

    shiftvalues <- outputs$output$term

    n_true <- sum(grepl("fd_leads", shiftvalues))

    expect_equal(n_true, 0)
})

test_that("removes the correct column when normalize > 0", {

    post       <- 2
    pre        <- 3
    overidpre  <- 4
    overidpost <- 7
    normalize  <- 2

    outputs <- EventStudy(estimator = "OLS", data = example_data, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                          normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE)

    shiftvalues <- outputs$output$term

    normalization_column <- paste0("z", "_fd_lag", normalize)
    expect_equal(stringr::str_extract(normalization_column, "lag"), "lag")
    expect_true(!normalization_column %in% shiftvalues)
    expect_true(normalize > 0)
})

test_that("removes the correct column when normalize = - (pre + overidpre + 1)", {

    post       <- 3
    pre        <- 2
    overidpre  <- 1
    overidpost <- 4
    normalize  <- -4

    outputs <- EventStudy(estimator = "OLS", data = example_data, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                          normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE)

    shiftvalues <- outputs$output$term

    normalization_column <- paste0("z", "_lead", -1 * (normalize + 1))
    expect_equal(stringr::str_extract(normalization_column, "lead"), "lead")
    expect_true(!normalization_column %in% shiftvalues)
})

test_that("removes the correct column when normalize = post + overidpost", {

    post       <- 3
    pre        <- 2
    overidpre  <- 1
    overidpost <- 4
    normalize  <- 5

    outputs <- EventStudy(estimator = "OLS", data = example_data, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                          normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE)

    shiftvalues <- outputs$output$term

    normalization_column <- paste0("z", "_lag", normalize)
    expect_equal(stringr::str_extract(normalization_column, "lag"), "lag")
    expect_true(!normalization_column %in% shiftvalues)
})

# FHS ---------------------------------------------------------------------

test_that("correctly creates highest order leads and shiftvalues", {

    post       <- 2
    pre        <- 3
    overidpre  <- 4
    overidpost <- 11

    outputs <- suppressWarnings(
        EventStudy(estimator = "FHS", data = example_data, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost, normalize = - 1, cluster = TRUE, anticipation_effects_normalization = TRUE)
    )

    shiftvalues     <- outputs$output$term
    largest_fd_lag  <- as.double(stringr::str_extract(shiftvalues, "(?<=fd_lag)[0-9]+"))
    largest_fd_lead <- as.double(stringr::str_extract(shiftvalues, "(?<=fd_lead)[0-9]+"))
    largest_lag     <- as.double(stringr::str_extract(shiftvalues, "(?<=lag)[0-9]+"))
    largest_lead    <- as.double(stringr::str_extract(shiftvalues, "(?<=lead)[0-9]+"))

    expect_equal(max(largest_fd_lag, na.rm = TRUE), post + overidpost - 1)
    expect_equal(max(largest_fd_lead, na.rm = TRUE), pre + overidpre)
    expect_equal(max(largest_lag, na.rm = TRUE), post + overidpost)
    expect_equal(max(largest_lead, na.rm = TRUE), pre + overidpre)
})

test_that("correctly throws an error when normalized coefficient is outside event-study window", {

    post       <- 2
    pre        <- 3
    overidpre  <- 4
    overidpost <- 7
    normalize  <- 15

    expect_error(EventStudy(estimator = "FHS", data = example_data, outcomevar = "y_base",
                            policyvar = "z", idvar = "id", timevar = "t",
                            controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                            post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                            normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE))
})

test_that("throws an error when post + pre + overidpre + overidpost exceeds the data window", {

    post       <- 10
    pre        <- 15
    overidpre  <- 20
    overidpost <- 25
    normalize  <- 2

    expect_error(EventStudy(estimator = "FHS", data = example_data, outcomevar = "y_base",
                            policyvar = "z", idvar = "id", timevar = "t",
                            controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                            post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                            normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE))
})

test_that("removes the correct column when normalize < 0", {

    post       <- 2
    pre        <- 3
    overidpre  <- 4
    overidpost <- 7
    normalize  <- -2

    outputs <- EventStudy(estimator = "FHS", data = example_data, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                          normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE)

    shiftvalues <- outputs$output$term

    normalization_column <- paste0("z", "_fd_lead", (-1 * normalize))

    expect_equal(stringr::str_extract(normalization_column, "lead"), "lead")
    expect_true(!normalization_column %in% shiftvalues)
    expect_true(-1 * normalize > 0)

})

test_that("removes the correct column when normalize = 0", {

    post       <- 2
    pre        <- 3
    overidpre  <- 4
    overidpost <- 7
    normalize  <- 0

    outputs <- EventStudy(estimator = "FHS", data = example_data, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                          normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE)

    shiftvalues <- outputs$output$term

    normalization_column <- paste0("z", "_fd")
    expect_equal(stringr::str_extract(normalization_column, "fd"), "fd")
    expect_true(!normalization_column %in% shiftvalues)
    expect_true(normalize == 0)
})

test_that("FHS does not run when post, pre, overidpre, and overidpost are all 0", {

    post       <- 0
    pre        <- 0
    overidpre  <- 0
    overidpost <- 0
    normalize  <- -1

    expect_error(
        outputs <-
            EventStudy(estimator = "FHS", data = example_data, outcomevar = "y_base",
                       policyvar = "z", idvar = "id", timevar = "t",
                       controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                       post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                       normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE)
    )
})

test_that("removes the correct column when normalize > 0", {

    post       <- 2
    pre        <- 3
    overidpre  <- 4
    overidpost <- 7
    normalize  <- 2

    outputs <- EventStudy(estimator = "FHS", data = example_data, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                          normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE)

    shiftvalues <- outputs$output$term

    normalization_column <- paste0("z", "_fd_lag", normalize)
    expect_equal(stringr::str_extract(normalization_column, "lag"), "lag")
    expect_true(!normalization_column %in% shiftvalues)
    expect_true(normalize > 0)
})

test_that("removes the correct column when normalize = - (pre + overidpre + 1)", {

    post       <- 3
    pre        <- 2
    overidpre  <- 1
    overidpost <- 4
    normalize  <- -4

    outputs <- EventStudy(estimator = "FHS", data = example_data, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                          normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE)

    shiftvalues <- outputs$output$term

    normalization_column <- paste0("z", "_lead", -1 * (normalize + 1))
    expect_equal(stringr::str_extract(normalization_column, "lead"), "lead")
    expect_true(!normalization_column %in% shiftvalues)
})

test_that("removes the correct column when normalize = post + overidpost", {

    post       <- 3
    pre        <- 2
    overidpre  <- 1
    overidpost <- 4
    normalize  <- 5

    outputs <- EventStudy(estimator = "FHS", data = example_data, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE, "eta_m",
                          post = post, pre = pre, overidpre = overidpre, overidpost = overidpost,
                          normalize = normalize, cluster = TRUE, anticipation_effects_normalization = TRUE)

    shiftvalues <- outputs$output$term

    normalization_column <- paste0("z", "_lag", normalize)
    expect_equal(stringr::str_extract(normalization_column, "lag"), "lag")
    expect_true(!normalization_column %in% shiftvalues)
})

test_that("proxyIV selection works", {

    expect_message(
        suppressWarnings(
            EventStudy(estimator = "FHS", data = example_data, outcomevar = "y_base", policyvar = "z", idvar = "id",
                   timevar = "t", controls = "x_r", proxy = "eta_m", FE = TRUE, TFE = TRUE, post = 2,
                   overidpost = 2, pre = 1, overidpre = 2, normalize = -1, cluster = TRUE, anticipation_effects_normalization = TRUE)
            ),
        "Defaulting to strongest lead of differenced policy variable: proxyIV = z_fd_lead3. To specify a different proxyIV use the proxyIV argument."
    )

    expect_message(
        suppressWarnings(
            EventStudy(estimator = "FHS", data = example_data, outcomevar = "y_base", policyvar = "z", idvar = "id",
                   timevar = "t", controls = "x_r", proxy = "eta_m", FE = TRUE, TFE = TRUE, post = 1,
                   overidpost = 2, pre = 2, overidpre = 2, normalize = -1, cluster = TRUE, anticipation_effects_normalization = TRUE)
            ),
        "Defaulting to strongest lead of differenced policy variable: proxyIV = z_fd_lead4. To specify a different proxyIV use the proxyIV argument."
    )

    expect_message(
        suppressWarnings(
            EventStudy(estimator = "FHS", data = example_data, outcomevar = "y_base", policyvar = "z", idvar = "id",
                   timevar = "t", controls = "x_r", proxy = "eta_m", FE = TRUE, TFE = TRUE, post = 1,
                   overidpost = 2, pre = 6, overidpre = 2, normalize = -1, cluster = TRUE, anticipation_effects_normalization = TRUE)
            ),
        "Defaulting to strongest lead of differenced policy variable: proxyIV = z_fd_lead5. To specify a different proxyIV use the proxyIV argument."
    )
})

test_that("warning with correct normalize and pre is thrown when anticpation effects are allowed and anticipation_effects_normalization is TRUE", {

    expect_warning(
        EventStudy(estimator = "OLS", data = example_data, outcomevar = "y_base",
               policyvar = "z", idvar = "id", timevar = "t",
               controls = "x_r", FE = TRUE, TFE = TRUE,
               post = 1, pre = 1, overidpre = 4, overidpost = 5, normalize = - 1, cluster = TRUE, anticipation_effects_normalization = TRUE),
        paste("You allowed for anticipation effects 1 periods before the event, so the coefficient at -2 was selected to be normalized to zero.",
        "To override this, change anticipation_effects_normalization to FALSE.")
    )
})
