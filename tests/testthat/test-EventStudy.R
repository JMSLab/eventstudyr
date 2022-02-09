
test_that("correctly creates highest order leads and lags", {

    M  <- 2
    G  <- 3
    LG <- 4
    LM <- 11

    outputs <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          M = M, G = G, LG = LG, LM = LM, normalize = - 1, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term
    largest_fd_lag  <- as.double(stringr::str_extract(leads_lags, "(?<=fd_lag)[0-9]+"))
    largest_fd_lead <- as.double(stringr::str_extract(leads_lags, "(?<=fd_lead)[0-9]+"))
    largest_lag     <- as.double(stringr::str_extract(leads_lags, "(?<=lag)[0-9]+"))
    largest_lead    <- as.double(stringr::str_extract(leads_lags, "(?<=lead)[0-9]+"))

    expect_equal(max(largest_fd_lag, na.rm = TRUE), M + LM - 1)
    expect_equal(max(largest_fd_lead, na.rm = TRUE), G + LG)
    expect_equal(max(largest_lag, na.rm = TRUE), M + LM)
    expect_equal(max(largest_lead, na.rm = TRUE), G + LG)


})

test_that("removes the correct column when normalize < 0", {

    M  <- 2
    G  <- 3
    LG <- 4
    LM <- 7
    normalize <- -2

    outputs <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          M = M, G = G, LG = LG, LM = LM, normalize = normalize, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term

    normalization_column <- paste0("z", "_fd_lead", (-1 * normalize))

    expect_equal(stringr::str_extract(normalization_column, "lead"), "lead")
    expect_true(!normalization_column %in% leads_lags)
    expect_true(-1 * normalize > 0)

})

test_that("removes the correct column when normalize = 0", {

    M  <- 2
    G  <- 3
    LG <- 4
    LM <- 7
    normalize <- 0

    outputs <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          M = M, G = G, LG = LG, LM = LM, normalize = normalize, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term

    normalization_column <- paste0("z", "_fd")
    expect_equal(stringr::str_extract(normalization_column, "fd"), "fd")
    expect_true(!normalization_column %in% leads_lags)
    expect_true(normalize == 0)

})

test_that("removes the correct column when normalize > 0", {

    M  <- 2
    G  <- 3
    LG <- 4
    LM <- 7
    normalize <- 2

    outputs <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                          policyvar = "z", idvar = "id", timevar = "t",
                          controls = "x_r", FE = TRUE, TFE = TRUE,
                          M = M, G = G, LG = LG, LM = LM, normalize = normalize, cluster = TRUE)

    leads_lags      <- outputs[[1]]$term

    normalization_column <- paste0("z", "_fd_lag")
    expect_equal(stringr::str_extract(normalization_column, "lag"), "lag")
    expect_true(!normalization_column %in% leads_lags)
    expect_true(normalize > 0)

})

test_that("subtraction is peformed on the correct column", {

    M  <- 1
    G  <- 1
    LG <- 2
    LM <- 2

    df_first_diff <- GetFirstDifferences(df = df_sample_static, groupvar = "id", timevar = "t", diffvar = "z")

    num_fd_lag_periods   <- M + LM - 1
    num_fd_lead_periods  <- G + LG

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
    expect_equal(column_subtract_degree, G + LG)

})

