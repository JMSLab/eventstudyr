test_that("FE = TRUE,
           TFE = TRUE,
           cluster = TRUE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    idvar   <- "id"
    timevar <- "t"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"

    FE      <- TRUE
    TFE     <- TRUE
    cluster <- TRUE

    event_study_formula <- PrepareModelFormulaFEOLS(outcomevar, str_policy_vars,
                                                    controls = controls,
                                                    idvar = idvar, timevar = timevar,
                                                    FE = FE, TFE = TFE)

    if (FE & TFE & cluster) {

        reg <- EventStudyFEOLS(
            formula = event_study_formula,
            prepared_data = df_test_EventStudyOLS,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

    expect_true(class(reg) == "fixest")
    expect_true(all(reg$fixef_vars == c(idvar, timevar)))
    expect_true(length(reg$fixef_sizes) >= 1)

})

test_that("FE = FALSE,
           TFE = TRUE,
           cluster = TRUE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    idvar   <- "id"
    timevar <- "t"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"

    FE      <- FALSE
    TFE     <- TRUE
    cluster <- TRUE

    event_study_formula <- PrepareModelFormulaFEOLS(outcomevar, str_policy_vars,
                                                    controls = controls,
                                                    idvar = idvar, timevar = timevar,
                                                    FE = FE, TFE = TFE)

    if ((!FE) & TFE & cluster) {

        reg <- EventStudyFEOLS(
            formula = event_study_formula,
            prepared_data = df_test_EventStudyOLS,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

    expect_true(class(reg) == "fixest")
    expect_true(reg$fixef_vars == timevar)
    expect_true(length(reg$fixef_sizes) >= 1)

})

test_that("FE = TRUE,
           TFE = FALSE,
           cluster = TRUE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    idvar   <- "id"
    timevar <- "t"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"

    FE      <- TRUE
    TFE     <- FALSE
    cluster <- TRUE

    event_study_formula <- PrepareModelFormulaFEOLS(outcomevar, str_policy_vars,
                                                    controls = controls,
                                                    idvar = idvar, timevar = timevar,
                                                    FE = FE, TFE = TFE)

    if (FE & (!TFE) & cluster) {

        reg <- EventStudyFEOLS(
            formula = event_study_formula,
            prepared_data = df_test_EventStudyOLS,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

    expect_true(class(reg) == "fixest")
    expect_true(reg$fixef_vars == idvar)
    expect_true(length(reg$fixef_sizes) >= 1)

})

test_that("FE = FALSE,
           TFE = FALSE,
           cluster = TRUE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    idvar   <- "id"
    timevar <- "t"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"

    FE      <- FALSE
    TFE     <- FALSE
    cluster <- TRUE

    event_study_formula <- PrepareModelFormulaFEOLS(outcomevar, str_policy_vars,
                                                    controls = controls,
                                                    idvar = idvar, timevar = timevar,
                                                    FE = FE, TFE = TFE)

    if  ((!FE) & (!TFE) & cluster) {

        reg <- EventStudyFEOLS(
            formula = event_study_formula,
            prepared_data = df_test_EventStudyOLS,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

    expect_true(class(reg) == "fixest")
    expect_true(is.null(reg$fixef_vars))

})

test_that("FE = TRUE,
           TFE = TRUE,
           cluster = FALSE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    idvar   <- "id"
    timevar <- "t"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"

    FE      <- TRUE
    TFE     <- TRUE
    cluster <- FALSE

    event_study_formula <- PrepareModelFormulaFEOLS(outcomevar, str_policy_vars,
                                                    controls = controls,
                                                    idvar = idvar, timevar = timevar,
                                                    FE = FE, TFE = TFE)

    if (FE & TFE & (!cluster)) {

        reg <- EventStudyFEOLS(
            formula = event_study_formula,
            prepared_data = df_test_EventStudyOLS,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

    expect_true(class(reg) == "fixest")
    expect_true(all(reg$fixef_vars == c(idvar, timevar)))
    expect_true(length(reg$fixef_sizes) >= 1)

})

test_that("FE = FALSE,
           TFE = TRUE,
           cluster = FALSE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    idvar   <- "id"
    timevar <- "t"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"

    FE      <- FALSE
    TFE     <- TRUE
    cluster <- FALSE

    event_study_formula <- PrepareModelFormulaFEOLS(outcomevar, str_policy_vars,
                                                    controls = controls,
                                                    idvar = idvar, timevar = timevar,
                                                    FE = FE, TFE = TFE)

    if ((!FE) & TFE & (!cluster)) {

        reg <- EventStudyFEOLS(
            formula = event_study_formula,
            prepared_data = df_test_EventStudyOLS,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

    expect_true(class(reg) == "fixest")
    expect_true(reg$fixef_vars == timevar)
    expect_true(length(reg$fixef_sizes) >= 1)

})

test_that("FE = TRUE,
           TFE = FALSE,
           cluster = FALSE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    idvar   <- "id"
    timevar <- "t"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"

    FE      <- TRUE
    TFE     <- FALSE
    cluster <- FALSE

    event_study_formula <- PrepareModelFormulaFEOLS(outcomevar, str_policy_vars,
                                                    controls = controls,
                                                    idvar = idvar, timevar = timevar,
                                                    FE = FE, TFE = TFE)

    if (FE & (!TFE) & (!cluster))  {

        reg <- EventStudyFEOLS(
            formula = event_study_formula,
            prepared_data = df_test_EventStudyOLS,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

    expect_true(class(reg) == "fixest")
    expect_true(reg$fixef_vars == idvar)
    expect_true(length(reg$fixef_sizes) >= 1)

})

test_that("FE = FALSE,
           TFE = FALSE,
           cluster = FALSE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    idvar   <- "id"
    timevar <- "t"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"

    FE      <- FALSE
    TFE     <- FALSE
    cluster <- FALSE

    event_study_formula <- PrepareModelFormulaFEOLS(outcomevar, str_policy_vars,
                                                    controls = controls,
                                                    idvar = idvar, timevar = timevar,
                                                    FE = FE, TFE = TFE)

    if ((!FE) & (!TFE) & (!cluster))  {

        reg <- EventStudyFEOLS(
            formula = event_study_formula,
            prepared_data = df_test_EventStudyOLS,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

    expect_true(class(reg) == "fixest")
    expect_true(is.null(reg$fixef_vars))

})

test_that("Coefficients and Standard Errors agree with STATA", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    idvar   <- "id"
    timevar <- "t"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"

    FE      <- TRUE
    TFE     <- TRUE
    cluster <- TRUE

    event_study_formula <- PrepareModelFormulaFEOLS(outcomevar, str_policy_vars,
                                                    controls = controls,
                                                    idvar = idvar, timevar = timevar,
                                                    FE = FE, TFE = TFE)

    reg <- EventStudyFEOLS(
        formula = event_study_formula,
        prepared_data = df_test_EventStudyOLS,
        idvar = idvar,
        timevar = timevar,
        FE = FE,
        TFE = TFE,
        cluster = cluster
    )

    df_test_STATA <- read.csv("./input/df_test_base_STATA.csv", col.names = c("term", "coef", "std_error"))

    # Get coefficients and standard errors
    coef_feols <- coef(reg)
    se_feols <- fixest::se(reg)

    epsilon <- 1e-6
    epsilon_se <- 2e-2

    # Define coefficient mappings: R_name -> STATA_term
    coef_mappings <- list(
        "z_fd" = "zfd",
        "z_fd_lead2" = "F2.zfd",
        "z_fd_lead3" = "F3.zfd",
        "z_fd_lag1" = "L.zfd",
        "z_fd_lag2" = "L2.zfd",
        "z_lead3" = "F3.z",
        "z_lag3" = "L3.z",
        "x_r" = "x_r"
    )

    # Test coefficients
    for (r_name in names(coef_mappings)) {
        stata_term <- coef_mappings[[r_name]]
        expected <- df_test_STATA[df_test_STATA["term"] == stata_term, "coef"]
        if (r_name == "z_lead3") expected <- -1 * expected  # STATA sign convention
        expect_equal(unname(coef_feols[r_name]), expected, tolerance = epsilon)
    }

    # Test standard errors
    for (r_name in names(coef_mappings)) {
        stata_term <- coef_mappings[[r_name]]
        expected <- df_test_STATA[df_test_STATA["term"] == stata_term, "std_error"]
        expect_equal(unname(se_feols[r_name]), expected, tolerance = epsilon_se)
    }

})
