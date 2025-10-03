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

    expect_equal(unname(coef_feols["z_fd"]), df_test_STATA[df_test_STATA["term"] == "zfd",][[2]], tolerance = epsilon)
    expect_equal(unname(coef_feols["z_fd_lead2"]), df_test_STATA[df_test_STATA["term"] == "F2.zfd",][[2]], tolerance = epsilon)
    expect_equal(unname(coef_feols["z_fd_lead3"]), df_test_STATA[df_test_STATA["term"] == "F3.zfd",][["coef"]][1], tolerance = epsilon)
    expect_equal(unname(coef_feols["z_fd_lag1"]), df_test_STATA[df_test_STATA["term"] == "L.zfd",][[2]], tolerance = epsilon)
    expect_equal(unname(coef_feols["z_fd_lag2"]), df_test_STATA[df_test_STATA["term"] == "L2.zfd",][[2]], tolerance = epsilon)
    expect_equal(unname(coef_feols["z_lead3"]), -1 * df_test_STATA[df_test_STATA["term"] == "F3.z",][["coef"]], tolerance = epsilon)
    expect_equal(unname(coef_feols["z_lag3"]), df_test_STATA[df_test_STATA["term"] == "L3.z",][[2]], tolerance = epsilon)
    expect_equal(unname(coef_feols["x_r"]), df_test_STATA[df_test_STATA["term"] == "x_r",][[2]], tolerance = epsilon)

    expect_equal(unname(se_feols["z_fd"]), df_test_STATA[df_test_STATA["term"] == "zfd",][[3]], tolerance = 2e-2)
    expect_equal(unname(se_feols["z_fd_lead2"]), df_test_STATA[df_test_STATA["term"] == "F2.zfd",][[3]], tolerance = 2e-2)
    expect_equal(unname(se_feols["z_fd_lead3"]), df_test_STATA[df_test_STATA["term"] == "F3.zfd",][["std_error"]][1], tolerance = 2e-2)
    expect_equal(unname(se_feols["z_fd_lag1"]), df_test_STATA[df_test_STATA["term"] == "L.zfd",][[3]], tolerance = 2e-2)
    expect_equal(unname(se_feols["z_fd_lag2"]), df_test_STATA[df_test_STATA["term"] == "L2.zfd",][[3]], tolerance = 2e-2)
    expect_equal(unname(se_feols["z_lead3"]), df_test_STATA[df_test_STATA["term"] == "F3.z",][["std_error"]], tolerance = 2e-2)
    expect_equal(unname(se_feols["z_lag3"]), df_test_STATA[df_test_STATA["term"] == "L3.z",][[3]], tolerance = 2e-2)
    expect_equal(unname(se_feols["x_r"]), df_test_STATA[df_test_STATA["term"] == "x_r",][[3]], tolerance = 2e-2)

})
