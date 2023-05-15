
test_that("FE = TRUE,
           TFE = TRUE,
           cluster = TRUE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    estimator       <-  "OLS"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"

    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_vars,
                                               static = F, controls = controls)

    idvar   <- "id"
    timevar <- "t"

    FE      <- TRUE
    TFE     <- TRUE
    cluster <- TRUE

    if (FE & TFE & cluster) {

        reg <- EventStudyOLS(
            prepared_model_formula = event_study_formula,
            prepared_data = df_test_EventStudyOLS,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

        expect_true(all.equal(reg$felevels$`get(idvar)`, as.character(unique(df_test_EventStudyOLS$id))))
        expect_equal(reg$nclusters, length(unique(df_test_EventStudyOLS$id)))
        expect_true(reg$se_type == "stata")

})

test_that("FE = FALSE,
           TFE = TRUE,
           cluster = TRUE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    estimator       <-  "OLS"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"

    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_vars,
                                               static = F, controls = controls)

    idvar   <- "id"
    timevar <- "t"

    FE      <- FALSE
    TFE     <- TRUE
    cluster <- TRUE

    if ((!FE) & TFE & cluster) {

        reg <- EventStudyOLS(
            prepared_model_formula = event_study_formula,
            prepared_data = df_test_EventStudyOLS,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

    expect_equal(is.null(reg$felevels$`get(idvar)`), TRUE)
    expect_equal(reg$nclusters, length(unique(df_test_EventStudyOLS$id)))
    expect_true(reg$se_type == "stata")

})

test_that("FE = TRUE,
           TFE = FALSE,
           cluster = TRUE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    estimator       <-  "OLS"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"

    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_vars,
                                               static = F, controls = controls)

    idvar   <- "id"
    timevar <- "t"

    FE      <- TRUE
    TFE     <- FALSE
    cluster <- TRUE

    if (FE & (!TFE) & cluster) {

        reg <- EventStudyOLS(
            prepared_model_formula = event_study_formula,
            prepared_data = df_test_EventStudyOLS,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

    expect_true(all.equal(reg$felevels$V1, as.character(unique(df_test_EventStudyOLS$id))))
    expect_true(is.null(reg$felevels$`get(timevar)`), TRUE)
    expect_equal(reg$nclusters, length(unique(df_test_EventStudyOLS$id)))
    expect_true(reg$se_type == "stata")

})

test_that("FE = FALSE,
           TFE = FALSE,
           cluster = TRUE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    estimator       <-  "OLS"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"

    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_vars,
                                               static = F, controls = controls)

    idvar   <- "id"
    timevar <- "t"

    FE      <- FALSE
    TFE     <- FALSE
    cluster <- TRUE

    if  ((!FE) & (!TFE) & cluster) {

        reg <- EventStudyOLS(
            prepared_model_formula = event_study_formula,
            prepared_data = df_test_EventStudyOLS,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

    expect_true(is.null(reg$felevels$`get(idvar)`), TRUE)
    expect_true(is.null(reg$felevels$`get(timevar)`), TRUE)
    expect_equal(reg$nclusters, length(unique(df_test_EventStudyOLS$id)))
    expect_true(reg$se_type == "stata")

})

test_that("FE = TRUE,
           TFE = TRUE,
           cluster = FALSE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    estimator       <-  "OLS"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"

    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_vars,
                                               static = F, controls = controls)

    idvar   <- "id"
    timevar <- "t"

    FE      <- TRUE
    TFE     <- TRUE
    cluster <- FALSE

    if (FE & TFE & (!cluster)) {

        reg <- EventStudyOLS(
            prepared_model_formula = event_study_formula,
            prepared_data = df_test_EventStudyOLS,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

    expect_true(all.equal(reg$felevels$`get(idvar)`, as.character(unique(df_test_EventStudyOLS$id))))
    expect_true(is.null(reg$nclusters), TRUE)
    expect_true(reg$se_type %in% c("stata", "HC1"))

})

test_that("FE = FALSE,
           TFE = TRUE,
           cluster = FALSE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    estimator       <-  "OLS"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"

    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_vars,
                                               static = F, controls = controls)

    idvar   <- "id"
    timevar <- "t"

    FE      <- FALSE
    TFE     <- TRUE
    cluster <- FALSE

    if ((!FE) & TFE & (!cluster)) {

        reg <- EventStudyOLS(
            prepared_model_formula = event_study_formula,
            prepared_data = df_test_EventStudyOLS,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

    expect_true(is.null(reg$felevels$`get(idvar)`), TRUE)
    expect_true(is.null(reg$nclusters), TRUE)
    expect_true(reg$se_type %in% c("stata", "HC1"))

})

test_that("FE = TRUE,
           TFE = FALSE,
           cluster = FALSE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    estimator       <-  "OLS"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"

    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_vars,
                                               static = F, controls = controls)

    idvar   <- "id"
    timevar <- "t"

    FE      <- TRUE
    TFE     <- FALSE
    cluster <- FALSE

    if (FE & (!TFE) & (!cluster))  {

        reg <- EventStudyOLS(
            prepared_model_formula = event_study_formula,
            prepared_data = df_test_EventStudyOLS,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

    expect_true(all.equal(reg$felevels$V1, as.character(unique(df_test_EventStudyOLS$id))))
    expect_true(is.null(reg$felevels$`get(timevar)`), TRUE)
    expect_true(is.null(reg$nclusters), TRUE)
    expect_true(reg$se_type %in% c("stata", "HC1"))

})

test_that("FE = FALSE,
           TFE = FALSE,
           cluster = FALSE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    estimator       <- "OLS"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"

    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_vars,
                                               static = F, controls = controls)

    idvar   <- "id"
    timevar <- "t"

    FE      <- FALSE
    TFE     <- FALSE
    cluster <- FALSE

    if ((!FE) & (!TFE) & (!cluster))  {

        reg <- EventStudyOLS(
            prepared_model_formula = event_study_formula,
            prepared_data = df_test_EventStudyOLS,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

    expect_true(is.null(reg$felevels$`get(idvar)`), TRUE)
    expect_true(is.null(reg$felevels$`get(timevar)`), TRUE)
    expect_true(is.null(reg$nclusters), TRUE)
    expect_true(reg$se_type %in% c("stata", "HC1"))

})


test_that("Coefficients and Standard Errors agree with base STATA", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    estimator       <-  "OLS"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"

    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_vars,
                                               static = F, controls = controls)

    idvar   <- "id"
    timevar <- "t"

    FE      <- TRUE
    TFE     <- TRUE
    cluster <- TRUE

    reg <- EventStudyOLS(
            prepared_model_formula = event_study_formula,
            prepared_data = df_test_EventStudyOLS,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )

    df_test_STATA <- read.csv("./input/df_test_base_STATA.csv", col.names = c("term", "coef", "std_error"))

    epsilon <- 10e-7

    expect_equal(unname(reg$coefficients["z_fd"]), df_test_STATA[df_test_STATA["term"] == "zfd",][[2]], tolerance = epsilon)
    expect_equal(unname(reg$coefficients["z_fd_lead2"]), df_test_STATA[df_test_STATA["term"] == "F2.zfd",][[2]], tolerance = epsilon)
    expect_equal(unname(reg$coefficients["z_fd_lead3"]), df_test_STATA[df_test_STATA["term"] == "F3.zfd",][["coef"]][1], tolerance = epsilon)
    expect_equal(unname(reg$coefficients["z_fd_lag1"]), df_test_STATA[df_test_STATA["term"] == "L.zfd",][[2]], tolerance = epsilon)
    expect_equal(unname(reg$coefficients["z_fd_lag2"]), df_test_STATA[df_test_STATA["term"] == "L2.zfd",][[2]], tolerance = epsilon)
    expect_equal(unname(reg$coefficients["z_lead3"]), -1 * df_test_STATA[df_test_STATA["term"] == "F3.z",][["coef"]], tolerance = epsilon)
    expect_equal(unname(reg$coefficients["z_lag3"]), df_test_STATA[df_test_STATA["term"] == "L3.z",][[2]], tolerance = epsilon)
    expect_equal(unname(reg$coefficients["x_r"]), df_test_STATA[df_test_STATA["term"] == "x_r",][[2]], tolerance = epsilon)

    expect_equal(unname(reg$std.error["z_fd"]), df_test_STATA[df_test_STATA["term"] == "zfd",][[3]], tolerance = epsilon)
    expect_equal(unname(reg$std.error["z_fd_lead2"]), df_test_STATA[df_test_STATA["term"] == "F2.zfd",][[3]], tolerance = epsilon)
    expect_equal(unname(reg$std.error["z_fd_lead3"]), df_test_STATA[df_test_STATA["term"] == "F3.zfd",][["std_error"]][1], tolerance = epsilon)
    expect_equal(unname(reg$std.error["z_fd_lag1"]), df_test_STATA[df_test_STATA["term"] == "L.zfd",][[3]], tolerance = epsilon)
    expect_equal(unname(reg$std.error["z_fd_lag2"]),df_test_STATA[df_test_STATA["term"] == "L2.zfd",][[3]], tolerance = epsilon)
    expect_equal(unname(reg$std.error["z_lead3"]), df_test_STATA[df_test_STATA["term"] == "F3.z",][["std_error"]], tolerance = epsilon)
    expect_equal(unname(reg$std.error["z_lag3"]),df_test_STATA[df_test_STATA["term"] == "L3.z",][[3]], tolerance = epsilon)
    expect_equal(unname(reg$std.error["x_r"]), df_test_STATA[df_test_STATA["term"] == "x_r",][[3]], tolerance = epsilon * 10)
})
