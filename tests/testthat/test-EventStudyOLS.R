
test_that("FE = TRUE,
           TFE = TRUE,
           cluster = TRUE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    estimator       <-  "OLS"
    outcomevar      <- "y_base"
    str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lead3", "z_fd_lag1", "z_fd_lag2")
    str_policy_lead <-"z_lead3"
    str_policy_lag  <- "z_lag3"
    controls        <- "x_r"

    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls)

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
        expect_true(reg$se_type %in% c("stata", "HC1"))

})

test_that("FE = FALSE,
           TFE = TRUE,
           cluster = TRUE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    estimator       <-  "OLS"
    outcomevar      <- "y_base"
    str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lead3", "z_fd_lag1", "z_fd_lag2")
    str_policy_lead <-"z_lead3"
    str_policy_lag  <- "z_lag3"
    controls        <- "x_r"

    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls)

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
    expect_true(reg$se_type %in% c("stata", "HC1"))

})

test_that("FE = TRUE,
           TFE = FALSE,
           cluster = TRUE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    estimator       <-  "OLS"
    outcomevar      <- "y_base"
    str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lead3", "z_fd_lag1", "z_fd_lag2")
    str_policy_lead <-"z_lead3"
    str_policy_lag  <- "z_lag3"
    controls        <- "x_r"

    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls)

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
    expect_true(reg$se_type %in% c("stata", "HC1"))

})

test_that("FE = FALSE,
           TFE = FALSE,
           cluster = TRUE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    estimator       <-  "OLS"
    outcomevar      <- "y_base"
    str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lead3", "z_fd_lag1", "z_fd_lag2")
    str_policy_lead <-"z_lead3"
    str_policy_lag  <- "z_lag3"
    controls        <- "x_r"

    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls)

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
    expect_true(reg$se_type %in% c("stata", "HC1"))

})

test_that("FE = TRUE,
           TFE = TRUE,
           cluster = FALSE works", {

    df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

    estimator       <-  "OLS"
    outcomevar      <- "y_base"
    str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lead3", "z_fd_lag1", "z_fd_lag2")
    str_policy_lead <-"z_lead3"
    str_policy_lag  <- "z_lag3"
    controls        <- "x_r"

    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls)

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
    str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lead3", "z_fd_lag1", "z_fd_lag2")
    str_policy_lead <-"z_lead3"
    str_policy_lag  <- "z_lag3"
    controls        <- "x_r"

    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls)

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
    str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lead3", "z_fd_lag1", "z_fd_lag2")
    str_policy_lead <-"z_lead3"
    str_policy_lag  <- "z_lag3"
    controls        <- "x_r"

    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls)

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
    str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lead3", "z_fd_lag1", "z_fd_lag2")
    str_policy_lead <-"z_lead3"
    str_policy_lag  <- "z_lag3"
    controls        <- "x_r"

    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls)

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
    str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lead3", "z_fd_lag1", "z_fd_lag2")
    str_policy_lead <-"z_lead3"
    str_policy_lag  <- "z_lag3"
    controls        <- "x_r"

    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls)

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

    df_test_STATA <- read.csv("./input/df_test_base_STATA_1_1_2_2.csv")

    epsilon <- 10e-7

    expect_equal(unname(reg$coefficients["z_fd"]), as.double(df_test_STATA[df_test_STATA["term"] == "zfd"][2]), tolerance = epsilon)
    expect_equal(unname(reg$coefficients["z_fd_lead2"]), as.double(df_test_STATA[df_test_STATA["term"] == "F2"][2]), tolerance = epsilon)
    expect_equal(unname(reg$coefficients["z_fd_lead3"]), as.double(df_test_STATA[df_test_STATA["term"] == "F3"][3]), tolerance = epsilon)
    expect_equal(unname(reg$coefficients["z_fd_lag1"]), as.double(df_test_STATA[df_test_STATA["term"] == "L1"][2]), tolerance = epsilon)
    expect_equal(unname(reg$coefficients["z_fd_lag2"]), as.double(df_test_STATA[df_test_STATA["term"] == "L2"][2]), tolerance = epsilon)
    expect_equal(unname(reg$coefficients["z_lead3"]), -1 * as.double(df_test_STATA[df_test_STATA["term"] == "F3"][4]), tolerance = epsilon)
    expect_equal(unname(reg$coefficients["z_lag3"]), as.double(df_test_STATA[df_test_STATA["term"] == "L3"][2]), tolerance = epsilon)
    expect_equal(unname(reg$coefficients["x_r"]), as.double(df_test_STATA[df_test_STATA["term"] == "x_r"][2]), tolerance = epsilon)

    expect_equal(unname(reg$std.error["z_fd"]), as.double(df_test_STATA[df_test_STATA["term"] == "zfd"][3]), tolerance = epsilon)
    expect_equal(unname(reg$std.error["z_fd_lead2"]), as.double(df_test_STATA[df_test_STATA["term"] == "F2"][3]), tolerance = epsilon)
    expect_equal(unname(reg$std.error["z_fd_lead3"]), as.double(df_test_STATA[df_test_STATA["term"] == "F3"][5]), tolerance = epsilon)
    expect_equal(unname(reg$std.error["z_fd_lag1"]), as.double(df_test_STATA[df_test_STATA["term"] == "L1"][3]), tolerance = epsilon)
    expect_equal(unname(reg$std.error["z_fd_lag2"]), as.double(df_test_STATA[df_test_STATA["term"] == "L2"][3]), tolerance = epsilon)
    expect_equal(unname(reg$std.error["z_lead3"]), as.double(df_test_STATA[df_test_STATA["term"] == "F3"][6]), tolerance = epsilon)
    expect_equal(unname(reg$std.error["z_lag3"]), as.double(df_test_STATA[df_test_STATA["term"] == "L3"][3]), tolerance = epsilon)
    expect_equal(unname(reg$std.error["x_r"]), as.double(df_test_STATA[df_test_STATA["term"] == "x_r"][3]), tolerance = epsilon * 10)




})
