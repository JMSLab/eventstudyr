
df_test_EventStudyOLS <- read.csv("./input/df_test_EventStudyOLS.csv")

outcomevar      <- "z"
str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lead3", "z_fd_lag1", "z_fd_lag2")
str_policy_lead <-"z_lead3"
str_policy_lag  <- "z_lag3"
controls        <- "x_r"

event_study_formula <- PrepareModelFormula(outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls)

idvar   <- "id"
timevar <- "t"

test_that("FE = TRUE,
           TFE = TRUE,
           cluster = TRUE works", {

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
        expect_true(all.equal(reg$felevels$`get(timevar)`, as.character(unique(df_test_EventStudyOLS$t))))
        expect_equal(reg$nclusters, length(unique(df_test_EventStudyOLS$id)))
        expect_equal(reg$se_type, "stata")

})

test_that("FE = FALSE,
           TFE = TRUE,
           cluster = TRUE works", {

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
    expect_true(all.equal(reg$felevels$V1, as.character(unique(df_test_EventStudyOLS$t))))
    expect_equal(reg$nclusters, length(unique(df_test_EventStudyOLS$id)))
    expect_equal(reg$se_type, "stata")

})

test_that("FE = TRUE,
           TFE = FALSE,
           cluster = TRUE works", {

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
    expect_equal(reg$se_type, "stata")

})

test_that("FE = FALSE,
           TFE = FALSE,
           cluster = TRUE works", {

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
    expect_equal(reg$se_type, "stata")

})

test_that("FE = TRUE,
           TFE = TRUE,
           cluster = FALSE works", {

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
    expect_true(all.equal(reg$felevels$`get(timevar)`, as.character(unique(df_test_EventStudyOLS$t))))
    expect_true(is.null(reg$nclusters), TRUE)
    expect_equal(reg$se_type, "stata")

})

test_that("FE = FALSE,
           TFE = TRUE,
           cluster = FALSE works", {

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
    expect_true(all.equal(reg$felevels$V1, as.character(unique(df_test_EventStudyOLS$t))))
    expect_true(is.null(reg$nclusters), TRUE)
    expect_equal(reg$se_type, "stata")

})

test_that("FE = TRUE,
           TFE = FALSE,
           cluster = FALSE works", {

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
    expect_equal(reg$se_type, "stata")

})

test_that("FE = FALSE,
           TFE = FALSE,
           cluster = FALSE works", {

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
    expect_equal(reg$se_type, "stata")

})
