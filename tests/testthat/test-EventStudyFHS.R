
test_that("FE = TRUE,
           TFE = TRUE,
           cluster = TRUE works", {

    estimator       <-  "FHS"
    outcomevar      <- "y_base"
    str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lag1", "z_fd_lag2")
    str_policy_lead <-"z_lead3"
    str_policy_lag  <- "z_lag3"
    controls        <- "x_r"
    proxy           <- "eta_m"
    proxyIV         <- "z_fd_lead3"
    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls, proxy, proxyIV)

    idvar   <- "id"
    timevar <- "t"

    FE      <- TRUE
    TFE     <- TRUE
    cluster <- TRUE

    if (FE & TFE & cluster) {

        reg <- EventStudyFHS(
            prepared_model_formula = event_study_formula,
            prepared_data = df_EventStudyFHS_example,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

        expect_true(all.equal(reg$felevels$`get(idvar)`, as.character(unique(df_EventStudyFHS_example$id))))
        expect_equal(reg$nclusters, length(unique(df_EventStudyFHS_example$id)))
        expect_true(reg$se_type == "stata")

})

test_that("FE = FALSE,
           TFE = TRUE,
           cluster = TRUE works", {

    estimator       <-  "FHS"
    outcomevar      <- "y_base"
    str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lag1", "z_fd_lag2")
    str_policy_lead <-"z_lead3"
    str_policy_lag  <- "z_lag3"
    controls        <- "x_r"
    proxy           <- "eta_m"
    proxyIV         <- "z_fd_lead3"
    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls, proxy, proxyIV)

    idvar   <- "id"
    timevar <- "t"

    FE      <- FALSE
    TFE     <- TRUE
    cluster <- TRUE

    if ((!FE) & TFE & cluster) {

        reg <- EventStudyFHS(
            prepared_model_formula = event_study_formula,
            prepared_data = df_EventStudyFHS_example,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

    expect_equal(is.null(reg$felevels$`get(idvar)`), TRUE)
    expect_equal(reg$nclusters, length(unique(df_EventStudyFHS_example$id)))
    expect_true(reg$se_type == "stata")

})

test_that("FE = TRUE,
           TFE = FALSE,
           cluster = TRUE works", {

    estimator       <-  "FHS"
    outcomevar      <- "y_base"
    str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lag1", "z_fd_lag2")
    str_policy_lead <-"z_lead3"
    str_policy_lag  <- "z_lag3"
    controls        <- "x_r"
    proxy           <- "eta_m"
    proxyIV         <- "z_fd_lead3"
    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls, proxy, proxyIV)

    idvar   <- "id"
    timevar <- "t"

    FE      <- TRUE
    TFE     <- FALSE
    cluster <- TRUE

    if (FE & (!TFE) & cluster) {

        reg <- EventStudyFHS(
            prepared_model_formula = event_study_formula,
            prepared_data = df_EventStudyFHS_example,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

    expect_true(all.equal(reg$felevels$V1, as.character(unique(df_EventStudyFHS_example$id))))
    expect_true(is.null(reg$felevels$`get(timevar)`), TRUE)
    expect_equal(reg$nclusters, length(unique(df_EventStudyFHS_example$id)))
    expect_true(reg$se_type == "stata")

})

test_that("FE = FALSE,
           TFE = FALSE,
           cluster = TRUE works", {

    estimator       <-  "FHS"
    outcomevar      <- "y_base"
    str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lag1", "z_fd_lag2")
    str_policy_lead <-"z_lead3"
    str_policy_lag  <- "z_lag3"
    controls        <- "x_r"
    proxy           <- "eta_m"
    proxyIV         <- "z_fd_lead3"
    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls, proxy, proxyIV)

    idvar   <- "id"
    timevar <- "t"

    FE      <- FALSE
    TFE     <- FALSE
    cluster <- TRUE

    if  ((!FE) & (!TFE) & cluster) {

        reg <- EventStudyFHS(
            prepared_model_formula = event_study_formula,
            prepared_data = df_EventStudyFHS_example,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )
    }

    expect_true(is.null(reg$felevels$`get(idvar)`), TRUE)
    expect_true(is.null(reg$felevels$`get(timevar)`), TRUE)
    expect_equal(reg$nclusters, length(unique(df_EventStudyFHS_example$id)))
    expect_true(reg$se_type == "stata")

})

test_that("FE = TRUE,
           TFE = TRUE,
           cluster = FALSE does not work", {

    estimator       <-  "FHS"
    outcomevar      <- "y_base"
    str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lag1", "z_fd_lag2")
    str_policy_lead <-"z_lead3"
    str_policy_lag  <- "z_lag3"
    controls        <- "x_r"
    proxy           <- "eta_m"
    proxyIV         <- "z_fd_lead3"
    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls, proxy, proxyIV)

    idvar   <- "id"
    timevar <- "t"

    FE      <- TRUE
    TFE     <- TRUE
    cluster <- FALSE

    expect_error(
        if (FE & TFE & (!cluster)) {

            reg <- EventStudyFHS(
                prepared_model_formula = event_study_formula,
                prepared_data = df_EventStudyFHS_example,
                idvar = idvar,
                timevar = timevar,
                FE = FE,
                TFE = TFE,
                cluster = cluster
            )
        },
        "cluster=TRUE required when FE=TRUE."
    )
})

test_that("FE = FALSE,
           TFE = TRUE,
           cluster = FALSE works", {

    estimator       <-  "FHS"
    outcomevar      <- "y_base"
    str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lag1", "z_fd_lag2")
    str_policy_lead <-"z_lead3"
    str_policy_lag  <- "z_lag3"
    controls        <- "x_r"
    proxy           <- "eta_m"
    proxyIV         <- "z_fd_lead3"
    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls, proxy, proxyIV)

    idvar   <- "id"
    timevar <- "t"

    FE      <- FALSE
    TFE     <- TRUE
    cluster <- FALSE

    if ((!FE) & TFE & (!cluster)) {

        reg <- EventStudyFHS(
            prepared_model_formula = event_study_formula,
            prepared_data = df_EventStudyFHS_example,
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
           cluster = FALSE does not work", {

    estimator       <-  "FHS"
    outcomevar      <- "y_base"
    str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lag1", "z_fd_lag2")
    str_policy_lead <-"z_lead3"
    str_policy_lag  <- "z_lag3"
    controls        <- "x_r"
    proxy           <- "eta_m"
    proxyIV         <- "z_fd_lead3"
    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls, proxy, proxyIV)

    idvar   <- "id"
    timevar <- "t"

    FE      <- TRUE
    TFE     <- FALSE
    cluster <- FALSE

    expect_error(
        if (FE & (!TFE) & (!cluster))  {

            reg <- EventStudyFHS(
                prepared_model_formula = event_study_formula,
                prepared_data = df_EventStudyFHS_example,
                idvar = idvar,
                timevar = timevar,
                FE = FE,
                TFE = TFE,
                cluster = cluster
            )
        },
        "cluster=TRUE required when FE=TRUE."
    )
})

test_that("FE = FALSE,
           TFE = FALSE,
           cluster = FALSE works", {

    estimator       <- "FHS"
    outcomevar      <- "y_base"
    str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lag1", "z_fd_lag2")
    str_policy_lead <-"z_lead3"
    str_policy_lag  <- "z_lag3"
    controls        <- "x_r"
    proxy           <- "eta_m"
    proxyIV         <- "z_fd_lead3"
    event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls, proxy, proxyIV)

    idvar   <- "id"
    timevar <- "t"

    FE      <- FALSE
    TFE     <- FALSE
    cluster <- FALSE

    if ((!FE) & (!TFE) & (!cluster))  {

        reg <- EventStudyFHS(
            prepared_model_formula = event_study_formula,
            prepared_data = df_EventStudyFHS_example,
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

    bools <- c("TTT", "TFT", "FTF", "FTT", "FFF", "FFT")

    for (i in length(bools)) {
        bool <- bools[i]
        estimator       <-  "FHS"
        outcomevar      <- "y_base"
        str_policy_fd   <- c("z_fd", "z_fd_lead2", "z_fd_lag1", "z_fd_lag2")
        str_policy_lead <-"z_lead3"
        str_policy_lag  <- "z_lag3"
        controls        <- "x_r"
        proxy           <- "eta_m"
        proxyIV         <- "z_fd_lead3"
        event_study_formula <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls, proxy, proxyIV)

        idvar   <- "id"
        timevar <- "t"

        FE <- as.logical(substring(bool, 1, 1))
        TFE <- as.logical(substring(bool, 2, 2))
        cluster <- as.logical(substring(bool, 3, 3))

        reg <- EventStudyFHS(
            prepared_model_formula = event_study_formula,
            prepared_data = df_EventStudyFHS_example,
            idvar = idvar,
            timevar = timevar,
            FE = FE,
            TFE = TFE,
            cluster = cluster
        )

        df_test_STATA <- read.csv("./input/df_test_base_STATA_FHS.csv")
        epsilon <- 10e-6

        expect_equal(unname(reg$coefficients["z_fd"]),       df_test_STATA[df_test_STATA[1] ==         "z_fd",][[2*i]],      tolerance = epsilon)
        expect_equal(unname(reg$coefficients["z_fd_lead2"]), df_test_STATA[df_test_STATA[1] ==   "z_fd_lead2",][[2*i]],      tolerance = epsilon)
        expect_equal(unname(reg$coefficients["eta_m"]),      df_test_STATA[df_test_STATA[1] ==        "eta_m",][[2*i]],      tolerance = epsilon)
        expect_equal(unname(reg$coefficients["z_fd_lag1"]),  df_test_STATA[df_test_STATA[1] ==    "z_fd_lag1",][[2*i]],      tolerance = epsilon)
        expect_equal(unname(reg$coefficients["z_fd_lag2"]),  df_test_STATA[df_test_STATA[1] ==    "z_fd_lag2",][[2*i]],      tolerance = epsilon)
        expect_equal(unname(reg$coefficients["z_lead3"]),    df_test_STATA[df_test_STATA[1] ==      "z_lead3",][[2*i]]*(-1), tolerance = epsilon)
        expect_equal(unname(reg$coefficients["z_lag3"]),     df_test_STATA[df_test_STATA[1] ==       "z_lag3",][[2*i]],      tolerance = epsilon)
        expect_equal(unname(reg$coefficients["x_r"]),        df_test_STATA[df_test_STATA[1] ==          "x_r",][[2*i]],      tolerance = epsilon)

        expect_equal(unname(reg$std.error["z_fd"]),          df_test_STATA[df_test_STATA[1] ==       "z_fd",][[2*i+1]],      tolerance = epsilon)
        expect_equal(unname(reg$std.error["z_fd_lead2"]),    df_test_STATA[df_test_STATA[1] == "z_fd_lead2",][[2*i+1]],      tolerance = epsilon)
        expect_equal(unname(reg$std.error["eta_m"]),         df_test_STATA[df_test_STATA[1] ==      "eta_m",][[2*i+1]],      tolerance = epsilon)
        expect_equal(unname(reg$std.error["z_fd_lag1"]),     df_test_STATA[df_test_STATA[1] ==  "z_fd_lag1",][[2*i+1]],      tolerance = epsilon)
        expect_equal(unname(reg$std.error["z_fd_lag2"]),     df_test_STATA[df_test_STATA[1] ==  "z_fd_lag2",][[2*i+1]],      tolerance = epsilon)
        expect_equal(unname(reg$std.error["z_lead3"]),       df_test_STATA[df_test_STATA[1] ==    "z_lead3",][[2*i+1]],      tolerance = epsilon)
        expect_equal(unname(reg$std.error["z_lag3"]),        df_test_STATA[df_test_STATA[1] ==     "z_lag3",][[2*i+1]],      tolerance = epsilon)
        expect_equal(unname(reg$std.error["x_r"]),           df_test_STATA[df_test_STATA[1] ==        "x_r",][[2*i+1]],      tolerance = epsilon)
    }
})

