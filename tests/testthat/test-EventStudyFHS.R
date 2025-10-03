
test_that("FE = TRUE,
           TFE = TRUE,
           cluster = TRUE works", {

    estimator       <-  "FHS"
    outcomevar      <- "y_base"
    str_policy_vars <- c("z_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"
    proxy           <- "eta_m"
    proxyIV         <- "z_fd_lead3"
    event_study_formula <- PrepareModelFormula(estimator, outcomevar, 
                                               str_policy_vars, FALSE, controls, proxy, proxyIV)

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
    str_policy_vars <- c("z_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"
    proxy           <- "eta_m"
    proxyIV         <- "z_fd_lead3"
    event_study_formula <- PrepareModelFormula(estimator, outcomevar, 
                                               str_policy_vars, FALSE, controls, proxy, proxyIV)

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
    str_policy_vars <- c("z_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"
    proxy           <- "eta_m"
    proxyIV         <- "z_fd_lead3"
    event_study_formula <- PrepareModelFormula(estimator, outcomevar, 
                                               str_policy_vars, FALSE, controls, proxy, proxyIV)

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
    str_policy_vars <- c("z_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"
    proxy           <- "eta_m"
    proxyIV         <- "z_fd_lead3"
    event_study_formula <- PrepareModelFormula(estimator, outcomevar, 
                                               str_policy_vars, FALSE, controls, proxy, proxyIV)

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
    str_policy_vars <- c("z_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"
    proxy           <- "eta_m"
    proxyIV         <- "z_fd_lead3"
    event_study_formula <- PrepareModelFormula(estimator, outcomevar, 
                                               str_policy_vars, FALSE, controls, proxy, proxyIV)

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
    str_policy_vars <- c("z_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"
    proxy           <- "eta_m"
    proxyIV         <- "z_fd_lead3"
    event_study_formula <- PrepareModelFormula(estimator, outcomevar, 
                                               str_policy_vars, FALSE, controls, proxy, proxyIV)

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
    str_policy_vars <- c("z_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"
    proxy           <- "eta_m"
    proxyIV         <- "z_fd_lead3"
    event_study_formula <- PrepareModelFormula(estimator, outcomevar, 
                                               str_policy_vars, FALSE, controls, proxy, proxyIV)

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
    str_policy_vars <- c("z_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
    controls        <- "x_r"
    proxy           <- "eta_m"
    proxyIV         <- "z_fd_lead3"
    event_study_formula <- PrepareModelFormula(estimator, outcomevar, 
                                               str_policy_vars, FALSE, controls, proxy, proxyIV)

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


test_that("feols_FHS Coefficients and Standard Errors agree with base STATA", {

    bools <- c("TTT", "TFT", "FTF", "FTT", "FFF", "FFT")

    for (i in 1:length(bools)) {
        bool <- bools[i]
        estimator       <-  "feols_FHS"
        outcomevar      <- "y_base"
        str_policy_vars <- c("z_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
        controls        <- "x_r"
        proxy           <- "eta_m"
        proxyIV         <- "z_fd_lead3"

        idvar   <- "id"
        timevar <- "t"

        FE <- as.logical(substring(bool, 1, 1))
        TFE <- as.logical(substring(bool, 2, 2))
        cluster <- as.logical(substring(bool, 3, 3))

        # Prepare the model formula for feols_FHS
        formula <- PrepareModelFormulaFEOLS_FHS(outcomevar, str_policy_vars,
                                               controls, proxy, proxyIV,
                                               idvar, timevar, FE, TFE)

        reg <- EventStudyFEOLS_FHS(formula, df_EventStudyFHS_example, idvar, timevar, FE, TFE, cluster)

        df_test_STATA <- read.csv("./input/df_test_base_STATA_FHS.csv")
        epsilon <- 10e-6
        epsilon_se <- 10e-2  # More lenient tolerance for standard errors

        # For feols_FHS, the endogenous variable coefficient is "fit_eta_m" instead of "eta_m"
        # But STATA reports "eta_m", and from the comparison test we know fit_eta_m should equal eta_m

        expect_equal(unname(coef(reg)["z_fd"]),       df_test_STATA[df_test_STATA[1] ==         "z_fd",][[2*i]],      tolerance = epsilon)
        expect_equal(unname(coef(reg)["z_fd_lead2"]), df_test_STATA[df_test_STATA[1] ==   "z_fd_lead2",][[2*i]],      tolerance = epsilon)
        expect_equal(unname(coef(reg)["fit_eta_m"]),  df_test_STATA[df_test_STATA[1] ==        "eta_m",][[2*i]],      tolerance = epsilon)
        expect_equal(unname(coef(reg)["z_fd_lag1"]),  df_test_STATA[df_test_STATA[1] ==    "z_fd_lag1",][[2*i]],      tolerance = epsilon)
        expect_equal(unname(coef(reg)["z_fd_lag2"]),  df_test_STATA[df_test_STATA[1] ==    "z_fd_lag2",][[2*i]],      tolerance = epsilon)
        expect_equal(unname(coef(reg)["z_lead3"]),    df_test_STATA[df_test_STATA[1] ==      "z_lead3",][[2*i]]*(-1), tolerance = epsilon)
        expect_equal(unname(coef(reg)["z_lag3"]),     df_test_STATA[df_test_STATA[1] ==       "z_lag3",][[2*i]],      tolerance = epsilon)
        expect_equal(unname(coef(reg)["x_r"]),        df_test_STATA[df_test_STATA[1] ==          "x_r",][[2*i]],      tolerance = epsilon)

        expect_equal(unname(fixest::se(reg)["z_fd"]),          df_test_STATA[df_test_STATA[1] ==       "z_fd",][[2*i+1]],      tolerance = epsilon_se)
        expect_equal(unname(fixest::se(reg)["z_fd_lead2"]),    df_test_STATA[df_test_STATA[1] == "z_fd_lead2",][[2*i+1]],      tolerance = epsilon_se)
        expect_equal(unname(fixest::se(reg)["fit_eta_m"]),     df_test_STATA[df_test_STATA[1] ==      "eta_m",][[2*i+1]],      tolerance = epsilon_se)
        expect_equal(unname(fixest::se(reg)["z_fd_lag1"]),     df_test_STATA[df_test_STATA[1] ==  "z_fd_lag1",][[2*i+1]],      tolerance = epsilon_se)
        expect_equal(unname(fixest::se(reg)["z_fd_lag2"]),     df_test_STATA[df_test_STATA[1] ==  "z_fd_lag2",][[2*i+1]],      tolerance = epsilon_se)
        expect_equal(unname(fixest::se(reg)["z_lead3"]),       df_test_STATA[df_test_STATA[1] ==    "z_lead3",][[2*i+1]],      tolerance = epsilon_se)
        expect_equal(unname(fixest::se(reg)["z_lag3"]),        df_test_STATA[df_test_STATA[1] ==     "z_lag3",][[2*i+1]],      tolerance = epsilon_se)
        expect_equal(unname(fixest::se(reg)["x_r"]),           df_test_STATA[df_test_STATA[1] ==        "x_r",][[2*i+1]],      tolerance = epsilon_se)
    }
})

test_that("Coefficients and Standard Errors agree with base STATA", {

    bools <- c("TTT", "TFT", "FTF", "FTT", "FFF", "FFT")

    for (i in length(bools)) {
        bool <- bools[i]
        estimator       <-  "FHS"
        outcomevar      <- "y_base"
        str_policy_vars <- c("z_lead3", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_fd_lag2", "z_lag3")
        controls        <- "x_r"
        proxy           <- "eta_m"
        proxyIV         <- "z_fd_lead3"
        event_study_formula <- PrepareModelFormula(estimator, outcomevar, 
                                               str_policy_vars, FALSE, controls, proxy, proxyIV)

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

