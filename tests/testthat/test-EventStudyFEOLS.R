test_that("FHS coefficients and Standard Errors agree with base STATA", {

    bools <- c("TTT", "TFT", "FTF", "FTT", "FFF", "FFT")

    for (i in 1:length(bools)) {
        bool <- bools[i]
        estimator       <-  "FHS"
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

        formula <- PrepareModelFormula(estimator = "FHS", outcomevar, str_policy_vars,
                                       static = FALSE, controls = controls, proxy = proxy, proxyIV = proxyIV,
                                       kernel = "fixest", idvar = idvar, timevar = timevar,
                                       FE = FE, TFE = TFE)

        reg <- EventStudyFEOLS_FHS(formula, df_EventStudyFHS_example, idvar, timevar, FE, TFE, cluster)

        df_test_STATA <- read.csv("./input/df_test_base_STATA_FHS.csv")
        epsilon <- 10e-4

        coef_mappings <- list(
            "z_fd" = "z_fd",
            "z_fd_lead2" = "z_fd_lead2",
            "fit_eta_m" = "eta_m",
            "z_fd_lag1" = "z_fd_lag1",
            "z_fd_lag2" = "z_fd_lag2",
            "z_lead3" = "z_lead3",
            "z_lag3" = "z_lag3",
            "x_r" = "x_r"
        )

        # Test coefficients
        for (r_name in names(coef_mappings)) {
            stata_name <- coef_mappings[[r_name]]
            expected <- df_test_STATA[df_test_STATA[1] == stata_name,][[2*i]]
            if (r_name == "z_lead3") expected <- expected * (-1)  # STATA sign convention
            tolerance <- abs(expected) * epsilon
            expect_equal(unname(coef(reg)[r_name]), expected, tolerance = tolerance)
        }

        # Test standard errors
        for (r_name in names(coef_mappings)) {
            stata_name <- coef_mappings[[r_name]]
            expected <- df_test_STATA[df_test_STATA[1] == stata_name,][[2*i+1]]
            tolerance <- abs(expected) * epsilon
            expect_equal(unname(fixest::se(reg)[r_name]), expected, tolerance = tolerance)
        }
    }
})
