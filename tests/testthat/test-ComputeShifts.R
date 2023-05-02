test_that("correctly recognizes wrong variable types", {
    df_test <- read.csv("./input/df_test.csv")

    # Character variables
    expect_error(ComputeShifts(df_test, 1,        "periods", "values",  3, F, T))
    expect_error(ComputeShifts(df_test, "country", 1,        "values",  3, F, T))
    expect_error(ComputeShifts(df_test, "country", "periods", 1,        3, F, T))
    expect_error(ComputeShifts(df_test, "country", "periods", "policy", 3, F, T))

    # Logical variables
    expect_error(ComputeShifts(df_test, "country", "periods", "values", 3, "T", F))
    expect_error(ComputeShifts(df_test, "country", "periods", "values", 3, T, "T"))

    # Numeric variables
    expect_error(ComputeShifts(df_test, "country", "periods", "values", "2", F, T))
})

test_that("correctly recognizes variables missing from dataset", {
    df_test <- read.csv("./input/df_test.csv")

    # Character variables
    expect_error(ComputeShifts(df_test, "country", "periods", "policy", 3, F, T))
    expect_error(ComputeShifts(df_test, "state",  "periods", "values", 3, F, T))
})

test_that("correctly adds the desired number of shifts", {
    df_test <- read.csv("./input/df_test.csv")

    # Add 1 lead/lag
    expect_equal(ncol(ComputeShifts(df_test, idvar = "country", timevar = "periods",
                                    shiftvar = "values",
                                    shiftvalues = 2)),
                 ncol(df_test) + 1)
    expect_equal(ncol(ComputeShifts(df_test, idvar = "country", timevar = "periods",
                                    shiftvar = "values",
                                    shiftvalues = -2)),
                 ncol(df_test) + 1)

    # Add multiple leads/lags
    expect_equal(ncol(ComputeShifts(df_test, idvar = "country", timevar = "periods",
                                    shiftvar = "values",
                                    shiftvalues = 1:2)),
                 ncol(df_test) + 2)
    expect_equal(ncol(ComputeShifts(df_test, idvar = "country", timevar = "periods",
                                    shiftvar = "values",
                                    shiftvalues = -2:-1)),
                 ncol(df_test) + 2)
})

test_that("the columns added have correct suffixes", {
    df_test <- read.csv("./input/df_test.csv")

    df_lags <- ComputeShifts(df_test, idvar = "country", timevar = "periods",
                             shiftvar = "values",
                             shiftvalues = 1:2)

    v_newvars <- setdiff(colnames(df_lags), colnames(df_test))

    expect_true(all(grepl("_lag", v_newvars)))

    df_leads <- ComputeShifts(df_test, idvar = "country", timevar = "periods",
                              shiftvar = "values",
                              shiftvalues = -2:-1)

    v_newvars <- setdiff(colnames(df_leads), colnames(df_test))

    expect_true(all(grepl("_lead", v_newvars)))
})

test_that("lags are correctly taken when there is a groupvar", {
    df <- data.frame(
        id      = c(rep("A", 4), rep("B", 2), rep("C", 3)),
        time    = c(1, 2, 4, 5, 2, 3, 2, 3, 4),
        z       = c(10, 12, 13, 14, 8, 9, 10, 11, 12),
        z_lag1  = c(NA, 10, 12, 13, NA, 8, NA, 10, 11),
        z_lead1 = c(12, 13, 14, NA, 9, NA, 11, 12, NA)
    )

    df_shifts <- ComputeShifts(df[, c("id", "time", "z")],
                               idvar = "id", timevar = "time",
                               shiftvar = "z",
                               shiftvalues = c(-1, 1))

    expect_equal(df$z_lead1, df_shifts$z_lead1)
    expect_equal(df$z_lag1,  df_shifts$z_lag1)
})
