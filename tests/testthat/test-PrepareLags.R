test_that("correctly recognizes wrong variable type for grouping variable", {
    df_test <- read.csv("./input/df_test.csv")

    expect_error(PrepareLags(df_test, country, "periods", "values", 2))
})

test_that("correctly recognizes wrong variable type for lags variable", {
    df_test <- read.csv("./input/df_test.csv")

    expect_error(PrepareLags(df_test, "country", "periods", "values", "2"))
})

test_that("correctly adds the desired number of lags when there is no groupvar", {
    df_test <- read.csv("./input/df_test.csv")

    expect_equal(ncol(PrepareLags(df_test, timevar = "periods", lagvar = "values", lags = 2)),
                 ncol(df_test) + 1)
})

test_that("correctly adds the desired number of lags when there is a groupvar", {
    df_test <- read.csv("./input/df_test.csv")

    expect_equal(ncol(PrepareLags(df_test, groupvar = "country", timevar = "periods", lagvar = "values", lags = 2)),
                 ncol(df_test) + 1)
})

test_that("correctly adds the desired number of lags when lags are specified as a vector", {
    df_test <- read.csv("./input/df_test.csv")

    expect_equal(ncol(PrepareLags(df_test, groupvar = "country", timevar = "periods", lagvar = "values", c(1,3))),
                 ncol(df_test) + 2)
})  

test_that("the columns added have _lag suffix", {
    df_test <- read.csv("./input/df_test.csv")

    df_lags <- PrepareLags(df_test, groupvar = "country", timevar = "periods", lagvar = "values", c(1,3))
    
    v_newvars <- setdiff(colnames(df_lags), colnames(df_test))
    
    expect_true(all(grepl("_lag", v_newvars)))
})  

test_that("lags are correctly taken when there is no groupvar", {
    df_test <- read.csv("./input/df_test.csv")
    
    df_lags <- PrepareLags(df_test, timevar = "periods", lagvar = "values", lags = 1)
    v_lag1_package <- na.omit(dplyr::pull(df_lags, values_lag1))
    
    v_lag1_alternative <- na.omit(dplyr::lag(dplyr::pull(df_test, values)))
    
    expect_equal(v_lag1_package, v_lag1_alternative)
})

test_that("lags are correctly taken when there is a groupvar", {
    df_test <- read.csv("./input/df_test.csv")
    
    df_lags <- PrepareLags(df_test, groupvar = "country", timevar = "periods", lagvar = "values", lags = 1)
    v_lag1_package <- dplyr::filter(df_lags, country == "B")
    v_lag1_package <- na.omit(dplyr::pull(v_lag1_package, values_lag1))
                                                                            
    v_lag1_alternative <- na.omit(dplyr::lag(dplyr::pull(dplyr::filter(df_test, country == "B"), values)))
    
    expect_equal(v_lag1_package, v_lag1_alternative)
})

