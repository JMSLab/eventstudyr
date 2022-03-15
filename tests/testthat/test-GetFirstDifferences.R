test_that("correctly recognizes wrong variable type for grouping variable", {
    df_test <- read.csv("./input/df_test.csv")

    expect_error(GetFirstDifferences(df_test, country, "periods", "values"))
})

test_that("correctly recognizes wrong variable type for diffvar variable", {
    df_test <- read.csv("./input/df_test.csv")

    expect_error(GetFirstDifferences(df_test, "country", "periods", values))
})

test_that("correctly adds a column of first differences when there is no groupvar", {
    df_test <- read.csv("./input/df_test.csv")
    
    df_diff <- GetFirstDifferences(df_test, timevar = "periods", diffvar = "values")
    v_fd_package <- na.omit(dplyr::pull(df_diff, values_fd))
    
    v_fd_alternative <- dplyr::pull(df_test, values) - dplyr::lag(dplyr::pull(df_test, values))
    v_fd_alternative <- na.omit(v_fd_alternative)

    expect_equal(v_fd_package, v_fd_alternative)
})

test_that("correctly adds a column of first differences when there is a groupvar", {
    df_test <- read.csv("./input/df_test.csv")
    
    df_diff <- GetFirstDifferences(df_test, "country", "periods", "values")
    v_fd_package <- na.omit(dplyr::pull(dplyr::filter(df_diff, country == "B"), values_fd))
    
    df_group <- dplyr::filter(df_test, country == "B")
    v_fd_alternative <- dplyr::pull(df_group, values) - dplyr::lag(dplyr::pull(df_group, values))
    v_fd_alternative <- na.omit(v_fd_alternative)

    expect_equal(v_fd_package, v_fd_alternative)
})

test_that("a column with _fd suffix is added", {
    df_test <- read.csv("./input/df_test.csv")
    
    df_diff <- GetFirstDifferences(df_test, "country", "periods", "values")

    expect_true("values_fd" %in% colnames(df_diff))
})

