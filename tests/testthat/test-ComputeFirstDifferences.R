test_that("correctly recognizes wrong variable type for grouping variable", {
    df_test <- read.csv("./input/df_test.csv")

    expect_error(ComputeFirstDifferences(df_test, country, "periods", "values"))
})

test_that("correctly recognizes wrong variable type for diffvar variable", {
    df_test <- read.csv("./input/df_test.csv")

    expect_error(ComputeFirstDifferences(df_test, "country", "periods", values))
})

test_that("a column with _fd suffix is added", {
    df_test <- read.csv("./input/df_test.csv")

    df_diff <- ComputeFirstDifferences(df_test, "country", "periods", "values")

    expect_true("values_fd" %in% colnames(df_diff))
})

test_that("correctly computes differences with an unbalanced dataset", {
    df <- data.frame(
        id     = rep(c("A", "B"), each = 4),
        time   = rep(1:4, times = 2),
        policy = c(10, 12, 11, 13, 8, 9, 10, 11),
        policy_fd_expected = c(NA, 2, -1, 2, NA, 1, 1, 1)
    )

    df_out <- ComputeFirstDifferences(df, "id", "time", "policy", timevar_holes = FALSE)

    expect_identical(df_out$policy_fd, df$policy_fd_expected)
})

test_that("correctly computes differences with a dataset that has holes in time var", {
    df <- data.frame(
        id = c(rep("A", 4), rep("B", 2), rep("C", 3)),
        time = c(1, 2, 4, 5, 2, 3, 2, 3, 4),
        policy = c(10, 12, 13, 14, 8, 9, 10, 11, 12),
        policy_fd_expected = c(NA, 2, NA, 1, NA, 1, NA, 1, 1)
    )

    df_out <- ComputeFirstDifferences(df, "id", "time", "policy", 
                                      timevar_holes = TRUE)

    expect_identical(df_out$policy_fd, df$policy_fd_expected)
})
