context("PrepareLeads")

test_that("correctly recognizes wrong variable type for grouping variable", {
    df_test <- read.csv("./input/df_test.csv")

    expect_error(PrepareLeads(df_test, country, "periods", "values", 2))
})
