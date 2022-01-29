test_that("correctly recognizes wrong variable type for grouping variable", {
    df_test <- read.csv("./input/df_test.csv")

    expect_error(PrepareLeads(df_test, country, "periods", "values", 2))
})

test_that("correctly recognizes wrong variable type for leads variable", {
    df_test <- read.csv("./input/df_test.csv")

    expect_error(PrepareLeads(df_test, country, "periods", "values", "2"))
})

test_that("correctly adds the desired number of leads when there is no groupvar", {
    df_test <- read.csv("./input/df_test.csv")

    expect_equal(ncol(PrepareLeads(df_test, timevar = "periods", leadvar = "values", leads = 2)),
                 ncol(df_test) + 1)
})

test_that("correctly adds the desired number of leads when there is a groupvar", {
    df_test <- read.csv("./input/df_test.csv")

    expect_equal(ncol(PrepareLeads(df_test, groupvar = "country", timevar = "periods", leadvar = "values", leads = 2)),
                 ncol(df_test) + 1)
})

test_that("correctly adds the desired number of leads when leads are specified as a vector", {
    df_test <- read.csv("./input/df_test.csv")

    expect_equal(ncol(PrepareLeads(df_test, groupvar = "country", timevar = "periods", leadvar = "values", c(1,3))),
                 ncol(df_test) + 2)
})  

test_that("leads are correctly taken when there is no groupvar", {
    df_test <- read.csv("./input/df_test.csv")
    
    df_leads <- PrepareLeads(df_test, timevar = "periods", leadvar = "values", leads = 1)
    lead1_package <- na.omit(dplyr::pull(df_leads, values_lead1))
    
    lead1_alternate <- na.omit(dplyr::lead(dplyr::pull(df_test, values)))
    
    expect_equal(lead1_package, lead1_alternate)
})

test_that("leads are correctly taken when there is a groupvar", {
    df_test <- read.csv("./input/df_test.csv")
    
    df_leads <- PrepareLeads(df_test, groupvar = "country", timevar = "periods", leadvar = "values", leads = 1)
    lead1_package <- dplyr::filter(df_leads, country == "B")
    lead1_package <- na.omit(dplyr::pull(lead1_package, values_lead1))
                                                                            
    lead1_alternate <- na.omit(dplyr::lead(dplyr::pull(dplyr::filter(df_test, country == "B"), values)))
    
    expect_equal(lead1_package, lead1_alternate)
})

