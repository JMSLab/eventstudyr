test_that("correctly recognizes wrong variable type for grouping variable", {
    df_test <- read.csv("./input/df_test.csv")

    expect_error(GetFirstDifferences(df_test, country, "periods", "values", 2))
})

test_that("correctly recognizes wrong variable type for leads variable", {
    df_test <- read.csv("./input/df_test.csv")

    expect_error(GetFirstDifferences(df_test, "country", "periods", "values", "2"))
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

test_that("the columns added have _lead suffix", {
    df_test <- read.csv("./input/df_test.csv")

    df_leads <- PrepareLeads(df_test, groupvar = "country", timevar = "periods", leadvar = "values", c(1,3))
    
    v_newvars <- setdiff(colnames(df_leads), colnames(df_leads))
    
    expect_true(all(grepl("_lead", v_newvars)))
})  

test_that("leads are correctly taken when there is no groupvar", {
    df_test <- read.csv("./input/df_test.csv")
    
    df_leads <- PrepareLeads(df_test, timevar = "periods", leadvar = "values", leads = 1)
    v_lead1_package <- na.omit(dplyr::pull(df_leads, values_lead1))
    
    v_lead1_alternative <- na.omit(dplyr::lead(dplyr::pull(df_test, values)))
    
    expect_equal(v_lead1_package, v_lead1_alternative)
})

test_that("leads are correctly taken when there is a groupvar", {
    df_test <- read.csv("./input/df_test.csv")
    
    df_leads <- PrepareLeads(df_test, groupvar = "country", timevar = "periods", leadvar = "values", leads = 1)
    v_lead1_package <- dplyr::filter(df_leads, country == "B")
    v_lead1_package <- na.omit(dplyr::pull(v_lead1_package, values_lead1))
                                                                            
    v_lead1_alternative <- na.omit(dplyr::lead(dplyr::pull(dplyr::filter(df_test, country == "B"), values)))
    
    expect_equal(v_lead1_package, v_lead1_alternative)
})

