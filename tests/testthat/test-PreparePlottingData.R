# OLS
test_that("labels are unique", {


    list_EventStudy <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                  policyvar = "z", idvar = "id", timevar = "t",
                                  controls = "x_r", FE = TRUE, TFE = TRUE,
                                  post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_tidy_estimates <- estimatr::tidy(list_EventStudy[[1]])

    policyvar <- list_EventStudy[[2]]$policyvar
    post <- list_EventStudy[[2]]$post
    overidpost <- list_EventStudy[[2]]$overidpost
    pre <- list_EventStudy[[2]]$pre
    overidpre <- list_EventStudy[[2]]$overidpre
    normalization_column <- list_EventStudy[[2]]$normalization_column
    proxyIV <- list_EventStudy[[2]]$proxyIV

    df_PreparePlottingData <- PreparePlottingData(df_tidy_estimates, policyvar, post, overidpost, pre, overidpre, normalization_column, proxyIV)

    labels_actual <- as.character(df_PreparePlottingData$label)
    num_labels <- length(labels_actual)
    num_unique_labels <- length(unique(labels_actual))

    expect_equal(num_unique_labels, num_labels)


})

test_that("the correct labels are created", {


    list_EventStudy <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                  policyvar = "z", idvar = "id", timevar = "t",
                                  controls = "x_r", FE = TRUE, TFE = TRUE,
                                  post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_tidy_estimates <- estimatr::tidy(list_EventStudy[[1]])

    policyvar <- list_EventStudy[[2]]$policyvar
    post <- list_EventStudy[[2]]$post
    overidpost <- list_EventStudy[[2]]$overidpost
    pre <- list_EventStudy[[2]]$pre
    overidpre <- list_EventStudy[[2]]$overidpre
    normalization_column <- list_EventStudy[[2]]$normalization_column
    proxyIV <- list_EventStudy[[2]]$proxyIV

    df_PreparePlottingData <- PreparePlottingData(df_tidy_estimates, policyvar, post, overidpost, pre, overidpre, normalization_column, proxyIV)

    labels_actual <- as.character(df_PreparePlottingData$label)

    expect_equal(labels_actual, c("0", "1", "2", "3", "4", "5", "6", "7", "-1", "-2", "-4", "-5", "-6", "-7+", "8+",  "-3" ))

})

test_that("the labels are ordered correctly", {


    list_EventStudy <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                  policyvar = "z", idvar = "id", timevar = "t",
                                  controls = "x_r", FE = TRUE, TFE = TRUE,
                                  post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_tidy_estimates <- estimatr::tidy(list_EventStudy[[1]])

    policyvar <- list_EventStudy[[2]]$policyvar
    post <- list_EventStudy[[2]]$post
    overidpost <- list_EventStudy[[2]]$overidpost
    pre <- list_EventStudy[[2]]$pre
    overidpre <- list_EventStudy[[2]]$overidpre
    normalization_column <- list_EventStudy[[2]]$normalization_column
    proxyIV <- list_EventStudy[[2]]$proxyIV

    df_PreparePlottingData <- PreparePlottingData(df_tidy_estimates, policyvar, post, overidpost, pre, overidpre, normalization_column, proxyIV)

    levels_coefficients <- stringr::str_remove(levels(df_PreparePlottingData$label), "\\+")
    actual_levels_as_integer <- as.integer(levels_coefficients)

    expect_equal(actual_levels_as_integer, (-pre - overidpre - 1):(post + overidpost))

})

test_that("the control variable is removed", {


    list_EventStudy <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                  policyvar = "z", idvar = "id", timevar = "t",
                                  controls = "x_r", FE = TRUE, TFE = TRUE,
                                  post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_tidy_estimates <- estimatr::tidy(list_EventStudy[[1]])

    policyvar <- list_EventStudy[[2]]$policyvar
    post <- list_EventStudy[[2]]$post
    overidpost <- list_EventStudy[[2]]$overidpost
    pre <- list_EventStudy[[2]]$pre
    overidpre <- list_EventStudy[[2]]$overidpre
    normalization_column <- list_EventStudy[[2]]$normalization_column
    controls <- list_EventStudy[[2]]$controls
    proxyIV <- list_EventStudy[[2]]$proxyIV

    df_PreparePlottingData <- PreparePlottingData(df_tidy_estimates, policyvar, post, overidpost, pre, overidpre, normalization_column, proxyIV)

    terms_actual <- df_PreparePlottingData$term

    expect_true(!controls %in% terms_actual)

})

test_that("the largest lag label is correctly created", {


    list_EventStudy <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                    policyvar = "z", idvar = "id", timevar = "t",
                                    controls = "x_r", FE = TRUE, TFE = TRUE,
                                    post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_tidy_estimates <- estimatr::tidy(list_EventStudy[[1]])

    policyvar <- list_EventStudy[[2]]$policyvar
    post <- list_EventStudy[[2]]$post
    overidpost <- list_EventStudy[[2]]$overidpost
    pre <- list_EventStudy[[2]]$pre
    overidpre <- list_EventStudy[[2]]$overidpre
    normalization_column <- list_EventStudy[[2]]$normalization_column
    proxyIV <- list_EventStudy[[2]]$proxyIV

    df_PreparePlottingData <- PreparePlottingData(df_tidy_estimates, policyvar, post, overidpost, pre, overidpre, normalization_column, proxyIV)

    term_actual <- as.character(df_PreparePlottingData[df_PreparePlottingData["term"] == paste0(policyvar, "_lag", (post + overidpost)), ][["label"]])

    expect_equal(term_actual, "8+")


})

test_that("the largest lead label is correctly created", {


    list_EventStudy <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                  policyvar = "z", idvar = "id", timevar = "t",
                                  controls = "x_r", FE = TRUE, TFE = TRUE,
                                  post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_tidy_estimates <- estimatr::tidy(list_EventStudy[[1]])

    policyvar <- list_EventStudy[[2]]$policyvar
    post <- list_EventStudy[[2]]$post
    overidpost <- list_EventStudy[[2]]$overidpost
    pre <- list_EventStudy[[2]]$pre
    overidpre <- list_EventStudy[[2]]$overidpre
    normalization_column <- list_EventStudy[[2]]$normalization_column
    proxyIV <- list_EventStudy[[2]]$proxyIV

    df_PreparePlottingData <- PreparePlottingData(df_tidy_estimates, policyvar, post, overidpost, pre, overidpre, normalization_column, proxyIV)

    term_actual <- as.character(df_PreparePlottingData[df_PreparePlottingData["term"] == paste0(policyvar, "_lead", (pre + overidpre)), ][["label"]])

    expect_equal(term_actual, "-7+")


})

test_that("all columns besides 'term' and 'label' are 0 for the normalization column", {


    list_EventStudy <- EventStudy(estimator = "OLS", data = df_sample_dynamic, outcomevar = "y_base",
                                  policyvar = "z", idvar = "id", timevar = "t",
                                  controls = "x_r", FE = TRUE, TFE = TRUE,
                                  post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_tidy_estimates <- estimatr::tidy(list_EventStudy[[1]])

    policyvar <- list_EventStudy[[2]]$policyvar
    post <- list_EventStudy[[2]]$post
    overidpost <- list_EventStudy[[2]]$overidpost
    pre <- list_EventStudy[[2]]$pre
    overidpre <- list_EventStudy[[2]]$overidpre
    normalization_column <- list_EventStudy[[2]]$normalization_column
    proxyIV <- list_EventStudy[[2]]$proxyIV

    df_PreparePlottingData <- PreparePlottingData(df_tidy_estimates, policyvar, post, overidpost, pre, overidpre, normalization_column, proxyIV)

    all_columns <- names(df_PreparePlottingData)
    v_zero_column_names <- all_columns[all_columns != c("term", "label")]

    normalized_row <- df_PreparePlottingData[df_PreparePlottingData["term"] == normalization_column, v_zero_column_names]
    actual_row_total <- apply(normalized_row, MARGIN = 1, function(x) sum(as.integer(x)))

    expect_equal(unname(actual_row_total), 0)


})

# FHS

test_that("labels are unique", {


    list_EventStudy <- EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base",
                                  policyvar = "z", idvar = "id", timevar = "t",
                                  controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                                  post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_tidy_estimates <- estimatr::tidy(list_EventStudy[[1]])

    policyvar <- list_EventStudy[[2]]$policyvar
    post <- list_EventStudy[[2]]$post
    overidpost <- list_EventStudy[[2]]$overidpost
    pre <- list_EventStudy[[2]]$pre
    overidpre <- list_EventStudy[[2]]$overidpre
    normalization_column <- list_EventStudy[[2]]$normalization_column
    proxyIV <- list_EventStudy[[2]]$proxyIV

    df_PreparePlottingData <- PreparePlottingData(df_tidy_estimates, policyvar, post, overidpost, pre, overidpre, normalization_column, proxyIV)

    labels_actual <- as.character(df_PreparePlottingData$label)
    num_labels <- length(labels_actual)
    num_unique_labels <- length(unique(labels_actual))

    expect_equal(num_unique_labels, num_labels)


})

test_that("the correct labels are created", {


    list_EventStudy <- EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base",
                                  policyvar = "z", idvar = "id", timevar = "t",
                                  controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                                  post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_tidy_estimates <- estimatr::tidy(list_EventStudy[[1]])

    policyvar <- list_EventStudy[[2]]$policyvar
    post <- list_EventStudy[[2]]$post
    overidpost <- list_EventStudy[[2]]$overidpost
    pre <- list_EventStudy[[2]]$pre
    overidpre <- list_EventStudy[[2]]$overidpre
    normalization_column <- list_EventStudy[[2]]$normalization_column
    proxyIV <- list_EventStudy[[2]]$proxyIV

    df_PreparePlottingData <- PreparePlottingData(df_tidy_estimates, policyvar, post, overidpost, pre, overidpre, normalization_column, proxyIV)

    labels_actual <- as.character(df_PreparePlottingData$label)

    expect_equal(labels_actual, c("0", "1", "2", "3", "4", "5", "6", "7", "-1", "-2", "-4", "-6", "-7+", "8+",  "-3", "-5"))

})

test_that("the labels are ordered correctly", {


    list_EventStudy <- EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base",
                                  policyvar = "z", idvar = "id", timevar = "t",
                                  controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                                  post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_tidy_estimates <- estimatr::tidy(list_EventStudy[[1]])

    policyvar <- list_EventStudy[[2]]$policyvar
    post <- list_EventStudy[[2]]$post
    overidpost <- list_EventStudy[[2]]$overidpost
    pre <- list_EventStudy[[2]]$pre
    overidpre <- list_EventStudy[[2]]$overidpre
    normalization_column <- list_EventStudy[[2]]$normalization_column
    proxyIV <- list_EventStudy[[2]]$proxyIV

    df_PreparePlottingData <- PreparePlottingData(df_tidy_estimates, policyvar, post, overidpost, pre, overidpre, normalization_column, proxyIV)

    levels_coefficients <- stringr::str_remove(levels(df_PreparePlottingData$label), "\\+")
    actual_levels_as_integer <- as.integer(levels_coefficients)

    expect_equal(actual_levels_as_integer, (-pre - overidpre - 1):(post + overidpost))

})

test_that("the control variable is removed", {


    list_EventStudy <- EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base",
                                  policyvar = "z", idvar = "id", timevar = "t",
                                  controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                                  post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_tidy_estimates <- estimatr::tidy(list_EventStudy[[1]])

    policyvar <- list_EventStudy[[2]]$policyvar
    post <- list_EventStudy[[2]]$post
    overidpost <- list_EventStudy[[2]]$overidpost
    pre <- list_EventStudy[[2]]$pre
    overidpre <- list_EventStudy[[2]]$overidpre
    normalization_column <- list_EventStudy[[2]]$normalization_column
    controls <- list_EventStudy[[2]]$controls
    proxyIV <- list_EventStudy[[2]]$proxyIV

    df_PreparePlottingData <- PreparePlottingData(df_tidy_estimates, policyvar, post, overidpost, pre, overidpre, normalization_column, proxyIV)

    terms_actual <- df_PreparePlottingData$term

    expect_true(!controls %in% terms_actual)

})

test_that("the largest lag label is correctly created", {


    list_EventStudy <- EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base",
                                  policyvar = "z", idvar = "id", timevar = "t",
                                  controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                                  post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_tidy_estimates <- estimatr::tidy(list_EventStudy[[1]])

    policyvar <- list_EventStudy[[2]]$policyvar
    post <- list_EventStudy[[2]]$post
    overidpost <- list_EventStudy[[2]]$overidpost
    pre <- list_EventStudy[[2]]$pre
    overidpre <- list_EventStudy[[2]]$overidpre
    normalization_column <- list_EventStudy[[2]]$normalization_column
    proxyIV <- list_EventStudy[[2]]$proxyIV

    df_PreparePlottingData <- PreparePlottingData(df_tidy_estimates, policyvar, post, overidpost, pre, overidpre, normalization_column, proxyIV)

    term_actual <- as.character(df_PreparePlottingData[df_PreparePlottingData["term"] == paste0(policyvar, "_lag", (post + overidpost)), ][["label"]])

    expect_equal(term_actual, "8+")


})

test_that("the largest lead label is correctly created", {


    list_EventStudy <- EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base",
                                  policyvar = "z", idvar = "id", timevar = "t",
                                  controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                                  post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_tidy_estimates <- estimatr::tidy(list_EventStudy[[1]])

    policyvar <- list_EventStudy[[2]]$policyvar
    post <- list_EventStudy[[2]]$post
    overidpost <- list_EventStudy[[2]]$overidpost
    pre <- list_EventStudy[[2]]$pre
    overidpre <- list_EventStudy[[2]]$overidpre
    normalization_column <- list_EventStudy[[2]]$normalization_column
    proxyIV <- list_EventStudy[[2]]$proxyIV

    df_PreparePlottingData <- PreparePlottingData(df_tidy_estimates, policyvar, post, overidpost, pre, overidpre, normalization_column, proxyIV)

    term_actual <- as.character(df_PreparePlottingData[df_PreparePlottingData["term"] == paste0(policyvar, "_lead", (pre + overidpre)), ][["label"]])

    expect_equal(term_actual, "-7+")


})

test_that("all columns besides 'term' and 'label' are 0 for the normalization column", {


    list_EventStudy <- EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base",
                                  policyvar = "z", idvar = "id", timevar = "t",
                                  controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                                  post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_tidy_estimates <- estimatr::tidy(list_EventStudy[[1]])

    policyvar <- list_EventStudy[[2]]$policyvar
    post <- list_EventStudy[[2]]$post
    overidpost <- list_EventStudy[[2]]$overidpost
    pre <- list_EventStudy[[2]]$pre
    overidpre <- list_EventStudy[[2]]$overidpre
    normalization_column <- list_EventStudy[[2]]$normalization_column
    proxyIV <- list_EventStudy[[2]]$proxyIV

    df_PreparePlottingData <- PreparePlottingData(df_tidy_estimates, policyvar, post, overidpost, pre, overidpre, normalization_column, proxyIV)

    all_columns <- names(df_PreparePlottingData)
    v_zero_column_names <- all_columns[all_columns != c("term", "label")]

    normalized_row <- df_PreparePlottingData[df_PreparePlottingData["term"] == normalization_column, v_zero_column_names]
    actual_row_total <- apply(normalized_row, MARGIN = 1, function(x) sum(as.integer(x)))

    expect_equal(unname(actual_row_total), 0)


})

test_that("all columns besides 'term' and 'label' are 0 for the proxyIV column", {


    list_EventStudy <- EventStudy(estimator = "FHS", data = df_sample_dynamic, outcomevar = "y_base",
                                  policyvar = "z", idvar = "id", timevar = "t",
                                  controls = "x_r", FE = TRUE, TFE = TRUE, proxy = "eta_m",
                                  post = 3, pre = 2, overidpre = 4, overidpost = 5, normalize = - 3, cluster = TRUE)

    df_tidy_estimates <- estimatr::tidy(list_EventStudy[[1]])

    policyvar <- list_EventStudy[[2]]$policyvar
    post <- list_EventStudy[[2]]$post
    overidpost <- list_EventStudy[[2]]$overidpost
    pre <- list_EventStudy[[2]]$pre
    overidpre <- list_EventStudy[[2]]$overidpre
    normalization_column <- list_EventStudy[[2]]$normalization_column
    proxyIV <- list_EventStudy[[2]]$proxyIV

    df_PreparePlottingData <- PreparePlottingData(df_tidy_estimates, policyvar, post, overidpost, pre, overidpre, normalization_column, proxyIV)

    all_columns <- names(df_PreparePlottingData)
    v_zero_column_names <- all_columns[all_columns != c("term", "label")]

    proxyIV_row <- df_PreparePlottingData[df_PreparePlottingData["term"] == proxyIV, v_zero_column_names]
    actual_row_total <- apply(proxyIV_row, MARGIN = 1, function(x) sum(as.integer(x)))

    expect_equal(unname(actual_row_total), 0)


})
