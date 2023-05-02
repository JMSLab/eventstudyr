
test_that("outcomevar is the dependent variable for OLS", {

  estimator       <-  "OLS"
  outcomevar      <- "y_base"
  str_policy_vars <- c("Z_lead2", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_lag2")

  fmla            <- PrepareModelFormula(estimator, outcomevar, str_policy_vars)
  expect_equal(all.vars(fmla)[1], outcomevar)
})

test_that("formula does not have an intercept for OLS", {

  estimator       <- "OLS"
  outcomevar      <- "y_base"
  str_policy_vars <- c("Z_lead2", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_lag2")

  fmla            <- PrepareModelFormula(estimator, outcomevar, str_policy_vars)
  expect_equal(attr(terms(fmla), "intercept"), 0)
})

test_that("vars in str_policy_vars argument are incorporated into formula for OLS", {

  estimator       <- "OLS"
  outcomevar      <- "y_base"
  str_policy_vars <- c("z_lead2", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_lag2")

  fmla            <- PrepareModelFormula(estimator, outcomevar, str_policy_vars)

  z_vars_mask <- grepl("z_fd|z_lead|z_lag", attr(terms(fmla), "term.labels"))
  expect_equal(sum(z_vars_mask), length(str_policy_vars))
})

test_that("control arguments are incorporated into formula for OLS", {

  estimator       <- "OLS"
  outcomevar      <- "y_base"
  str_policy_vars <- c("z_lead2", "z_fd_lead2", "z_fd", "z_fd_lag1", "z_lag2")
  ctrls           <- c("x1", "x2")

  fmla            <- PrepareModelFormula(estimator, outcomevar, str_policy_vars,
                                         controls = ctrls)
  controls_in_model <- attr(terms(fmla), "term.labels") %in% ctrls
  expect_equal(sum(controls_in_model), length(ctrls))
})

test_that("formula for IV regression is correct", {
    estimator         <-  "FHS"
    outcomevar        <-  "y_base"
    str_policy_fd     <-  c("z_fd", "z_fd_lead2", "z_fd_lead3", "z_fd_lag1", "z_fd_lag2", "z_fd_lag3")
    str_policy_lead   <-  "z_lead3"
    str_policy_lag    <-  "z_lag4"
    controls          <-  "x_r"
    proxy             <-  "eta_m"
    proxyIV           <-  "z_fd_lead3"

    fmla <- PrepareModelFormula(estimator, outcomevar,
                                str_policy_vars = c(str_policy_fd, str_policy_lead, str_policy_lag),
                                static = FALSE,
                                controls, proxy, proxyIV)

    expect_equal(class(fmla), "formula")
    expect_equal(deparse(fmla[[2]]), "y_base")
    expect_equal(sort(all.vars(fmla[[c(3,2)]])),
                 sort(c("z_fd", "z_fd_lead2", "z_fd_lag1", "z_fd_lag2", "z_fd_lag3", "z_lead3", "z_lag4", "x_r", "eta_m")))
    expect_equal(sort(all.vars(fmla[[c(3,3)]])),
                 sort(c("z_fd", "z_fd_lead2", "z_fd_lead3", "z_fd_lag1", "z_fd_lag2", "z_fd_lag3", "z_lead3", "z_lag4", "x_r")))
})

test_that("formula for static model is correct", {
    estimator       <- "OLS"
    outcomevar      <- "y_base"
    str_policy_vars <- "z"

    fmla <- PrepareModelFormula(estimator, outcomevar, str_policy_vars, static = TRUE)

    expect_equal(class(fmla), "formula")
    expect_equal(deparse(fmla[[2]]), "y_base")
    expect_equal(all.vars(fmla[[3]]), "z")
})
