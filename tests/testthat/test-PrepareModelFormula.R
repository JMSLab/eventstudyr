
test_that("outcomevar is the dependent variable for OLS", {

  estimator       <-  "OLS"
  outcomevar      <- "y_base"
  str_policy_fd   <-  c("z_fd_lead1", "z_fd_lead2", "z_fd_lead4",
                      "z_fd_lead5", "z_fd_lead6", "z_fd_lag1", "z_fd_lag2",
                      "z_fd_lag3", "z_fd_lag4", "z_fd_lag5", "z_fd_lag6", "z_fd_lag7")
  str_policy_lead <- "z_lead6"
  str_policy_lag  <- "z_lag8"
  controls        <- "x_r"

  reg             <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls)
  expect_equal(all.vars(reg)[1], outcomevar)
})

test_that("formula does not have an intercept for OLS", {

  estimator       <-  "OLS"
  outcomevar      <- "y_base"
  str_policy_fd   <-  c("z_fd_lead1", "z_fd_lead2", "z_fd_lead4",
                        "z_fd_lead5", "z_fd_lead6", "z_fd_lag1", "z_fd_lag2",
                        "z_fd_lag3", "z_fd_lag4", "z_fd_lag5", "z_fd_lag6", "z_fd_lag7")
  str_policy_lead <- "z_lead6"
  str_policy_lag  <- "z_lag8"
  controls        <- "x_r"

  reg             <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls)
  expect_equal(attr(terms(reg), "intercept"), 0)

})

test_that("str_policy_fd arguments are incorporated into formula for OLS", {

  estimator              <-  "OLS"
  outcomevar             <- "y_base"
  str_policy_fd          <-  c("z_fd_lead1", "z_fd_lead2", "z_fd_lead4",
                               "z_fd_lead5", "z_fd_lead6", "z_fd_lag1", "z_fd_lag2",
                               "z_fd_lag3", "z_fd_lag4", "z_fd_lag5", "z_fd_lag6", "z_fd_lag7")
  str_policy_lead        <- "z_lead6"
  str_policy_lag         <- "z_lag8"
  controls               <- "x_r"

  reg                    <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls)
  str_policy_fd_in_model <- str_policy_fd %in% attr(terms(reg), "term.labels")
  expect_equal(sum(str_policy_fd_in_model), length(str_policy_fd))
})

test_that("str_policy_lead arguments are incorporated into formula for OLS", {

  estimator                <-  "OLS"
  outcomevar               <- "y_base"
  str_policy_fd            <-  c("z_fd_lead1", "z_fd_lead2", "z_fd_lead4",
                                 "z_fd_lead5", "z_fd_lead6", "z_fd_lag1", "z_fd_lag2",
                                 "z_fd_lag3", "z_fd_lag4", "z_fd_lag5", "z_fd_lag6", "z_fd_lag7")
  str_policy_lead          <- "z_lead6"
  str_policy_lag           <- "z_lag8"
  controls                 <- "x_r"

  reg                      <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls)
  str_policy_lead_in_model <- str_policy_lead %in% attr(terms(reg), "term.labels")
  expect_equal(sum(str_policy_lead_in_model), length(str_policy_lead))
})

test_that("str_policy_lag arguments are incorporated into formula for OLS", {

  estimator               <-  "OLS"
  outcomevar              <- "y_base"
  str_policy_fd           <-  c("z_fd_lead1", "z_fd_lead2", "z_fd_lead4",
                                "z_fd_lead5", "z_fd_lead6", "z_fd_lag1", "z_fd_lag2",
                                "z_fd_lag3", "z_fd_lag4", "z_fd_lag5", "z_fd_lag6", "z_fd_lag7")
  str_policy_lead         <- "z_lead6"
  str_policy_lag          <- "z_lag8"
  controls                <- "x_r"

  reg                     <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls)
  str_policy_lag_in_model <- str_policy_lag %in% attr(terms(reg), "term.labels")
  expect_equal(sum(str_policy_lag_in_model), length(str_policy_lag))
})

test_that("controls arguments are incorporated into formula for OLS", {

  estimator         <-  "OLS"
  outcomevar        <- "y_base"
  str_policy_fd     <-  c("z_fd_lead1", "z_fd_lead2", "z_fd_lead4",
                          "z_fd_lead5", "z_fd_lead6", "z_fd_lag1", "z_fd_lag2",
                          "z_fd_lag3", "z_fd_lag4", "z_fd_lag5", "z_fd_lag6", "z_fd_lag7")
  str_policy_lead   <- "z_lead6"
  str_policy_lag    <- "z_lag8"
  controls          <- "x_r"

  reg               <- PrepareModelFormula(estimator, outcomevar, str_policy_fd, str_policy_lead, str_policy_lag, controls)
  controls_in_model <- controls %in% attr(terms(reg), "term.labels")
  expect_equal(sum(controls_in_model), length(controls))
})


