# xtevent y_base x_r, pol(z) proxy(eta_m) window(3) vce(robust)
#
# estimatr::iv_robust(formula = y_base ~ eta_m + x_r + z_lead3 + z_fd_lead2 + z_fd + z_fd_lag1 + z_fd_lag2 + z_fd_lag3 + z_lag4 | z_fd_lead3 + x_r + z_lead3 + z_fd_lead2 + z_fd + z_fd_lag1 + z_fd_lag2 + z_fd_lag3 + z_lag4,
#                     data = df,
#                     fixed_effects = ~id+t,
#                     se_type="classical")
library(magrittr)
library(dplyr)

data <- df_sample_dynamic %>% select(y_base, z, id, t, x_r, eta_m) %>% filter(id <= 12)
results <- EventStudy(estimator = "FHS",
                       data = data,
                       outcomevar = "y_base",
                       policyvar = "z",
                       idvar = "id",
                       timevar = "t",
                       controls = "x_r",
                       proxy = "eta_m",
                       proxyIV = "z_fd_lead3",
                       FE = TRUE,
                       TFE = TRUE,
                       post = 3,
                       overidpost = 1,
                       pre = 0,
                       overidpre = 3,
                       normalize = -1,
                       cluster = TRUE)

print(results[[1]])
data <- results[[2]]$data
model_formula <-  PrepareModelFormula(estimator = "FHS",
                                      outcomevar = "y_base",
                                      str_policy_fd = c("z_fd", "z_fd_lead2", "z_fd_lead3", "z_fd_lag1", "z_fd_lag2", "z_fd_lag3"),
                                      str_policy_lead = "z_lead3",
                                      str_policy_lag = "z_lag4",
                                      controls = "x_r",
                                      proxy = "eta_m",
                                      proxyIV = "z_fd_lead3")

results <- EventStudyFHS(prepared_model_formula = model_formula,
                         prepared_data = data,
                         idvar = "id",
                         timevar = "t",
                         FE = TRUE,
                         TFE = TRUE,
                         cluster = TRUE)
print(results)
