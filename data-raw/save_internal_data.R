
library(dplyr)
library(haven)

indir  <- 'examples/source/raw/eventstudy_illustration_data/orig'

example_data <- read_dta(sprintf('%s/simulation_data_dynamic.dta', indir))

policyvar <- "z"
idvar <- "id"
timevar <- "t"

post <- 1
pre <- 1
overidpost <- 2
overidpre <- 2

df_first_diff <- GetFirstDifferences(df = example_data, timevar = "t", groupvar="id", diffvar = "z")

num_fd_lag_periods   <- post + overidpost - 1
num_fd_lead_periods  <- pre + overidpre

furthest_lag_period  <- num_fd_lag_periods + 1

df_first_diff_leads      <- PrepareLeads(df_first_diff, groupvar = idvar, timevar,
                                         leadvar = paste0(policyvar, "_fd"), leads = 1:num_fd_lead_periods)
df_first_diff_leads_lags <- PrepareLags(df_first_diff_leads, groupvar = idvar, timevar,
                                        lagvar = paste0(policyvar, "_fd"), lags = 1:num_fd_lag_periods)

df_lag           <- PrepareLags(df_first_diff_leads_lags, groupvar = idvar, timevar,
                                lagvar = policyvar, lags = furthest_lag_period)
df_lag_lead      <- PrepareLeads(df_lag, groupvar = idvar, timevar,
                                 leadvar = policyvar, leads = num_fd_lead_periods)

column_subtract_1              <- paste0(policyvar, "_lead", num_fd_lead_periods)
df_lag_lead[column_subtract_1] <- 1 - df_lag_lead[column_subtract_1]

v_OLS_variables <- c("id", "t", "z", "y_base", "x_r", "z_fd",
                 "z_fd_lead1", "z_fd_lead2", "z_fd_lead3",
                 "z_fd_lag1", "z_fd_lag2", "z_lag3","z_lead3")

v_FHS_variables <- c(v_OLS_variables, "eta_m")

df_EventStudyOLS_example <- df_lag_lead[v_OLS_variables]
df_EventStudyFHS_example <- df_lag_lead[v_FHS_variables]
df_sample_static  <- read_dta(sprintf('%s/simulation_data_static.dta', indir))

usethis::use_data(df_EventStudyOLS_example, df_EventStudyFHS_example, df_sample_static, overwrite = TRUE, version = 3, internal = TRUE)
