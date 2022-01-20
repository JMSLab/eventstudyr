
pacman::p_load(tidyverse,
               plm,
               estimatr,
               fixest,
               lmtest,
               rbenchmark,
               kableExtra)

PATH_TO_DATA <- "C:/Users/c1nhs01/Downloads/Burda-Harding.csv"

df_raw <- read_csv(PATH_TO_DATA) %>% 
  glimpse()

str_response <- "LSALES1"

fe_formula <- df_raw %>% 
  select(PAT_ANY, LGMALSPI, LGMALSPT) %>% 
  names() %>% 
  reformulate(termlabels = ., response = str_response)

RunEstimatr <- function(se_method) {
  
  reg_table <- estimatr::lm_robust(fe_formula, data = df_raw, fixed_effects = ~ FIRM + YEAR, se_type = se_method) %>% 
    tidy() %>% 
    mutate(
      package = "estimatr",
      se_type = se_method
    ) %>% 
    select(package, term, se_type, estimate, std.error, statistic, p.value, df)
  
  return(reg_table)
  
}

RunFixest <- function(se_method) {
  
  reg <- fixest::feols(fe_formula, data = df_raw, fixef = c("FIRM", "YEAR"), vcov = se_method) 
  
  df <- degrees_freedom(reg, type = "t")
  
  reg_table <- reg %>%
    tidy() %>% 
    mutate(
      package = "fixest",
      se_type = se_method,
      df = df
    ) %>% 
    select(package, term, se_type, estimate, std.error, statistic, p.value, df)
  
  return(reg_table)
  
}

RunPlm <- function(se_method) {
  
  reg <- plm::plm(fe_formula, df_raw, model = "within", index = c("FIRM", "YEAR"))
  
  df <- reg$df.residual
  
  reg_table <- coeftest(reg, vcovHC(reg, type = se_method)) %>% 
    tidy() %>% 
    transmute(
      package = "plm",
      term, 
      se_type = se_method, 
      estimate,
      std.error, 
      statistic,
      p.value,
      df
    )
  
  return(reg_table)
  
}

estimatr_stata <- benchmark(estimatr::lm_robust(fe_formula, data = df_raw, fixed_effects = ~ FIRM + YEAR, se_type = "stata"), replications = 1000)
estimatr_HC0 <- benchmark(estimatr::lm_robust(fe_formula, data = df_raw, fixed_effects = ~ FIRM + YEAR, se_type = "HC0"), replications = 1000)

fixest_HC1<- benchmark(fixest::feols(fe_formula, data = df_raw, fixef = c("FIRM", "YEAR"), vcov = "HC1"), replications = 1000)
fixest_twoway <- benchmark(fixest::feols(fe_formula, data = df_raw, fixef = c("FIRM", "YEAR"), vcov = "twoway"), replications = 1000)

CalcAvgRuntime <- function(benchmark_data) {
  
  num_replications <- benchmark_data$replications
  elapsed_time <- benchmark_data$elapsed
  
  return(elapsed_time / num_replications)
  
}


`avg_run_time (s)` <- c(CalcAvgRuntime(estimatr_stata), 
                        CalcAvgRuntime(fixest_HC1), 
                        CalcAvgRuntime(estimatr_HC0), 
                        CalcAvgRuntime(fixest_twoway)) %>% 
  rep(., each = 3)

df_results <- bind_rows(
  RunEstimatr("stata"),
  RunFixest("HC1"),
  RunEstimatr("HC0"),
  RunFixest("twoway")
) %>% 
  mutate(
    `avg_run_time (s)`
  )

kable(df_results, digits = 4, format = "simple", row.names = TRUE) 
















