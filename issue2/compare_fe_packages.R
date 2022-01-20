
pacman::p_load(tidyverse,
               plm,
               estimatr,
               fixest,
               lmtest,
               rbenchmark,
               kableExtra)

PATH_TO_DATA <- "C:/Users/c1nhs01/Downloads/Burda-Harding.csv"

NUM_REPLICATIONS <- 100

df_raw <- read_csv(PATH_TO_DATA) %>% 
  mutate(
    YEAR_FIRM = (YEAR * 1000) + FIRM # unique year-firm combination for clustering
  ) %>% 
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



estimatr_stata_firm <- benchmark(estimatr::lm_robust(fe_formula, data = df_raw, fixed_effects = ~ FIRM + YEAR, se_type = "stata", clusters = FIRM), 
                            replications = NUM_REPLICATIONS)

estimatr_stata_year <- benchmark(estimatr::lm_robust(fe_formula, data = df_raw, fixed_effects = ~ FIRM + YEAR, se_type = "stata", clusters = YEAR), 
                                 replications = NUM_REPLICATIONS)

estimatr_stata_year_firm <- benchmark(estimatr::lm_robust(fe_formula, data = df_raw, fixed_effects = ~ FIRM + YEAR, se_type = "stata", clusters = YEAR_FIRM), 
                                 replications = NUM_REPLICATIONS)


fixest_firm <- benchmark(fixest::feols(fe_formula, data = df_raw, fixef = c("FIRM", "YEAR"), cluster = "FIRM"), 
                       replications = NUM_REPLICATIONS)

fixest_year <- benchmark(fixest::feols(fe_formula, data = df_raw, fixef = c("FIRM", "YEAR"), cluster = "YEAR"), 
                           replications = NUM_REPLICATIONS)

fixest_year_firm <- benchmark(fixest::feols(fe_formula, data = df_raw, fixef = c("FIRM", "YEAR"), cluster = "YEAR_FIRM"), 
                         replications = NUM_REPLICATIONS)

RunEstimatrCluster <- function(cluster) {
  
  string_cluster <- deparse(substitute(cluster))
  reg_table <- estimatr::lm_robust(fe_formula, data = df_raw, fixed_effects = ~ FIRM + YEAR, se_type = "stata", clusters = !!enquo(cluster)) %>% 
    tidy() %>% 
    mutate(
      package = "estimatr",
      se_type = "stata",
      cluster_level = string_cluster
    ) %>% 
    select(package, term, cluster_level, se_type, estimate, std.error, statistic, p.value, df)
  
  return(reg_table)
}

RunFixestCluster <- function(string_cluster) {
  
  reg <- fixest::feols(fe_formula, data = df_raw, fixef = c("FIRM", "YEAR"), cluster = string_cluster)
  
  df <- degrees_freedom(reg, type = "t")
  
  reg_table <- reg %>%
    tidy() %>% 
    mutate(
      package = "fixest",
      se_type = "clustered",
      df = df,
      cluster_level = string_cluster
    ) %>% 
    select(package, term, cluster_level, se_type, estimate, std.error, statistic, p.value, df)
  
  return(reg_table)
  
}


CalcAvgRuntime <- function(benchmark_data) {
  
  num_replications <- benchmark_data$replications
  elapsed_time <- benchmark_data$elapsed
  
  return(elapsed_time / num_replications)
  
}


`avg_run_time (s)` <- c(CalcAvgRuntime(estimatr_stata_firm),
                        CalcAvgRuntime(estimatr_stata_year),
                        CalcAvgRuntime(estimatr_stata_year_firm),
                        CalcAvgRuntime(fixest_firm), 
                        CalcAvgRuntime(fixest_year), 
                        CalcAvgRuntime(fixest_year_firm)) %>% 
  rep(., each = 3)

df_results <- bind_rows(
  RunEstimatrCluster(FIRM),
  RunEstimatrCluster(YEAR),
  RunEstimatrCluster(YEAR_FIRM),
  RunFixestCluster("FIRM"),
  RunFixestCluster("YEAR"),
  RunFixestCluster("YEAR_FIRM"),
) %>% 
  mutate(
    `avg_run_time (s)`
  )

kable(df_results, digits = c(0, 0, 0, 0, 4, 6, 4, 4, 0, 3), format = "simple", row.names = TRUE) 
















