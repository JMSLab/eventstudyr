## code to prepare `df_sample_dynamic` dataset goes here

library(haven)

indir  <- 'examples/source/raw/eventstudy_illustration_data/orig'

df_sample_dynamic <- read_dta(sprintf('%s/simulation_data_dynamic.dta', indir))
usethis::use_data(df_sample_dynamic, overwrite = TRUE, version = 3)
