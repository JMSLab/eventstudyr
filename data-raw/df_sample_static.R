## code to prepare `df_sample_static` dataset goes here

library(haven)

indir  <- 'examples/source/raw/eventstudy_illustration_data/orig'
df_sample_static  <- read_dta(sprintf('%s/simulation_data_static.dta', indir))
usethis::use_data(df_sample_static, overwrite = TRUE)
