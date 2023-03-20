library(haven)

indir  <- 'examples/source/raw/eventstudy_illustration_data/orig'

df_sample_dynamic <- read_dta(sprintf('%s/simulation_data_dynamic.dta', indir))
usethis::use_data(df_sample_dynamic, overwrite = TRUE, version = 3)

df_sample_static  <- read_dta(sprintf('%s/simulation_data_static.dta', indir))
usethis::use_data(df_sample_static, overwrite = TRUE, version = 3)

