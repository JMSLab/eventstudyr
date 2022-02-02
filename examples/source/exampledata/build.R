library(dplyr)
library(haven)

indir  <- 'examples/source/raw/eventstudy_illustration_data/orig'
outdir <- 'examples/output/exampledata'

df_dynamic <- read_dta(sprintf('%s/simulation_data_dynamic.dta', indir))
saveRDS(df_dynamic, sprintf('%s/simulation_data_dynamic.RDS', outdir))

df_static  <- read_dta(sprintf('%s/simulation_data_static.dta', indir))
saveRDS(df_static, sprintf('%s/simulation_data_static.RDS', outdir))

