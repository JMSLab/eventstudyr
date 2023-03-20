library(haven)

indir  <- 'examples/source/raw/eventstudy_illustration_data/orig'
outdir <- 'data'

df_sample_dynamic <- read_dta(sprintf('%s/simulation_data_dynamic.dta', indir))
save(df_sample_dynamic, file = sprintf('%s/df_sample_dynamic.RData', outdir))

df_sample_static  <- read_dta(sprintf('%s/simulation_data_static.dta', indir))
save(df_sample_static, file = sprintf('%s/df_sample_static.RData', outdir))

