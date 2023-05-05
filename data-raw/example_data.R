
library(haven)

indir  <- 'examples/source/raw/eventstudy_illustration_data/orig'

example_data <- read_dta(sprintf('%s/simulation_data_dynamic.dta', indir))
usethis::use_data(example_data, overwrite = TRUE, version = 3)
