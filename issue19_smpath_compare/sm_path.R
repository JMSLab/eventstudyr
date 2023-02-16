remove(list=ls())
library(devtools)

setwd("..")
load_all()
setwd("issue19_smpath_compare/")

write.csv(df_sample_dynamic,
          "data_dynamic.csv", row.names = T)

