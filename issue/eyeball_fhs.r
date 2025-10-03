library(haven)
library(data.table)
library(tidyverse)
devtools::load_all()

indir  <- 'examples/source/raw/eventstudy_illustration_data/orig'
data <- read_dta(sprintf('%s/simulation_data_dynamic.dta', indir)) |> as.data.table()
outcomevar <- "y_base"
policyvar <- "z"
idvar <- "id"
timevar <- "t"
controls <- "x_r"
proxy <- "eta_m"
proxyIV <- NULL
FE <- TRUE
TFE <- TRUE
post <- 2
pre <- 2
overidpost <- 1
overidpre <- post + pre
normalize <- -1 * (pre + 1)
cluster <- TRUE
anticipation_effects_normalization <- TRUE
allow_duplicate_id <- FALSE
avoid_internal_copy <- FALSE


output_feols_fhs <- EventStudy(
    estimator = 'feols_FHS',
    data,
    outcomevar,
    policyvar,
    idvar,
    timevar,
    controls = controls,
    proxy = proxy,
    proxyIV = proxyIV,
    FE = FE,
    TFE = TFE,
    post,
    overidpost = overidpost,
    pre = pre,
    overidpre = post + pre,
    normalize = -1 * (pre + 1),
    cluster = cluster,
    anticipation_effects_normalization = anticipation_effects_normalization,
    allow_duplicate_id = allow_duplicate_id,
    avoid_internal_copy = avoid_internal_copy
)

output_fhs <- EventStudy(
    estimator = 'FHS',
    data,
    outcomevar,
    policyvar,
    idvar,
    timevar,
    controls = controls,
    proxy = proxy,
    proxyIV = proxyIV,
    FE = FE,
    TFE = TFE,
    post,
    overidpost = overidpost,
    pre = pre,
    overidpre = post + pre,
    normalize = -1 * (pre + 1),
    cluster = cluster,
    anticipation_effects_normalization = anticipation_effects_normalization,
    allow_duplicate_id = allow_duplicate_id,
    avoid_internal_copy = avoid_internal_copy
)


EventStudyPlot(output_fhs)
EventStudyPlot(output_feols_fhs)
