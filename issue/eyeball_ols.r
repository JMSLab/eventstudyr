library(haven)
library(data.table)
library(tidyverse)
devtools::load_all()

indir  <- 'examples/source/raw/eventstudy_illustration_data/orig'
data <- read_dta(sprintf('%s/simulation_data_dynamic.dta', indir)) |> as.data.table()
estimator <- "OLS"
outcomevar <- "y_base"
policyvar <- "z"
idvar <- "id"
timevar <- "t"
controls <- NULL
proxy <- NULL
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
kernel <- "estimatr"


estimates_feols <- EventStudy(
    estimator = 'OLS',
    kernel = 'fixest',
    data,
    outcomevar,
    policyvar,
    idvar,
    timevar,
    controls = NULL,
    proxy = NULL,
    proxyIV = NULL,
    FE = TRUE,
    TFE = TRUE,
    post,
    overidpost = 1,
    pre, overidpre = post + pre,
    normalize = -1 * (pre + 1),
    cluster = TRUE,
    anticipation_effects_normalization = TRUE,
    allow_duplicate_id = FALSE,
    avoid_internal_copy = FALSE
)

estimates <- EventStudy(
    estimator = 'OLS',
    data,
    outcomevar,
    policyvar,
    idvar,
    timevar,
    controls = NULL,
    proxy = NULL,
    proxyIV = NULL,
    FE = TRUE,
    TFE = TRUE,
    post,
    overidpost = 1,
    pre, overidpre = post + pre,
    normalize = -1 * (pre + 1),
    cluster = TRUE,
    anticipation_effects_normalization = TRUE,
    allow_duplicate_id = FALSE,
    avoid_internal_copy = FALSE
)


EventStudyPlot(estimates)
EventStudyPlot(estimates_feols)
