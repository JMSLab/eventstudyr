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

detect_holes <- function(dt, idvar, timevar) {
    holes_per_id <- dt[, .SD[!is.na(base::get(timevar))], by = c(idvar)
                        ][, list(holes = any(base::diff(base::get(timevar)) != 1)),
                        by = c(idvar)]

    return(any(holes_per_id$holes))
}

if (detect_holes(data, idvar, timevar)) {
    warning(paste0("Note: gaps of more than one unit in the time variable '", timevar, "' were detected. ",
                    "Treating these as gaps in the panel dimension."))
    timevar_holes <- TRUE
} else {
    timevar_holes <- FALSE
}

PrepareModelFormulaFEOLS <- function(outcomevar, str_policy_vars,
                                     controls = NULL, proxy = NULL, proxyIV = NULL,
                                     idvar = NULL, timevar = NULL, FE = FALSE, TFE = FALSE) {
    stopifnot(!is.null(idvar))
    stopifnot(!is.null(timevar))

    regressors <- c(str_policy_vars, controls)

    if (FE | TFE) {
        fes <- c()
        if (FE) {
            fes <- c(fes, idvar)
        }
        if (TFE) {
            fes <- c(fes, timevar)
        }

        formula_str <- paste(
            outcomevar,
            "~",
            paste(regressors, collapse = " + "),
            "|",
            paste(fes, collapse = " + ")
        )
        formula <- stats::as.formula(formula_str)
    } else {
        formula <- stats::reformulate(
            termlabels = regressors,
            response = outcomevar,
            intercept = TRUE
        )
    }
    return(formula)
}

EventStudyFEOLS <- function(formula, prepared_data,
                          idvar, timevar, FE, TFE, cluster) {

    cluster = ifelse(cluster, idvar, NULL)

    ols_output <- fixest::feols(
        fml = formula,
        data = prepared_data,
        cluster = cluster
    )
    return(ols_output)
}


output_feols <- EventStudy(
    estimator = 'feols',
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

output <- EventStudy(
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


EventStudyPlot(output)
EventStudyPlot(output_feols)

estimates = output
xtitle = "Event time"
ytitle = "Coefficient"
ybreaks = NULL
conf_level = .95
supt = .95
num_sim = 1000
add_mean = FALSE
pre_event_coeffs = TRUE
post_event_coeffs = TRUE
add_zero_line = TRUE
smpath = FALSE

