library(broom)

Main <- function() {
    INDIR      <- 'data'
    OUTDIR     <- 'issue2'
    load(file.path(INDIR, 'df_sample_dynamic.RData'))

    EstimateOLS(df_sample_dynamic, OUTDIR)
}

EstimateOLS <- function(df_sample_dynamic, OUTDIR) {
    list_estimates <- EventStudy("OLS", df_sample_dynamic, outcomevar = "y_base",
                                 policyvar = "z", idvar = "id", timevar = "t",
                                 M = 2, G = 0, LM = 2, LG = 5)
    df_estimates   <- tidy(list_estimates[[1]])
    data.table::fwrite(df_estimates, file.path(OUTDIR, 'coeffs_0_5_2_2.csv'))
}

Main()

