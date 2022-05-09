#' Adds the mean of the outcome variable for unit-time pairs where the variable whose coefficient is normalized is non-zero.
#'
#' @param df Data frame containing the additional variables created for estimation.
#' @param normalization_column Variable whose coefficient will be normalized.
#' @param policyvar Policy variable.
#' @param outcomevar Outcome variable.
#' @param custom_scalar Scalar to be displayed along y = 0 line if the user doesn't want the mean computed by the package.


AddMeans <- function(df, normalization_column, policyvar, outcomevar,
                     custom_scalar = NULL) {
    if (is.null(custom_scalar)) {

        if (grepl(paste0(policyvar, "_lead"), normalization_column)) {
            df_change <- df[df[, normalization_column] == 0, ]
        }else {
            df_change <- df[df[, normalization_column] != 0, ]
        }
        mean <- mean(df_change[[outcomevar]], na.rm = T)

    } else {
        mean <- custom_scalar
    }

    return(mean)
}

