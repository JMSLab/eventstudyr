#' Adds the mean of the outcome variable for unit-time pairs where the variable whose coefficient is normalized is non-zero.
#'
#' @param df Data frame containing the additional variables created for estimation.
#' @param normalization_column Character indicating variable whose coefficient will be normalized.
#' @param policyvar Character indicating column of policy variable z.
#' @param outcomevar Character indicating column of outcome variable y.
#' @param custom_scalar Scalar to be displayed along y = 0 line if the user doesn't want the mean computed by the package.
#' @keywords internal
#' @noRd

AddMeans <- function(df, normalization_column, policyvar, outcomevar,
                     custom_scalar = NULL) {

    if (! is.data.frame(df)) {stop("df should be a data frame.")}
    if (! is.character(outcomevar)) {stop("outcomevar should be a character.")}
    if (! is.character(policyvar)) {stop("policyvar should be a character.")}
    if (! is.character(normalization_column)) {stop("normalization_column should be a character.")}
    if (! (is.numeric(custom_scalar) | is.null(custom_scalar))) {stop("custom_scalar should be numeric.")}

    if (is.null(custom_scalar)) {
        if (grepl(paste0(policyvar, "_lead"), normalization_column)) {
            df_change <- df[get(normalization_column) == 0 & !is.na(get(normalization_column))]
        } else {
            df_change <- df[get(normalization_column) != 0 & !is.na(get(normalization_column))]
        }
        mean <- df_change[, mean(get(outcomevar), na.rm = TRUE)]
    } else {
        mean <- custom_scalar
    }

    return(mean)
}

