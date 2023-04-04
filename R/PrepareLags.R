#' Adds lags of a variable as new columns
#'
#' @description `PrepareLags` adds past values of a variable for a given number of periods based upon the time variable.
#'
#' @param df Data frame that will be modified.
#' @param groupvar Character indicating column of optional grouping variable.
#' @param timevar Character indicating column of time periods.
#' @param lagvar Character indicating column of variable whose lags will be added.
#' @param lags Numeric vector specifying the lags to be computed.
#'
#' @return The passed data frame augmented with columns called lagvar_lagN, where lagvar is the value specified and N is the selected lag(s).
#'
#' @seealso \link[data.table]{shift}
#'
#' @examples
#' PrepareLags(
#'     df_sample_dynamic,
#'     groupvar = "id",
#'     timevar = "t",
#'     lagvar = "z",
#'     lags = 1:5
#' )
#'
#'
#' @importFrom data.table setDT setorderv shift :=
#' @keywords internal
#' @noRd

PrepareLags <- function(df, groupvar = NULL, timevar, lagvar, lags) {
    if (! is.data.frame(df)) {
        stop("df should be a data frame.")
    }
    if ((! is.null(groupvar)) & (! is.character(groupvar))) {
        stop("groupvar should be a character.")
    }
    if (! is.character(timevar)) {
        stop("timevar should be a character.")
    }
    if (! is.character(lagvar)) {
        stop("lagvar should be a character.")
    }
    if (! is.numeric(lags)) {
        stop("lags should be numeric.")
    }

    df <- data.table::setDT(df)

    if (is.null(groupvar)) {
        data.table::setorderv(df, cols = timevar)
        df <- df[, paste0(lagvar, "_lag", lags) :=
                     data.table::shift(get(lagvar), lags, type = "lag")]
    } else {
        data.table::setorderv(df, cols = c(groupvar, timevar))
        df <- df[, paste0(lagvar, "_lag", lags) :=
                     data.table::shift(get(lagvar), lags, type = "lag"),
                 by = groupvar]
    }

    df <- as.data.frame(df)
    return(df)
}

