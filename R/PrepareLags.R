#' Adds lags of a variable as new columns
#'
#' @param df Data frame that will be modified
#' @param groupvar Optional grouping variable.
#' @param timevar Variable indicating time periods.
#' @param lagvar Variable whose lags will be added.
#' @param lags Numeric vector specifying the lags to be computed.
#'
#' @seealso \link[data.table]{shift}
#'
#' @examples
#' \dontrun{
#' #Compute lags of minimum wage in a panel data of states
#' PrepareLags(df, groupvar = "state", timevar = "year", lagvar = "minwage", 1:5)
#' }
#' @import data.table
#' @export


PrepareLags <- function(df, groupvar = NULL, timevar, lagvar, lags) {
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

