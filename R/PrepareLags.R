#' Adds lags of a variable as new columns
#'
#' @param df Data frame that will be modified
#' @param groupvar Optional grouping variable, should be a character.
#' @param timevar Variable indicating time periods, should be a character.
#' @param lagvar Variable whose lags will be added, should be a character.
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
    if (! is.data.frame(df)) {stop("df should be a data frame.")}
    if ((! is.null(groupvar)) & (! is.character(groupvar))) {stop("groupvar should be a character.")}
    if (! is.character(timevar)) {stop("timevar should be a character.")}
    if (! is.character(lagvar)) {stop("lagvar should be a character.")}
    if (! is.numeric(lags)) {stop("lags should be numeric.")}
    
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

