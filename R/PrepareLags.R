#' Adds lags of a variable as new columns
#'
#' @param df Data frame that will be modified
#' @param groupvar Optional grouping variable.
#' @param timevar Variable indicating time periods.
#' @param leadvar Variable whose lags will be added.
#' @param nleads Number of lags to be added.
#'
#' @seealso \link[data.table]{shift}
#'
#' @examples
#' \dontrun{
#' #Compute lags of minimum wage in a panel data of states
#' PrepareLeads(df, groupvar = "state", timevar = "year", leadvar = "minwage", 5)
#' }
#'
#' @importFrom data.table setDT
#' @importFrom data.table shift
#' @importFrom data.table setorderv
#' @export


PrepareLags <- function(df, groupvar = NULL, timevar, leadvar, nlags) {
    df <- data.table::setDT(df)

    if (is.null(groupvar)) {
        print(timevar)
        data.table::setorderv(df_test, cols = timevar)
        print(data.table::is.data.table(df))
        df <- df[, paste0(leadvar, "_lag", 1L:nlags) :=
                     data.table::shift(get(leadvar), 1L:nlags, type = "lag")]
    } else {
        data.table::setorderv(df_test, cols = c(groupvar, timevar))
        df <- df[, paste0(leadvar, "_lag", 1L:nlags) :=
                     data.table::shift(get(leadvar), 1L:nlags, type = "lag"),
                 by = groupvar]
    }

    df <- as.data.frame(df)
    return(df)
}

