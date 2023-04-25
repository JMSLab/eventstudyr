#' Adds first difference of a variable as a new column
#'
#' @param df Data frame that will be modified
#' @param groupvar Character indicating column of optional grouping variable.
#' @param timevar Character indicating column of time periods.
#' @param diffvar Character indicating column of variable whose first difference will be taken.
#'
#' @seealso [data.table::shift()]
#'
#' @importFrom data.table setDT setorderv shift :=
#'
#' @keywords internal
#' @noRd

ComputeFirstDifferences <- function(df, groupvar = NULL, timevar, diffvar) {
    if (! is.data.frame(df)) {stop("df should be a data frame.")}
    if ((! is.null(groupvar)) & (! is.character(groupvar))) {stop("groupvar should be a character.")}
    if (! is.character(timevar)) {stop("timevar should be a character.")}
    if (! is.character(diffvar)) {stop("diffvar should be a character.")}

    df <- data.table::setDT(df)
    if (is.null(groupvar)) {
        data.table::setorderv(df, cols = timevar)
        df[,paste0(diffvar, "_fd") := get(diffvar) - data.table::shift((get(diffvar)))]
    } else {
        data.table::setorderv(df, cols = c(groupvar, timevar))
        df[,paste0(diffvar, "_fd") := get(diffvar) - data.table::shift((get(diffvar))), by = groupvar]
    }

    df <- as.data.frame(df)
    return(df)
}

