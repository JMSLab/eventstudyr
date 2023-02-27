#' Adds first difference of a variable as a new column
#'
#' @param df Data frame that will be modified
#' @param groupvar Optional grouping variable, should be a character.
#' @param timevar Variable indicating time periods, should be a character.
#' @param diffvar Variable whose first difference will be taken, should be a character.
#'
#' @seealso \link[data.table]{shift}  # Not sure what this is referencing? The link doesn't show up - MZW
#'
#' @importFrom data.table setDT setorderv shift :=
#'
#' @keywords internal
#' @noRd

GetFirstDifferences <- function(df, groupvar = NULL, timevar, diffvar) {
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

