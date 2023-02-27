#' Adds leads of a variable as new columns
#'
#' @param df Data frame that will be modified.
#' @param groupvar Optional grouping variable, should be a character.
#' @param timevar Variable indicating time periods, should be a character.
#' @param leadvar Variable whose leads will be added, should be a character.
#' @param leads Numeric vector specifying the leads to be computed.
#'
#' @seealso \link[data.table]{shift}
#'
#' @examples
#' PrepareLeads(
#'     df_sample_dynamic,
#'     groupvar = "id",
#'     timevar = "t",
#'     leadvar = "z",
#'     leads = 1:5
#' )
#'
#' @importFrom data.table setDT setorderv shift :=
#' @keywords internal
#' @noRd

PrepareLeads <- function(df, groupvar = NULL, timevar, leadvar, leads) {
    if (! is.data.frame(df)) {stop("df should be a data frame.")}
    if ((! is.null(groupvar)) & (! is.character(groupvar))) {stop("groupvar should be a character.")}
    if (! is.character(timevar)) {stop("timevar should be a character.")}
    if (! is.character(leadvar)) {stop("leadvar should be a character.")}
    if (! is.numeric(leads)) {stop("leads should be numeric.")}

    df <- data.table::setDT(df)

    if (is.null(groupvar)) {
        data.table::setorderv(df, cols = timevar)
        df <- df[, paste0(leadvar, "_lead", leads) :=
                 data.table::shift(get(leadvar), leads, type = "lead")]
    } else {
        data.table::setorderv(df, cols = c(groupvar, timevar))
        df <- df[, paste0(leadvar, "_lead", leads) :=
                 data.table::shift(get(leadvar), leads, type = "lead"),
                 by = groupvar]
    }

    df <- as.data.frame(df)
    return(df)
}

