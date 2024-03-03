#' Adds leads or lags of a variable, robustly to gaps in time variable, as new columns in a panel dataset
#'
#' @param df Data frame that will be modified.
#' @param idvar Character indicating column of units.
#' @param timevar Character indicating column of time periods.
#' @param shiftvar Character indicating column of variable that will be shifted.
#' @param shiftvalues Numeric vector specifying the leads/lags to be computed. For example, c(-1, 0, 1) will compute the lead, current, and lag values.
#' @param timevar_holes Logical indicating whether the panel contains gaps in the time variable. Defaults to FALSE.
#' @param return_df Logical indicating whether the function should return a data frame (TRUE) or data.table (FALSE). Defaults to TRUE.
#'
#' @return The passed dataset augmented with columns that reflect the desired shift values

#' @seealso [data.table::shift()]
#'
#' @examples
#' ComputeShifts(
#'     example_data,
#'     idvar = "id",
#'     timevar = "t",
#'     shiftvar = "z",
#'     shiftvalues = -2:2
#' )
#'
#'
#' @importFrom data.table setDT setorderv shift := CJ .SD
#' @keywords internal
#' @noRd

ComputeShifts <- function(df, idvar, timevar, shiftvar, shiftvalues,
                          timevar_holes = FALSE, return_df = TRUE) {
    if (! is.data.frame(df)) {
        stop("df should be a data frame.")
    }
    for (var in c(idvar, timevar, shiftvar)) {
        if ((! is.character(var))) {
            stop(paste0(var, " should be a character."))
        }
        if (! var %in% colnames(df)) {
            stop(paste0(var, " should be the name of a variable in the dataset."))
        }
    }
    if (! is.numeric(shiftvalues)) {
        stop("shiftvalues should be numeric.")
    }
    if (0 %in% shiftvalues & length(shiftvalues) == 1) {
        stop("shiftvalues must be different than 0 to compute leads/lags.")
    }
    if (! is.logical(timevar_holes)) {
        stop("timevar_holes should be logical.")
    }
    if (! is.logical(return_df)) {
        stop("return_df should be logical.")
    }

    data.table::setDT(df)
    data.table::setorderv(df, cols = c(idvar, timevar))

    lags  <-    shiftvalues[shiftvalues >  0]
    leads <- -1*shiftvalues[shiftvalues <= 0]

    compute_shifts <- function(dat, idvar, shiftvar, shiftvalues, lags, leads) {

        if (any(shiftvalues > 0)) {
            dat[, paste0(shiftvar, "_lag", lags) :=
                     data.table::shift(get(shiftvar), lags, type = "lag"),
                 by = c(idvar)]
        }
        if (any(shiftvalues <= 0)) {
            dat[, paste0(shiftvar, "_lead", leads) :=
                     data.table::shift(get(shiftvar), leads, type = "lead"),
                 by = c(idvar)]
        }
        return(dat)
    }

    if (!timevar_holes) {
        df = compute_shifts(df, idvar, shiftvar, shiftvalues, lags, leads)
    } else {
        ## Create dataset with all combinations to compute shifts
        all_combinations <- CJ(unique(df[[idvar]]),
                               min(df[[timevar]]):max(df[[timevar]]))
        setnames(all_combinations, new = c(idvar, timevar))

        vars_to_keep <- c(idvar, timevar, shiftvar)

        df_all <- merge(df[, .SD, .SDcols = vars_to_keep], all_combinations,
                        by = c(idvar, timevar), all = TRUE)

        df_all = compute_shifts(df_all, idvar, shiftvar, shiftvalues, lags, leads)

        ## Bring shifts back to the original dataset
        df <- merge(df, df_all,
                    by = c(idvar, timevar, shiftvar), all.x = TRUE)
    }

    if (return_df) {
        df <- as.data.frame(df)
    }

    return(df)
}
