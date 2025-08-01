#' Adds first differences of a variable, robustly to gaps in time variable, as new columns in a panel dataset
#'
#' @param dt Data frame that will be modified.
#' @param idvar Character indicating column of units.
#' @param timevar Character indicating column of time periods.
#' @param diffvar Character indicating column of variable whose first difference will be taken.
#' @param timevar_holes Logical indicating whether the panel contains gaps in the time variable. Defaults to FALSE.
#'
#' @return The passed dataset augmented with columns that reflect the desired first differences
#'
#' @seealso [data.table::shift()]
#'
#' @examples
#' ComputeFirstDifferences(
#'     example_data,
#'     idvar = "id",
#'     timevar = "t",
#'     diffvar = "z"
#' )
#'
#' @importFrom data.table setorderv setnames shift := CJ .SD
#'
#' @keywords internal
#' @noRd

ComputeFirstDifferences <- function(dt, idvar, timevar, diffvar,
                                    timevar_holes = FALSE) {
    if (! data.table::is.data.table(dt)) {
        stop("Input data should be a data.table.")
    }
    for (var in c(idvar, timevar, diffvar)) {
        if ((! is.character(var))) {
            stop(paste0(var, " should be a character."))
        }
        if (! var %in% colnames(dt)) {
            stop(paste0(var, " should be the name of a variable in the dataset."))
        }
    }
    if (! is.logical(timevar_holes)) {
        stop("timevar_holes should be logical.")
    }

    data.table::setorderv(dt, cols = c(idvar, timevar))

    if (!timevar_holes) {
        dt[, paste0(diffvar, "_fd") := get(diffvar) - data.table::shift((get(diffvar))),
           by = idvar]
    } else {
        ## Create dataset with all combinations to compute first differences
        all_combinations <- data.table::CJ(unique(dt[[idvar]]),
                                           min(dt[[timevar]]):max(dt[[timevar]]))
        data.table::setnames(all_combinations, new = c(idvar, timevar))

        dt_all <- data.table::merge.data.table(dt, all_combinations,
                                               by = c(idvar, timevar), all = TRUE)

        dt_all[, paste0(diffvar, "_fd") := get(diffvar) - data.table::shift((get(diffvar))),
                by = idvar]

        ## Bring first differences back to the original dataset
        vars_to_keep <- c(idvar, timevar, paste0(diffvar, "_fd"))

        dt <- data.table::merge.data.table(dt, dt_all[, .SD, .SDcols = vars_to_keep],
                                           by = c(idvar, timevar), all.x = TRUE)
    }

    return(dt)
}
