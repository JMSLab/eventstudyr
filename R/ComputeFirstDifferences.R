#' Adds first difference of a variable as a new column
#'
#' @param df Data frame that will be modified
#' @param idvar Character indicating column of grouping variable.
#' @param timevar Character indicating column of time variable.
#' @param diffvar Character indicating column of variable whose first difference will be taken.
#' @param unbalanced Logical indicating whether the dataset is unbalanced.
#'
#' @seealso [data.table::shift()]
#'
#' @importFrom data.table CJ setDT setorderv shift :=
#'
#' @keywords internal
#' @noRd

ComputeFirstDifferences <- function(df, idvar, timevar, diffvar, 
                                    unbalanced = FALSE) {
    if (! is.data.frame(df)) {stop("df should be a data frame.")}
    if ((! is.null(idvar)) & (! is.character(idvar))) {stop("idvar should be a character.")}
    if (! is.character(timevar)) {stop("timevar should be a character.")}
    if (! is.character(diffvar)) {stop("diffvar should be a character.")}
    if (! is.logical(unbalanced)) {stop("unbalanced should be a logical.")}

    data.table::setDT(df)
    data.table::setorderv(df, cols = c(idvar, timevar))

    if (!unbalanced) {
        df[, paste0(diffvar, "_fd") := get(diffvar) - shift((get(diffvar))), 
           by = idvar]
    } else {
        ## Create datset with all combinations to compute first differences
        all_combinations <- CJ(unique(df[[idvar]]), unique(df[[timevar]]))
        setnames(all_combinations, new = c(idvar, timevar))
        
        df_all <- merge(df, all_combinations, 
                        by = c(idvar, timevar), all = TRUE)
        
        df_all[, paste0(diffvar, "_fd") := get(diffvar) - shift((get(diffvar))), 
                by = idvar]
        
        ## Bring first differences back to the original dataset
        vars_to_keep <- c(idvar, timevar, paste0(diffvar, "_fd"))
        df <- merge(df, df_all[, ..vars_to_keep], 
                    by = c(idvar, timevar), all.x = TRUE)
    }

    df <- as.data.frame(df)

    return(df)
}
