#' Adds leads of a variable as new columns
#'
#' @param df Data frame that will be modified
#' @param groupvar Optional grouping variable.
#' @param timevar Variable indicating time periods.
#' @param leadvar Variable whose leads will be added.
#' @param leads Numeric vector specifying the leads to be computed.
#'
#' @seealso \link[data.table]{shift}
#'
#' @examples
#' \dontrun{
#' #Compute leads of minimum wage in a panel data of states
#' PrepareLeads(df, groupvar = "state", timevar = "year", leadvar = "minwage", 5)
#' }
#'
#' @importFrom data.table setDT
#' @importFrom data.table shift
#' @importFrom data.table setorderv
#' @export


PrepareLeads <- function(df, groupvar = NULL, timevar, leadvar, leads) {
    df <- setDT(df)
    
    if (is.null(groupvar)) {
        print(timevar)
        setorderv(df_test, cols = timevar)
        print(is.data.table(df))
        df <- df[, paste0(leadvar, "_lead", leads) :=
                 shift(get(leadvar), leads, type = "lead")]
    } else {
        setorderv(df_test, cols = c(groupvar, timevar))
        df <- df[, paste0(leadvar, "_lead", leads) :=
                 shift(get(leadvar), leads, type = "lead"),
                 by = groupvar]
    }
        
    df <- as.data.frame(df)
    return(df)
}

