GetFirstDifferences <- function(df, groupvar = NULL, timevar, diffvar) {
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

