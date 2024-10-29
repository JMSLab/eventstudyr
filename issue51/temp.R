
test = mtcars |>
    as.data.table()

df = mtcars
normalization_column = 'mpg'

df_change <- df[df[, normalization_column] == 0 & !is.na(df[, normalization_column]), ]
    df_change <- df[df[, normalization_column] != 0 & !is.na(df[, normalization_column]), ]
mean <- mean(df_change[[outcomevar]], na.rm = T)


if (is.null(custom_scalar)) {
    if (grepl(paste0(policyvar, "_lead"), normalization_column)) {
        df_change <- df[df[, normalization_column] == 0 & !is.na(df[, normalization_column]), ]
    }else {
        df_change <- df[df[, normalization_column] != 0 & !is.na(df[, normalization_column]), ]
    }
    mean <- mean(df_change[[outcomevar]], na.rm = T)

} else {
    mean <- custom_scalar
}


if (is.null(custom_scalar)) {
    if (grepl(paste0(policyvar, "_lead"), normalization_column)) {
        df_change <- df[get(normalization_column) == 0 & !is.na(get(normalization_column))]
    } else {
        df_change <- df[get(normalization_column) != 0 & !is.na(get(normalization_column))]
    }
    mean <- df_change[, mean(get(outcomevar), na.rm = TRUE)]
} else {
    mean <- custom_scalar
}
