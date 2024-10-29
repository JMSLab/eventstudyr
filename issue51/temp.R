library(data.table)

dt = mtcars |>
    as.data.table()

df = mtcars


MockComputeFirstDifferences <- function(data) {

    setDT(data)
    data[, mpg_fd := 1]

}


MockEventStudy <- function(data) {

    data <- MockComputeFirstDifferences(data)

    return(0)
}

MockES(dt)
names(dt)
MockES(df)
names(df)
