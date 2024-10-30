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

    return(names(copy))
}

MockES(dt)
names(dt)
MockES(df)
names(df)

# Potential solution: Shallow copy?

MockEventStudyDeepCopy <- function(data) {

    copy = copy(data)
    copy <- MockComputeFirstDifferences(copy)

    return(names(copy))
}

MockEventStudyDeepCopy(dt)
names(dt)


# Time Benchmarks
dt_large <- rbindlist(replicate(1000000, dt, simplify = FALSE))
df_large = as.data.frame(dt_large)

# Benchmark against original
MockEventStudyOrig = function(data) {

    data_dt = as.data.table(data)
    data_dt <- MockComputeFirstDifferences(data_dt)

    return(names(copy))
}

MockEventStudyShallowCopy <- function(data) {

    copy = data[T,]
    copy <- MockComputeFirstDifferences(copy)

    return(names(copy))
}

time_orig <- system.time(MockEventStudyOrig(df_large))
print(time_orig) # 1.158
names(df_large) # no mpg_fd7

time_deep_copy <- system.time(MockEventStudyDeepCopy(dt_large))
print(time_deep_copy) # 1.147
names(dt_large) # no mpg_fd7

time_shallow_copy <- system.time(MockEventStudyShallowCopy(dt_large))
print(time_shallow_copy) # 0.029
names(dt_large) # no mpg_fd
# read more on https://github.com/Rdatatable/data.table/issues/3665

time_no_copy <- system.time(MockEventStudy(dt_large))
print(time_no_copy) # 0.03
names(dt_large) # mpg_fd
