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

# MockES(dt)
# names(dt)
MockES(df)
names(df)

# Potential solution: Shallow copy?

MockEventStudyDeepCopy <- function(data) {

    copy = copy(data)
    copy <- MockComputeFirstDifferences(copy)

    return(names(copy))
}

# MockEventStudyDeepCopy(dt)
# names(dt)


# Time Benchmarks
dt_large <- rbindlist(replicate(1000000, dt, simplify = FALSE))
dt_large |> names()
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

MockEventStudyShallowCopy2 <- function(data) {

    copy = data
    copy <- MockComputeFirstDifferences(copy)

    return(names(copy))
}

print('Original Time')
time_orig <- system.time(MockEventStudyOrig(df_large))
print(time_orig)
print(paste0('Adding redundant cols?, ', 'mpg_fd' %in% names(df_large)))

dt_large <- rbindlist(replicate(1000000, dt, simplify = FALSE))
print('Time if using no copy')
time_no_copy <- system.time(MockEventStudy(dt_large))
print(time_no_copy)
print(paste0('Adding redundant cols?, ', 'mpg_fd' %in% names(dt_large)))

dt_large <- rbindlist(replicate(1000000, dt, simplify = FALSE))
print('Time if using deep copy i.e. copy()')
time_deep_copy <- system.time(MockEventStudyDeepCopy(dt_large))
print(time_deep_copy)
print(paste0('Adding redundant cols?, ', 'mpg_fd' %in% names(dt_large)))

dt_large <- rbindlist(replicate(1000000, dt, simplify = FALSE))
print('Time if using shallow copy i.e. [T,]')
time_shallow_copy <- system.time(MockEventStudyShallowCopy(dt_large))
print(time_shallow_copy)
print(paste0('Adding redundant cols?, ', 'mpg_fd' %in% names(dt_large)))
# read more on https://github.com/Rdatatable/data.table/issues/3665

dt_large <- rbindlist(replicate(1000000, dt, simplify = FALSE))
print('Time if using shallow copy i.e. <-')
time_shallow_copy <- system.time(MockEventStudyShallowCopy2(dt_large))
print(time_shallow_copy)
print(paste0('Adding redundant cols?, ', 'mpg_fd' %in% names(dt_large)))


