df_test <- data.frame(country = rep(c('A', 'B'), 4),
                      values = runif(8),
                      periods = c(4,4,2,2,3,3,1,1))

write.csv(df_test, 'tests/testthat/input/df_test.csv',
          row.names = F)

