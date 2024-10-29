
test = mtcars |>
    as.data.table()
lead_endpoint_var = 'disp'
test[, (lead_endpoint_var) := - get(lead_endpoint_var)]

