
  estimates_ols <- EventStudy(
    estimator = "OLS",
    data = example_data,
    outcomevar = "y_jump_m", policyvar = "z",
    idvar      = "id",       timevar   = "t",
    controls = "x_r",
    post = 5, overidpost = 2,
    pre  = 0,  overidpre = 6
  )
  
  y_breaks = seq(-1, 2, 1)
  plts <- list()
  
  plt<- EventStudyPlot(estimates         = estimates_ols,
                                          pre_event_coeffs  = FALSE,
                                          post_event_coeffs = FALSE,
                                          ybreaks           = y_breaks,
                                          smpath            = TRUE) 
  print(plt)
