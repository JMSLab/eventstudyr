# Functions for Finding Minimum Order 
FindOrder <- function(coeffs, inv_covar, Wcritic, maxorder) {
  ########################################################################
  # Find minimum order of polynomial such that the constraint is satisfied
  ########################################################################
  
  norm_index <- which(coeffs == 0)
  
  Wvalue = 1e6
  error  = F
  poly_order = 0
  
  # Compute Wald value for polynomials of increasing order until Wald Value < Critical Value
  while (poly_order <= maxorder & Wvalue >= Wcritic ) {
    
    min_results <- SolutionInWaldRegion(coeffs, inv_covar, norm_index, poly_order)
    
    if (is.null(min_results)) {
      error = T
      break
    } else {
      Wvalue = min_results$W
      poly_order = poly_order + 1
    }
    
  }
  
  if (error){
    return("Error")
  }else{
    return(poly_order - 1)
  }

}

SolutionInWaldRegion <- function(coeffs, coeff_length, inv_covar, norm_index, poly_order) {
  ##########################################################################
  # Minimize Wald objective given coefficients and inverse covariance matrix
  ##########################################################################

  coeff_length = length(coeffs)
  
  if (poly_order == 0) {
    trfit = rep(0, coeff_length)
    W     = (t(coeffs)%*%inv_covar)%*%coeffs
    vhat  = 0
  }
  else {
    Fmat <- sapply(seq(0, poly_order),
                   function(j) {coeff_length^(j)})

    Anorm   <- matrix(Fmat[norm_index,])

    FtinvVd    = (t(Fmat)%*%inv_covar)%*%matrix(coeffs)
    invFtinvVF = inv((t(Fmat)%*%inv_covar)%*%Fmat)
    AtFtinvVFA = (t(Anorm)%*%invFtinvVF)%*%Anorm
    multiple   = (Anorm%*%inv(AtFtinvVFA))%*%t(Anorm)

    difference = FtinvVd - (multiple%*%invFtinvVF)%*%FtinvVd
    vhat       = invFtinvVF%*%difference

    trfit <- Fmat%*%vhat
    W     <- (t(d-trfit)%*%inv_covar)%*%(d-trfit)
  }

  return(list("trfit" = trfit,
              "W"     = W,
              "vhat"  = vhat))
}

# Functions for Finding Minimum Coefficient on Highest Term
Objective <- function(v, coeffs, inv_covar) {
  return(v[length(v)]^2)
}

IneqConstraint <- function(v, coeffs, inv_covar) {
  p <- length(d)
  r <- length(v)
  
  k    <- seq(3, p+2)/(p-1)
  Fmat <- sapply(seq(0, r-1),
                 function(j) {k^(j)})
  trfit <- Fmat %*% v
  
  W     <- (t(d-trfit)%*%invV)%*%(d-trfit)
  return(W)
}
