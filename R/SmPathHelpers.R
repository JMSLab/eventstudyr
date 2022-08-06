# Functions for Finding Minimum Order 
FindOrder <- function(coefficients, coeff_length, inv_covar_matrix, Wcritic, maxorder) {
  ########################################################################
  # Find minimum order of polynomial such that the constraint is satisfied
  ########################################################################
  
  normalized_index <- which(coefficients %in% c(0))
  
  Wvalue = 1e6
  error  = F
  polynomial_order = 0
  
  # Compute Wald value for polynomials of increasing order until Wald Value < Critical Value
  while (polynomial_order <= maxorder & Wvalue >= Wcritic ) {
    
    min_results <- PolyWaldMin(coefficients, coeff_length, inv_covar_matrix, 
                               normalized_index, polynomial_order)
    
    if (is.null(min_results)) {
      error = T
      break
    } else {
      Wvalue = min_results$W
      polynomial_order = polynomial_order + 1
    }
    
  }
  
  if (error){
    return("Error")
  }else{
    return(polynomial_order - 1)
  }

}

PolyWaldMin <- function(coefficients, coeff_length, inv_covar_matrix, normalized_index, polynomial_order) {
  ##########################################################################
  # Minimize Wald objective given coefficients and inverse covariance matrix
  ##########################################################################

  error = F
  
  if (polynomial_order == 0) {
    trfit <- rep(0, coeff_length)
    W     <- (t(coefficients)%*%inv_covar_matrix)%*%coefficients
    a     <- 0
  }
  else {
    
    k    <- seq(0, coeff_length-1)/(coeff_length-1)
    Fmat <- sapply(seq(0, polynomial_order),
                   function(j) {k^(j)})
    
    Anorm   <- matrix(Fmat[normalized_index,])
    ZeroMat <- matrix(0, nrow = length(normalized_index),
                      ncol = length(normalized_index))
    ZeroVec <- matrix(0, nrow = length(normalized_index))
    
    XX = 2*(t(Fmat)%*%inv_covar_matrix)%*%Fmat
    Xy = 2*(t(Fmat)%*%inv_covar_matrix)%*%coefficients
    A  = rbind(cbind(XX,       Anorm),
               cbind(t(Anorm), ZeroMat))
    b  = rbind(Xy, ZeroVec)
    
    tryCatch(
      {
        a_long <- solve(A, b)
        a      <- a_long[1:(r+1)]
        
        trfit <- Fmat%*%a
        W     <- (t(coefficients-trfit)%*%inv_covar_matrix)%*%(coefficients-trfit)
      },
      error = function(cond) {
        print("The solver in PolyWaldMin failed. Here is the original error message:")
        print(cond)
        
        error <<- T
      }
    )
  }
  
  if (error) {
    return(NULL)
  } else {
    return(list("trfit" = trfit,
                "W"     = W,
                "a"     = a))
  }
}

# Functions for Finding Minimum Coefficient on Highest Term 
Objective <- function(v, d, invV) {
  return(v[length(v)]^2)
}

IneqConstraint <- function(v, d, invV) {
  p <- length(d)
  r <- length(v)
  
  k    <- seq(3, p+2)/(p-1)
  Fmat <- sapply(seq(0, r-1),
                 function(j) {k^(j)})
  trfit <- Fmat %*% v
  
  W     <- (t(d-trfit)%*%invV)%*%(d-trfit)
  return(W)
}
