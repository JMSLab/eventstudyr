# Function to add zero where normalized coefficient should be in covar matrix
AddZerosCovar <- function(vcov_matrix_all, eventstudy_coeffs, norm_column,
                          coeffs_order) {

  v_terms_to_keep <- colnames(vcov_matrix_all) %in% c(eventstudy_coeffs)
  covar           <- vcov_matrix_all[v_terms_to_keep, v_terms_to_keep]

  n_coefs = nrow(covar)

  # Add row and col of zeros at the end
  covar           <- rbind(cbind(covar, matrix(0, nrow = n_coefs)),
                           matrix(0, ncol = n_coefs+1))
  rownames(covar) <- c(eventstudy_coeffs, norm_column)
  colnames(covar) <- c(eventstudy_coeffs, norm_column)

  # Sort matrix
  covar           <- covar[coeffs_order, coeffs_order]

  return(covar)
}

# Function that takes coeff_length and poly_order as arguments and computes Fmat
GetFmat <- function(coeff_length, poly_order) {

  k    = seq(0, coeff_length-1)/(coeff_length-1)
  Fmat = sapply(seq(1, poly_order),
                function(j) {k^(j-1)})

  return(Fmat)
}

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
    Wvalue     = min_results$W
    poly_order = poly_order + 1
  }

  return(list(order = poly_order - 1,
              results = min_results))
}

SolutionInWaldRegion <- function(coeffs, inv_covar, norm_index, poly_order) {
  ##########################################################################
  # Minimize Wald objective given coefficients and inverse covariance matrix
  ##########################################################################

  coeff_length = length(coeffs)

  if (poly_order == 0) {
    trfit = rep(0, coeff_length)
    W     = (t(coeffs)%*%inv_covar)%*%coeffs
    vhat  = 0

  } else {
    Fmat  <- GetFmat(coeff_length, poly_order)
    Anorm <- matrix(Fmat[norm_index,])

    FtinvVd    = (t(Fmat)%*%inv_covar)%*%matrix(coeffs)
    invFtinvVF = pracma::inv((t(Fmat)%*%inv_covar)%*%Fmat)
    AtFtinvVFA = (t(Anorm)%*%invFtinvVF)%*%Anorm
    multiple   = (Anorm%*%pracma::inv(AtFtinvVFA))%*%t(Anorm)

    difference = FtinvVd - (multiple%*%invFtinvVF)%*%FtinvVd
    vhat       = invFtinvVF%*%difference

    trfit <- Fmat%*%vhat
    W     <- (t(trfit-coeffs)%*%inv_covar)%*%(trfit-coeffs)
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

  Fmat  <- GetFmat(length(coeffs), length(v))
  trfit <- Fmat %*% v

  W     <- (t(trfit-coeffs)%*%inv_covar)%*%(trfit-coeffs)
  return(W)
}

EqConstraint <- function(v, coeffs, inv_covar) {

  norm_index <- which(coeffs == 0)

  Fmat  <- GetFmat(length(coeffs), length(v))
  trfit <- Fmat %*% v

  norm_sm_path <- trfit[norm_index]

  return(norm_sm_path)
}
