library(pracma) # install.packages("pracma")
library(Rsolnp) # install.packages("Rsolnp")

AddSmPath <- function(dhat, Vhat,
                      alpha = 0.05, maxiter = 30, maxorder = 10) {

  ###################### FUNCTIONS

  FindOrder <- function(d, invV, Wcritic, maxorder) {
    # Find minimum order of polynomial such that the constraint is satisfied

    normalized_index <- which(d == 0)

    Wstart = 1e6
    error = F
    r     = 0

    while (r <= maxorder & Wstart >= Wcritic ) {

      min_results <- PolyWaldMin(d, invV, normalized_index, r)

      if (is.null(min_results)) {
        error = T
        break
      } else {
        Wstart = min_results$W
        r      = r + 1
      }
    }

    if (error) return("Error")
    else       return(r - 1)
  }

  PolyWaldMin <- function(d, invV, normalized_index, r) {
    # Minimize Wald objective given coefficients and inverse covariance matrix
    #   d = coefficients
    #   invV = inverse covariance matrix
    #   normalized_index = index of normalized coefficients
    #   r = degree of polynomial

    p = length(d)
    k = seq(0, p-1)/(p-1)

    if (r == 0) {
      trfit <- rep(0, p)
      W     <- (t(d)%*%invV)%*%d
      vhat  <- 0
    }
    else {
      Fmat <- sapply(seq(0, r),
                     function(j) {k^(j)})

      Anorm   <- matrix(Fmat[normalized_index,])

      FtinvVd    = (t(Fmat)%*%invV)%*%matrix(d)
      invFtinvVF = inv((t(Fmat)%*%invV)%*%Fmat)
      AtFtinvVFA = (t(Anorm)%*%invFtinvVF)%*%Anorm
      multiplo   = (Anorm%*%inv(AtFtinvVFA))%*%t(Anorm)

      difference = FtinvVd - (multiplo%*%invFtinvVF)%*%FtinvVd
      vhat = invFtinvVF%*%difference

      trfit <- Fmat%*%vhat
      W     <- (t(d-trfit)%*%invV)%*%(d-trfit)
    }
    return(list("trfit" = trfit,
                "W"     = W,
                "vhat"  = vhat))
  }


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

  ################################# MAIN

  p          = length(dhat)
  invVhat    = pinv(Vhat)
  Wcritic    = qchisq(1-alpha/2, p)

  # First step
  order <- FindOrder(dhat, invVhat,
                     Wcritic, maxorder)

  if (order == "Error") {
    stop("The program to find the smoothest path failed. Please unselect that option.")
  }
  else {
    # Second step: Find minimizing coefficients

    optim <-
        solnp(pars    = rep(0, order),
              fun     = Objective,
              ineqfun = IneqConstraint,
              ineqUB  = Wcritic,
              ineqLB  = -1e6,
              d = dhat, invV = invVhat)

    if (optim$convergence != 0) {
      stop("The search for parameters for the polynomial did not converge. Please unselect the smoothest path option.")

    } else {
      vstar <- optim$pars

      p <- length(dhat)
      r <- order

      k    <- seq(0, p-1)/(p-1)
      Fmat <- sapply(seq(0, r-1),
                     function(j) {k^(j)})

      return(Fmat %*% vstar)
    }
  }
}


