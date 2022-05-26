AddSmPath <- function(dhat, Vhat, norm_index,
                      alpha = 0.05, maxiter = 30, maxorder = 10) {

  Main <- function() {

    p          = length(dhat)
    invVhat    = solve(Vhat)
    Wcritic    = qchisq(1-alpha, p)

    order <- FindOrder(d, invVhat, norm_index,
                       Wcritic, maxorder)

    if (order == "Error") {
      stop("The program to find the smoothest path failed. Please unselect that option.")
    }
    else {

    }
  }

  FindOrder <- function(d, invV, normalized_index,
                        Wcritic, maxorder) {
    Wstart = 1e6

    r = 2
    while (r <= maxorder & Wstart >= Wcritic) {
      print(r)
      min_results <- PolyWaldMin(d, invV, normalized_index, r)

      print(min_results)
      if (is.null(min_results)) {
        return("Error.")
      } else {
        Wstart = min_results$W
        r      = r + 1
      }
    }

    return(r - 1)
  }

  PolyWaldMin <- function(d, invV, normalized_index, r) {
    # Minimize Wald objective given coefficients and inverse covariance matrix
    #   d = coefficients
    #   invV = inverse covariance matrix
    #   normalized_index = index of normalized coefficients
    #   r = degree of polynomial

    p = length(d)

    if (r == 0) {
      trfit <- rep(0, p)
      W     <- (t(d)%*%invV)%*%d
      a     <- 0
    }
    else {

      k    <- seq(0, p-1)/(p-1)
      Fmat <- sapply(seq(1, r),
                     function(j) {k^(j-1)})

      Anorm   <- matrix(Fmat[normalized_index,])
      ZeroMat <- matrix(0, nrow = length(normalized_index),
                           ncol = length(normalized_index))
      ZeroVec <- matrix(0, nrow = length(normalized_index))

      XX = 2*(t(Fmat)%*%invV)%*%Fmat
      Xy = 2*(t(Fmat)%*%invV)%*%d
      A  = rbind(cbind(XX,       Anorm),
                 cbind(t(Anorm), ZeroMat))
      b  = rbind(Xy, ZeroVec)

      tryCatch(
        {
          a_long  <- solve(A, b)
        },
        error = function(cond) {
          print("The solver in PolyWaldMin failed. Here is the original error message:")
          print(cond)

          return(NULL)
        }
      )
      a_short <- a_long[2:(r+1)]

      trfit <- Fmat%*%a_short
      W     <- (t(d-trfit)%*%invV)%*%(d-trfit)
    }

    return(list("trfit" = trfit,   "W"    = W,
                "a"     = a_short, "Fmat" = Fmat))
  }


}


