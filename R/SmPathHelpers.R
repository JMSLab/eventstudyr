# Add zero where normalized coefficient(s) should be in covar matrix
AddZerosCovar <- function(vcov_matrix_all, eventstudy_coeffs, norm_column,
                          coeffs_order) {

    v_terms_to_keep <- colnames(vcov_matrix_all) %in% eventstudy_coeffs
    covar           <- vcov_matrix_all[v_terms_to_keep, v_terms_to_keep]

    n_coefs      = length(coeffs_order)
    needed_zeros = length(norm_column)

    # Add row and col of zeros at the end
    ZerosRight  = matrix(0, ncol = needed_zeros, nrow = nrow(covar))
    ZerosBottom = matrix(0, ncol = n_coefs,      nrow = needed_zeros)
    covar           <- rbind(cbind(covar, ZerosRight),
                            ZerosBottom)
    rownames(covar) <- c(eventstudy_coeffs, norm_column)
    colnames(covar) <- c(eventstudy_coeffs, norm_column)

    # Sort matrix
    covar           <- covar[coeffs_order, coeffs_order]

    return(covar)
}

# Computes F matrix using coeff_length and poly_order as arguments
GetFmat <- function(coeff_length, poly_order) {

    k    = seq(0, coeff_length-1)/(coeff_length-1)
    Fmat = sapply(seq(1, poly_order+1),
                function(j) {k^(j-1)})

    return(Fmat)
}

# Find minimum order of polynomial such that the constraint is satisfied
FindOrder <- function(coeffs, inv_covar, Wcritic, maxorder) {

    norm_index <- which(coeffs == 0)

    Wvalue = 1e6
    error  = F
    poly_order = 0

    # Compute Wald value for polynomials of increasing order until Wald Value < Critical Value
    while (poly_order <= maxorder & Wvalue >= Wcritic) {

        min_results <- SolutionInWaldRegion(coeffs, inv_covar, norm_index, poly_order)
        Wvalue     = min_results$W
        poly_order = poly_order + 1
    }

    return(list(order = poly_order - 1,
                results = min_results))
}

# Minimize Wald objective given coefficients and inverse covariance matrix
SolutionInWaldRegion <- function(coeffs, inv_covar, norm_index, poly_order) {

    coeff_length = length(coeffs)

    if (poly_order == 0) {
        trfit = rep(0, coeff_length)
        W     = (t(coeffs)%*%inv_covar)%*%coeffs
        vhat  = 0

    } else {
        Fmat  <- GetFmat(coeff_length, poly_order)
        Anorm <- Fmat[norm_index, , drop = F]

        FtinvVd    = (t(Fmat)%*%inv_covar)%*%matrix(coeffs)
        invFtinvVF = pracma::inv((t(Fmat)%*%inv_covar)%*%Fmat)
        AtFtinvVFA = (Anorm%*%invFtinvVF)%*%t(Anorm)
        multiple   = (t(Anorm)%*%pracma::inv(AtFtinvVFA))%*%Anorm

        difference = FtinvVd - (multiple%*%invFtinvVF)%*%FtinvVd
        vhat       = invFtinvVF%*%difference

        trfit <- Fmat%*%vhat
        W     <- (t(trfit-coeffs)%*%inv_covar)%*%(trfit-coeffs)
    }

    return(list("trfit" = trfit,
                "W"     = W,
                "vhat"  = vhat))
}

# Find coefficients such that square of highest order term is minimized
# Num normalized coefficients less than order of polynomial
FindCoeffs <- function(res_order, coeffs, inv_covar, Wcritic, pN, order, norm_idxs, Fmat,
                       maxiter_solver = 1e6) {

    if (is.null(dim(Fmat))) { # If one-dimensional make sure it's also a matrix object
        Fmat <- matrix(Fmat)
    }

    # Prevent conversion to vector with drop = F
    Anorm <- Fmat[norm_idxs, , drop = F]

    stopifnot(ncol(Anorm) == ncol(Fmat))

    colindex_b = 1:(ncol(Anorm)-pN-1)
    colindex_1 = (ncol(Anorm)-pN):(ncol(Anorm)-1)
    colindex_2 = ncol(Anorm)

    Ab <- Anorm[, colindex_b, drop = F]
    A1 <- Anorm[, colindex_1, drop = F]
    A2 <- Anorm[, colindex_2, drop = F]

    Fb <- Fmat[, colindex_b, drop = F]
    F1 <- Fmat[, colindex_1, drop = F]
    F2 <- Fmat[, colindex_2, drop = F]

    x0 = res_order$vhat[1:ncol(Fb)]

    optim_pos <- optim(par     = x0,
                       fn      = Objective,
                       method  = "Nelder-Mead",
                       control = list("maxit" = maxiter_solver),
                       d   = coeffs, inv_covar = inv_covar,
                       Fb  = Fb, F1 = F1, F2 = F2, Ab = Ab, A1 = A1, A2 = A2,
                       Wcritic = Wcritic, positive = T)
    optim_neg <- optim(par     = x0,
                       fn      = Objective,
                       method  = "Nelder-Mead",
                       control = list("maxit" = maxiter_solver),
                       d   = coeffs, inv_covar = inv_covar,
                       Fb  = Fb, F1 = F1, F2 = F2, Ab = Ab, A1 = A1, A2 = A2,
                       Wcritic = Wcritic, positive = F)

    if (optim_pos$convergence != 0 | optim_neg$convergence != 0) {
        stop("Numerical optimization failed when searching for the smoothest path. Please set 'Smpath' to FALSE.")
    }

    vb_pos <- optim_pos$par
    v2_pos <- sqrt(optim_pos$value)

    vb_neg <- optim_neg$par
    v2_neg <- sqrt(optim_neg$value)

    if (abs(v2_pos) < abs(v2_neg)) {
        vb = vb_pos
        v2 = v2_pos
    } else {
        vb = vb_neg
        v2 = v2_neg
    }
    v1 = inv(A1)%*%(-Ab%*%vb - A2%*%v2)

    return(c(vb, v1, v2))
}

d0 <- function(d, inv_covar, F1, F2, A1, A2) {
    single_factor = F2 - F1%*%pinv(A1)%*%A2

    return(t(single_factor)%*%inv_covar%*%single_factor)
}

d1 <- function(vb, d, inv_covar, Fb, F1, F2, Ab, A1, A2) {
    pre_factor  = (Fb - F1%*%(pinv(A1)%*%Ab))%*%vb - d
    post_factor = F2 - F1%*%pinv(A1)%*%A2

    return(2*t(pre_factor)%*%inv_covar%*%post_factor)
}

d2 <- function(vb, d, inv_covar, Fb, F1, Ab, A1, Wcritic) {
    single_factor = (Fb - F1%*%(pinv(A1)%*%Ab))%*%vb - d

    return(t(single_factor)%*%inv_covar%*%single_factor - Wcritic)
}

Objective <- function(vb, d, inv_covar, Fb, F1, F2, Ab, A1, A2, Wcritic, 
                      positive = T) {

    vb = matrix(vb)

    d0_ = d0(    d, inv_covar,     F1, F2,     A1, A2)
    d1_ = d1(vb, d, inv_covar, Fb, F1, F2, Ab, A1, A2)
    d2_ = d2(vb, d, inv_covar, Fb, F1,     Ab, A1,    Wcritic)

    discriminant = d1_^2 - 4*d0_*d2_

    if (discriminant < 0) {
        return(Inf)
    }

    if (positive) {
        return(( (-d1_ + sqrt(discriminant))/(2*d0_) )^2)
    } else {
        return(( (-d1_ - sqrt(discriminant))/(2*d0_) )^2)
    }
}


# Find coeffs such that square of highest order term is minimized
# Num normalized coefficients equals order of polynomial
FindCoeffsEq <- function(res_order, coeffs, inv_covar, Wcritic, pN, order, norm_idxs, Fmat,
                         maxiter_solver = 1e6) {

    if (is.null(dim(Fmat))) { # If one-dimensional make sure it's also a matrix object
        Fmat <- matrix(Fmat)
    }

    # Prevent conversion to vector with drop = F
    Anorm <- Fmat[norm_idxs, , drop = F]

    stopifnot(ncol(Anorm) == ncol(Fmat))

    colindex_1 = (ncol(Anorm)-pN):(ncol(Anorm)-1)
    colindex_2 = ncol(Anorm)

    A1 <- Anorm[, colindex_1, drop = F]
    A2 <- Anorm[, colindex_2, drop = F]

    F1 <- Fmat[, colindex_1, drop = F]
    F2 <- Fmat[, colindex_2, drop = F]

    d0_ = d0Eq(coeffs, inv_covar,         F1, F2, A1, A2)
    d1_ = d1Eq(coeffs, inv_covar,         F1, F2, A1, A2)
    d2_ = d2Eq(coeffs, inv_covar, Wcritic)

    discriminant = d1_^2 - 4*d0_*d2_

    v2_pos <- (-d1_ + sqrt(discriminant))/(2*d0_)
    v2_neg <- (-d1_ - sqrt(discriminant))/(2*d0_)

    if (abs(v2_pos) < abs(v2_neg)) {
        v2 = v2_pos
    } else {
        v2 = v2_neg
    }
    v1 = -pinv(A1)%*%(A2%*%v2)

    return(c(v1, v2))
}

d0Eq <- function(d, inv_covar, F1, F2, A1, A2) {
    single_factor = F2 - F1%*%pinv(A1)%*%A2

    return(t(single_factor)%*%inv_covar%*%single_factor)
}

d1Eq <- function(d, inv_covar, F1, F2, A1, A2) {
    pre_factor = F2 - F1%*%pinv(A1)%*%A2

    return(-2*t(pre_factor)%*%inv_covar%*%d)
}

d2Eq <- function(d, inv_covar, Wcritic) {

    return(t(d)%*%inv_covar%*%d - Wcritic)
}
