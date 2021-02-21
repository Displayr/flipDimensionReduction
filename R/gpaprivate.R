# The functions in this file are copies of private functions in GPArotation
# GPArotation is licensed under GPLv3

# GPArotation:::NormalizingWeight
#' @importFrom verbs SumRows
gpa.normalizing.weight <- function (A, normalize = FALSE)
{
    if ("function" == mode(normalize))
        normalize <- normalize(A)
    if (is.logical(normalize)) {
        if (normalize)
            normalize <- sqrt(SumRows(A^2, remove.missing = FALSE))
        else return(array(1, dim(A)))
    }
    if (is.vector(normalize)) {
        if (nrow(A) != length(normalize))
            stop("normalize length wrong in NormalizingWeight")
        return(array(normalize, dim(A)))
    }
    stop("normalize argument not recognized in NormalizingWeight")
}

# GPArotation:::vgQ.varimax
#' @importFrom verbs Sum
gpa.vgQ.varimax <- function (L)
{
    QL <- sweep(L^2, 2, colMeans(L^2), "-")
    list(Gq = -L * QL, f = -sqrt(Sum(diag(crossprod(QL)), remove.missing = FALSE))^2/4,
        Method = "varimax")
}

# GPArotation:::vgQ.quartimax
#' @importFrom verbs Sum
gpa.vgQ.quartimax <- function (L)
{
    list(Gq = -L^3, f = -Sum(diag(crossprod(L^2)), remove.missing = FALSE)/4, Method = "Quartimax")
}

# GPArotation::vgQ.cf
#' @importFrom verbs Sum
gpa.vgQ.cf <- function (L, kappa = 0)
{
    k <- ncol(L)
    p <- nrow(L)
    N <- matrix(1, k, k) - diag(k)
    M <- matrix(1, p, p) - diag(p)
    L2 <- L^2
    f1 <- (1 - kappa) * Sum(diag(crossprod(L2, L2 %*% N)), remove.missing = FALSE)/4
    f2 <- kappa * Sum(diag(crossprod(L2, M %*% L2)), remove.missing = FALSE)/4
    list(Gq = (1 - kappa) * L * (L2 %*% N) + kappa * L * (M %*%
        L2), f = f1 + f2, Method = paste("Crawford-Ferguson:k=",
        kappa, sep = ""))
}

#GPArotation::vg.oblimin
#' @importFrom verbs Sum
gpa.vgQ.oblimin <- function (L, gam = 0)
{
    X <- L^2 %*% (!diag(TRUE, ncol(L)))
    if (0 != gam) {
        p <- nrow(L)
        X <- (diag(1, p) - matrix(gam/p, p, p)) %*% X
    }
    list(Gq = L * X, f = Sum(L^2 * X, remove.missing = FALSE)/4, Method = if (gam ==
        0) "Oblimin Quartimin" else if (gam == 0.5) "Oblimin Biquartimin" else if (gam ==
        1) "Oblimin Covarimin" else paste("Oblimin g=", gam,
        sep = ""))
}
