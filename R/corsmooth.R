# This function is a  modification of psych::cor.smooth
# (written by William Revelle; licensed under GPLv2+)
# In the original function, the smoothing is applied if any of the eigenvalues
# are smaller than .Machine$double.eps. But we found examples where eigenvalues
# are slightly larger than this threshold but still require smoothing.
# Here, we increase the threshold to sqrt(.Machine$double.eps) consistent with mgcv etc

#' @importFrom stats cov2cor
#' @importFrom verbs Sum
cor.smooth2 <- function (x, eig.tol = 10^-12)
{
    eigens <- try(eigen(x), TRUE)
    if (inherits(eigens, as.character("try-error")))
    {
        warning("I am sorry, there is something seriously wrong with the correlation matrix,",
                "\ncor.smooth failed to  smooth it because some of the eigen values are NA.",
                "\nAre you sure you specified the data correctly?")
    }else
    {
        if (min(eigens$values) < sqrt(.Machine$double.eps)) {
            warning("Matrix was not positive definite, smoothing was done")
            eigens$values[eigens$values < eig.tol] <- 100 * eig.tol
            nvar <- dim(x)[1]
            tot <- Sum(eigens$values, remove.missing = FALSE)
            eigens$values <- eigens$values * nvar/tot
            cnames <- colnames(x)
            rnames <- rownames(x)
            ## For dense M, diagonal matrix D=diag(d): MDM^T = (MD^.5)(MD^.5)^T
            ## MD = t(t(M)*d)
            ## x <- eigens$vectors %*% diag(eigens$values) %*% t(eigens$vectors)
            x <- crossprod(t(eigens$vectors)*sqrt(eigens$values))
            x <- cov2cor(x)
            colnames(x) <- cnames
            rownames(x) <- rnames
        }
    }
    return(x)
}
