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
            warning("The analysis has failed to satisfy a technical assumption of PCA ",
                    "(i.e., that the input matrix is positive definite). We've solved this ",
                    "by smoothing the input matrix into a positive definite one.")
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

checkDataAfterCorrelationSmoothing <- function(data, prepared.data, missing)
{
    known.problem <- " There seems to be a problem with the input data where"
    p.bigger.n.msg <- paste0(" there are more variables than there are cases. ",
                             "The input matrix needs at more observations than the number ",
                             "of variables to be positive definite. To fix this, consider ",
                             "reducing the number of input variables")
    # If p > n, the number of variables is larger than the number of cases
    if (diff(dim(data)) > 0L)
        return(paste0(known.problem, p.bigger.n.msg, "."))
    if (missing == "Exclude cases with missing data" && diff(dim(prepared.data$subset.data)) > 0L)
        return(paste0(known.problem, ", after removing the cases with missing data,",
                      p.bigger.n.msg, " or using an alternative missing data option instead."))
    output.msg <- " However, there's a good chance that there is a problem in your data"
    # If partial data as missing data option used
    if (missing == "Use partial data (pairwise correlations)")
        return(paste0(output.msg, " (e.g. Using partial data (partial correlations) to handle ",
                      "missing data can sometimes lead to non positive definite matrices. Consider using ",
                      "an alternative missing data option instead)."))
    paste0(output.msg,
           " (e.g., the same variable is included twice or maybe the data has been transformed ",
           "to have a mean of 0, which is not appropriate, as it means ",
           "that at least one variable is perfectly correlated with a combination ",
           "of the other variables).")
}
