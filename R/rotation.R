
#' \code{RotateLoadings}
#' @description Rotate the component loadings from a principal components analysis or factor analysis.
#' @param loadings A matrix containing the un-rotated loadings from a PCA or factor analysis.
#' @param rotation A string specifying the kind of rotation to be conducted. The allowed values are:
#' "varimax", "quartimax", "equamax", "oblimin", and "promax".
#' @param delta The parameter supplied to oblimin.
#' @param kappa The exponent supplied to promax.
#' @param covar Whether or not the loadings were generated by analysing a covariance matrix rather than a
#' correlation matrix. This is only relevant if using a promax rotation, as an extra scaling step
#' is required in order to match the outputs from SPSS.
#' @param stds If a covariance matrix was used (covar == TRUE) then the standard deviations of the original
#' input variables must be supplied. Again, this is required for a scaling step in promax.
#' @details This function utilizes the rotation options from \code{GPArotation} to match SPSS'
#' component rotations as closely as possible. As SPSS uses different rotation algorithms, the match is
#' not exact in all cases, but testing shows that the rotated loadings are effectively the same. Differences
#' will be most noticable for Oblimin rotations, as the GPA package does a much better job at minimizing
#' the oblimin function compared with SPSS, and for larger loading matrices where the solution space is larger.
#'
#' A Kaiser normalization step is always included, to match what SPSS does.
#'
#' The promax rotation, which is done algebraicly rather than by optimizing an objective function, uses our own function,
#' \code{flipPromax}, as the promax functions from \code{\link{stats}} and \code{\link{psych}} do not match
#' the SPSS outputs.
#' @importFrom GPArotation GPForth GPFoblq
#' @export
RotateLoadings <- function(loadings, rotation = "varimax", delta = 0, kappa = 4, covar = FALSE, stds = NULL) {

    component.correlations <- NULL

    # Kaiser normalize up front
    # This is to allow us to get the values of the objective functions more easily
    # Not needed for Promax because promax does not have an objective function, and because it
    # deals with normalization differently.
    if (rotation != "promax") {
        W <- gpa.normalizing.weight(loadings, normalize = TRUE)
        normalized.loadings <- loadings / W
    }

    if (rotation == "varimax")
    {
        rotation.results <- GPForth(normalized.loadings, Tmat=diag(ncol(normalized.loadings)), normalize=FALSE, eps=1e-5, maxit=1000, method=rotation, methodArgs=NULL)
        rotated.loadings <- rotation.results$loadings
        objective <- gpa.vgQ.varimax(rotated.loadings)$f
        iterations <- rotation.results$Table
    } else if (rotation == "quartimax")
    {
        rotation.results <- GPForth(normalized.loadings, Tmat=diag(ncol(normalized.loadings)), normalize=FALSE, eps=1e-5, maxit=1000, method=rotation, methodArgs=NULL)
        rotated.loadings <- rotation.results$loadings
        objective <- gpa.vgQ.quartimax(rotated.loadings)$f
        iterations <- rotation.results$Table
    } else if (rotation == "equamax")
    {
        rotation.results <- GPForth(normalized.loadings, Tmat=diag(ncol(normalized.loadings)), normalize=FALSE, eps=1e-5, maxit=1000, method="cf", methodArgs=list(kappa = ncol(loadings)/(2*nrow(loadings)) ))
        rotated.loadings <- rotation.results$loadings
        objective <- gpa.vgQ.cf(rotated.loadings, kappa = ncol(rotated.loadings)/(2*nrow(rotated.loadings)))$f
        iterations <- rotation.results$Table
    } else if (rotation == "oblimin")
    {
        rotation.results <- GPFoblq(normalized.loadings, Tmat=diag(ncol(normalized.loadings)), normalize=FALSE, eps=1e-5, maxit=1000, method=rotation, methodArgs=list(gam = delta))
        rotated.loadings <- rotation.results$loadings
        component.correlations <- rotation.results$Phi
        objective <- gpa.vgQ.oblimin(rotated.loadings, gam  = delta)$f
        iterations <- rotation.results$Table
    } else if (rotation == "promax")
    {
        rotation.results <- flipPromax(loadings, m = kappa, covar = covar, stds = stds)
        rotated.loadings <- rotation.results$loadings
        component.correlations <- rotation.results$Phi
        objective <- NULL
        iterations <- NULL
    } else
    {
        StopForUserError("Rotation method of ", rotation, " is not supported.")
    }

    # Kaiser un-normalization step
    # Again, not needed for promax
    if (rotation != "promax") {
        rotated.loadings <- rotated.loadings * W
    }

    # Sort the components according to the sum of squared loadings in each column
    # For some reason SPSS doesn't reorder the components with Oblimin

    if (rotation != "oblimin")
    {
        cs <- diag(t(rotated.loadings) %*% rotated.loadings)
        cs.order <- order(cs, decreasing = TRUE)
        rotated.loadings <- rotated.loadings[, cs.order]
        if (rotation == "promax")
        {
            component.correlations <- component.correlations[cs.order, cs.order]
        }
    }

    structure.matrix <- NULL
    if (rotation == "promax" | rotation == "oblimin")
    {
        structure.matrix <- rotated.loadings %*% component.correlations
    }
    return(list(rotated.loadings = rotated.loadings,
                structure.matrix = structure.matrix,
                component.correlations = component.correlations,
                objective = objective,
                iterations = iterations))

}



# Function to replicate SPSSs promax rotation.
# This has several differences with existing promax functions
# in \code{stats} and \code{psych}:
#
# 1. The GPArotation package is used to do the initial varimax rotation.
# 2. This initial varimax is done with a Kaiser rotation (as in SPSS).
# 3. The target matrix (to which the promax power m is applied) is normalized
#    as per the SPSS algorithm notes.
# 4. An additional normalization step, denoted below by the matrix C, which
#    appears in the SPSS notes but not in the existing algorithms is included as well.
# 5. If the analysis is done using a covariance matrix rather than a correlation
#    matrix, then the output loadings of the initial varimax step are divided
#    by the standard deviations of the original variables
flipPromax <- function(x, m = 4, covar = FALSE, stds = NULL)
{
    if (!is.matrix(x) & !is.data.frame(x)) {
        if (!is.null(x$loadings))
            x <- as.matrix(x$loadings)
    }

    if (ncol(x) < 2)
        return(x)
    dn <- dimnames(x)

    # Use GPA to do a better varimax for the initial rotation
    xx <- GPArotation::GPForth(x, Tmat=diag(ncol(x)), normalize=TRUE, eps=1e-5, maxit=1000, method="varimax", methodArgs=NULL)
    x <- xx$loadings

    # When a covariance matrix is used, the loadings must be scaled by the standard
    # deviations of the input variables.
    # Testing seems to show that it doesn't matter if you scale the inputs before
    # the varimax step or after it.
    if (covar)
    {
        stdmat <- matrix(rep(stds, ncol(x)), ncol = ncol(x))
        x <- x/stdmat
    }

    # Kaiser-normalize the varimax results
    x1 <- diag(1/sqrt(diag(x %*% t(x)))) %*% x
    Q <- x1 * abs(x1)^(m - 1)
    U <- lm.fit(x, Q)$coefficients
    d <- diag(solve(t(U) %*% U))
    U <- U %*% diag(sqrt(d))
    dimnames(U) <- NULL

    # Extra normalization in SPSS formula
    UUinv <- solve(t(U) %*% U)
    C <- diag(1 / sqrt(diag(UUinv)))

    z <- x %*% U %*% solve(C)
    U <- xx$Th %*% U
    #ui <- solve(U)
    #Phi <- ui %*% t(ui)
    # Correlation matrix of factors
    rotation.matrix <- C %*% UUinv %*% t(C)
    dimnames(z) <- dn
    class(z) <- "loadings"
    if (covar)
    {
        # This is what SPSS calls the "Raw" pattern matrix
        z <- z*stdmat
    }
    result <- list(loadings = z, rotmat = U, Phi = rotation.matrix)
    class(result) <- c("psych", "fa")
    return(result)
}


# Use GPArotation package to work out the objective function for a rotated loadings matrix (usually from SPSS)
get.objective.for.loadings <- function(loadings, method, gam = NULL, kappa = NULL)
{
    x <- loadings
    x1 <- x1 <- diag(1/sqrt(diag(x %*% t(x)))) %*% x
    if (method == "varimax")
    {
        objective <- gpa.vgQ.varimax(x1)$f
    } else if (method == "quartimax")
    {
        objective <- gpa.vgQ.quartimax(x1)$f
    } else if (method == "equamax")
    {
        objective <- gpa.vgQ.cf(x1, kappa = ncol(x1)/(2*nrow(x1)))$f
    } else if (method == "oblimin")
    {
        objective <- gpa.vgQ.oblimin(x1, gam = gam)$f
    } else {
        warning(paste0("No objective function for ", method))
        objective <- NaN
    }
    return(objective)
}

sortLoadingsByComponents <- function(rotated.loadings) {
    cs <- diag(t(rotated.loadings) %*% rotated.loadings)
    cs.order <- order(cs, decreasing = TRUE)
    rotated.loadings <- rotated.loadings[, cs.order]
    return(rotated.loadings)
}

# Rotate correspondence analysis so focus.i row or column is along first axis.
# Second axis maximises variance whilst being orthogonal.
#' @importFrom nloptr nloptr nl.jacobian
#' @importFrom GPArotation targetT
#' @importFrom verbs First Last Sum
setFocus <- function(original, focus.i) {

    if (length(original$rownames) <= 2 || length(original$colnames) <= 2)
    {
        warning("Output is one dimensional and focus has no effect.")
        return(list(rowcoord = original$rowcoord,
                    colcoord = original$colcoord,
                    sv = original$sv))
    }

    # Select principal coordinates
    principal <- CANormalization(original, normalization = "Principal")
    coords <- rbind(principal$row.coordinates, principal$column.coordinates)

    # Find the direction of most variance that is perpendicular to the focus direction
    # Minimise negative of length of vector d
    objective <- function(d) {
        return(-sum(d^2))
    }

    grad_objective <- function(d) {
        return(-2 * d)
    }

    # d must be perpendicular to focus direction and on variance ellipsoid
    constraint <- function(d) {
        perp <- d %*% coords[focus.i, ]
        ellipse <- sum((d / original$sv)^2) - 1
        return(rbind(perp, ellipse))
    }

    grad_constraint <- function(d) {
        return(nl.jacobian(d, constraint))
    }

    opt <- nloptr(rep(1, ncol(coords)), eval_f = objective, eval_grad_f = grad_objective, eval_g_eq = constraint,
                  eval_jac_g_eq = grad_constraint, opts = list("algorithm" = "NLOPT_LD_SLSQP", "xtol_rel" = 1e-6))

    # Add new point to coords perpendicular to focus and in plane of most variance
    coords <- rbind(coords, opt$solution / sqrt(Sum(opt$solution^2, remove.missing = FALSE)))

    tgt <- coords
    tgt[, ] <- NA
    tgt[focus.i, ] <- 0
    tgt[focus.i, 1] <- sqrt(Sum(coords[focus.i, ]^2, remove.missing = FALSE))    # rotate focus point to x-axis
    tgt[nrow(tgt), ] <- 0
    tgt[nrow(tgt), 2] <- 1    # rotate max variance dirn (perp to focus point) to y-axis
    rotated <- targetT(coords, Target = tgt, eps = 1e-5, maxit = 10000)

    # Calculate rotated eigenvalues
    old.eigen.mat <- diag(original$sv^2)
    theta <- rotated$Th
    new.eigen.mat <- t(theta) %*% old.eigen.mat %*% theta
    rotated.eigenvalues <- sqrt(diag(new.eigen.mat))

    # Convert back to standard coordinates
    rotated.principal <- list(rowcoord = First(First(rotated$loadings, -1), nrow(original$rowcoord)),
                              colcoord = Last(First(rotated$loadings, -1), nrow(original$colcoord)),
                              sv = rotated.eigenvalues)
    rotated.standard <- CANormalization(rotated.principal, "Inverse")

    return(list(rowcoord = rotated.standard$row.coordinates,
                colcoord = rotated.standard$column.coordinates,
                sv = rotated.eigenvalues))
}
