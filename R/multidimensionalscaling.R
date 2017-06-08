#' \code{MultiDimesnsionalScaling}
#' @description Perform Multidimesnsional Scaling to produce a 2-dimensional embedding of
#' a distance matrix.
#' @param distance.matrix A \code{\link{matrix}} of distances between points.
#' @param metric A \code{\link{factor}} used to group cases. Ignored if \code{is.distance} is TRUE.
#' @importFrom stats cmdscale
#' @importFrom MASS isoMDS
#' @export

MultiDimesnsionalScaling <- function(distance.matrix, metric = TRUE) {

    mds <- if (metric) {
        cmdscale(distance.matrix)
    } else {
        isoMDS(distance.matrix)$points
    }

    output <- list(embedding = mds[, 1:2])

    output$label <- if (class(distance.matrix) == "dist") {
        attr(distance.matrix, "Labels")
    }
    else
    {
        colnames(distance.matrix)
    }

    output$is.distance <- TRUE
    output$title <- ifelse(metric, "MDS - Metric", "MDS - Non-metric")
    class(output) <- "MDS"
    return(output)
}

#' \code{print.MDS}
#' @param x Object of class \code{"MDS"}.
#' @param ... Not used.
#' @export
print.MDS <- function(x, ...) {
    class(x) <- c("2Dreduction", "MDS")
    print(x)
}
