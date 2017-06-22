#' \code{MultiDimesnsionalScaling}
#' @description Perform Multidimesnsional Scaling to produce a 2-dimensional embedding of
#' a distance matrix.
#' @param distance.matrix A \code{\link{matrix}} of distances between points.
#' @param metric Whether to use metric or non-metric scaling.
#' @importFrom stats cmdscale
#' @importFrom MASS isoMDS
#' @export

MultiDimesnsionalScaling <- function(distance.matrix, metric = TRUE) {

    mds <- if (metric) {
        cmdscale(distance.matrix)
    } else {
        isoMDS(distance.matrix, trace = FALSE)$points
    }

    output <- list(embedding = mds[, 1:2])

    output$label <- if (class(distance.matrix) == "dist") {
        attr(distance.matrix, "Labels")
    }
    else
    {
        colnames(distance.matrix)
    }

    output$input.is.distance <- TRUE
    output$input.data <- distance.matrix
    output$title <- ifelse(metric, "MDS (Metric)", "MDS (Non-metric)")
    class(output) <- c("2Dreduction", "MDS")
    return(output)
}

