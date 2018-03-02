#' \code{UMAP}
#' @description Produce a Uniform Manifold Approximation and Projection embedding.
#' @param data A code{\link{data.frame}} or numeric \code{\link{matrix}} containing the data to be
#' analyzed with each case as a row and each variable as a column.
#' @param n.neighbours A coefficient which determines the number of neighboring points used in local
#' approximations of manifold structured.
#' @param min.dist Controls how tightly the embedding is allowed compress points together.
#' @param seed Random seed.
#'
#' @importFrom reticulate import

UMAP <- function(data, n.neighbours = 10, min.dist = 0.1, seed = 1066) {

    set.seed(seed)

    # Convert to numeric matrix
    data <- data.matrix(data)

    output <- list(title = "UMAP", input.data = data)

    # Import and use python module
    umap <- import("umap")
    output$embedding <- umap$UMAP(n_neighbors = as.integer(n.neighbours), min_dist = min.dist)$fit_transform(data)[, 1:2]

    # Ensure first dimension has largest range
    if (range(output$embedding)[2] > range(output$embedding)[1])
        output$embedding[, c(1, 2)] <- output$embedding[, c(2, 1)]

    output$input.is.distance <- FALSE
    class(output) <- c(class(output), "UMAP")
    return(output)
}
