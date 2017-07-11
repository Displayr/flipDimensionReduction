#' \code{tSNE}
#' @description Produce a t-Distributed Stochastic Neighbour Embedding.
#' @param data A numeric \code{\link{data.frame}} or \code{\link{matrix}} containing the data to be analyzed.
#' @param is.distance Whether \code{data} are a distance \code{\link{matrix}} between points or a
#' numeric \code{\link{data.frame}} of cases by row and features by column.
#' @param perplexity The perplexity coefficient which defines the extent of the locality
#' of the dimension reduction.
#' @param seed Random seed.
#'
#' @details If input is not a distance matrix, there must be no missing data or duplicates.
#' @importFrom Rtsne Rtsne
#' @importFrom flipTransformations AsNumeric
#' @importFrom stats complete.cases
#' @importFrom flipFormat Labels
#' @export

tSNE <- function(data, is.distance = FALSE, perplexity = 10, seed = 1066) {

    if (!is.distance && !all(sapply(data, is.numeric)))
        stop("A numeric data.frame must be supplied.")

    set.seed(seed)

    output <- list(title = "t-SNE", input.data = data)
    output$embedding <- Rtsne(data, perplexity = perplexity, is_distance = is.distance)$Y

    if (is.distance) {
        output$label <- if (class(data) == "dist") {
            attr(data, "Labels")
        }
        else
        {
            colnames(data)
        }
        rownames(output$embedding) <- output$label
    }

    # Ensure first dimension has largest range
    if (range(output$embedding)[2] > range(output$embedding)[1])
        output$embedding[, c(1, 2)] <- output$embedding[, c(2, 1)]

    output$input.is.distance <- is.distance
    class(output) <- "tSNE"
    return(output)
}

