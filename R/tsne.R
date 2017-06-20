#' \code{tSNE}
#' @description Produce a t-Distributed Stochastic Neighbour Embedding.
#' @param data A \code{\link{data.frame}} or \code{\link{matrix}} containing the data to be analyzed.
#' @param is.distance Whether \code{data} are a distance \code{\link{matrix}} between points or a
#' \code{\link{data.frame}} of cases by row and features by column.
#' @param subset A logical vector which describes the subset of \code{data} to be analyzed.
#' Ignored if \code{is.distance} is TRUE.
#' @param perplexity The perplexity coefficient which defines the extent of the locality
#' of the dimension reduction.
#' @param binary If \code{TRUE}, unordered factors are converted to dummy variables. Otherwise,
#' they are treated as sequential integers. Ignored if \code{is.distance} is FALSE.
#' @param seed Random seed.
#'
#' @details If input is not a distance matrix, cases with missing data and duplicates are ignored.
#' @importFrom Rtsne Rtsne
#' @importFrom flipTransformations AsNumeric
#' @importFrom stats complete.cases
#' @importFrom flipFormat Labels
#' @export

tSNE <- function(data, subset = NULL, is.distance = FALSE,
                 binary = TRUE, perplexity = 10, seed = 1066) {

    if (is.distance && !is.null(subset) && !all(subset))
        warning("Subset will be ignored for distance matrix.")

    output <- list(title = "t-SNE")

    if (!is.distance)
    {
        # Convert unordered factors to binary variables and dates to factors
        data <- AsNumeric(ProcessQVariables(data), binary = binary, remove.first = TRUE)

        # Identify subset, complete cases, not duplicates
        if (is.null(subset) || (length(subset) == 1 && subset == TRUE))
            subset <- rep(TRUE, nrow(data))
        if (length(subset) != nrow(data))
            stop("Input data and subset must be same length.")

        subset <- subset & complete.cases(data) & !duplicated(data)
        data <- data[subset, ]
    }

    set.seed(seed)

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

    if (!is.distance) {
        # Expand the output to be same size as data, filling with NA by default
        expanded <- matrix(nrow = length(subset), ncol = 2)
        expanded[subset] <- output$embedding
        output$embedding <- expanded
    }

    output$input.data <- data
    output$print.as.distance <- is.distance
    class(output) <- c("2Dreduction", "tSNE")
    return(output)
}




