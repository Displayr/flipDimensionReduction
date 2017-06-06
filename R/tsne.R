#' \code{tSNE}
#' @description Produce a t-Distributed Stochastic Neighbour Embedding.
#' @param data A \code{\link{data.frame}} or \code{\link{matrix}} containing the data to be analyzed.
#' @param data.groups A \code{\link{factor}} used to group cases. Ignored if \code{is.distance} is TRUE.
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
#' @details If input is not a distance matrix, any case with missing data or a missing label is ignored.
#' @importFrom Rtsne Rtsne
#' @importFrom flipTransformations AsNumeric
#' @importFrom stats complete.cases
#' @importFrom flipFormat Labels
#' @export

tSNE <- function(data, subset = NULL, data.groups = NULL, is.distance = FALSE,
                 binary = TRUE, perplexity = 10, seed = 1066) {

    if (is.distance && !is.null(data.groups))
        warning("Labels will be taken from distance matrix columns. Separate data.groups will be ignored.")
    if (is.distance && !is.null(subset))
        warning("Subset will be ignored for distance matrix.")
    if (!is.distance && !is.null(data.groups) && nrow(data) != length(data.groups))
        stop("Input data and data.groups must be same length.")

    output <- list(title = ifelse(is.distance || is.null(data.groups),
                                  "t-SNE",
                                  paste("t-SNE", "categories:", Labels(data.groups))))

    if (!is.distance)
    {
        # Convert dates to factors, retain subset only
        data <- ProcessQVariables(data)
        data.groups <- ProcessQVariables(data.groups)
        if (!is.null(subset)) {
            if (length(subset) == 1 && subset == TRUE)
                subset <- rep(TRUE, nrow(data))
            if (length(subset) != nrow(data))
                stop("Input data and subset must be same length.")
            data <- data[subset, ]
            data.groups <- data.groups[subset]
        }

        # Remove cases with incomplete data or missing labels
        complete <- complete.cases(data)
        if (!is.null(data.groups))
            complete <- complete & complete.cases(data.groups)
        data.groups <- data.groups[complete]

        # Convert unordered factors to binary variables
        data <- AsNumeric(data[complete, ], binary = binary, remove.first = TRUE)

        # Remove duplicates
        duplicates <- duplicated(data)
        data <- data[!duplicates, ]
        data.groups <- data.groups[!duplicates]
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
    }

    output$data.groups <- data.groups
    output$is.distance <- is.distance
    class(output) <- "tSNE"
    return(output)

}

#' @export
print.tSNE <- function(x, ...) {
    class(x) <- c("2Dreduction", "tSNE")
    print(x)
}
