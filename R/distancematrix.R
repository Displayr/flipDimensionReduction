#' \code{DistanceMatrix}
#' @description Compute a distance (dissimilarity) or similarity matrix between variables or cases.
#' @param input.data A \code{\link{data.frame}} with cases as rows and variables as columns.
#' @param compare Whether \code{"Variables"} or \code{"Cases"} are to be compared.
#' @param case.labels If \code{"compare"} is set to \code{"Cases"}, a list of labels for each case. If not supplied
#' the indices are used.
#' @param variable.names Whether to display variable names or labels.
#' @param binary Whether to convert categorical variables to binary.
#' @param measure Whether to measure \code{"Similarities"} or \code{"Dissimilarities"}.
#' @param similarity.measure How to calculate similarities. Options are \code{"Correlation"}
#' or \code{"Cosine"}.
#' @param distance.measure The method for mesuring distances. Options are \code{"Euclidean"},
#' \code{"Squared Euclidean"}, \code{"Maximum"}, \code{"Manhattan"} and \code{"Minkowski"}.
#' @param minkowski The power used by \code{"Minkowski"} \code{"distance.measure"}.
#' @param standardization The method used to standardize the data. Options are \code{"None"},
#' \code{"z-scores"}, \code{"Range [-1,1]"}, \code{"Range [0,1]"}, \code{"Mean of 1"} and
#' \code{"Standard deviation of 1"}.
#' @param standardize.by Whether to perform \code{"standardization"} by \code{"Variable"} or
#' by \code{"Case"}.
#' @param measure.transformation How to transform the measure. Options are \code{"None"},
#' \code{"Absolute values"}, \code{"Reverse sign"}, and \code{"Range [0,1]"}.
#' @param show.cell.values Whether to show cell values on heatmap output. Choices are
#' \code{"Automatic"}, \code{"Yes"} and \code{"No"}.
#' @param show.row.labels \code{"Yes"} or \code{"No"}.
#' @param show.column.labels \code{"Yes"} or \code{"No"}.
#' @param subset An optional vector specifying a subset of cases to be used.
#' @param weights An optional vector of sampling weights.
#'
#' @importFrom flipTransformations AsNumeric ProcessQVariables
#' @importFrom flipStatistics CosineSimilarities
#' @importFrom weights wtd.cors
#' @export
DistanceMatrix <- function(input.data,
                           compare = "Variables",
                           case.labels = NULL,
                           variable.names = FALSE,
                           binary = FALSE,
                           measure = "Dissimilarities",
                           similarity.measure = "Correlation",
                           distance.measure = "Euclidian",
                           minkowski = 2,
                           standardization = "None",
                           standardize.by = "Case",
                           measure.transformation = "None",
                           show.cell.values = "Automatic",
                           show.row.labels = "Yes",
                           show.column.labels = "Yes",
                           subset = NULL,
                           weights = NULL) {

    dat <- AsNumeric(ProcessQVariables(input.data), binary = binary, remove.first = FALSE)

    if (is.null(subset) || (length(subset) == 1 && subset == TRUE))
        subset <- rep(TRUE, nrow(input.data))

    if (compare == "Cases") {
        dat <- dat[subset, , drop = FALSE]
        if (nrow(dat) > 100)
            stop("There are more than 100 cases to compare. Apply a filter to reduce the number of cases.")
        if (!is.null(weights))
            warning("Weights applied to this item have been ignored as they are not applicable when cases are compared.")
        if (length(case.labels) > 1)
            rownames(dat) <- case.labels[subset]
    } else {
        if (ncol(dat) < 2)
            stop("There needs to be two or more variables to compare, with the currently selected options.")

        # Changing names to labels.
        if (!variable.names)
            names(dat) <- ExtractCommonPrefix(Labels(dat))$shortened.labels

        dat <- dat[subset, , drop = FALSE]

        # Weight
        wgt <- if (is.null(weights)) {
            rep(1, nrow(dat))
        } else {
            weights
        }
        wgt <- wgt[subset]
    }

    # Filtering out missing values.
    ind <- complete.cases(dat)
    dat.matrix <- data.matrix(dat[ind, , drop = FALSE])
    if (compare != "Cases")
        wgt <- wgt[ind]

    # Standardization
    want.transposed <- xor(compare == "Cases", measure == "Dissimilarities")
    dat.matrix <- if (want.transposed) {
        if (standardization == "None") {
            t(dat.matrix)
        } else if (standardize.by == "Case") {
            StandardizeData(t(dat.matrix), standardization, no.variation = "ignore", mean.zero = "ignore")
        } else {
            t(StandardizeData(dat.matrix, standardization))
        }
    } else {
        if (standardization == "None") {
            dat.matrix
        } else if (standardize.by == "Case") {
            t(StandardizeData(t(dat.matrix), standardization, no.variation = "ignore", mean.zero = "ignore"))
        } else {
            StandardizeData(dat.matrix, standardization)
        }
    }

    # Compute measures
    distance.matrix <- if (measure == "Dissimilarities") {
        # Weighting the data (note that this method should not be applied to other problems, as it is unique to this one).
        if (compare != "Cases")
            dat.matrix <- sweep(dat.matrix, 2, wgt, "*")

        p <- if (distance.measure == "Minkowski") minkowski else 2
        if (p <= 0 || p > 100)
            stop(paste0("Minkowski power must be positive and less than 100 but is ", p, "."))

        method <- if (distance.measure == "Squared Euclidean") "euclidean" else tolower(distance.measure)

        mat <- as.matrix(dist(dat.matrix, method = method, p = p, diag = TRUE, upper = TRUE))
        if (distance.measure == "Squared Euclidean")
            mat <- mat * mat
        mat

    } else if (similarity.measure == "Correlation") {
        if (compare == "Cases")
            cor(dat.matrix)
        else
            wtd.cors(dat.matrix, weight = wgt)

    } else if (similarity.measure == "Cosine") {
        if (compare == "Cases")
            CosineSimilarities(dat.matrix)
        else
            CosineSimilarities(dat.matrix, weight = wgt)
    } else
        stop(paste("Unhandled similarity measure:", similarity.measure))

    # Measure transformations
    if (measure.transformation == "Absolute values") {
        distance.matrix <- abs(distance.matrix)

    } else if (measure.transformation == "Reverse sign") {
        distance.matrix <- -distance.matrix

    } else if (measure.transformation == "Range [0,1]") {
        tri <- distance.matrix[lower.tri(distance.matrix)]
        min.val <- min(tri, na.rm = TRUE)
        max.val <- max(tri, na.rm = TRUE)
        if (max.val > min.val) {
            diagonal <- diag(distance.matrix)
            distance.matrix <- (distance.matrix - min.val) / (max.val - min.val)
            diag(distance.matrix) <- diagonal
        } else
            stop("The measures are constant and cannot be transformed to the range [0,1].")
    }

    output <- list(distance = distance.matrix, show.cell.values = show.cell.values,
                   show.row.labels = show.row.labels, show.column.labels = show.column.labels,
                   measure = measure)
    class(output) <- "DistanceMatrix"
    output
}

#' @export
ExtractChartData.DistanceMatrix <- function(x)
{
    return(x$distance)
}

#' \code{print.DistanceMatrix}
#' @description Display distance matrix as a heatmap.
#' @param x An object of class \code{"DistanceMatrix"}.
#' @param ... Not used.
#' @importFrom flipFormat FormatAsReal
#' @export
#' @method print DistanceMatrix
print.DistanceMatrix <- function(x, ...)
{
    d <- x$distance
    n <- ncol(d)
    show.cell.values <- x$show.cell.values
    cellnote <- matrix("", n, n)
    for (i in 1:n)
        for (j in 1:n)
            cellnote[i, j] <- FormatAsReal(d[i, j], decimals = 2)
    show.cellnote.in.cell <- (n <= 10 && show.cell.values != "No") || show.cell.values == "Yes"
    show.x.axes.labels <- x$show.column.labels == "Yes"
    show.y.axes.labels <- x$show.row.labels == "Yes"

    if (x$measure == "Similarities") {
        # Values may be slightly larger than 1 due to numerical errors
        d[d > 1] <- 1
        d[d < -1] <- -1
        colours <- "RdBu"
        colour.range <- c(-1, 1)
    } else {
        colours <- "Blues"
        colour.range <- NULL
    }

    dm <- rhtmlHeatmap::Heatmap(d,
                          Rowv = NULL,
                          Colv = NULL,
                          cellnote = cellnote,
                          colors = colours,
                          show_cellnote_in_cell = show.cellnote.in.cell,
                          xaxis_location = "bottom",
                          yaxis_location = "left",
                          lower_triangle = TRUE,
                          cexRow = 0.79,
                          xaxis_hidden = !show.x.axes.labels,
                          yaxis_hidden = !show.y.axes.labels,
                          color_range = colour.range)
    print(dm)
}

