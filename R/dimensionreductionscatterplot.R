#' \code{DimensionReductionScatterplot}
#' @rdname DimensionReduction
#' @export
DimensionReductionScatterplot <- function(algorithm,
                                          data = NULL,
                                          data.groups = NULL,
                                          table = NULL,
                                          raw.table = FALSE,
                                          subset = NULL,
                                          perplexity = 10,
                                          binary = TRUE,
                                          normalization = FALSE) {

    DimensionReduction(algorithm, data = data, data.groups = data.groups, table = table, raw.table = raw.table,
                       subset = subset, perplexity = perplexity, binary = binary, normalization = normalization)
}


#' \code{DimensionReduction}
#' @description Produces as 2-dimensional embedding. Takes either a
#'     \code{\link{data.frame}} of variables and optionally another
#'     \code{\link{vector}} to be used as a grouping variable, or
#'     takes a distance \code{\link{matrix}}.
#' @param algorithm Valid options are \code{"t-SNE"},
#'     \code{"MDS - Metric"}, \code{"MDS - Non-metric"} or
#'     \code{"PCA"}, where the latter does not accept a distance
#'     matrix as input.
#' @param data A \code{\link{data.frame}} with cases by row and
#'     variables by column.
#' @param data.groups A \code{\link{vector}} to be used as a grouping
#'     variable for the embedded \code{data}.
#' @param table A symmetrical distance \code{\link{matrix}} or an
#'     object of class \code{dist} or \code{DistanceMatrix}.
#' @param raw.table If \code{TRUE},
#'     \code{\link[flipTransformations]{ParseUserEnteredTable}} is
#'     called on \code{table} to create a numeric matrix from text.
#' @param subset A logical vector which describes the subset of
#'     \code{data} to be analyzed.  Not used for \code{table} input.
#' @param perplexity The perplexity coefficient which defines the
#'     extent of the locality of the dimension reduction. Used only
#'     when \code{algorithm} is \code{t-SNE}.
#' @param binary If \code{TRUE}, unordered factors are converted to
#'     dummy variables. Otherwise, they are treated as sequential
#'     integers. Ignored if input is provided by \code{table}.
#' @param normalization If \code{data} is supplied, whether to
#'     standardize the data so each variable has a mean of 0 and
#'     standard deviation of 1.
#'
#' @details For \code{data} input, all algorithms apart from \code{PCA} remove duplicated data and
#' any case with \code{NA} is ignored by all algorithms.
#'
#' @importFrom flipTransformations ParseUserEnteredTable AsNumeric StandardizeData
#' @importFrom stats dist
#' @export

DimensionReduction <- function(algorithm,
                                        data = NULL,
                                        data.groups = NULL,
                                        table = NULL,
                                        raw.table = FALSE,
                                        subset = NULL,
                                        perplexity = 10,
                                        binary = TRUE,
                                        normalization = FALSE) {

    if (!xor(is.null(data), is.null(table)))
        stop("One and only one of data and table must be supplied.")
    if (!is.null(data.groups) && length(data.groups) != nrow(data))
        stop("Lengths of data and data.groups must the the same.")
    if (!is.null(data))
        data <- AsNumeric(ProcessQVariables(data), binary = binary, remove.first = TRUE)

    if (algorithm == "PCA")
    {
        if (is.null(data))
            stop("PCA requires variables as input but a distance matrix was supplied.")
        pca <- PrincipalComponentsAnalysis(data = data, subset = subset,
                                           missing = "Exclude cases with missing data",
                                           use.correlation = normalization,
                                           rotation = "none",
                                           select.n.rule = "Number of factors",
                                           n.factors = 2,
                                           print.type = "2d",
                                           data.groups = data.groups)
        return(pca)
    }


    if (!is.null(data)) {
        input.distance <- FALSE

        if (is.null(subset) || (length(subset) == 1 && subset == TRUE))
            subset <- rep(TRUE, nrow(data))
        else if (length(subset) != nrow(data))
            stop("Input data and subset must be same length.")

        # Remove cases with missing data and duplicates
        used.subset <- subset & complete.cases(data) & !duplicated(data)
        processed.data <- data[used.subset, , drop = FALSE]
        if (normalization)
            processed.data <- data.frame(StandardizeData(processed.data, method = "Range [0,1]"))
        if (algorithm != "t-SNE" && is.null(table))
            distance.matrix <- dist(processed.data)
    }
    else
    {
        input.distance <- TRUE
        if (!is.null(subset) && !all(subset))
            warning("Subset will be ignored for distance matrix input.")

        distance.matrix <- if (raw.table) {
            ParseUserEnteredTable(table)
        } else {
            table
        }

        cls <- class(distance.matrix)
        if (cls == "DistanceMatrix")
            distance.matrix <- distance.matrix$distance
        if (cls != "dist" && cls != "DistanceMatrix") {
            if (cls != "matrix" || !is.numeric(distance.matrix) || !isSymmetric(distance.matrix))
                stop("Distance matrix must be symmetrical and numeric.")
            diag(distance.matrix)[is.na(diag(distance.matrix))] <- 0
            if (any(diag(distance.matrix) != 0))
                warning("Non-zero values along diagonal of distance matrix will be ignored.")
            diag(distance.matrix) <- 0
            if (any(distance.matrix < 0))
                stop("Distance matrix must not contain negative values.")
        }
    }

    if (algorithm == "t-SNE")
    {
        result <- tSNE(data = if (input.distance) distance.matrix else processed.data,
                       is.distance = input.distance,
                       perplexity = perplexity)
    }
    else if (algorithm == "MDS - Metric")
    {
        result <- MultiDimesnsionalScaling(distance.matrix = distance.matrix, metric = TRUE)
    }
    else if (algorithm == "MDS - Non-metric")
    {
        result <- MultiDimesnsionalScaling(distance.matrix = distance.matrix, metric = FALSE)
    }
    else
        stop("Algorithm not recognized.")

    if (!input.distance) {
        result$input.is.distance <- FALSE     # overwrite for MDS from data - determines output chart type
        result$used.subset <- used.subset

        # Expand the output to be same size as data, filling with NA by default
        expanded.embedding <- matrix(nrow = length(subset), ncol = 2)
        expanded.embedding[used.subset] <- result$embedding
        result$embedding <- expanded.embedding

        if (algorithm == "t-SNE") {
            # t-SNE from data - expand input to be same length as original data
            expanded.input <- matrix(nrow = length(subset), ncol = ncol(processed.data))
            expanded.input[used.subset] <- as.matrix(processed.data)
            result$input.data <- expanded.input
        }
    }

    result$data.groups <- data.groups
    result$normalized <- normalization
    class(result) <- c("2Dreduction", class(result))
    return(result)
}


#' \code{fitted.2Dreduction}
#' @param object Object of class \code{"2Dreduction"}.
#' @param ... Not used.
#' @export
fitted.2Dreduction <- function(object, ...)
{
    return(object$embedding)
}


# Converts a PCA output of class flipFactorAnalysis to an object of class 2Dreduction
convertFactorAnalysisTo2D <- function(x) {

    if (ncol(x$loadings) < 2)
        stop("There must be at least 2 components to produce 2-D output.")
    if (!is.null(x$data.groups) && length(x$data.groups) != nrow(x$scores))
        stop("Lengths of data and data.groups must the the same.")

    used.subset <- !is.na(x$scores[, 1])
    input.data <- x$original.data
    if (x$use.correlation) {
        # standardize the used.subset and re-expand
        data.subset <- input.data[used.subset, , drop = FALSE]
        data.subset <- StandardizeData(data.subset, method = "z-scores")
        input.data <- matrix(nrow = nrow(x$original.data), ncol = ncol(x$original.data))
        input.data[used.subset] <- data.subset
    }

    output <- list(embedding = x$scores[, 1:2],
                   data.groups = x$data.groups,
                   input.data = input.data,
                   used.subset = used.subset,
                   normalized = x$use.correlation,
                   input.is.distance = FALSE,
                   title = "PCA")
    class(output) <- c("2Dreduction", "flipFactorAnalysis")
    return(output)
}


#' \code{print.2Dreduction}
#' @description If 2Dreduction object is created from distance matrix, then print as labelled scatterplot.
#' If created from a \code{\link{data.frame}} of variables, print as a scatterplot of grouped cases.
#' @param x Object of class \code{"2Dreduction"}.
#' @param ... Not used.
#' @importFrom flipStandardCharts Chart
#' @importFrom grDevices rgb
#' @importFrom class knn.cv
#' @importFrom flipU IsCount
#' @export
#' @method print 2Dreduction
print.2Dreduction <- function(x, ...) {

    if (x$input.is.distance) {
        chart <- LabeledScatter(x$embedding[, 1], x$embedding[, 2],
                       label = x$label,
                       fixed.aspect = TRUE,
                       title = x$title,
                       x.title = "Dimension 1",
                       y.title = "Dimension 2",
                       point.radius = 4,
                       labels.font.size = 14,
                       x.title.font.size = 14,
                       y.title.font.size = 14)
    }

    else {
        # Scatterplot with groups
        scatter.group.indices <- ""
        scatter.group.labels <- ""
        legend <- TRUE
        colors <- "Default colors"
        title <- x$title
        set.seed(1066)

        embedding <- x$embedding[x$used.subset, ]
        groups <- x$data.groups[x$used.subset]

        if (!is.null(groups)) {

            if (is.factor(groups)) {
                scatter.group.indices <- paste(as.numeric(groups), collapse = ", ")
                scatter.group.labels <- paste(levels(groups), collapse = ", ")
                nearest <- knn.cv(train = embedding, cl = groups, k = 1)
                same.category <- sum(nearest == factor(groups, ordered = FALSE)) / length(groups)
                title <- paste0(title, " - Nearest neighbor accuracy: ", sprintf("%1.2f%%", 100 * same.category))
            }
            else if (all(groups == floor(groups))) {
                unique.labels <- sort(unique(groups))
                indices <- match(groups, unique.labels)
                scatter.group.labels <- paste(unique.labels, collapse = ", ")
                scatter.group.indices <- paste(indices, collapse = ", ")
                colors <- "Reds, light to dark"
                nearest <- knn.cv(train = embedding, cl = groups, k = 1)
                same.category <- sum(nearest == groups) / length(groups)
                title <- paste0(title, ". Nearest neighbor accuracy: ", sprintf("%1.2f%%", 100 * same.category))
            }
            else {       # numeric: create 20 buckets and treat as factors
                groups <- cut(groups, 20)
                scatter.group.indices <- paste(as.numeric(groups), collapse = ", ")
                levels(groups) <- sub("[^,]*,([^]]*)\\]", "\\1", levels(groups))
                scatter.group.labels <- paste(levels(groups), collapse = ", ")
                colors <- "Reds, light to dark"
            }
        }

        chart <- Chart(y = embedding,
                       type = "Scatterplot",
                       transpose = FALSE,
                       title = title,
                       title.font.family = NULL,
                       title.font.color = NULL,
                       title.font.size = 16,
                       colors = colors,
                       colors.reverse = FALSE,
                       opacity = NULL,
                       background.fill.opacity = 0,
                       charting.area.fill.opacity = 0,
                       legend.fill.opacity = 0,
                       legend.show = legend,
                       legend.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                       legend.border.color = rgb(44, 44, 44, maxColorValue = 255),
                       legend.border.line.width = 0,
                       legend.font.color = NULL,
                       legend.font.family = NULL,
                       legend.font.size = 10,
                       legend.position = "right",
                       legend.ascending = TRUE,
                       margin.top = NULL,
                       margin.bottom = NULL,
                       margin.left = NULL,
                       margin.right = NULL,
                       margin.inner.pad = NULL,
                       y.title = "Dimension 2",
                       y.title.font.color = NULL,
                       y.title.font.family = NULL,
                       y.title.font.size = 12,
                       y.line.width = 0,
                       y.line.color = rgb(0, 0, 0, maxColorValue = 255),
                       y.tick.marks = "",
                       y.tick.mark.length = 5,
                       y.bounds.minimum = NULL,
                       y.bounds.maximum = NULL,
                       y.tick.distance = NULL,
                       y.zero.line.width = 0,
                       y.zero.line.color = rgb(44, 44, 44, maxColorValue = 255),
                       y.position = "left",
                       y.data.reversed = FALSE,
                       y.grid.width = 1,
                       y.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                       y.tick.show = TRUE,
                       y.tick.suffix = "",
                       y.tick.prefix = "",
                       y.tick.decimals = NULL,
                       y.tick.format.manual = "",
                       y.hovertext.decimals = NULL,
                       y.hovertext.format.manual = "",
                       y.tick.angle = NULL,
                       y.tick.font.color = NULL,
                       y.tick.font.family = NULL,
                       y.tick.font.size = 10,
                       x.title = "Dimension 1",
                       x.title.font.color = NULL,
                       x.title.font.family = NULL,
                       x.title.font.size = 12,
                       x.line.width = 0,
                       x.line.color = rgb(0, 0, 0, maxColorValue = 255),
                       x.tick.marks = "",
                       x.tick.mark.length = 5,
                       x.bounds.minimum = NULL,
                       x.bounds.maximum = NULL,
                       x.tick.distance = NULL,
                       x.zero.line.width = 0,
                       x.zero.line.color = rgb(44, 44, 44, maxColorValue = 255),
                       x.position = "bottom",
                       x.data.reversed = FALSE,
                       x.grid.width = 1,
                       x.grid.color = rgb(225, 225, 225, maxColorValue = 255),
                       x.tick.show = TRUE,
                       x.tick.suffix = "",
                       x.tick.prefix = "",
                       x.tick.decimals = NULL,
                       x.tick.format.manual = "",
                       x.hovertext.decimals = NULL,
                       x.hovertext.format.manual = "",
                       x.tick.angle = NULL,
                       x.tick.font.color = NULL,
                       x.tick.font.family = NULL,
                       x.tick.font.size = 10,
                       x.tick.label.autoformat = TRUE,
                       series.marker.show = "automatic",
                       series.marker.colors = NULL,
                       series.marker.colors.reverse = FALSE,
                       series.marker.opacity = 1,
                       series.marker.size = 6,
                       series.marker.border.width = 1,
                       series.marker.border.colors = NULL,
                       series.marker.border.colors.reverse = FALSE,
                       series.marker.border.opacity = 1,
                       series.line.width = 0,
                       series.line.colors = NULL,
                       series.line.colors.reverse = FALSE,
                       series.line.opacity = 1,
                       tooltip.show = TRUE,
                       modebar.show = FALSE,
                       global.font.family = "Arial",
                       global.font.color = rgb(44, 44, 44, maxColorValue=255),
                       rows.to.ignore = "",
                       cols.to.ignore = "",
                       bar.gap = 0.15,
                       data.label.show = FALSE,  # was NULL for Labeled Scatterplot
                       data.label.font.family = NULL,
                       data.label.font.size = 10,
                       data.label.font.color = NULL,
                       data.label.decimals = 0,
                       data.label.prefix = "",
                       data.label.suffix = "",
                       data.label.threshold = NULL,
                       data.label.position = "top middle",
                       pie.order = "initial",
                       pie.groups.order = "initial",
                       pie.subslice.colors = NULL,
                       pie.subslice.colors.reverse = FALSE,
                       pie.subslice.colors.repeat = TRUE,
                       pie.border.color = rgb(255, 255, 255, maxColorValue = 255),
                       pie.inner.radius = NULL,
                       pie.show.percentages = FALSE,
                       z.title = "",
                       scatter.group.indices = scatter.group.indices,
                       scatter.group.labels = scatter.group.labels,
                       us.date.format = NULL)
    }

    print(chart)
}


