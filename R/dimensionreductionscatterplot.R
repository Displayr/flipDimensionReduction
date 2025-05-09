#' \code{DimensionReductionScatterplot}
#' @inherit DimensionReduction
#' @export
DimensionReductionScatterplot <- function(algorithm,
                                          data = NULL,
                                          data.groups = NULL,
                                          table = NULL,
                                          raw.table = FALSE,
                                          subset = NULL,
                                          perplexity = 10,
                                          binary = TRUE,
                                          normalization = FALSE,
                                          seed = 1066,
                                          use.combined.scatter = FALSE,
                                          ...)
{

    DimensionReduction(algorithm, data = data, data.groups = data.groups, table = table, raw.table = raw.table,
                       subset = subset, perplexity = perplexity, binary = binary, normalization = normalization,
                       seed = seed, use.combined.scatter = use.combined.scatter, ...)
}


#' \code{DimensionReduction}
#' @description Produces as 2-dimensional embedding. Takes either a
#'     \code{\link{data.frame}} of variables and optionally another
#'     \code{\link{vector}} to be used as a grouping variable, or
#'     takes a distance \code{\link{matrix}}.
#' @param algorithm Valid options are \code{"t-SNE"},
#'     \code{"MDS - Metric"}, \code{"MDS - Non-metric"}, or
#'     \code{"PCA"}. Note that \code{'PCA'} does not accept a
#'     distance matrix as input.
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
#'     dummy variables. Otherwise, their value attributes/levels are
#'     used to coerce each factor to a single numeric variable. See
#'     the Details of \code{\link{AsNumeric}}.
#' @param normalization If \code{data} is supplied, whether to
#'     standardize the data so each variable has a mean of 0 and
#'     standard deviation of 1.
#' @param seed Random seed. Used only when \code{algorithm} is
#'     \code{"t-SNE"}.
#' @param print.type Specifies output produced if algorithm is PCA.
#' @param use.combined.scatter Draw scatterplots using rhtmlCombinedScatter.
#' @param ... Other parameters passed to
#'     \link{PrincipalComponentsAnalysis}.
#' @details For \code{data} input, all algorithms apart from
#'     \code{PCA} remove duplicated data and any case with \code{NA}
#'     is ignored by all algorithms.
#' @importFrom flipU CopyAttributes StopForUserError
#' @importFrom flipData SplitFormQuestions
#' @importFrom flipTransformations ParseUserEnteredTable AsNumeric
#'     StandardizeData
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
                               normalization = FALSE,
                               seed = 1066,
                               print.type = "2d",
                               use.combined.scatter = FALSE,
                               ...)
{
    data.from.dropbox <- !is.null(data) && !is.data.frame(data) && is.list(data)
    if (data.from.dropbox)
    {
        show.labels <- list(...)[["show.labels"]]
        if (is.null(show.labels))
            show.labels <- TRUE
        ## Need to call AsNumeric() on multi variable sets before
        ## SplitFormQuestions(), which drops the attributes (variablevalues, etc.)
        ## needed by numbersFromCategoricalVariableSets()
        if (!binary)
            data <- lapply(data, function(x) CopyAttributes(AsNumeric(x, binary = FALSE), x))
        data <- SplitFormQuestions(data, show.labels)
    }

    if (!xor(is.null(data), is.null(table)))
        StopForUserError("One and only one of data and table must be supplied.")
    if (!is.null(data.groups) && length(data.groups) != nrow(data))
        StopForUserError("Lengths of data and data.groups must the the same.")
    if (!is.null(data) && !(data.from.dropbox && !binary))
        data <- AsNumeric(ProcessQVariables(data), binary = binary, remove.first = TRUE)

    if (algorithm == "PCA")
    {
        if (is.null(data))
            StopForUserError("PCA requires variables as input but a distance matrix was supplied.")
        pca <- PrincipalComponentsAnalysis(data = data,
                                           subset = subset,
                                           use.correlation = normalization,
                                           data.groups = data.groups,
                                           print.type = print.type,
                                           use.combined.scatter = use.combined.scatter,
                                           ...)
        return(pca)
    }


    if (!is.null(data)) {
        input.distance <- FALSE

        if (is.null(subset) || (length(subset) == 1 && subset == TRUE))
            subset <- rep(TRUE, nrow(data))
        else if (length(subset) != nrow(data))
            StopForUserError("Input data and subset must be same length.")

        if (normalization)
            data <- data.frame(StandardizeData(data, method = "Range [0,1]"))
        # Remove cases with missing data and duplicates
        used.subset <- subset & complete.cases(data) & !duplicated(data)
        processed.data <- data[used.subset, , drop = FALSE]
        # Normalization may cause duplicates so is done before removing duplicates
        # Renormalize after subset, since subset may remove extreme values
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

            # Always expect row and column names
            as.matrix(ParseUserEnteredTable(table, want.data.frame = TRUE,
            want.col.names = TRUE, want.row.names = TRUE))

        } else {
            table
        }

        cls <- class(distance.matrix)
        if ("DistanceMatrix" %in% cls)
            distance.matrix <- distance.matrix$distance
        if (!("dist" %in% cls) && !("DistanceMatrix" %in% cls)) {
            if (!("matrix" %in% cls) || !is.numeric(distance.matrix) || !isSymmetric(distance.matrix))
                StopForUserError("Distance matrix must be symmetrical and numeric.")
            diag(distance.matrix)[is.na(diag(distance.matrix))] <- 0
            if (any(diag(distance.matrix) != 0))
                warning("Non-zero values along diagonal of distance matrix will be ignored.")
            diag(distance.matrix) <- 0
            if (any(distance.matrix < 0))
                StopForUserError("Distance matrix must not contain negative values.")
        }
    }

    if (algorithm == "t-SNE")
    {
        result <- tSNE(data = if (input.distance) distance.matrix else processed.data,
                       is.distance = input.distance,
                       perplexity = perplexity,
                       seed = seed)
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
        StopForUserError("Algorithm not recognized.")

    if (!input.distance) {
        result$input.is.distance <- FALSE     # overwrite for MDS from data - determines output chart type
        result$used.subset <- used.subset

        # Expand the output to be same size as data, filling with NA by default
        expanded.embedding <- matrix(nrow = length(subset), ncol = 2)
        expanded.embedding[used.subset] <- result$embedding
        if (!is.null(rownames(data)))
            rownames(expanded.embedding) <- rownames(data)
        result$embedding <- expanded.embedding

        if (algorithm == "t-SNE") {
            # expand output to be same length as original data (not distance matrix).
            expanded.input <- matrix(nrow = length(subset), ncol = ncol(processed.data))
            expanded.input[used.subset] <- as.matrix(processed.data)
            result$input.data <- expanded.input
        }
    }

    result$data.groups <- data.groups
    result$normalized <- normalization
    result$use.combined.scatter <- use.combined.scatter
    class(result) <- c("2Dreduction", "visualization-selector", class(result))
    attr(result, "ChartData") <- ExtractChartData(result)
    attr(result, "ChartType") <- "X Y Scatter"
    return(result)
}

#' @export
ExtractChartData.2Dreduction <- function(x)
{
    data <- x$embedding[,1:2]
    colnames(data) <- paste("Dimension", 1:2)
    if (!is.null(x$data.groups))
        data <- data.frame(data, Group = x$data.groups, stringsAsFactors = FALSE,
                    check.names = FALSE, check.rows = FALSE)

    if (!is.null(x$used.subset))
        data <- data[x$used.subset,]
    if (!is.null(x$label))
        rownames(data) <- x$label
    attr(data, "scatter.variable.indices") <- c(x = 1, y = 2, sizes = NA,
                                                colors = if (is.null(x$data.groups)) NA else 3)
    return(data)
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

    if (inherits(x, "TextPCA"))
        StopForUserError("2D plot representations are not supported for Text PCA outputs.")
    if (ncol(x$loadings) < 2)
        StopForUserError("There must be at least 2 components to produce 2-D output.")
    if (!is.null(x$data.groups) && length(x$data.groups) != nrow(x$scores))
        StopForUserError("Lengths of data and data.groups must the the same.")

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
                   title = "PCA",
                   use.combined.scatter = x$use.combined.scatter)
    class(output) <- c("2Dreduction", "flipFactorAnalysis")
    return(output)
}


#' \code{print.2Dreduction}
#' @description If 2Dreduction object is created from distance matrix, then print as labelled scatterplot.
#' If created from a \code{\link{data.frame}} of variables, print as a scatterplot of grouped cases.
#' @param x Object of class \code{"2Dreduction"}.
#' @param ... Not used.
#' @importFrom flipStandardCharts Chart
#' @importFrom rhtmlCombinedScatter CombinedScatter
#' @importFrom grDevices rgb
#' @importFrom class knn.cv
#' @importFrom flipU IsCount
#' @importFrom verbs Sum
#' @export
#' @method print 2Dreduction
print.2Dreduction <- function(x, ...) {

    if (x$input.is.distance) {
        if (isTRUE(x$use.combined.scatter)) {
            chart <- CombinedScatter(x$embedding[, 1], x$embedding[, 2],
                                     label = x$label,
                                     title = x$title,
                                     x.title = "Dimension 1",
                                     y.title = "Dimension 2",
                                     labels.font.size = 14,
                                     x.title.font.size = 14,
                                     y.title.font.size = 14,
                                     point.radius = 4,
                                     fixed.aspect = TRUE,
                                     plot.border.show = TRUE,
                                     origin = TRUE,
                                     legend.show = FALSE)
        } else {
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
    }
    else if (isTRUE(x$use.combined.scatter)) {
        # Scatterplot with groups
        scatter.group.indices <- ""
        scatter.group.labels <- ""
        colors <- ChartColors(12)
        title <- x$title
        set.seed(1066)

        embedding <- x$embedding[x$used.subset, ]
        groups <- x$data.groups[x$used.subset]

        if (!is.null(groups)) {
            if (is.factor(groups)) {
                nearest <- knn.cv(train = embedding, cl = groups, k = 1)
                same.category <- Sum(nearest == factor(groups, ordered = FALSE), remove.missing = FALSE) / length(groups)
                title <- paste0(title, " - Nearest neighbor accuracy: ", sprintf("%1.2f%%", 100 * same.category))
            }
            else if (all(groups == floor(groups))) {
                groups <- factor(groups)
                colors <- ChartColors(length(unique(groups)), "Reds, light to dark")
                nearest <- knn.cv(train = embedding, cl = groups, k = 1)
                same.category <- Sum(nearest == groups, remove.missing = FALSE) / length(groups)
                title <- paste0(title, ". Nearest neighbor accuracy: ", sprintf("%1.2f%%", 100 * same.category))
            }
            else {       # numeric: create 20 buckets and treat as factors
                groups <- cut(groups, 20)
                levels(groups) <- sub("[^,]*,([^]]*)\\]", "\\1", levels(groups))
                colors <- ChartColors(20, "Reds, light to dark")
            }
        }

        # Reorder data so that legend is ordered according to factor levels
        ind <- if (!is.null(groups)) order(as.numeric(groups)) else seq_len(nrow(embedding))

        chart <- CombinedScatter(embedding[ind, 1],
                                 embedding[ind, 2],
                                 group = groups[ind],
                                 colors = colors,
                                 title = title,
                                 y.title = "Dimension 2",
                                 x.title = "Dimension 1",
                                 labels.show = FALSE,
                                 point.radius = 3,
                                 x.axis.line.width = 0,
                                 y.axis.line.width = 0,
                                 legend.show = !is.null(groups))
    } else {
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
                same.category <- Sum(nearest == factor(groups, ordered = FALSE), remove.missing = FALSE) / length(groups)
                title <- paste0(title, " - Nearest neighbor accuracy: ", sprintf("%1.2f%%", 100 * same.category))
            }
            else if (all(groups == floor(groups))) {
                unique.labels <- sort(unique(groups))
                indices <- match(groups, unique.labels)
                scatter.group.labels <- paste(unique.labels, collapse = ", ")
                scatter.group.indices <- paste(indices, collapse = ", ")
                colors <- "Reds, light to dark"
                nearest <- knn.cv(train = embedding, cl = groups, k = 1)
                same.category <- Sum(nearest == groups, remove.missing = FALSE) / length(groups)
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
