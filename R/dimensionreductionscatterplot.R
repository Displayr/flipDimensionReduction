#' \code{DimensionReductionScatterplot}
#' @description Reduces input to 2 dimensions. Takes either data.frame of variables and
#' another variable of data.groups, to produce a Scatterplot.  Or takes a distance matrix
#' to produce a labelled scatterplot.
#' @param algorithm TODO
#' @param data TODO
#' @param data.groups TODO
#' @param table TODO
#' @param raw.table TODO So we know whether to parse user input table. Cols contain labels.
#' @param subset A logical vector which describes the subset of \code{data} to
#'   be analyzed. Not used for \code{table} input.
#' @param perplexity TODO
#' @param binary TODO
#' @importFrom flipTransformations ParseEnteredData AsNumeric
#' @export

DimensionReductionScatterplot <- function(algorithm,
                                        data = NULL,
                                        data.groups = NULL,
                                        table = NULL,
                                        raw.table = FALSE,
                                        subset = NULL,
                                        perplexity = TRUE,
                                        binary = TRUE) {

    if (!xor(is.null(data), is.null(table)))
        stop("One and only one of data and table must be supplied.")


    if (is.null(table))
    {
        distance <- FALSE
    }
    else
    {
        distance <- TRUE
        distance.matrix <- if (!raw.table) {
            table
        } else {
            ParseEnteredData(table)
        }

        cls <- class(distance.matrix)
        if (cls != "dist" && cls != "Distance" && (cls != "matrix" || !is.numeric(distance.matrix) || !isSymmetric(distance.matrix)))
            stop("An invalid distance matrix was supplied.")
        if (!is.null(subset))
            warning("Subset is ignored for distance matrix input.")
        if (algorithm == "PCA")
            stop("PCA requires variables as input but a distance matrix was supplied.")
    }

    if (algorithm == "t-SNE")
    {
        result <- tSNE(data = data, data.groups = data.groups, subset = subset, is.distance = distance,
                       binary = binary, perplexity = perplexity)
    }
    else if (algorithm == "PCA")
    {
        # TO DO check these defaults .....
        dat <- AsNumeric(data, binary = binary, remove.first = TRUE)
        pca <- PrincipalComponentsAnalysis(data = dat, subset = subset,
                                                missing = "Exclude cases with missing data",
                                                use.correlation = TRUE,
                                                rotation = "none",
                                                select.n.rule = "Number of factors",
                                                n.factors = 2,
                                                groups = data.groups)

        complete <- complete.cases(pca$scores[, 1:2])
        if (!is.null(pca$groups))
            complete <- complete & complete.cases(pca$groups)

        result <- list(embedding = pca$scores[complete, 1:2],
                        data.groups = pca$groups[complete],
                        title = ifelse(is.null(pca$groups), "PCA", paste("PCA", "categories:", Labels(pca$groups))))
        result$is.distance <- FALSE
        class(result) <- c("2Dreduction", "flipFactorAnalysis")
        result
    }
    else if (algorithm == "MDS - Metric")
    {
        result <- MultiDimesnsionalScaling(distance.matrix = table, metric = TRUE)
    }
    else if (algorithm == "MDS - Non-metric")
    {
        result <- MultiDimesnsionalScaling(distance.matrix = table, metric = FALSE)
    }
    else
        stop("Algorithm not recognized.")

}

#' @export
#' @importFrom flipStandardCharts Chart
#' @importFrom grDevices rgb
#' @importFrom class knn.cv
#' @importFrom flipU IsCount
print.2Dreduction <- function(x, ...) {

    if (x$is.distance) {
        chart <- LabeledScatter(x$embedding[, 1], x$embedding[, 2],
                       label = x$label,
                       x.title = "Dimension 1",
                       y.title = "Dimension 2",
                       point.radius = 4,
                       labels.font.size = 14,
                       x.title.font.size = 14,
                       y.title.font.size = 14)
    }
    else
    {
        # Scatterplot with groups
        scatter.group.indices <- ""
        scatter.group.labels <- ""
        legend <- TRUE
        colors <- "Default colors"
        title <- x$title

        if (!is.null(x$data.groups)) {

            if (is.factor(x$data.groups)) {
                scatter.group.indices <- paste(as.numeric(x$data.groups), collapse = ", ")
                scatter.group.labels <- paste(levels(x$data.groups), collapse = ", ")
                nearest <- knn.cv(train = x$embedding, cl = x$data.groups, k = 1)
                same.category <- sum(nearest == x$data.groups) / length(x$data.groups)
                title <- paste0(title, ". Nearest neighbor accuracy: ", sprintf("%1.2f%%", 100 * same.category))
            }
            else if (all(x$data.groups == floor(x$data.groups))) {
                unique.labels <- sort(unique(x$data.groups))
                indices <- match(x$data.groups, unique.labels)
                scatter.group.labels <- paste(unique.labels, collapse = ", ")
                scatter.group.indices <- paste(indices, collapse = ", ")
                colors <- "Reds, light to dark"
                nearest <- knn.cv(train = x$embedding, cl = x$data.groups, k = 1)
                same.category <- sum(nearest == x$data.groups) / length(x$data.groups)
                title <- paste0(title, ". Nearest neighbor accuracy: ", sprintf("%1.2f%%", 100 * same.category))
            }
            else {       # numeric: create 20 buckets and treat as factors
                x$data.groups <- cut(x$data.groups, 20)
                scatter.group.indices <- paste(as.numeric(x$data.groups), collapse = ", ")
                levels(x$data.groups) <- sub("[^,]*,([^]]*)\\]", "\\1", levels(x$data.groups))
                scatter.group.labels <- paste(levels(x$data.groups), collapse = ", ")
                colors <- "Reds, light to dark"
            }
        }

        chart <- Chart(y = x$embedding,
                       type = "Scatterplot",
                       transpose = FALSE,
                       title = title,
                       title.font.family = NULL,
                       title.font.color = NULL,
                       title.font.size = 16,
                       colors = colors,
                       colors.reverse = FALSE,
                       opacity = NULL,
                       background.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                       background.fill.opacity = 1,
                       charting.area.fill.color = rgb(255, 255, 255, maxColorValue = 255),
                       charting.area.fill.opacity = 1,
                       legend.show = legend,
                       legend.fill = rgb(255, 255, 255, maxColorValue = 255),
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


