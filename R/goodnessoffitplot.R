#' @importFrom flipStatistics GoodnessOfFitPlot
#' @export
flipStatistics::GoodnessOfFitPlot


#' Goodness-of-fit plot for a flipFactorAnalysis object
#' @param object An object for which a summary is desired.
#' @param max.points The maximum numner of points to plot.
#' @param ... Additional arguments affecting the goodness-of-fit displayed.
#' @export
GoodnessOfFitPlot.flipFactorAnalysis = function(object, max.points = 1000, ...) {
    GoodnessOfFitPlot.2Dreduction(convertFactorAnalysisTo2D(object), max.points = max.points)
}


#' Goodness-of-fit plot for a 2Dreduction object
#' @param object An object for which a summary is desired.
#' @param max.points The maximum numner of points to plot.
#' @param ... Additional arguments affecting the goodness-of-fit displayed.
#' @importFrom flipStandardCharts Chart
#' @importFrom stats cor complete.cases
#' @importFrom utils combn
#' @export
GoodnessOfFitPlot.2Dreduction = function(object, max.points = 1000, ...) {

    if (object$input.is.distance || "MDS" %in% class(object)) {
        # object$input.data is distance matrix
        input.distances <- if ("dist" %in% class(object$input.data)) {
            as.vector(object$input.data)
        } else {
            # take only lower triangle of full symmetric matrix
            object$input.data[lower.tri(object$input.data)]
        }
        y <- cbind(input.distances, as.vector(dist(object$embedding[complete.cases(object$embedding), ])))
        rownames(y) <- apply(combn(object$label, 2), 2, function(x) {paste(x[1], x[2])})
    }
    else   # object$input.data is data.frame
    {
        subset <- !is.na(object$embedding[, 1])
        input.distances <- as.vector(dist(object$input.data[subset, ]))
        y <- cbind(input.distances, as.vector(dist(object$embedding[subset, ])))
    }
    correlation <- cor(y, method = "spearman")

    # Sample randomly if too many rows to plot
    set.seed(1066)
    if (nrow(y) > max.points)
        y <- y[sample(nrow(y), max.points), ]

    x.title <- "Input distance"
    if (object$normalized)
        x.title <- paste0(x.title, " (normalized)")
    title <- paste0(object$title, " - Shepard Diagram - Rank correlation: ", sprintf("%1.2f%%", 100 * correlation[2, 1]))
    Chart(y = y,
        type = "Scatterplot",
        title = title,
        x.title = x.title,
        y.title = "Output distance",
        background.fill.opacity = 0,
        charting.area.fill.opacity = 0,
        legend.fill.opacity = 0)
}

