#' \code{GoodnessOfFitPlot}
#' @description A generic function used to produce plots illustrating the goodness-of-fit of
#' the model object.  The function invokes particular \code{\link{methods}}
#' which depend on the \code{\link{class}} of the first argument.
#'
#' Reports the goodness-of-fit of an object.
#' @param object An object for which a summary is desired.
#' @param ... Additional arguments affecting the goodness-of-fit displayed.
#' @param digits Minimal number of significant digits, see \code{\link{print.default}}.
#' @param max.points The maximum numner of points to plot.
#' @export
GoodnessOfFitPlot <- function(object, ...) {

    UseMethod("GoodnessOfFitPlot")
}


#' @describeIn GoodnessOfFitPlot  Default goodness-of-fit plot
#' @importFrom stats lm predict resid
#' @export
GoodnessOfFitPlot.default = function(object, digits = max(3L, getOption("digits") - 3L), max.points = 1000, ...) {

    fitted <- fitted(object)
    if(is.null(fitted))
        fitted <- predict(object)
    observed <- fitted + resid(object)
    linear.regression = lm(fitted ~ observed)
    GoodnessOfFitPlot.Regression(linear.regression, digits, max.points, ...)
}


#' @describeIn GoodnessOfFitPlot  Goodness-of-fit plot for a Regression object
#' @importFrom graphics plot abline
#' @importFrom flipData Observed
#' @importFrom stats complete.cases fitted
#' @export
GoodnessOfFitPlot.Regression = function(object, digits = max(3L, getOption("digits") - 3L), max.points = 1000, ...) {

    y <- cbind(Observed(object), fitted(object))
    y <- y[object$subset & complete.cases(y), ]

    correlation <- cor(y, method = "spearman")

    # Sample randomly if too many rows
    set.seed(1066)
    if (nrow(y) > max.points)
        y <- y[sample(nrow(y), max.points), ]

    title <- paste0(object$type, " Regression - Shepard Diagram - Rank correlation: ", sprintf("%1.2f%%", 100 * correlation[2, 1]))
    chart <- Chart(y = y,
                   type = "Scatterplot",
                   title = title,
                   x.title = "Observed",
                   y.title = "Fitted")
}


#' @describeIn GoodnessOfFitPlot  Goodness-of-fit plot for a flipFactorAnalysis object
#' @export
GoodnessOfFitPlot.flipFactorAnalysis = function(object, max.points = 1000, ...) {
    GoodnessOfFitPlot.2Dreduction(convertFactorAnalysisTo2D(object), max.points = max.points)
}

#' @describeIn GoodnessOfFitPlot  Goodness-of-fit plot for a 2Dreduction object
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
    chart <- Chart(y = y,
                   type = "Scatterplot",
                   title = title,
                   x.title = x.title,
                   y.title = "Output distance")
}
