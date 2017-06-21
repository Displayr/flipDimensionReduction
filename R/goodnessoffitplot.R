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
    GoodnessOfFitPlot.lm(linear.regression, digits, max.points, ...)
}


#' @describeIn GoodnessOfFitPlot  Goodness-of-fit plot for a linear model
#' @importFrom graphics plot abline
#' @export
GoodnessOfFitPlot.lm = function(object, digits = max(3L, getOption("digits") - 3L), max.points = 1000, ...) {

    r2 =  paste0(formatC(100 * summary(object)$r.squared, digits = digits), "%")
    plot(fitted(object)~object$model[,1], xlab = "Observed", ylab = "Fitted", main = paste0("Adjusted R-squared: ", r2), asp = 1)
    abline(object, lty = 2, col = "red")
}


#' @describeIn GoodnessOfFitPlot  Goodness-of-fit plot for a flipFactorAnalysis object
#' @export
GoodnessOfFitPlot.flipFactorAnalysis = function(object, max.points = 1000, ...) {
    GoodnessOfFitPlot.2Dreduction(convertFactorAnalysisTo2D(object), max.points = max.points)
}

#' @describeIn GoodnessOfFitPlot  Goodness-of-fit plot for a 2Dreduction object
#' @importFrom flipStandardCharts Chart
#' @importFrom stats cor
#' @export
GoodnessOfFitPlot.2Dreduction = function(object, max.points = 1000, ...) {

    if (object$print.as.distance || "MDS" %in% class(object)) {
        y <- cbind(as.vector(object$input.data), as.vector(dist(object$embedding)))
    }
    else
    {
        subset <- !is.na(object$embedding[, 1])
        input.distances <- as.vector(dist(object$input.data[subset, ]))
        y <- cbind(input.distances, as.vector(dist(object$embedding[subset, ])))
    }
    correlation <- cor(y, method = "spearman")

    # Sample randomly if too many rows
    set.seed(1066)
    if (nrow(y) > max.points)
        y <- y[sample(nrow(y), max.points), ]

    # TODO TOOLTIP LABELS

    title <- paste0(object$title, " - Shepard Diagram - Correlation: ", sprintf("%1.2f%%", 100 * correlation[2, 1]))
    chart <- Chart(y = y,
                   type = "Scatterplot",
                   title = title,
                   x.title = "Input distance",
                   y.title = "Output distance")
}

