#' \code{CorrespondenceAnalysis}
#' @description Removes rows or columns from the table.
#' @param x A table.
#' @param normalization The method used to normalize the coordinates of the
#'   correspondence analysis plot (this changes the plot, but not the outputs of
#'   \code{\link[ca]{ca}} itself. The default method is \code{"Principal"},
#'   which plots the principal coordinates (i.e., the standard coordinates
#'   multiplied by the singular values). \code{"Row principal"} and \code{"Column
#'   principal"} plot the standard coordinates of the columns (rows) against the
#'   principal coordinates. Note that the plotting occurs via
#'   \code{\link{print.CorrespondenceAnalysis}}.
#' @param output How the map is displayed: \code{"Scatterplot"}, or \code{"Moonplot"}, \code{"Text"}, or \code{"ggplot2"}.
#' @param row.names.to.remove A vector of the row labels to remove.
#' @param column.names.to.remove A vector of the column labels to remove.
#'   variable is provided, any cases with missing values on this variable are
#'   excluded from the final data file.
#' @details Where a matrix or array is passed in containing names for the dimensions, these are used to represent the rows
#' and columns in the legend. If there are no names, then the names are assumed to be the contents of \code{attr(x, "row.column.names")}.
#' If there are still no names, they are assumed to be \code{Rows} and \code{Columns}, respectively.
#' @param row.color Color to display row-attributes in scatterplot.
#' @param col.color Color to display column-attributes in scatterplot.
#' @param ... Optional arguments for \code{\link[ca]{ca}}.
#' @importFrom flipData GetTidyTwoDimensionalArray
#' @importFrom ca ca
#' @export
CorrespondenceAnalysis = function(x,
                                  normalization = "Principal",
                                  output = c("Scatterplot", "Moonplot", "Text", "ggplot2")[1],
                                  row.names.to.remove = c("NET", "Total", "SUM"),
                                  column.names.to.remove = c("NET", "Total", "SUM"),
                                  row.color = '#5B9BD5',
                                  col.color = '#ED7D31',
                                  ...)
{
    row.column.names.attribute <- attr(x, "row.column.names")
    x <- GetTidyTwoDimensionalArray(x, row.names.to.remove, column.names.to.remove)
    row.column.names <- names(dimnames(x))
    if (is.null(row.column.names))
        row.column.names <- row.column.names.attribute
    if (is.null(row.column.names))
        row.column.names <- c("Rows", "Columns")
    result <- list(x = x,
                   row.column.names = row.column.names,
                   normalization = normalization,
                   output = output,
                   row.color = row.color,
                   col.color = col.color,
                   original = ca(x, ...))
    class(result) <- c("CorrespondenceAnalysis")
    result
}


#' \code{print.CorrespondenceAnalysis}
#' @description Creates a plot displaying the correspondence analysis results.
#' @param x CorrespondenceAnalysis object.
#' @param ... further arguments passed to or from other methods.
#' @import ca
#' @importFrom rhtmlMoonPlot moonplot
#' @importFrom rhtmlLabeledScatter LabeledScatter
#' @export
print.CorrespondenceAnalysis <- function(x, ...)
{
    ca.obj <- x$original
    normed <- CANormalization(ca.obj, x$normalization)
    singular.values <- round(ca.obj$sv^2, 6)
    variance.explained <- paste(as.character(round(100 * prop.table(singular.values), 1)), "%", sep = "")[1:2]
    column.labels <- paste("Dimension", 1:2, paste0("(", variance.explained, ")"))
    row.coordinates <- normed$row.coordinates
    column.coordinates <- normed$column.coordinates
    coords <- rbind(row.coordinates, column.coordinates)
    row.column.names <- x$row.column.names
    groups <- rep(x$row.column.names, c(nrow(row.coordinates), nrow(column.coordinates)))
    x.data <- as.matrix(x$x)
    if (x$output == "Scatterplot")
    {
        #tooltip.text <- c(CreateInteractiveScatterplotTooltips(x.data), CreateInteractiveScatterplotTooltips(t(x.data)))
        print(LabeledScatter(X = coords[, 1],
                       Y = coords[, 2],
                       label = rownames(coords),
                       group = groups,
                       colors = c(x$row.color, x$col.color),
                       fixed.aspect = TRUE,
                       title = "Correspondence analysis",
                       x.title = column.labels[1],
                       y.title = column.labels[2],
                       axis.font.size = 8,
                       labels.font.size = 12,
                       title.font.size = 20,
                       y.title.font.size = 16,
                       x.title.font.size = 16))
        #print(InteractiveLabeledScatterPlot(coords, column.labels = column.labels, group = groups, fixed.aspect = TRUE, tooltip.text = tooltip.text))
    }
    else if (x$output == "Moonplot")
    {
        if (x$normalization != "Row principal")
            warning("It is good practice to set 'Normalization' to 'Row principal' when 'Output' is set to 'Moonplot'.")
        print(moonplot(ca.obj$rowcoord[,1:2], ca.obj$colcoord[,1:2]))
    }
    else
        print(ca.obj, ...)
}


#' \code{CANormalization}
#' @description Normalizes the coordinates of a \code{\link[ca]{ca}} object.
#' @param ca.object The object to normalize.
#' @param normalization The method used to normalize the coordinates of the
#'   correspondence analysis plot (this changes the plot, but not the outputs of
#'   \code{\link[ca]{ca}} itself. The default method is \code{"Principal"},
#'   which plots the principal coordinates (i.e., the standard coordinates
#'   multipled by the singular values). \code{"Row principal"} and \code{"Column
#'   principal"} plot the standard coordinates of the columns (rows) against the
#'   principal coordinates. \code{"Symmetrical (\u00BD)"} plots the standard
#'   coordinates multiplied by the square root of the singular values.
#'   \code{"None"} plots the standard coordinates.
#' @export
CANormalization <- function(ca.object, normalization = "Principal")
{
    .normalize = function(coords, power) sweep(coords[,1:2], 2, ca.object$sv[1:2]^power, "*")
    rows <- .normalize(ca.object$rowcoord, switch(normalization,
        "Principal" = 1, "Row principal" = 1, "Column principal" = 0, "Symmetrical (\u00BD)" = 0.5, "None" = 0))
    columns <- .normalize(ca.object$colcoord, switch(normalization,
        "Principal" = 1, "Row principal" = 0, "Column principal" = 1, "Symmetrical (\u00BD)" = 0.5, "None" = 0))
    list(row.coordinates = rows, column.coordinates = columns)
}
