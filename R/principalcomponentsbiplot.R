#' \code{PrincipalComponentsBiplot}
#' @description Performs PCA on a table and outputs biplot
#' @param x A table containing rows and columns
#' @param normalization Method used to standarize coordinates of biplot. The default method is \code{"Principal"},
#'   which plots the principal coordinates (i.e., the standard coordinates
#'   multiplied by the singular values). \code{"Row principal"} and \code{"Column
#'   principal"} plot the standard coordinates of the columns (rows) against the
#'   principal coordinates.
#' @param output Specify output generated. May be one of \code{"Scatterplot", "Moonplot"} or \code{"Text"}.
#' @param row.names.to.remove A vector of the row labels to remove.
#' @param column.names.to.remove A vector of the column labels to remove.
#' @param row.color Color to display row-attributes in scatterplot.
#' @param col.color Color to display column-attributes in scatterplot.
#' @details Where a matrix or array is passed in containing names for the dimensions, these are used to represent the rows
#' and columns in the legend. If there are no names, then the names are assumed to be the contents of \code{attr(x, "row.column.names")}.
#' If there are still no names, they are assumed to be \code{Rows} and \code{Columns}, respectively.
#' @importFrom flipTables TidyTabularData
#' @importFrom verbs Sum
#' @examples
#' x2 <- cbind(Glance=c(0.016, 0.058, 0.061, 0.038, 0.01),
#'            FairlyThorough=c(0.022, 0.147, 0.093, 0.128, 0.022),
#'            VeryThorough=c(0.006,0.064,0.125,0.157,0.051))
#' rownames(x2) <- c("SomePrimary","PrimaryCompleted","SomeSecondary",
#'                   "SecondaryCompleted","SomeTertiary")
#' PrincipalComponentsBiplot(x2, output="Moonplot")
#'
#' @export
PrincipalComponentsBiplot <- function(x,
                      normalization = c("Principal", "Row principal", "Column principal", "Symmetrical", "None")[1],
                      output = c("Scatterplot", "Moonplot", "Text")[1],
                      row.names.to.remove = c("NET", "Total", "SUM"),
                      column.names.to.remove = c("NET", "Total", "SUM"),
                      row.color = '#5B9BD5',
                      col.color = '#ED7D31')
{
    row.column.names.attribute <- attr(x, "row.column.names")
    x <- TidyTabularData(x, row.names.to.remove = row.names.to.remove,
                    col.names.to.remove = column.names.to.remove)
    row.column.names <- names(dimnames(x))
    if (is.null(row.column.names))
        row.column.names <- row.column.names.attribute
    if (is.null(row.column.names))
        row.column.names <- c("Rows", "Columns")

    row.names <- rownames(x)
    col.names <- colnames(x)
    xm <- apply(x, 2, mean)
    z <- sweep(x, 2, xm, "-")
    res <- svd(z)

    pc.names <- sprintf("Component %d", 1:ncol(res$u))
    colnames(res$u) <- pc.names
    colnames(res$v) <- pc.names
    rownames(res$u) <- row.names
    rownames(res$v) <- col.names

    res$row.column.names <- row.column.names
    res$normalization <- normalization
    p.row <- switch(normalization, 'Principal'= 1, 'Row principal' = 1, 'Column principal' = 0,
                                   'Symmetrical' = 0.5, 'None' = 0)
    p.col <- switch(normalization, 'Principal'= 1, 'Row principal' = 0, 'Column principal' = 1,
                                   'Symmetrical' = 0.5, 'None' = 0)

    c.row <- res$d^{p.row}
    c.col <- res$d^{p.col}
    res$rowcoords <- sweep(res$u, 2, c.row/c.row[1], "*")
    res$colcoords <- sweep(res$v, 2, c.col/c.col[1], "*")
    res$row.color <- row.color
    res$col.color <- col.color
    res$output <- output
    class(res) <- c("PCAbiplot", "visualization-selector")

    evals <- res$d ^ 2
    pvar <- evals/Sum(evals, remove.missing = FALSE) * 100
    groups <- rep(row.column.names, c(nrow(res$rowcoords), nrow(res$colcoords)))
    coords <- data.frame(rbind(res$rowcoords[,1:2], res$colcoords[,1:2]),
                    Group = groups, stringsAsFactors = FALSE,
                    check.names = FALSE, check.rows = FALSE)
    colnames(coords)[1:2] = sprintf("%s (%.1f%%)", colnames(coords)[1:2], pvar[1:2])
    cdat <- coords
    attr(cdat, "scatter.variable.indices") <- if (NCOL(cdat) == 3) c(x = 1, y = 2, sizes = NA, colors = 3)
                                              else                 c(x = 1, y = 2, sizes = NA, colors = NA)
    attr(res, "ChartData") <- cdat
    attr(res, "ChartType") <- "X Y Scatter"
    return(res)
}

#' \code{print.PCAbiplot}
#' @description Plots biplot of PCA analysis, showing both the component loadings and scores simultaneously.
#' @param x An object created using \code{PrincipalComponentsBiplot}.
#' @param ... Not used
#' @importFrom rhtmlLabeledScatter LabeledScatter
#' @importFrom rhtmlCombinedScatter CombinedScatter
#' @importFrom rhtmlMoonPlot moonplot
#' @importFrom verbs Sum
#' @export
#' @method print PCAbiplot
print.PCAbiplot <- function(x, ...)
{
    if (x$output == "Scatterplot")
    {
       coords <- attr(x, "ChartData")
       print(CombinedScatter(X = coords[,1],
               Y = coords[,2],
               label = rownames(coords),
               group = coords[,3],
               colors = c(x$row.color, x$col.color),
               fixed.aspect = TRUE,
               title = "Principal Components Analysis Biplot",
               x.title = colnames(coords)[1],
               y.title = colnames(coords)[2],
               axis.font.size = 10,
               labels.font.size = 12,
               title.font.size = 20,
               y.title.font.size = 16,
               x.title.font.size = 16,
               plot.border.show = TRUE))
    } else if (x$output == "Moonplot")
    {
        print(moonplot(x$rowcoords, x$colcoords))

    } else
    {
        evals <- x$d ^ 2
        pvar <- evals/Sum(evals, remove.missing = FALSE) * 100
        etab <- cbind(Eigenvalue=evals,
                      'Percent variance' = pvar,
                      'Cumulative percent' = cumsum(pvar))
        rownames(etab) <- colnames(x$rowcoords)

        cat("Principal Components Analysis Biplot\n")
        cat("\nVariance explained:\n")
        print(round(etab, digits=2))
        cat("\nRow coordinates:\n")
        print(round(x$rowcoords, digits=2))
        cat("\nColumn coordinates:\n")
        print(round(x$colcoords, digits=2))
    }
}
