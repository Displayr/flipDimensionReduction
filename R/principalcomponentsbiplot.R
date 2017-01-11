#' \code{PrincipalComponentsBiplot}
#' @description Performs PCA on a table and outputs biplot
#' @param x A table containing rows and columns
#' @param normalization Method used to standarize coordinates of biplot. The default method is \code{"Principal"},
#'   which plots the principal coordinates (i.e., the standard coordinates
#'   multiplied by the singular values). \code{"Row principal"} and \code{"Column
#'   principal"} plot the standard coordinates of the columns (rows) against the
#'   principal coordinates.
#' @param output Specify output generated. May be one of \code{"Scatterplot"} or \code{"Text"}.
#' @param row.names.to.remove A vector of the row labels to remove.
#' @param column.names.to.remove A vector of the column labels to remove.
#'   variable is provided, any cases with missing values on this variable are
#'   excluded from the final data file.
#' @importFrom flipData GetTidyTwoDimensionalArray
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
                      column.names.to.remove = c("NET", "Total", "SUM"))
{
    if (length(dim(x)) != 2)
        stop("Input should be a table with rows and columns\n")

    x <- GetTidyTwoDimensionalArray(x, row.names.to.remove, column.names.to.remove)
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

    res$normalization <- normalization
    p.row <- switch(normalization, 'Principal'= 1, 'Row principal' = 1, 'Column principal' = 0,
                                   'Symmetrical' = 0.5, 'None' = 0)
    p.col <- switch(normalization, 'Principal'= 1, 'Row principal' = 0, 'Column principal' = 1,
                                   'Symmetrical' = 0.5, 'None' = 0)

    res$rowcoords <- sweep(res$u, 2, res$d^{p.row}, "*")
    res$colcoords <- sweep(res$v, 2, res$d^{p.col}, "*")
    res$output <- output
    res$output <- output
    class(res) <- "PCAbiplot"
    return(res)
}

#' \code{print.PCAbiplot}
#' @description Plots biplot of PCA analysis, showing both the component loadings and scores simultaneously.
#' @param x An object created using \code{PrincipalComponentsBiplot}.
#' @param ... Not used
#' @importFrom rhtmlLabeledScatter LabeledScatter
#' @importFrom rhtmlMoonPlot moonplot
#' @export
print.PCAbiplot <- function(x, ...)
{
    if (x$output == "Scatterplot")
    {
       coords <- rbind(x$rowcoords[,1:2],
                       x$colcoords[,1:2])
       groups <- rep(c("Rows","Columns"), c(nrow(x$rowcoords), nrow(x$colcoords)))
       print(LabeledScatter(X = coords[, 1],
               Y = coords[, 2],
               label = rownames(coords),
               group = groups,
               fixed.aspect = TRUE,
               title = "Principal Components Analysis Biplot",
               x.title = colnames(coords)[1],
               y.title = colnames(coords)[2],
               axis.font.size = 8,
               labels.font.size = 12,
               title.font.size = 20,
               y.title.font.size = 16,
               x.title.font.size = 16))
    } else if (x$output == "Moonplot")
    {
        print(moonplot(x$rowcoords, x$colcoords))

    } else
    {
        evals <- x$d ^ 2
        pvar <- evals/sum(evals) * 100
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
