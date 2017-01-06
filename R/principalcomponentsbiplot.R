#' \code{PrincipalComponentsBiplot}
#' @description Performs PCA on a table and outputs biplot
#' @param x A table containing rows and columns#'
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
#' rownames(x2) <- c("SomePrimary","PrimaryCompleted","SomeSecondary","SecondaryCompleted","SomeTertiary")
#' PrincipalComponentsBiplot(x2, output="Moonplot")
#'
#' @export
PrincipalComponentsBiplot <- function(x,
                      output = c("Scatterplot", "Moonplot", "Text")[1],
                      row.names.to.remove = c("NET", "Total", "SUM"),
                      column.names.to.remove = c("NET", "Total", "SUM"))
{
    if (length(dim(x)) != 2)
        stop("Input should be a table with rows and columns\n")

    x <- GetTidyTwoDimensionalArray(x, row.names.to.remove, column.names.to.remove)
    res <- suppressWarnings(prcomp(x))
    pc.names <- sprintf("Component %d", 1:ncol(res$x))
    colnames(res$x) <- pc.names
    colnames(res$rotation) <- pc.names
    res$output <- output
    class(res) <- "PCAbiplot"
    return(res)
}

#' \code{print.PCAbiplot}
#' @description Plots biplot of PCA analysis, showing both the component loadings and scores simultaneously.
#' @param x An object created using \code{PrincipalComponentsBiplot}.
#' @importFrom rhtmlLabeledScatter LabeledScatter
#' @importFrom rhtmlMoonPlot moonplot
#' @export
print.PCAbiplot <- function(x, ...)
{
    denom <- colSums(x$x^2)
    pc.names <- sprintf("Component %d", 1:ncol(x$x))
    if (x$output == "Scatterplot")
    {
       coords <- rbind(x$x[,1:2]/sqrt(denom[1]),
                       x$rotation[,1:2])
       groups <- rep(c("Rows","Columns"), c(nrow(x$x), nrow(x$rotation)))
       #print(coords)
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
        rcoords <- x$x[,1:2]/sqrt(denom[1])
        ccoords <- x$rotation[,1:2]
        print(moonplot(rcoords, ccoords))

    } else
    {
        evals <- x$sdev ^ 2
        pvar <- evals/sum(evals) * 100
        etab <- cbind(Eigenvalue=evals,
                      'Percent variance' = pvar,
                      'Cumulative percent' = cumsum(pvar))
        rownames(etab) <- colnames(x$x)

        cat("Principal Components Analysis Biplot\n")
        cat("\nVariance explained:\n")
        print(round(etab, digits=2))
        cat("\nRow coordinates:\n")
        # standardize columns of row.coord to have mean square 1
        row.coord <- sweep(x$x, 2, sqrt(denom), "/")
        print(round(row.coord, digits=2))
        cat("\nColumn coordinates:\n")
        print(round(x$rotation, digits=2))
    }
}
