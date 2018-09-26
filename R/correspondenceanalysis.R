#' \code{CorrespondenceAnalysis}
#' @description Performs correspondence analysis to analyze a table or list of tables.
#' @param x A \code{\link{matrix}} or a list of matrices.
#' @param normalization The method used to normalize the coordinates of the
#'   correspondence analysis plot. The default method is \code{"Principal"},
#'   which plots the principal coordinates (i.e., the standard coordinates
#'   multiplied by the singular values). \code{"Row principal"} and \code{"Column
#'   principal"} plot the standard coordinates of the columns (rows) against the
#'   principal coordinates. \code{"Row principal (scaled)"} is the same as \code{"Row principal"}
#'   except that both column coordinates are equally scaled so that column points appear
#'   on a similar scale to row points. \code{"Column principal (scaled)"} is analagous.
#'   Note that the plotting occurs via \code{\link{print.CorrespondenceAnalysis}}.
#' @param output How the map is displayed: \code{"Scatterplot"}, \code{"Moonplot"},
#' \code{"Input Table"}, \code{"Diagnostics"}, \code{"Bubble Chart"} or \code{"Text"}.
#' @param focus The label of a row or column category. The output is rotated
#'   so that the variance of this category lies along the first dimension.
#' @param supplementary A vector of rows or columns to be treated as supplementary, i.e. not included
#'   in the calculation of the coordinate space but to be plotted.
#' @param row.names.to.remove A vector of the row labels to remove.
#' @param column.names.to.remove A vector of the column labels to remove.
#' @param mirror.horizontal Boolean indicating whether to reverse the sign of values plotted along the horizontal axis.
#' @param mirror.vertical Boolean indicating whether to reverse the sign of values plotted along the vertical axis.
#' @param row.color Color to display row-attributes in scatterplot with one table.
#' @param col.color Color to display column-attributes in scatterplot with one table.
#' @param color.palette Palette used to color scatterplot when multiple tables are used.
#' @param bubble.size A vector of magnitudes for the row coordinate (for bubble charts). This is optional.
#' @param bubble.title A label for the legend.
#' @param chart.title Title of chart.
#' @param max.row.labels.plot A number specifying the maximum of row labels shown in bubble or scatterplots. The remaining rows will be shown with labels hidden.
#' @param max.col.labels.plot A number specifying the maximum number column labels shown.
#' @param max.labels.plot Deprecated. Use max.row.labels.plot instead.
#' @param logos Optional list of images to be used to label scatterplot instead of the row names. It should be inputted as a comma-seperated list of URLs.
#' @param logo.size Numeric controlling the size of the logos.
#' @param transpose Boolean indicating whether the rows and columns of \code{x} should be swapped.
#' @param trend.lines Boolean indicating whether to draw trend lines when multiple tables are supplied.
#' @param show.gridlines Boolean indicating whether to show gridlines for \code{"Scatterplot"} and \code{"Bubble Chart"}.
#' @param multiple.tables Deprecated.
#' @param square Boolean indicating whether the input table is square. If true the row and column names of the table must be the same.
#' @param dim1.plot Dimension to show in X-axis of bubble or scatterplot.
#' @param dim2.plot Dimension to show in Y-axis of bubble or scatterplot.
#' @param title.font.size Font size of the chart title.
#' @param x.title.font.size Font size of the horizontal axis title.
#' @param y.title.font.size Font size of the vertical axis title.
#' @param labels.font.size Font size of the labels on the scatterplot.
#' @param axis.font.size Font size of the labels on the x- and y-axis.
#' @param legend.font.size Font size of the legend.
#' @param footer.wrap.length Maximum number of characters in the footer. If longer, the text will be wrapped.
#' @param ... Optional arguments for \code{\link[ca]{ca}}.
#' @importFrom flipTables TidyTabularData RemoveRowsAndOrColumns
#' @importFrom ca ca
#' @export
CorrespondenceAnalysis = function(x,
                                  normalization = "Principal",
                                  output = c("Scatterplot", "Bubble Chart", "Moonplot", "Text", "Input Table")[1],
                                  focus = NULL,
                                  supplementary = NULL,
                                  row.names.to.remove = c("NET", "Total", "SUM"),
                                  column.names.to.remove = c("NET", "Total", "SUM"),
                                  mirror.horizontal = FALSE,
                                  mirror.vertical = FALSE,
                                  color.palette = "Default colors",
                                  row.color = '#5B9BD5',
                                  col.color = '#ED7D31',
                                  bubble.size = NULL,
                                  bubble.title = "",
                                  chart.title = "Correspondence analysis",
                                  transpose = FALSE,
                                  logos = NULL,
                                  logo.size = 0.5,
                                  trend.lines = FALSE,
                                  show.gridlines = TRUE,
                                  multiple.tables = NA,
                                  square = FALSE,
                                  max.row.labels.plot = 200,
                                  max.col.labels.plot = 200,
                                  max.labels.plot = NA,
                                  dim1.plot = 1,
                                  dim2.plot = 2,
                                  title.font.size = 20,
                                  x.title.font.size = 16,
                                  y.title.font.size = 16,
                                  labels.font.size = 14,
                                  axis.font.size = 10,
                                  legend.font.size = 15,
                                  footer.wrap.length = 80,
                                  ...)
{
    # Backwards compatibility
    if (!is.na(max.labels.plot) && max.row.labels.plot == 200)
        max.row.labels.plot <- max.labels.plot

    if (max.row.labels.plot != round(max.row.labels.plot))
        stop("Parameter 'Maximum row labels to plot' must be an integer.")
    if (max.col.labels.plot != round(max.col.labels.plot))
        stop("Parameter 'Maximum column labels to plot' must be an integer.")
    check1 <- try(is.null(dim1.plot))
    check2 <- try(is.null(dim2.plot))
    if (inherits(check1, "try-error") || check1)
        dim1.plot <- 1
    if (inherits(check2, "try-error") || check2)
        dim2.plot <- 2

    # Mask undefined arguments for R Gui control
    if (!output %in% c("Scatterplot", "Bubble Chart"))
    {
        chart.title <- ""
        row.color <- ""
        col.color <- ""
        max.row.labels.plot <- 0
        max.col.labels.plot <- 0
    }
    if (output != "Bubble Chart")
    {
        bubble.size <- NULL
        bubble.title <- NULL
    }
    if (output != "Scatterplot")
        logos <- NULL
    if (is.null(logos))
        logo.size <- 0
    if (!is.numeric(logo.size))
        stop("Logo size must be a numeric")

    if (is.null(x))
        stop("No table has been entered.")

    # Multiple tables
    # note that a dataframe is actually a list
    x.stat <- attr(x, "statistic")
    if (!is.null(dim(x[[1]])) && length(x) > 1 && !is.data.frame(x))
    {
        if (!output %in% c("Scatterplot", "Input Table"))
            stop(sprintf("Output '%s' is not valid with multiple input tables.", output))
        row.color <- '#5B9BD5'
        col.color <- '#ED7D31'
        square <- FALSE

        # Get table names
        num.tables <- length(x)
        x.names <- rep("", num.tables)
        unnamed.tables <- FALSE
        used.names <- c()
        for (i in 1:num.tables)
        {
            if (is.null(attr(x[[i]], "name")))
            {
                unnamed.tables <- TRUE
                attr(x[[i]], "name") <- as.character(i)
                used.names <- c(used.names, i)
            }
            x.names[i] <- attr(x[[i]], "name")[1]
        }
        if (unnamed.tables & !trend.lines)
            warning(sprintf("Tables have been automatically assigned names '%s'. You can name tables using R code: 'attr(table.name, \"name\") <- \"Description\"'", paste(used.names, collapse="', '")))
        if (any(duplicated(x.names)) & !trend.lines)
            warning(sprintf("Tables have duplicate names: '%s'. Points from duplicated tables cannot be distinguised.", paste(x.names[duplicated(x.names)], collapse = "', '")))

        ## Check tables match - order of rows will match first table
        x[[1]] <- TidyTabularData(x[[1]], row.names.to.remove = row.names.to.remove,
                             col.names.to.remove = column.names.to.remove, transpose = transpose)
        r.names <- rownames(x[[1]])
        c.names <- colnames(x[[1]])
        for (i in 2:num.tables)
        {
            x[[i]] <- TidyTabularData(x[[i]], transpose = transpose)
            r.tmp <- match(r.names, rownames(x[[i]]))
            c.tmp <- match(c.names, colnames(x[[i]]))

            if (any(is.na(r.tmp)))
                stop(sprintf("Tables do not match. Table '%s' missing row '%s'.",
                             x.names[i], r.names[which(is.na(r.tmp))[1]]))
            if (any(is.na(c.tmp)))
                stop(sprintf("Tables do not match. Table '%s' missing column '%s'.",
                             x.names[i], c.names[which(is.na(c.tmp))[1]]))

            x[[i]] <- x[[i]][r.names,c.names]
        }
        x <- do.call(rbind, x)
        rownames(x) <- sprintf("%s: %s", rep(x.names, each=length(r.names)), rownames(x))
        row.column.names <- r.names

    } else
    {
        if (!is.null(dim(x[[1]])))
            stop("Input data 'x' contains only one table. Unselect checkbox for 'multiple tables'\n")

        num.tables <- 1
        color.palette <- "Default colors"
        trend.lines <- FALSE

        row.column.names.attribute <- attr(x, "row.column.names")
        row.column.names <- names(dimnames(x))[1:2]

        x <- TidyTabularData(x, row.names.to.remove = row.names.to.remove,
                        col.names.to.remove = column.names.to.remove)
        if (transpose)
        {
            x <- t(x)
            row.column.names <- rev(row.column.names)
        }

        if (!is.null(row.column.names.attribute))
            row.column.names <- attr(x, "row.column.names")
        else if (is.null(row.column.names))
            row.column.names <- c("Rows", "Columns")

        if (square)
        {
            if (output == "Moonplot")
                stop("Output 'Moonplot' is not valid with square matrixes.")
            if (nrow(x) != ncol(x))
                stop("Input Table is not a square matrix.")

            valid.stat <- c("n", "Total %", "Population", "Correlation", "Index")
            if (!is.null(x.stat) && !(x.stat %in% valid.stat))
                warning("Underlying table may not have the appropriate structure. ",
                        "Correspondence Analysis of Square Tables should only be applied to tables containing one of '",
                        paste(valid.stat, collapse="', '"), "'.")

            r.names <- gsub("^\\s+", "", gsub("\\s+$", "", rownames(x)))
            c.names <- gsub("^\\s+", "", gsub("\\s+$", "", colnames(x)))
            dimnames(x) <- list(r.names, c.names)

            if (any(duplicated(r.names)))
                stop("Row labels are not unique.")
            if (any(duplicated(c.names)))
                stop("Column labels are not unique.")

            c.ind <- match(r.names, c.names)
            if (any(is.na(c.ind)))
                stop(sprintf("Row and column labels in square matrix do not match. Missing '%s' in column labels",
                             paste(r.names[which(is.na(c.ind))], collapse="', '")))
            x <- x[,c.ind]
        }

        # Check for empty rows/columns
        rSum <- rowSums(abs(x), na.rm=T)
        cSum <- colSums(abs(x), na.rm=T)
        if (any(rSum == 0) || any(cSum == 0))
        {
            empty.dim <- "Row"
            empty.name <- ""
            if (any(rSum == 0))
            {
                if (transpose)
                    empty.dim <- "Column"
                empty.name <- rownames(x)[which(rSum == 0)[1]]
            } else if (any(cSum == 0))
            {
                if (!transpose)
                    empty.dim <- "Column"
                empty.name <- colnames(x)[which(cSum == 0)[1]]
            }
            stop(sprintf("%s '%s' contains only zeros or NAs.", empty.dim, empty.name))
        }

        if (output == "Bubble Chart")
        {
            table.maindim <- ifelse(transpose, "columns", "rows")
            if(is.null(bubble.size))
                stop("Bubble Charts require bubble sizes.")
            bubble.names <- rownames(bubble.size)
            if (is.null(bubble.names) && !is.null(names(bubble.size)))
                bubble.names <- names(bubble.size)
            if (is.null(bubble.names))
                stop("The bubble sizes need to be named.")
            bubble.size <- as.numeric(unlist(bubble.size))
            if (length(table.names <- rownames(x)) != length(bubble.names))
                stop("The number of bubble sizes does not match the number of ", table.maindim, " in the table.")
            if (length(unique(bubble.names)) !=  length(bubble.names))
                stop("There are duplicate bubble size names.")
            if (!all(sort(bubble.names) == sort(table.names)))
                if (!all(sort(tolower(bubble.names)) == sort(tolower(table.names))))
                {
                    stop("The bubble sizes must contain the same names as in the ",
                         table.maindim, " of the input data table: ",
                         paste0(paste0(table.names, ":", bubble.names), collapse = ", "), ".")
                }
            # Sorting bubble sizes to match the row names of the table.
            order = match(rownames(x), bubble.names)
            if (sum(order, na.rm = TRUE) != sum(1:length(order)))
                stop("The bubble sizes must contain the same names as in the rows of the input data table: ",
                     paste0(paste0(table.names, ":", bubble.names), collapse = ", "), ".")
            bubble.size = bubble.size[order]
        }

        # Expand square matrix after checking against bubble names
        if (square)
            x <- cbind(rbind(x, t(x)), rbind(t(x), x))
    }

    if (sum(x < 0) > 0)
        stop("Input tables must not contain negative values.")

    footer <- paste0("Normalization: ", normalization)

    suprow <- supcol <- integer(0)
    if (!is.null(supplementary)) {

        reduced.x <- RemoveRowsAndOrColumns(x, row.names.to.remove = supplementary,
                                            column.names.to.remove = supplementary)
        if (length(rownames(reduced.x)) < 2 || length(colnames(reduced.x)) < 2)
            stop("At least 2 rows and 2 columns must remain after removing supplementary points.")
        suprow <- seq(nrow(x))[is.na(match(rownames(x), rownames(reduced.x)))]
        supcol <- seq(ncol(x))[is.na(match(colnames(x), colnames(reduced.x)))]
        removed.labels <- c((rownames(x)[suprow]), (colnames(x)[supcol]))
        if (square)
            removed.labels <- unique(removed.labels)
        else if (any(duplicated(removed.labels)))
            warning("The following labels refer to multiple points: ", removed.labels[duplicated(removed.labels)])

        supp.labels <- unlist(strsplit(supplementary, split = ","))
        unmatched.labels <- setdiff(tolower(trimws(supp.labels)), tolower(trimws(removed.labels)))
        if (!identical(unmatched.labels, character(0)))
            stop(paste0("Supplementary rows or columns '", paste(unmatched.labels, collapse = ", "),
                        "' do not match any rows or columns in the data."))
        if (!identical(removed.labels, character(0)))
            footer <- paste0(footer, ". Supplementary points: ", paste(removed.labels, collapse = ", "))

    }

    original <- ca(x, suprow = suprow, supcol = supcol, ...)

    if (mirror.horizontal) {
        original$rowcoord[, dim1.plot] <- original$rowcoord[, dim1.plot] * -1
        original$colcoord[, dim1.plot] <- original$colcoord[, dim1.plot] * -1
    }
    if (mirror.vertical) {
        original$rowcoord[, dim2.plot] <- original$rowcoord[, dim2.plot] * -1
        original$colcoord[, dim2.plot] <- original$colcoord[, dim2.plot] * -1
    }

    focused <- if (!is.null(focus) && focus != "") {
        footer <- paste0(footer, ". Focus: ", focus)
        focus <- tolower(trimws(focus))
        row.col.names <- tolower(trimws(c(original$rownames, original$colnames)))
        if (!focus %in% row.col.names)
            stop(paste0("Focus label '", focus, "' is not a label in the input table."))
        focused <- setFocus(original, match(focus, row.col.names))
    }
    else
        NULL

    ca.obj <- if (!is.null(focused)) focused
    else original
    inertia <- round(ca.obj$sv^2, 6)
    col.labels <- sprintf("Dimension %d (%.1f%%)", 1:length(inertia),
                          100*prop.table(inertia))
    if (square)
    {
        n1 <- nrow(x)/2
        std.coords <- original$rowcoord[1:n1,]   # not normalized
        row.coordinates <- sweep(std.coords, 2, original$sv, "*")
        colnames(row.coordinates) <- col.labels
        column.coordinates <- NULL

    } else
    {
        normed <- CANormalization(ca.obj, normalization)
        row.coordinates <- normed$row.coordinates
        column.coordinates <- normed$column.coordinates
        colnames(row.coordinates) <- col.labels
        colnames(column.coordinates) <- col.labels
        if (ncol(row.coordinates) == 1)
        {
            row.coordinates <- cbind(row.coordinates, 'Dimension 2 (0.0%)' = 0)
            column.coordinates <- cbind(column.coordinates, 'Dimension 2 (0.0)' = 0)
        }
    }

    result <- list(x = x,
                   original = original,
                   focused = focused,
                   row.coordinates = row.coordinates,
                   column.coordinates = column.coordinates,
                   row.column.names = row.column.names,
                   normalization = normalization,
                   output = output,
                   color.palette = color.palette,
                   row.color = row.color,
                   col.color = col.color,
                   bubble.size = bubble.size,
                   bubble.title = bubble.title,
                   chart.title = chart.title,
                   logos = logos,
                   logo.size = logo.size,
                   transpose = transpose,
                   trend.lines = trend.lines,
                   show.gridlines = show.gridlines,
                   num.tables = num.tables,
                   max.row.labels.plot = max.row.labels.plot,
                   max.col.labels.plot = max.col.labels.plot,
                   square = square,
                   dim1.plot = dim1.plot,
                   dim2.plot = dim2.plot,
                   footer = footer,
                   footer.wrap.length = footer.wrap.length,
                   dim2.plot = 2,
                   title.font.size = title.font.size,
                   x.title.font.size = x.title.font.size,
                   y.title.font.size = y.title.font.size,
                   labels.font.size = labels.font.size,
                   axis.font.size = axis.font.size,
                   legend.font.size = legend.font.size
    )
    class(result) <- c("CorrespondenceAnalysis")
    nc <- min(ncol(row.coordinates), ncol(column.coordinates))
    if (dim1.plot < 0 || dim1.plot > nc)
        stop(sprintf("Dimension 1 should be between 1 and %d.", nc))
    if (dim2.plot < 0 || dim2.plot > nc)
        stop(sprintf("Dimension 2 should be between 1 and %d.", nc))

    # Store chart data - to use in print.CorrespondenceAnalysis
    plot.dims <- c(dim1.plot, dim2.plot)
    tmp.data <- rbind(row.coordinates[,plot.dims], column.coordinates[,plot.dims])
    if (num.tables == 1)
    {
        n1 <- nrow(row.coordinates)
        n2 <- sum(nrow(column.coordinates))
        groups <- rep(row.column.names, c(n1, n2))
    } else
    {
        n1 <- nrow(x)/num.tables
        n2 <- nrow(column.coordinates)
        groups <- c(rep(paste0("R", 1:n1), num.tables), paste0("C", 1:n2))
    }

    if (output == "Bubble Chart")
        attr(result, "ChartData") <- data.frame(tmp.data,
            Size = c(bubble.size, rep(max(bubble.size)/75, length(original$colnames))),
            Group = groups, check.names = FALSE, check.rows = FALSE, stringsAsFactors = FALSE)
    else
        attr(result, "ChartData") <-  data.frame(tmp.data, Group = groups,
            check.names = FALSE, check.rows = FALSE, stringsAsFactors = FALSE)

    result
}

#' @importFrom flipFormat ExtractChartData
#' @export
flipFormat::ExtractChartData

#' @export
ExtractChartData.CorrespondenceAnalysis <- function(x)
{
    data <- attr(x, "ChartData")
    if (NCOL(data) == 3)
        attr(data, "scatter.variable.indices") <- c(x = 1, y = 2, sizes = NA, colors = 3)
    if (!is.null(x$footer) && nchar(x$footer) > 0)
        attr(data, "footer") <- x$footer
    return(data)
}


#' \code{print.CorrespondenceAnalysis}
#' @description Creates a plot displaying the correspondence analysis results.
#' @param x CorrespondenceAnalysis object.
#' @param ... further arguments passed to or from other methods.
#' @import ca
#' @importFrom rhtmlMoonPlot moonplot
#' @importFrom rhtmlLabeledScatter LabeledScatter
#' @importFrom flipTransformations TextAsVector
#' @importFrom flipChartBasics ChartColors
#' @export
#' @method print CorrespondenceAnalysis
print.CorrespondenceAnalysis <- function(x, ...)
{
    if (x$output == "Diagnostics")
    {
        if (!is.null(x$focused))
            stop("Output should not be set to 'Diagnostics' when 'Focus' has been set.")
        return(summary(x$original))
    } else if (x$output == "Input Table")
    {
        if (x$square)
        {
            n1 <- nrow(x$x)/2
            x.data <- x$x[1:n1, 1:n1]
        } else
            x.data <- as.matrix(x$x)
        return(print(x.data))
    } else if (x$output == "Moonplot")
    {
        if (x$normalization != "Row principal" && x$normalization != "Row principal (scaled)")
            warning("It is good practice to set 'Normalization' to 'Row principal' when 'Output' is set to 'Moonplot'.")
        return(print(moonplot(x$row.coordinates[,1:2], x$column.coordinates[,1:2])))
    }

    # set up info for plotting
    coords <- attr(x, "ChartData")
    if (x$square)
    {
        n1 <- nrow(x$x)/2
        groups <- rep(1, n1)
        colors <- c(x$row.color, n1)
        n2 <- 0

        # Find asymmetric factors
        tmp.sv <- round(x$original$sv, 6)
        n.sv <- length(tmp.sv)
        ind.asym <- which(duplicated(tmp.sv) | duplicated(tmp.sv, fromLast=T))
        ind.sym <- setdiff(1:n.sv, ind.asym)
        if (x$output == "Scatterplot")
        {
            if (x$dim1.plot == x$dim2.plot)
                stop("Dimensions are not distinct.")
            if (x$dim1.plot < 1 || x$dim1.plot > n.sv)
                stop("Dimension 1 should be between 1 and ", n.sv, ".")
            if (x$dim2.plot < 1 || x$dim2.plot > n.sv)
                stop("Dimension 2 should be between 1 and ", n.sv, ".")

            num.asym <- sum(c(x$dim1.plot, x$dim2.plot)%in% ind.asym)
            if (num.asym > 0  && tmp.sv[x$dim1.plot] != tmp.sv[x$dim2.plot])
            {
                asym.pair <- sapply(ind.asym, function(ii){which(tmp.sv == tmp.sv[ii])})
                asym.str <- paste(apply(asym.pair[,seq(1, by=2, to=ncol(asym.pair)), drop=F], 2,
                                        function(x){paste(x, collapse=" and ")}),
                                  collapse="; or ")
                warning("Asymmetric dimensions should only be plotted in the following pairs: ",
                        asym.str, ". Alternatively, symmetric dimensions can be plotted together in any combination. ",
                        "The two first symmetric dimensions are ", paste(ind.sym[1:2], collapse=" and "), ".")
            }
        }

    } else if (x$num.tables == 1)
    {
        if (sum(nchar(x$row.column.names)) > 0 && x$row.column.names[1] == x$row.column.names[2])
            warning("Row and column titles are identical which will cause the same label to be used for both.")
        n1 <- nrow(x$row.coordinates)
        n2 <- nrow(x$column.coordinates)
        colors <- c(x$row.color, x$col.color)

    } else
    {
        n1 <- nrow(x$x)/x$num.tables
        n2 <- nrow(x$column.coordinates)
        colors <- ChartColors(n1+1, x$color.palette)
        colors <- colors[c((1:n1)+1, rep(1,n2))]
    }

    if (x$output %in% c("Scatterplot", "Bubble Chart"))
    {
        lab <- rownames(coords)
        x.nrow <- nrow(x$x) / (1 + x$square)
        if (x$num.tables > 1 && x$trend.lines)
            lab[1:n1] <- x$row.column.names[1:n1]
        logo.size <- NA
        logo.urls <- try(TextAsVector(x$logos)) # This function gives warnings if it doesn't work
        if (!is.null(logo.urls) && !inherits(logo.urls, "try-error"))
        {
            logo.required.length <- if (x$num.tables > 1) n1
                                    else              x.nrow
            if (length(logo.urls) != logo.required.length)
                stop(sprintf("Number of URLs supplied in logos must be equal to the number of %s in the table (%d)\n",
                             ifelse(x$transpose, "columns", "rows"), logo.required.length))
            if (any(nchar(logo.urls) == 0))
                stop("Logos cannot be an empty string\n")
            if (x$num.tables > 1)
                logo.urls <- rep(logo.urls, x$num.tables)
            lab[1:x.nrow] <- logo.urls
            logo.size <- rep(x$logo.size, length(lab))
        }

        n1.tot <- n1 * x$num.tables
        if (x$max.row.labels.plot >= 0 && (x$trend.lines && x$max.row.labels.plot < n1 ||
                                           !x$trend.lines && x$max.row.labels.plot < n1.tot))
        {
            warning("Some row labels have been hidden. Adjust 'Maximum row labels to plot' to show more labels.")
            lab[(x$max.row.labels.plot+1):n1.tot] <- ""
        }
        if (x$max.col.labels.plot >= 0 && x$max.col.labels.plot < n2)
        {
            warning("Some column labels have been hidden. Adjust 'Maximum column labels to plot' to show more labels.")
            lab[((x$max.col.labels.plot+1):n2)+n1.tot] <- ""
        }
        g.ind <- 3 + (NCOL(coords) > 3)
        print(LabeledScatter(X = coords[,1],
                             Y = coords[,2],
                             Z = if (NCOL(coords) > 3) coords[,3] else NULL,
                             label = lab,
                             label.alt = rownames(coords),
                             group = coords[, g.ind],
                             colors = colors,
                             labels.logo.scale = logo.size,
                             trend.lines.show = x$trend.lines,
                             trend.lines.line.thickness = 1,
                             trend.lines.point.size = 2,
                             fixed.aspect = TRUE,
                             title = x$chart.title,
                             x.title = colnames(coords)[1],
                             y.title = colnames(coords)[2],
                             z.title = x$bubble.title,
                             grid = x$show.gridlines,
                             axis.font.size = x$axis.font.size,
                             labels.font.size = x$labels.font.size,
                             title.font.size = x$title.font.size,
                             legend.show = x$num.tables==1 && !x$square && any(nchar(coords[,g.ind]) > 0),
                             legend.font.size = x$legend.font.size,
                             y.title.font.size = x$y.title.font.size,
                             x.title.font.size = x$x.title.font.size,
                             footer = wrapText(x$footer, x$footer.wrap.length),
                             footer.font.size = x$axis.font.size,
                             debug.mode = grepl("DEBUG_MODE_ON", x$chart.title)))

    } else if (x$square)
    {
        # Text output
        # No description of the data
        n1 <- nrow(x$x)/2
        coords <- x$row.coordinates
        std.coords <- x$original$rowcoord[1:n1,]
        colnames(coords) <- sprintf("Dimension %d", 1:ncol(coords))
        colnames(std.coords) <- colnames(coords)
        inertia <- x$original$sv^2
        cat("Correspondence analysis of a square table\n")
        cat("\nInertia(s):\n")
        res.summary <- cbind('Canonical Correlation' = x$original$sv,
                             'Inertia' = inertia,
                             'Proportion explained' = inertia/sum(inertia))
        rownames(res.summary) <- sprintf("Dimension %d", 1:nrow(res.summary))
        print(res.summary)
        cat("\nStandard coordinates:\n")
        print(std.coords)
        cat("\nPrincipal coordinates:\n")
        print(coords)

        prop.sym <- sum(inertia[ind.sym]/sum(inertia)) * 100
        cat(sprintf("\n%.1f%% symmetrical\n", prop.sym))
        cat("\nScores of symmetric dimensions:\n")
        print(coords[,ind.sym])
    } else
    {
        if (!is.null(x$focused)) {
            cat("**** AFTER FOCUS ROTATION ****\n")
            cat("\n Principal inertias (eigenvalues):\n")
            Value <- round(x$focused$sv^2, 6)
            Percentage <- paste(as.character(round(100 * Value/sum(Value), 2)), "%", sep = "")
            eigenvalues <- rbind(Value = as.character(Value), Percentage = as.character(Percentage))
            colnames(eigenvalues) <- 1:length(x$focused$sv)
            print.table(eigenvalues, width = 4)
            cat("\n  Rows in standard coordinates:\n")
            print(x$focused$rowcoord)
            cat("\n  Columns in standard coordinates:\n")
            print(x$focused$colcoord)
            cat("\n**** BEFORE FOCUS ROTATION ****\n")
        }
        unrotated <- x$original
        if (ncol(coords) == 1 || all(coords[,2] == 0))
            unrotated$nd <- 1
        cat("\nStandard coordinates:\n")
        print(unrotated, ...)
    }
}


#' \code{CANormalization}
#' @description Produces normalized coordinates from a \code{\link[ca]{ca}} object.
#' @param ca.object The object to normalize.
#' @param normalization The method used to normalize the coordinates of the
#'   correspondence analysis (this changes the plot, but not the outputs of
#'   \code{\link[ca]{ca}} itself. The default method is \code{"Principal"},
#'   which calculates the principal coordinates (i.e., the standard coordinates
#'   multipled by the singular values).
#'   \code{"Row principal"} and \code{"Column principal"} produce the standard
#'   coordinates of the columns (rows) against the principal coordinates.
#'   \code{"Row principal (scaled)"} and \code{"Column principal (scaled)"}
#'   produce the standard coordinates of the columns (rows) scaled by the first
#'   singular value so as to appear on a similar scale to rows (columns).
#'   Rows (columns) are in principal coordinates.
#'   \code{"Symmetrical"} returns the standard coordinates multiplied
#'   by the square root of the singular values.
#'   \code{"Inverse"} takes an object specified in terms of principal coordinates
#'   and calculates standard coordinates.
#'   \code{"None"} returns the standard coordinates.
#' @export
CANormalization <- function(ca.object, normalization = "Principal")
{
    .normalize = function(coords, power)
    {
        m <- dim(coords)[2]
        if (dim(coords)[2] == 1)
            coords[,1, drop = FALSE] * ca.object$sv[1]^power
        else
            sweep(coords[,1:m], 2, ca.object$sv[1:m]^power, "*")
    }
    rows <- .normalize(ca.object$rowcoord, switch(normalization,
                                                  "Principal" = 1, "Row principal" = 1, "Row principal (scaled)" = 1,
                                                  "Column principal" = 0, "Column principal (scaled)" = 0,
                                                  "Symmetrical (\u00BD)" = 0.5, "None" = 0, "Inverse" = -1))
    columns <- .normalize(ca.object$colcoord, switch(normalization,
                                                     "Principal" = 1, "Row principal" = 0, "Row principal (scaled)" = 0,
                                                     "Column principal" = 1, "Column principal (scaled)" = 1,
                                                     "Symmetrical (\u00BD)" = 0.5, "None" = 0, "Inverse" = -1))

    if (normalization == "Row principal (scaled)")
        columns = columns * ca.object$sv[1]
    if (normalization == "Column principal (scaled)")
        rows = rows * ca.object$sv[1]

    list(row.coordinates = rows, column.coordinates = columns)
}

#' \code{CAQuality}
#' @description Quality measures of a correspondence analysis.
#' @param x The object to compute quality for.
#' @importFrom methods is
#' @importFrom flipFormat FormatAsPercent
#' @export
CAQuality <- function(x)
{
    if (!is(x, "CorrespondenceAnalysis"))
        stop("Object must be of class 'CorrespondenceAnalysis' to calculate quality.")
    or <- if (is.null(x$focused)) x$original else x$focused
    n <- CANormalization(or, "Principal")
    row.masses <- x$original$rowmass
    row.masses[is.na(row.masses)] <- 0
    e <- colSums(sweep(n$row.coordinates^2, 1, row.masses, "*"))
    e <- FormatAsPercent(prop.table(e), decimals = 1, remove.leading.0 = TRUE)
    q <- rbind(n$row.coordinates, n$column.coordinates)
    q <- prop.table(q ^ 2, 1) * 100
    colnames(q) <- paste0(colnames(q), "\n", e)
    rownames(q) <- paste(FormatAsPercent((q[, 1] + q[, 2])/100, decimals = 0, pad = TRUE, remove.leading.0 = TRUE), rownames(q))
    attr(q, "statistic") <- "Quality %"
    q
}

