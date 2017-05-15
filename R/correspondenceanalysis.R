#' \code{CorrespondenceAnalysis}
#' @description Removes rows or columns from the table.
#' @param x A table or a list of tables.
#' @param normalization The method used to normalize the coordinates of the
#'   correspondence analysis plot (this changes the plot, but not the outputs of
#'   \code{\link[ca]{ca}} itself. The default method is \code{"Principal"},
#'   which plots the principal coordinates (i.e., the standard coordinates
#'   multiplied by the singular values). \code{"Row principal"} and \code{"Column
#'   principal"} plot the standard coordinates of the columns (rows) against the
#'   principal coordinates. Note that the plotting occurs via
#'   \code{\link{print.CorrespondenceAnalysis}}.
#' @param output How the map is displayed: \code{"Scatterplot"}, or \code{"Moonplot"}, or \code{"Text"}.
#' @param row.names.to.remove A vector of the row labels to remove.
#' @param column.names.to.remove A vector of the column labels to remove.
#'   variable is provided, any cases with missing values on this variable are
#'   excluded from the final data file.
#' @details Where a matrix or array is passed in containing names for the dimensions, these are used to represent the rows
#' and columns in the legend. If there are no names, then the names are assumed to be the contents of \code{attr(x, "row.column.names")}.
#' If there are still no names, they are assumed to be \code{Rows} and \code{Columns}, respectively.
#' @param row.color Color to display row-attributes in scatterplot with one table.
#' @param col.color Color to display column-attributes in scatterplot with one table.
#' @param color.palette Palette used to color scatterplot when multiple tables are used.
#' @param bubble.size A vector of magnitudes for the row coordinate (for bubble charts). This is optional.
#' @param bubble.title A label for the legend.
#' @param chart.title Title of chart.
#' @param logos Optional list of images to be used to label scatterplot instead of the row names. It should be inputted as a comma-seperated list of URLs.
#' @param logo.size Numeric controlling the size of the logos.
#' @param transpose Boolean indicating whether the rows and columns of \code{x} should be swapped.
#' @param trend.lines Boolean indicating whether to draw trend lines when multiple tables are supplied.
#' @param multiple.tables Optional boolean indicating whether or not multiple tables have been supplied.
#'   If no value is given, it will be guessed from the structure of \code{x}.
#' @param square Boolean indicating whether the input table is square. If true the row and column names of the table must be the same.
#' @param ... Optional arguments for \code{\link[ca]{ca}}.
#' @importFrom flipData GetTidyTwoDimensionalArray
#' @importFrom ca ca
#' @export
CorrespondenceAnalysis = function(x,
                                  normalization = "Principal",
                                  output = c("Scatterplot", "Bubble Chart", "Moonplot", "Text", "Input Table")[1],
                                  row.names.to.remove = c("NET", "Total", "SUM"),
                                  column.names.to.remove = c("NET", "Total", "SUM"),
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
                                  multiple.tables = NA,
                                  square = FALSE,
                                  ...)
{
    # Mask undefined arguments for R Gui control
    if (!output %in% c("Scatterplot", "Bubble Chart"))
    {
        chart.title <- ""
        row.color <- ""
        col.color <- ""
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

    # Multiple tables
    # note that a dataframe is actually a list
    if (!is.null(dim(x[[1]])) && length(x) > 1)
    {
        if (!is.na(multiple.tables) && !multiple.tables)
            stop("Input data 'x' contains multiple tables. Select checkbox for 'multiple tables'\n")

        if (!output %in% c("Scatterplot", "Input Table"))
            stop(sprintf("Output '%s' is not valid with multiple input tables.", output))
        row.color <- '#5B9BD5'
        col.color <- '#ED7D31'
        square <- FALSE

        # Get table names
        num.tables <- length(x)
        x.names <- rep("", num.tables)
        unnamed.tables <- FALSE
        for (i in 1:num.tables)
        {
            if (is.null(attr(x[[i]], "questions")))
            {
                unnamed.tables <- TRUE
                attr(x[[i]], "questions") <- as.character(i)
            }
            x.names[i] <- attr(x[[i]], "questions")
        }
        if (unnamed.tables)
                warning("You can name tables using R code: 'attr(table.name, \"questions\") <- \"Description\"'")

        # Check tables match - order of rows will match first table
        x[[1]] <- if (transpose) GetTidyTwoDimensionalArray(t(x[[1]]), row.names.to.remove, column.names.to.remove)
                  else GetTidyTwoDimensionalArray(x[[1]], row.names.to.remove, column.names.to.remove)
        r.names <- rownames(x[[1]])
        c.names <- colnames(x[[1]])
        for (i in 2:num.tables)
        {
            if (transpose)
                x[[i]] <- t(x[[i]])
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
        # Convert list of 1 table
        if (!is.null(dim(x[[1]])))
            x <- x[[1]]

        if (!is.na(multiple.tables) && multiple.tables)
            stop("Input data 'x' contains only one table. Unselect checkbox for 'multiple tables'\n")

        num.tables <- 1
        color.palette <- "Default colors"
        trend.lines <- FALSE
        row.column.names.attribute <- attr(x, "row.column.names")
        x <- GetTidyTwoDimensionalArray(x, row.names.to.remove, column.names.to.remove)
        if (transpose)
            x <- t(x)
        row.column.names <- names(dimnames(x))
        if (is.null(row.column.names))
            row.column.names <- row.column.names.attribute
        if (is.null(row.column.names))
            row.column.names <- c("Rows", "Columns")

        if (square)
        {
            if (nrow(x) != ncol(x) || any(rownames(x) != colnames(x)))
                stop("Input Table is not a square matrix.")
            if (output == "Moonplot")
                stop("Output 'Moonplot' is not valid with square matrixes.")
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
            if (is.null(bubble.names <- names(bubble.size)))
                stop("The bubble sizes need to be named.")
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
            order = match(rownames(x), names(bubble.size))
            if (sum(order, na.rm = TRUE) != sum(1:length(order)))
                stop("The bubble sizes must contain the same names as in the rows of the input data table: ",
                     paste0(paste0(table.names, ":", bubble.names), collapse = ", "), ".")
            bubble.size = bubble.size[order]
        }

        # Expand square matrix after checking against bubble names
        if (square)
            x <- cbind(rbind(x, t(x)), rbind(t(x), x))
    }
    result <- list(x = x,
                   row.column.names = row.column.names,
                   normalization = normalization,
                   output = output,
                   color.palette = color.palette,
                   row.color = row.color,
                   col.color = col.color,
                   original = ca(x, ...),
                   bubble.size = bubble.size,
                   bubble.title = bubble.title,
                   chart.title = chart.title,
                   logos = logos,
                   logo.size = logo.size,
                   transpose = transpose,
                   trend.lines = trend.lines,
                   num.tables = num.tables,
                   square = square)
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
#' @importFrom flipTransformations TextAsVector
#' @importFrom flipChartBasics ChartColors
#' @export
print.CorrespondenceAnalysis <- function(x, ...)
{
    ca.obj <- x$original
    singular.values <- round(ca.obj$sv^2, 6)
    variance.explained <- paste(as.character(round(100 * prop.table(singular.values), 1)), "%", sep = "")[1:2]
    column.labels <- paste("Dimension", 1:2, paste0("(", variance.explained, ")"))

    if (x$square)
    {
        n2 <- nrow(x$x)/2
        colnames(ca.obj$rowcoord) <- sprintf("Dimension %d", 1:ncol(ca.obj$rowcoord))
        coords <- sweep(ca.obj$rowcoord[1:n2,], 2, ca.obj$sv, "*")
        x.data <- x$x[1:n2, 1:n2]

    } else
    {
        normed <- CANormalization(ca.obj, x$normalization)
        row.coordinates <- normed$row.coordinates
        column.coordinates <- normed$column.coordinates
        coords <- rbind(row.coordinates, column.coordinates)
        row.column.names <- x$row.column.names
        x.data <- as.matrix(x$x)
    }

    if (ncol(coords) == 1) # dealing with 1D case
    {
        if (x$output == "Text")
            ca.obj$nd <- 1
        else if(x$output == "Scatterplot")
            coords <- cbind(coords, 0)
        else
        {
            ca.obj$rowcoord <- cbind(ca.obj$rowcoord, 0)
            ca.obj$colcoord <- cbind(ca.obj$colcoord, 0)
        }
    }

    if (x$square)
    {
        groups <- rep(1, n2)
        colors <- c(x$row.color, n2)

    } else if (x$num.tables == 1)
    {
        if (sum(nchar(x$row.column.names)) > 0 && x$row.column.names[1] == x$row.column.names[2])
            warning("Row and column titles should not be the same.")
        groups <- rep(x$row.column.names, c(nrow(row.coordinates), nrow(column.coordinates)))
        colors <- c(x$row.color, x$col.color)

    } else
    {
        n2 <- nrow(x$x)/x$num.tables
        ncol <- nrow(column.coordinates)
        #groups <- c(rep(x$row.column.names, x$num.tables), rep("Columns", nrow(column.coordinates)))
        groups <- c(rep(paste0("R", 1:n2), x$num.tables), paste0("C", 1:ncol))
        colors <- ChartColors(n2+1, x$color.palette, trim.light.colors=TRUE)
        colors <- colors[c((1:n2)+1, rep(1,ncol))]
    }

    if (x$output %in% c("Scatterplot", "Bubble Chart"))
    {
        bubble.size <- if (x$output == "Bubble Chart")
            c(x$bubble.size, rep(max(x$bubble.size) / 75, length(x$original$colnames)))
        else
            NULL

        lab <- rownames(coords)
        if (x$num.tables > 1 && x$trend.lines)
            lab[1:n2] <- x$row.column.names[1:n2]
        logo.size <- NA
        logo.urls <- try(TextAsVector(x$logos)) # This function gives warnings if it doesn't work
        if (!is.null(logo.urls) && !inherits(logo.urls, "try-error"))
        {
            logo.required.length <- if (x$num.tables > 1) n2
                                    else                  nrow(x.data)
            if (length(logo.urls) != logo.required.length)
                stop(sprintf("Number of URLs supplied in logos must be equal to the number of %s in the table (%d)\n",
                             ifelse(x$transpose, "columns", "rows"), logo.required.length))
            if (any(nchar(logo.urls) == 0))
                stop("Logos cannot be an empty string\n")
            if (x$num.tables > 1)
                logo.urls <- rep(logo.urls, x$num.tables)
            lab[1:nrow(x.data)] <- logo.urls
            logo.size <- rep(x$logo.size, length(lab))
        }

        print(LabeledScatter(X = coords[,1],
                       Y = coords[,2],
                       Z = bubble.size,
                       label = lab,
                       label.alt = rownames(coords),
                       group = groups,
                       colors = colors,
                       labels.logo.scale = logo.size,
                       trend.lines.show = x$trend.lines,
                       trend.lines.line.thickness = 1,
                       trend.lines.point.size = 2,
                       fixed.aspect = TRUE,
                       title = x$chart.title,
                       x.title = column.labels[1],
                       y.title = column.labels[2],
                       z.title = x$bubble.title,
                       axis.font.size = 10,
                       labels.font.size = 14,
                       title.font.size = 20,
                       legend.show = (x$num.tables==1 && !x$square && all(nchar(groups) > 0)),
                       legend.font.size = 15,
                       y.title.font.size = 16,
                       x.title.font.size = 16))
    }
    else if (x$output == "Moonplot")
    {
        if (x$normalization != "Row principal")
            warning("It is good practice to set 'Normalization' to 'Row principal' when 'Output' is set to 'Moonplot'.")
        print(moonplot(ca.obj$rowcoord[,1:2], ca.obj$colcoord[,1:2]))
    }
    else if (x$output == "Input Table")
    {
        print(x.data)
    }
    else if (x$square)
    {
        # Text output
        # No description of the data
        inertia <- ca.obj$sv^2
        cat("Correspondence analysis of a square table\n")
        cat("\nInertia(s):\n")
        res.summary <- cbind('Canonical Correlation' = ca.obj$sv,
                             'Inertia' = inertia,
                             'Proportion explained' = inertia/sum(inertia))
        rownames(res.summary) <- sprintf("Dimension %d", 1:nrow(res.summary))
        print(res.summary)
        cat("\nStandard coordinates:\n")
        print(ca.obj$rowcoord[1:n2,])
        cat("\nPrincipal coordinates:\n")
        print(coords)

        # Find asymmetric factors
        tmp.sv <- round(ca.obj$sv, 6)
        ind.sym <- which(!duplicated(tmp.sv) & !duplicated(tmp.sv, fromLast=T))

        prop.sym <- sum(inertia[ind.sym]/sum(inertia)) * 100
        cat(sprintf("\n%.1f%% symmetrical\n", prop.sym))
        cat("\nScores of symmetric dimensions:\n")
        print(coords[,ind.sym])
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
    .normalize = function(coords, power)
        if (dim(coords)[2] == 1)
            coords[,1, drop = FALSE] * ca.object$sv[1]^power
        else
            sweep(coords[,1:2], 2, ca.object$sv[1:2]^power, "*")
    rows <- .normalize(ca.object$rowcoord, switch(normalization,
        "Principal" = 1, "Row principal" = 1, "Column principal" = 0, "Symmetrical (\u00BD)" = 0.5, "None" = 0))
    columns <- .normalize(ca.object$colcoord, switch(normalization,
        "Principal" = 1, "Row principal" = 0, "Column principal" = 1, "Symmetrical (\u00BD)" = 0.5, "None" = 0))
    list(row.coordinates = rows, column.coordinates = columns)
}
