
# Sort a loadings table in the same way that SPSS does. This is to:
# 1. Assign each variable to the component to which it has the largest
#    loading (in absolute value).
# 2. Sort the variables for the first component, followed by the variables
#    for the second component, and so on until all variables have been sorted.
sortLoadings <- function(x) {
    mc <- max.col(abs(x))
    inds <- cbind(1:nrow(x), mc)
    vals <- abs(x)[inds] * 10^(-2*mc)
    y <- x[order(vals, decreasing = TRUE),]
    return(y)
}



#' @importFrom stats lm.fit setNames
#' @importFrom flipFormat PCALoadingsTable VarianceExplainedTable FormatWithDecimals FormatAsPercent ExtractCommonPrefix
#' @export
print.flipFactorAnalysis <- function(x, digits = 3,...)
{
    min.display.loading.value <- if (x$suppress.small.coefficients) x$min.display.loading.value else 0

    .tidy.loadings <- function(x, input.matrix)
    {
        if (x$sort.coefficients.by.size)
            input.matrix <- sortLoadings(input.matrix)
        as.matrix(unclass(input.matrix))
    }

    .create.printed.loadings <- function(x, input.matrix, digits)
    {
        tidy.matrix <- .tidy.loadings(x, input.matrix)

        p <- nrow(tidy.matrix)
        n.factors <- ncol(tidy.matrix)

        printed.matrix <- setNames(format(round(tidy.matrix, digits)), NULL)
        nc <- nchar(printed.matrix[1], type = "c")
        printed.matrix[abs(tidy.matrix) < min.display.loading.value] <- paste(rep(" ", nc), collapse = "")
        return(printed.matrix)
    }

    print.type <- x$print.type
    if (print.type == "Loadings Table")
    {
        print.type <- "loadings"
    } else if (print.type == "Structure Matrix")
    {
        print.type <- "structure"
    } else if (print.type == "Detailed Output")
    {
        print.type <- "details"
    } else if (print.type == "Variance Explained")
    {
        print.type <- "variance"
    } else if (print.type == "2D Scatterplot")
    {
        print.type <- "2d"
    }


    # Work out caption information. This is shared between a number of print types
    caption.info <- list()
    if (x$use.correlation)
    {
        caption.info$correlation <- "Input: Correlation matrix"
    } else {
        caption.info$correlation <- "Input: Covariance matrix"
    }
    caption.info$missing <- paste0("Missing data setting: ", x$missing)

    if (x$missing == "Use partial data (pairwise correlations)" || x$missing == "pairwise")
    {
        min.sample <- length(which(!is.na(rowSums(x$data.used$subset.data))))
        max.sample <- nrow(x$data.used$subset.data)
        caption.info$sample <- paste0("Sample size: ", min.sample, " to ", max.sample)

    } else {
        caption.info$sample <- paste0("Sample size: ", nrow(x$data.used$subset.data))
    }
    caption.info$rotation <- paste0("Rotation: ", toupper(substr(x$rotation, 1, 1)), substr(x$rotation, 2, nchar(x$rotation)))

    if (print.type == "scree" || print.type == "Scree Plot") {
        print(ScreePlot(x))
    } else if (print.type == "Component Plot") {
        print(ComponentPlot(x, show.labels = x$plot.labels))
    } else if (print.type == "variance") {
        eigenvalues <- x$values
        variance.proportions = eigenvalues / sum(eigenvalues)
        cumulative.proportions = cumsum(variance.proportions)
        # Don't mention the rotation as this information is relevant to the unrotated components
        caption.info$rotation <- NULL
        table.caption <- paste(c("Unrotated Variance Explained", unlist(caption.info)), collapse = "; ")
        tbl <- VarianceExplainedTable(eigenvalues, variance.proportions, cumulative.proportions,
                                      title = "Variance Explained", footer = table.caption)
        print(tbl)
    } else {
        # What kind of table is being printed?
        if (x$use.correlation)
        {
            if (x$rotation != "none")
            {
                loadings.caption <- "Rotated loadings"
                structure.caption <- "Rotated structure matrix"
            } else {
                loadings.caption <- "Loadings"
                structure.caption <- "Structure matrix"
            }
        } else {
            if (x$rotation != "none")
            {
                loadings.caption <- "Rescaled rotated loadings"
                structure.caption <- "Rescaled rotated structure matrix"
            } else {
                loadings.caption <- "Rescaled loadings"
                structure.caption <- "Rescaled structure matrix"
            }
        }

        nvar <- ncol(x$original.data)
        eigenvalues.caption <- paste("Unrotated eigenvalues:",
                                     paste0(paste0(rep("(", nvar), 1:nvar, rep(") ", nvar), FormatWithDecimals(x$values, 2)), collapse = ", "))

        oblique.rotation <- x$rotation == "oblimin" || x$rotation == "promax"
        if (print.type == "structure" && !oblique.rotation)
            warning(paste0("The structure matrix is the same as the loadings matrix for the rotation option: ", x$rotation))

        ss.loadings <- if (oblique.rotation)
            colSums(x$structure.matrix ^ 2)
        else
            colSums(x$loadings ^ 2)

        if (!oblique.rotation)
        {
            ss.loadings <- colSums(x$loadings ^ 2)
            eigenvalue.label <- "Eigenvalue"
        }
        else
        {
            ss.loadings <- colSums(x$structure.matrix ^ 2)
            eigenvalue.label <- "Eigenvalue*"
        }

        if (print.type == "loadings") {
            if (!oblique.rotation) {
                subtitle <- paste(ncol(x$loadings), ifelse(ncol(x$loadings) == 1, "component", "components"),
                                  "explaining", FormatAsPercent(sum(ss.loadings) / nvar, 3), "of the variance")
                variance.explained <- ss.loadings / nvar
            } else {
                subtitle <- ""
                variance.explained <- NULL
            }
            footer <- if (!oblique.rotation)
                paste(c(loadings.caption, unlist(caption.info), eigenvalues.caption), collapse = "; ")
            else
                paste(c(loadings.caption, unlist(caption.info), "*Rotation Sums of Squared Loadings",
                        eigenvalues.caption), collapse = "; ")
            loadings <- .tidy.loadings(x, input.matrix = x$loadings)
            extracted <- ExtractCommonPrefix(row.names(loadings))
            title <- "Principal Component Loadings"
            if (!is.na(extracted$common.prefix))
            {
                title <- paste0(title, ": ", extracted$common.prefix)
                row.names(loadings) <- extracted$shortened.labels
            }
            tbl <- PCALoadingsTable(loadings, variance.explained, ss.loadings, min.display.loading.value,
                                    title = title, subtitle = subtitle, footer = footer,
                                    eigenvalue.label = eigenvalue.label)
            print(tbl)

        } else if (print.type == "structure") {

            footer <- if (!oblique.rotation)
                paste(c(structure.caption, unlist(caption.info), eigenvalues.caption), collapse = "; ")
            else
                paste(c(structure.caption, unlist(caption.info), "*Rotation Sums of Squared Loadings",
                        eigenvalues.caption), collapse = "; ")
            loadings <- .tidy.loadings(x, input.matrix = x$structure.matrix)
            extracted <- ExtractCommonPrefix(row.names(loadings))
            title <- "Principal Component Structure"
            if (!is.na(extracted$common.prefix))
            {
                title <- paste0(title, ": ", extracted$common.prefix)
                row.names(loadings) <- extracted$shortened.labels
            }
            tbl <- PCALoadingsTable(loadings, NULL, ss.loadings, min.display.loading.value, title = title,
                                    footer = footer, eigenvalue.label = eigenvalue.label)
            print(tbl)

        } else if (print.type == "details") {

            # Table printing
            tidied.loadings.matrix <- .create.printed.loadings(x, input.matrix = x$loadings, digits = digits)
            tidied.structure.matrix <- .create.printed.loadings(x, input.matrix = x$structure.matrix, digits = digits)

            # Analysis Information
            # - Missing data setting
            # - Sample size(s)
            # - Is weighted
            # - Used a correlation or covariance matrix
            cat("Principal Components Analysis\r\n\r\n")
            cat(paste(unlist(caption.info), collapse = "\r\n"))
            cat("\r\n\r\n")

            # Loading table
            cat(paste0(loadings.caption, ":\r\n\r\n"))
            print(tidied.loadings.matrix, quote = FALSE)

            # Structure table and sum of squares loadings
            # If there is an oblique rotation then
            # we don't print the variance-explained, because
            # the factors are correlated.
            if (oblique.rotation)
            {
                cat(paste0("\r\n", structure.caption, ":\r\n\r\n"))
                print(tidied.structure.matrix, quote = FALSE)
                ve.table <- rbind(`Sum of Square Loadings` = ss.loadings)
                cat("\r\n")
                print(round(ve.table, digits))
            } else {
                ve.table <- rbind(`Sum of Square Loadings` = ss.loadings)
                ve.table <- rbind(ve.table, `% of Variance` = ss.loadings/nvar*100)
                if (ncol(x$loadings) > 1)
                {
                    ve.table <- rbind(ve.table, `Cumulative %` = cumsum(ss.loadings/nvar*100))
                }
                cat("\r\n")
                print(round(ve.table, digits))
            }

            # Communalities
            cat("\r\nCommunalities:\r\n\r\n")
            communality.table <- cbind("Initial" = x$initial.communalities, "Extraction" = x$extracted.communalities)
            print(round(communality.table, digits))
            if (!x$use.correlation)
            {
                cat("\r\nRescaled Communalities:\r\n\r\n")
                communality.table <- cbind("Initial" = x$rescaled.initial.communalities,
                                           "Extraction" = x$rescaled.extracted.communalities)
                print(round(communality.table, digits))
            }

            # Score weights
            cat("\r\nScore Coefficient Matrix:\r\n\r\n")
            print(round(x$score.weights, digits))

        } else if (print.type == "2d") {

            if (ncol(x$loadings) < 2)
                stop("There aren't enough components to plot.")

            # Remove cases with no mapping or missing labels
            complete <- complete.cases(x$scores[, 1:2])
            if (!is.null(x$groups))
                complete <- complete & complete.cases(x$groups)

            plot.2d <- list(embedding = x$scores[complete, 1:2],
                            data.labels = x$groups[complete],
                            title = "PCA")
            class(plot.2d) <- "2D"
            print(plot.2d)

        } else {
            warning(paste0("Unknown print type for principal components analysis: ", print.type, ". Printing a component plot instead."))
            print(ComponentPlot(x, show.labels = x$plot.labels))
        }
    }
}
