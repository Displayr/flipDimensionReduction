
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




#' @importFrom flipFormat DataTableWithRItemFormat
#' @export
print.flipFactorAnalysis <- function(x, digits = 3,...)
{

    .tidy.loadings <- function(x, input.matrix, digits)
    {
        if (x$sort.coefficients.by.size)
        {
            input.matrix <- flipDimensionReduction:::sortLoadings(input.matrix)
        }

        if (x$suppress.small.coefficients)
        {
            min.display.loading.value <- x$min.display.loading.value
        }
        else
        {
            min.display.loading.value <- 0
        }

        tidy.matrix <- as.matrix(unclass(input.matrix))
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
    } else if (print.type == "Structure Matrix") {
        print.type <- "structure"
    } else if (print.type == "Detailed Output") {
        print.type <- "details"
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
    caption.info$rotation <- paste0("Rotation: ", x$rotation)


    if (print.type == "scree" || print.type == "Scree Plot") {
        print(ScreePlot(x))
    } else if (print.type == "Component Plot") {
        print(ComponentPlot(x, show.labels = x$plot.labels))
    } else {
        # Table printing

        if (print.type == "loadings" || print.type == "details")
        {
            tidied.loadings.matrix <- .tidy.loadings(x, input.matrix = x$loadings, digits = digits)
        }

        if (print.type == "structure" || print.type == "details")
        {
            tidied.structure.matrix <- .tidy.loadings(x, input.matrix = x$structure.matrix, digits = digits)
        }

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

        if (print.type == "structure" && x$rotation != "promax" && x$rotation != "oblimin")
        {
            warning(paste0("The structure matrix is the same as the loadings matrix for the rotation option: ", x$rotation))
        }

        if (print.type == "loadings" || print.type == "structure")
        {
            if (print.type == "loadings")
            {
                printed.matrix <- tidied.loadings.matrix
                table.caption <- loadings.caption
            } else {
                printed.matrix <- tidied.structure.matrix
                table.caption <- structure.caption
            }
            table.caption <- paste(c(table.caption, unlist(caption.info)), collapse = "; ")
            dt <- DataTableWithRItemFormat(as.data.frame(printed.matrix),
                                           caption = table.caption,
                                           allow.length.change = FALSE,
                                           page.length = nrow(printed.matrix),
                                           allow.paging = FALSE,
                                           show.info = FALSE,
                                           header.alignments = rep("right", ncol(printed.matrix)))
            print(dt)
        } else if (print.type == "details") {
            nvar <- ncol(x$original.data)
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
            if (x$rotation == "promax" || x$rotation == "oblimin")
            {
                cat(paste0("\r\n", structure.caption, ":\r\n\r\n"))
                print(tidied.structure.matrix, quote = FALSE)
                ss.loadings <- colSums(x$structure.matrix^2)
                ve.table <- rbind(`Sum of Square Loadings` = ss.loadings)
                cat("\r\n")
                print(round(ve.table, digits))
            } else {
                ss.loadings <- colSums(x$loadings^2)
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
        } else {
            warning(paste0("Unknown print type for principal components analysis: ", print.type, ". Printing a component plot instead."))
            print(ComponentPlot(x, show.labels = x$plot.labels))
        }
    }
}
