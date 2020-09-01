#' \code{PrincipalComponentsAnalysis}
#' @description Calculate a Principal Component Analysis
#' @param data A data frame with numeric columns which contains the data to be
#'   analyzed.
#' @param weights A numeric vector containing the weight for each case in data.
#' @param subset A logical vector which describes the subset of \code{data} to
#'   be analyzed.
#' @param missing A string specifiying what to do when the \code{data} contains
#'   missing values. The valid options are \code{"Error if missing data"},
#'   \code{"Exclude cases with missing data"}, \code{"Use partial data (pairwise
#'   correlations)"}, and \code{"Imputation (replace missing values with
#'   estimates)"}.
#' @param use.correlation A logical value specifying whether to use the
#'   correlation matrix (\code{TRUE}), or the covariance matrix (\code{FALSE}).
#' @param rotation A string specifying the type of rotation to be used. Valid
#'   options are  \code{"none"}, \code{"varimax"}, \code{"quartimax"},
#'   \code{"equamax"}, \code{"promax"}, and \code{"oblimin"}.
#' @param oblimin.delta A parameter supplied for oblimin rotations.
#' @param promax.kappa A parameter supplied for promax rotations.
#' @param select.n.rule Method for selecting the number of principal components to keep. May be one of \code{"Kaiser rule"}, \code{"Eigenvalues over"}, or \code{"Number of components"}.
#' @param n.factors An integer specifying the number of principal components to keep. Used if \code{select.n.rule} is \code{"Number of components"}.
#' @param eigen.min Cut-off above which eigenvalues are selected. Used if \code{select.n.rule} is \code{"Eigenvalues over"}.
#' @param sort.coefficients.by.size A logical value determining whether loadings
#'   should be sorted when printed.
#' @param suppress.small.coefficients A logical value specifying whether components
#' that are less than \code{min.display.loading.value} in magnitude will be replaced
#' with blanks when printing.
#' @param min.display.loading.value Loadings smaller than this value will not be
#'   displayed when printed.
#' @param print.type A string specifying the type of printing that should be
#'   done. Valid options are \code{"loadings"} to display a (rotated) loading table,
#'   \code{"structure"} to display a component structure matrix (which is the loadings
#'   multiplied by the component correlations), \code{"details"} to display a plain-text
#'   output containing more details from the analysis, \code{"variance"} to display a
#'   table showing the original eigenvalues of the input, and the corresponding variance
#'   explained,
#'   \code{"scree"} to display a Scree Plot, \code{"scatter"} to display a
#'   plot of the first two dimensions of the final loadings, and \code{"2d"} to plot the
#'   first two dimensions of the data, grouped by a categorical variable.
#'   The latter three options make use of HTML widgets.
#' @param show.labels If \code{TRUE}, labels are shown rather than name in outputs.
#' @param plot.labels A logical value which determines whether or not the
#'   scatter plot will show the labels of the input data, or just integers
#'   specifying the column number of each variable.
#' @param data.groups A \code{\link{vector}} of labels used to group the cases
#' when \code{"print.type"} is \code{"2d"}.
#' @param tol When the correlation martrix (or covariance) matrix has any singular values below this number
#'   the analysis will stop. Note that the function \code{principal} from package \code{psych} has its
#'   own internal cuttoff as well.
#'
#' @details This uses \code{\link[psych]{principal}} from package \code{psych} to compute the unrotated
#' PCA, and uses package \code{GPArotation} to find a rotated solution if required, to match SPSS' PCA. The
#' rotation includes a Kaiser normalization and a method of Promax which matches what SPSS does.
#' Components with large negative loadings will have signs flipped to give positive components after rotation.
#' Includes handling of missing data, weighting, and filtering.
#' @importFrom flipFormat Labels
#' @importFrom flipStatistics CovarianceAndCorrelationMatrix StandardDeviation
#' @importFrom psych principal factor.scores
#' @export
PrincipalComponentsAnalysis <- function(data,
                               weights = NULL,
                               subset = NULL,
                               missing = "Exclude cases with missing data",
                               use.correlation = TRUE,
                               rotation = "none",
                               oblimin.delta = 0,
                               promax.kappa = 4,
                               select.n.rule = "Number of factors",
                               eigen.min = 1.0,
                               n.factors = 2,
                               sort.coefficients.by.size = FALSE,
                               suppress.small.coefficients = FALSE,
                               min.display.loading.value = 0.1,
                               print.type = "loadings",
                               show.labels = TRUE,
                               plot.labels = TRUE,
                               data.groups = NULL,
                               tol = 1e-13)
{
    if (is.null(rownames(data)))
        rownames(data) <- 1:nrow(data)
    if (select.n.rule == "Kaiser rule")
        eigen.min <- 1.0
    if (show.labels && !is.null(Labels(data)))
        colnames(data) <- Labels(data)
    if (rotation != "Promax" && rotation != "promax")
        promax.kappa = NULL
    if (rotation != "Oblimin" && rotation != "oblimin")
        oblimin.delta = NULL
    if (print.type %in% c("Component Plot", "Scree Plot", "Variance Explained", "2D Scatterplot"))
    {
        sort.coefficients.by.size = FALSE
        suppress.small.coefficients = FALSE
        min.display.loading.value = 0.1
    }
    if (print.type != "Component Plot")
        plot.labels = TRUE

    # Generate the data that will be input to the correlation/covariance
    # matrix by filtering and imputing if specified.
    prepared.data <- prepareDataForFactorAnalysis(data, weights, subset, missing)

    # If any variables have a standard deviation of 0 remove from analysis
    stddevs <- StandardDeviation(prepared.data$subset.data, weights = prepared.data$subset.weights)
    ind.zero.variance <- which(stddevs == 0)
    if (length(ind.zero.variance) > 0)
    {
        warning("Some of your variables have no variation and have been removed for principal components analysis: ", paste(names(stddevs)[ind.zero.variance], sep = "", collapse = ", "))
        prepared.data$subset.data <- prepared.data$subset.data[,-ind.zero.variance]
        stddevs <- stddevs[-ind.zero.variance]
    }

    correlation.matrix <- CovarianceAndCorrelationMatrix(data = prepared.data$subset.data,
                                                         weights = prepared.data$subset.weights,
                                                         pairwise = missing == "Use partial data (pairwise correlations)",
                                                         use.correlation = TRUE)

    # SPSS computes the covariance matrix that it uses as an imput to PCA
    # by first calculating the pairwise correlation matrix and then
    # scaling by the products of the standard deviations of each pair of
    # variables. When there is no missing data this ought to match the
    # covariance matrix which would be computed by the usual covariance
    # formula, but when there is missing data the two results will differ.
    if (!use.correlation)
        input.matrix <- correlation.matrix * stddevs %o% stddevs
    else
        input.matrix <- correlation.matrix

    # Compute eigenvalues for component selection
    if (select.n.rule %in% c("Kaiser rule", "Eigenvalues over"))
    {
        eigens <- eigen(input.matrix, only.values=T)
        if (!use.correlation)
        {
            warning("Select components with eigenvalues > ",
                    eigen.min, " times mean of eigenvalues as we are using unscaled covariance matrix\n")
            eigen.min <- mean(eigens$values)
        }
        n.factors <- length(which(eigens$values > eigen.min))
    }
    stdmat <- matrix(rep(stddevs, n.factors), ncol = n.factors)

    # Unrotated loadings
    # Don't return all of the properties returned by
    # principal() as they are not designed to account
    # for weighting
    initial.results <- try(suppressWarnings(principal(input.matrix,
                                 nfactors = n.factors,
                                 rotate = "none",
                                 covar = !use.correlation,
                                 scores = FALSE)), silent = TRUE)
    if (inherits(initial.results, "try-error"))
        stop("Could not perform PCA on the input matrix")
    unrotated.loadings <- initial.results$loadings

    # Flip eigenvectors so the largest loadings are positive
    unrotated.loadings <- apply(unrotated.loadings, 2,
                               function(x){sg=sign(x); ss=sum(sg*x^2); return(x*sign(ss))})
    loadings <- unrotated.loadings

    # Work out which rotation to use
    # Convert from the strings that are to be used in the menus, which begin with upppercase letters
    substr(rotation, 1, 1) <- tolower(substr(rotation, 1, 1))

    oblique.rotation <- rotation == "oblimin" || rotation == "promax"

    # Rotate the loadings
    if (n.factors == 1 & rotation != "none")
    {
        warning("No rotation for single-component analysis\n")
        rotation <- "none"
    }

    if (rotation != "none")
    {
        rotation.results <- RotateLoadings(unrotated.loadings,
                                           rotation = rotation,
                                           delta = oblimin.delta,
                                           kappa = promax.kappa,
                                           covar = !use.correlation,
                                           stds = stddevs)
        sign.loadings <- apply(rotation.results$rotated.loadings, 2,
                                 function(x){sg=sign(x); ss=sum(sg*x^2); return(rep(sign(ss), length(x)))})
        rotated.loadings <- rotation.results$rotated.loadings * sign.loadings

        loadings <- rotated.loadings
        if (oblique.rotation)
            structure.matrix <- rotation.results$structure.matrix * sign.loadings
        else
            structure.matrix <- rotated.loadings
        sign.matrix <- tcrossprod(sign.loadings[1,], sign.loadings[1,])
        component.correlations <- rotation.results$component.correlations
        if (!is.null(component.correlations))
            component.correlations <- component.correlations * sign.matrix
        colnames(structure.matrix) <- colnames(loadings)

    } else {
        sign.loadings <- apply(unrotated.loadings, 2,
                                 function(x){sg=sign(x); ss=sum(sg*x^2); return(rep(sign(ss), length(x)))})
        unrotated.loadings <- unrotated.loadings * sign.loadings
        loadings <- unrotated.loadings
        rotated.loadings <- unrotated.loadings
        structure.matrix <- loadings
        component.correlations <- NULL
    }
    comp.names <- paste("Component", 1:n.factors)
    colnames(rotated.loadings) <- comp.names
    colnames(structure.matrix) <- comp.names
    colnames(loadings) <- comp.names
    if (!is.null(component.correlations))
    {
        rownames(component.correlations) <- comp.names
        colnames(component.correlations) <- comp.names
    }

    # Rescale the loadings
    raw.loadings <- loadings
    raw.unrotated.loadings <- unrotated.loadings
    raw.rotated.loadings <- rotated.loadings
    raw.structure.matrix <- structure.matrix
    if (!use.correlation)
    {
        loadings <- loadings/stdmat
        unrotated.loadings <- unrotated.loadings/stdmat
        rotated.loadings <- rotated.loadings/stdmat
        structure.matrix <- structure.matrix/stdmat
    }

    # Generate score weights using the regression method.
    # For PCA, all methods give the same answer as scores
    # are well-defined, so there is no need to use Bartlett
    # or Anderson.

    # For oblique rotations, use structure matrix
    # otherwise use pattern matrix
    if (rotation == "promax" || rotation == "oblimin")
    {
        S <- structure.matrix
    } else {
        S <- loadings
    }

    # Smooth non-positive definite correlation in the same way as psych::principal
    cor <- try(cor.smooth2(correlation.matrix), silent = TRUE)
    score.weights <- try(solve(cor, S), silent = TRUE)
    if (inherits(score.weights, "try-error"))
        stop("Component scores could not be computed as the correlation or correlation matrix is singular.")

    # Original data is scaled befor generating scores
    if (!is.null(weights))
        scaled.data <- scaleDataUsingWeights(data = prepared.data$subset.data, weights = prepared.data$subset.weights)
    else
        scaled.data <- scale(prepared.data$subset.data)

    # Multiply the scaled data by the weights to produce scores
    scores <- as.matrix(scaled.data) %*% score.weights

    # Fill out any additional cases with missing values, so that the size of the output scores
    # matches the number of respondents in the original data set, and that the cases are
    # matched up correctly
    new.data <- matrix(NA, nrow = nrow(data), ncol = ncol(scores))
    row.names(new.data) <- row.names(data)
    colnames(new.data) <- colnames(scores)
    if (!any(duplicated(row.names(data))) && !"" %in% row.names(data))
    {
        common.names <- intersect(rownames(new.data), row.names(scores))
        new.data[common.names,] <- scores[common.names,]
    } else if (nrow(prepared.data$subset.data) == nrow(data))
        new.data <- scores
    else # subset should always be non-null at this point.
        new.data[subset, ] <- scores
    scores <- new.data


    # Communalities
    initial.communalities <- diag(input.matrix)
    extracted.communalities <- initial.results$communality
    if (!use.correlation)
    {
        rescaled.initial.communalities <- rep(1, nrow(input.matrix))
        rescaled.extracted.communalities <- rowSums(as.matrix(unrotated.loadings)^2)
    }

    # Variance explained by factors


    # Results
    results <- list()
    results$unrotated.loadings <- unrotated.loadings
    if (rotation != "none")
    {
        results$rotated.loadings <- rotated.loadings
    }


    results$loadings <- loadings
    results$structure.matrix <- structure.matrix

    results$raw.loadings <- raw.loadings
    results$raw.unrotated.loadings <- raw.unrotated.loadings
    results$raw.rotated.loadings <- raw.rotated.loadings
    results$raw.structure.matrix <- raw.structure.matrix
    results$unrotated.loadings <- unrotated.loadings
    results$rotated.loadings <- rotated.loadings

    results$sort.coefficients.by.size <- sort.coefficients.by.size
    results$suppress.small.coefficients <- suppress.small.coefficients
    results$min.display.loading.value <- min.display.loading.value
    results$original.data <- data
    results$original.weights <- weights
    results$data.used <- prepared.data
    results$use.correlation <- use.correlation
    results$print.type <- print.type
    results$plot.labels <- plot.labels
    results$input.matrix <- input.matrix
    results$values <- initial.results$values
    results$scores <- scores
    results$score.weights <- score.weights
    results$rotation <- rotation
    results$missing <- missing
    results$component.correlations <- component.correlations
    results$data.groups <- data.groups


    results$initial.communalities <- initial.communalities
    results$extracted.communalities <- extracted.communalities
    if (!use.correlation)
    {
        results$rescaled.initial.communalities <- rescaled.initial.communalities
        results$rescaled.extracted.communalities  <- rescaled.extracted.communalities
    }


    class(results) <- "flipFactorAnalysis"
    attr(results, "ChartData") <- ExtractChartData(results)
    return(results)
}

#' @rdname PrincipalComponentsAnalysis
#' @param object Object of class \code{"flipFactorAnalysis"} created using \code{PrincipalComponentsAnalysis}.
#' @param ... Not used.
#' @export

fitted.flipFactorAnalysis <- function(object, ...)
{
    return(object$scores)
}


#' \code{ScreePlot}
#' @description Plot the eigenvalues from an existing principal component or
#'   factor analysis or plot the eigenvalues from the correlation or covariance
#'   matrix of a data frame.
#' @param x Either a data frame, a numeric vector of eigenvalues, or the
#'   eigenvalues from an analysis of class \code{flipFactorAnalysis} from
#'   \code{\link{PrincipalComponentsAnalysis}}, or \code{fa} or \code{principal} from package
#'   psych. When x is a data frame, additional arguments can be supplied as to
#'   how to compute the covariance or correlation matrix.
#' @param trim.padding Logical; whether to remove extra padding around the htmlwidget.
#'   By default this is set to \code{FALSE} to be the same as old charts
#' @inheritParams PrincipalComponentsAnalysis
#'
#' @return An HTML widget object from plotly containing the Scree Plot.
#' @importFrom flipStatistics CovarianceAndCorrelationMatrix
#' @importFrom plotly plot_ly layout
#' @export
ScreePlot <- function(x, weights = NULL, subset = NULL, missing = "Exclude cases with missing data",
    use.correlation = TRUE, trim.padding = FALSE)
{
    if ("data.frame" %in% class(x))
    {
        prepared.data <- prepareDataForFactorAnalysis(data = x, weights = weights, subset = subset, missing = missing)
        input.matrix <- CovarianceAndCorrelationMatrix(
            data = prepared.data$subset.data,
            weights = prepared.data$subset.weights,
            pairwise = missing == "Use partial data (pairwise correlations)",
            use.correlation = use.correlation)
        input.values <- eigen(input.matrix)$values
    }
    else if ("numeric" %in% class(x))
    {
        input.values <- x
    }
    else if (inherits(x, "flipFactorAnalysis") || any(class(x) == "fa") || any(class(x) == "principal"))
    {
        input.values <- x$values
    }
    else
    {
        stop(paste0("Can't make a Scree Plot for object of class ", paste(class(x), collapse = ", ")),
             ". Select a Principal Components Analysis output.")
    }

    input.values <- sort(input.values, decreasing = TRUE)

    df = data.frame(eig.num = 1:length(input.values), eig.vals = input.values)

    `Component Number` <- 1:length(input.values)
    Eigenvalue <- input.values

    my.plot <- plot_ly(x = ~`Component Number`,
                       y = ~Eigenvalue,
                       mode = "lines+markers", type="scatter")
    my.plot <- layout(p = my.plot,
                      title = "Scree Plot",
                      yaxis = list(range = c(0, max(input.values) + 1)),
                      xaxis = list(title = "Component Number"))
    my.plot$sizingPolicy$browser$padding <- if (trim.padding) 0 else 40
    my.plot <- plotly::config(p = my.plot, displayModeBar = FALSE)
    return(my.plot)
}


#' \code{ComponentPlot}
#'
#' @description Create a scatter plot showing the loadings of each variable on
#'   the first two principal components.
#'
#' @param x An object of class \code{flipFactorAnalysis}.
#' @param show.labels Label the points with the row names.
#' @importFrom rhtmlLabeledScatter LabeledScatter
#' @export
ComponentPlot <- function(x, show.labels = TRUE)
{
    if (is.null(x$loadings))
    {
        stop("Input should be created by Data Reduction - Principal Components Analysis")
    }

    if (ncol(x$loadings) < 2)
    {
        stop("There aren't enough components to plot.")
    }

    labels <- as.character(1:nrow(x$loadings))
    if (show.labels)
    {
        if (is.null(row.names(x$loadings)))
            warning("The loadings do not contain labels.")
        labels <- row.names(x$loadings)
    }

    x.label <- "Component 1"
    y.label <- "Component 2"
    add.ve <- FALSE
    if ("princomp" %in% class(x))
        add.ve <- TRUE
    if (any(c("psych", "flipFactorAnalysis") %in% class(x)) && !(x$rotation %in% c("promax", "oblimin")))
        add.ve <- TRUE

    if (add.ve)
    {
        ss.loadings <- colSums(x$loadings^2)
        variance.explained <- ss.loadings/nrow(x$loadings)
        x.label <- sprintf("%s (%.1f%% variance explained)", x.label, variance.explained[1]*100)
        y.label <- sprintf("%s (%.1f%% variance explained)", y.label, variance.explained[2]*100)
    }


    coords <- x$loadings
    groups <- 1:nrow(coords)
    colors <- rep(c('#5B9BD5', '#ED7D31', '#A5A5A5', '#1EC000', '#4472C4', '#70AD47','#255E91','#9E480E','#636363','#997300','#264478','#43682B','#FF2323'),
                  length = length(groups))
    # Append a transparent point to force the origin to be shown
    # Note that axis bounds cannot be set with fixed.aspect
    LabeledScatter(X = c(0, coords[, 1]),
                   Y = c(0, coords[, 2]),
                   label = c(" ", labels),
                   group = c("Origin", groups),
                   colors = c("transparent", colors),
                   fixed.aspect = TRUE,
                   title = "Component Plot",
                   x.title = x.label,
                   y.title = y.label,
                   axis.font.size = 10,
                   labels.font.size = 12,
                   title.font.size = 20,
                   y.title.font.size = 16,
                   x.title.font.size = 16,
                   legend.show = FALSE)

}

#' @export
ExtractChartData.flipFactorAnalysis <- function(x)
{
    if (x$print.type == "scree" || x$print.type == "Scree Plot")
        return(sort(x$values, decreasing = TRUE))
    if (x$print.type == "2d" || x$print.type == "2D Scatterplot")
    {
        tmp <- convertFactorAnalysisTo2D(x)
        if (is.null(tmp$data.groups))
            return(tmp$embedding)
        data <- data.frame(tmp$embedding, Group = tmp$data.groups, stringsAsFactors = FALSE,
            check.names = FALSE, check.rows = FALSE)
        attr(data, "scatter.variable.indices") <- c(x = 1, y = 2, sizes = NA, colors = 3)
        return(data)
    }
    if (x$print.type == "loadings" || x$print.type == "Loadings Table")
        return(.tidy.loadings(x, input.matrix = x$loadings))
    if (x$print.type == "structure" || x$print.type == "Structure Matrix")
        return(.tidy.loadings(x, input.matrix = x$structure.matrix))
    if (x$print.type =="variance" || x$print.type == "Variance Explained")
    {
        eigenvalues <- x$values
        variance.proportions = eigenvalues / sum(eigenvalues)
        cumulative.proportions = cumsum(variance.proportions)
        result <- cbind('Eigenvalue' = eigenvalues,
                     '% of Variance' = variance.proportions,
                     'Cumulative %' = cumulative.proportions)
        rownames(result) <- paste("Component", 1:length(eigenvalues))
        return(result)
    }
    if (x$print.type == "details" || x$print.type == "Detailed Output")
    {
        headings <- matrix("", nrow = 10, ncol = ncol(x$loadings) + 1)
        headings[,1] <- c("", "Loadings", "", "Structure matrix", "", "",
                          "", "Communalities", "", "Score Cofficient Matrix")

        .print <- function(x, digits = 3)
        {
            n <- nrow(x)
            m <- ncol(x)
            res <- matrix("", nrow = n+1, ncol = m+1, dimnames = list(rep("", n+1), rep("", m+1)))
            res[1,(1:m)+1] <- colnames(x)
            res[(1:n)+1,1] <- rownames(x)
            res[(1:n)+1,(1:m)+1] <- round(x, digits)
            return(res)
        }

        ss.loadings <- colSums(x$loadings ^ 2)
        nvar <- ncol(x$original.data)
        ve.table <- rbind(`Sum of Square Loadings` = ss.loadings)
        ve.table <- rbind(`Sum of Square Loadings` = ss.loadings)
        ve.table <- rbind(ve.table, `% of Variance` = ss.loadings/nvar*100)
        if (ncol(x$loadings) > 1)
            ve.table <- rbind(ve.table, `Cumulative %` = cumsum(ss.loadings/nvar*100))

        if (x$use.correlation)
            communality.table <- cbind("Initial" = x$initial.communalities,
                                       "Extraction" = x$extracted.communalities)
        else
            communality.table <- cbind("Initial" = x$rescaled.initial.communalities,
                                       "Extraction" = x$rescaled.extracted.communalities)

        empty.mat <- matrix("", nrow = length(x$initial.communalities) + 1, ncol = ncol(headings)-3)
        colnames(empty.mat) <- rep("", ncol(headings) - 3)
        all.tables <- rbind(headings[1:2,], .print(.tidy.loadings(x, input.matrix = x$loadings)),
             headings[3:4,], .print(.tidy.loadings(x, input.matrix = x$structure.matrix)),
             headings[5:6,], .print(ve.table),
             headings[7:9,], cbind(.print(communality.table), empty.mat),
             headings[9:10,], .print(x$score.weights))
        return(all.tables)
    }

    # Otherwise return data for a component plot
    if (NCOL(x$loadings) < 2)
        return(x$loadings)
    component.data <- x$loadings[,1:2]
    if (!(x$rotation %in% c("promax", "oblimin")))
    {
        var.exp <- colSums(x$loadings^2)/nrow(x$loadings)
        colnames(component.data) <- sprintf("Component %d (%.1f%% variance explained)",
                                            1:2, var.exp[1:2] * 100)
    }
    return(component.data)
}


#' \code{prepareDataForFactorAnalysis}
#' @description Filter data, remove cases with missing values, and impute where requested.
#' @inheritParams PrincipalComponentsAnalysis
#' @return A list containing \code{subset.data}, which is a data frame which has had subset applied
#' and missing values removed or imputed as specified by the parameter \code{missing}, and \code{prepared.weights}
#' which is a nuneric vector containing the weight values that correspond to the remaining cases (or NULL when
#' the input weight is NULL).
#' @importFrom flipImputation Imputation
#' @importFrom flipData ExcludeCasesWithCompletelyMissingData ExcludeCasesWithAnyMissingData ErrorIfMissingDataFound
#' @importFrom flipTransformations ProcessQVariables
prepareDataForFactorAnalysis <- function(data, weights, subset, missing)
{
    data <- ProcessQVariables(data)

    row.names <- rownames(data)

    # Create the input data by filtering and removing missing values or
    # imputing where specified.

    # If no filter specified, create a subset containing all rows
    if (is.null(subset))
        subset <- rep(TRUE, nrow(data))

    # Check filtered rows for missing data
    subset.data <- data[subset, ]
    imputation.label <- NULL
    if (missing == "Error if missing data")
    {
        ErrorIfMissingDataFound(subset.data)
    } else if (missing == "Imputation (replace missing values with estimates)") {
        imputed.data <- Imputation(data)[[1]]
        imputation.label <- attr(imputed.data, "imputation.method")
        subset.data <- imputed.data[subset, ]
    } else if (missing == "Exclude cases with missing data") {
        # Ensure only complete responses remain
        subset.data <- ExcludeCasesWithAnyMissingData(subset.data)
    } else if (missing == "Use partial data (pairwise correlations)") {
        subset.data <- ExcludeCasesWithCompletelyMissingData(subset.data)
    } else {
        stop(paste0("Don't recognize the missing data option", missing))
    }

    # Figure out which of the total set of weight values correspond to the
    # remaining respondents.
    subset.weights <- NULL
    if (!is.null(weights))
    {
        if (missing == "Exclude cases with missing data")
            row.to.remove <- apply(data, 1, function(x) any(is.na(x)))
        else if (missing == "Use partial data (pairwise correlations)")
            row.to.remove <- apply(data, 1, function(x) all(is.na(x)))
        else
            row.to.remove <- rep(FALSE, nrow(data))
        subset.weights <- weights[subset & !row.to.remove]
    }
    return(list(subset.data = subset.data,
                subset.weights = subset.weights,
                imputation.label = imputation.label))
}


#' \code{BartlettTestOfSphericity}
#'
#' @description Conduct the Bartlett Test of Sphericity for a set of data, which
#'   tests that the correlation matrix of the data is not the identity matrix.
#' @param data A data frame containing the data to test.
#' @inheritParams PrincipalComponentsAnalysis
#' @return A list containing the Chi-Square value, degrees of freedom
#'   (\code{df}), and p-value for the test.
#' @details This function wraps \code{\link[psych]{cortest.bartlett}}. In
#'   particular, it extends the existing funcitonality to weighted data, and it
#'   computes the test using a more conservative value of the sample size when
#'   there is missing data. The value for the sample size that is used is the
#'   size of the smallest pairwise-complete set of cases among all pairs of
#'   variables. This is consistent with SPSS.
#' @importFrom flipStatistics CovarianceAndCorrelationMatrix
#' @importFrom psych cortest.bartlett
#' @export

# For the sample size, use the min sample size of the correlation matrix
BartlettTestOfSphericity <- function(data,
                         weights = NULL,
                         subset = NULL,
                         missing = "Exclude cases with missing data")
{
    prepared.data <- prepareDataForFactorAnalysis(data, weights, subset, missing)
    correlation.matrix <- CovarianceAndCorrelationMatrix(
        data = prepared.data$subset.data,
        weights = prepared.data$subset.weights,
        pairwise = missing == "Use partial data (pairwise correlations)",
        use.correlation = TRUE)

    # If using a weight, supply the Effective Sample Size, which is the sum of the weights, otherwise
    # supply the actual sample size of the prepared data.
    # When missing is set to "Use partial data (pairwise correlations)" then the sample size can vary between cells in
    # correlation matrix. In this case, use the smallest sample size, or effective sameple size.
    if (missing == "Use partial data (pairwise correlations)")
    {
        sample.size.matrix <- sampleSizeMatrix(data, weights)
        sample.size <- min(sample.size.matrix)
    }
    else if (!is.null(weights))
        sample.size <- sum(prepared.data$subset.weights)
    else
        sample.size <- nrow(prepared.data$subset.data)
    test.results <- cortest.bartlett(correlation.matrix, n = sample.size)
    test.results$n.estimation <- nrow(prepared.data$subset.data)
    test.results$imputation.label <- prepared.data$imputation.label
    class(test.results) <- "flipBartlett"

    return(test.results)
}

# Scale and center data using the weighted mean and standard deviation
scaleDataUsingWeights <- function(data, weights)
{
    .weightedMeanAndSD <- function(x, weights)
    {
        complete.cases <- !is.na(x) & weights > 0
        wx <- x * weights
        weighted.mean <- sum(wx[complete.cases]) / sum(weights[complete.cases])
        sx <- x - weighted.mean
        wsx2 <- weights * sx * sx
        weighted.variance <- sum(wsx2[complete.cases]) / (sum(weights[complete.cases]) - 1)
        return(list(weighted.mean = weighted.mean, weighted.sd = sqrt(weighted.variance)))
    }

    for (j in 1L:ncol(data))
    {
        weighted.stats <- .weightedMeanAndSD(data[,j], weights)
        data[,j] <- (data[,j] - weighted.stats$weighted.mean) / weighted.stats$weighted.sd
    }
    return(data)
}


# Calculate the (effective) sample size for each pair of variables in data
sampleSizeMatrix <- function(data, weights)
{
    if (is.null(weights))
    {
        weights <- rep(1, nrow(data))
    }
    numvars <- ncol(data)
    sample.size.matrix <- matrix(NA, nrow = numvars, ncol = numvars)
    for (row in 1L:numvars)
    {
        for (col in 1L:numvars)
        {
            if (row == col) {
                sample.size.matrix[row, row] <- sum(weights[!is.na(data[, row])])
            }
            else
            {
                indicator <- !is.na(data[, row] * data[, col])
                sample.size <- sum(weights[indicator])
                sample.size.matrix[row, col] <- sample.size
                sample.size.matrix[col, row] <- sample.size
            }
        }
    }
    return(sample.size.matrix)
}

# Convert a variable to the appropriate form for use in a PCA.
# - Numeric variables are unchanged.
# - Ordered factors are replaced with their levels
# - Non-ordered factors are converted to a set of indicator variables
# with one variable for each level but the first.
#' @importFrom flipTransformations FactorToNumeric
convertVariableForFactorAnalysis <- function(variable, include.question.name = TRUE)
{
    if (is.numeric(variable))
    {
        return(variable)
    }

    if (is.ordered(variable))
    {
        return(unclass(variable))
    }

    # Non-ordered factors
#     if (include.question.name)
#     {
#         vn <- paste0(attr(variable, "question"), ":", attr(variable, "label"))
#     } else {
#         vn <- attr(variable, "label")
#     }
    vn <- attr(variable, "label")
    if (is.null(vn))
    {
        vn <- deparse(substitute(x))
    }
    indicator.matrix <- FactorToNumeric(variable, name = vn)
    if (ncol(indicator.matrix) > 2)
    {
        indicator.matrix <- indicator.matrix[, 2:ncol(indicator.matrix)]
    } else if (ncol(indicator.matrix) == 2) {
        cn <- colnames(indicator.matrix)[2]
        indicator.matrix <- as.matrix(indicator.matrix[,2])
        colnames(indicator.matrix) <- cn
    }
    return(indicator.matrix)
}

# A modification of psych::cor.smooth
# In the original function, the smoothing is applied if any of the eigenvalues
# are smaller than .Machine$double.eps. But we found examples where eigenvalues
# are slightly larger than this threshold but still require smoothing.
# Here, we apply the smoothing if any of the eigenvalues are on the
# same order of magnitude as .Machine$double.eps

#' @importFrom stats cov2cor
cor.smooth2 <- function (x, eig.tol = 10^-12)
{
    eigens <- try(eigen(x), TRUE)
    if (inherits(eigens, as.character("try-error"))) {
        warning("I am sorry, there is something seriously wrong with the correlation matrix,\ncor.smooth failed to  smooth it because some of the eigen values are NA.  \nAre you sure you specified the data correctly?")
    }
    else {
        if (min(eigens$values) < 10 * .Machine$double.eps) {
            warning("Matrix was not positive definite, smoothing was done")
            eigens$values[eigens$values < eig.tol] <- 100 * eig.tol
            nvar <- dim(x)[1]
            tot <- sum(eigens$values)
            eigens$values <- eigens$values * nvar/tot
            cnames <- colnames(x)
            rnames <- rownames(x)
            x <- eigens$vectors %*% diag(eigens$values) %*% t(eigens$vectors)
            x <- cov2cor(x)
            colnames(x) <- cnames
            rownames(x) <- rnames
        }
    }
    return(x)
}
