# Factor Analysis



# #' Q23 and Weights from Phone.sav
# #'
# #' 25 variables from a 5-point scale. Extra missing data has been added at random.
# #' This makes up about 20% of the values. This is to test PCA and Factor analysis.
# #'
# #' @format A list containing:
# #' \describe{
# #'   \item{data.set}{25 variables from q23}
# #'   \item{weight}{A vector of weights}
# #' }
# "pcaPhoneTestData"


# Currently the factor analysis isn't being included because some results don't
# match up well with SPSS.
#
# #TODO: Add handling of factor and ordered factor variables. This is mostly done in R Factor Analysis,
# # but there is currently a bug preventing me from wrapping QInputs in a list.
#
#
# #' \code{FactorAnalysis}
# #'
# #' @description Calculate a Factor Analysis or Principal Component Analysis
# #'
# #' @param data A data frame with numeric columns which contains the data to be
# #'   analyzed.
# #' @param weights A numeric vector containing the weight for each case in data.
# #' @param subset A logical vector which describes the subset of \code{data} to
# #'   be analyzed.
# #' @param missing A string specifiying what to do when the \code{data} contains
# #'   missing values. The valid options are \code{"Error if missing data"},
# #'   \code{"Exclude cases with missing data"}, \code{"Use partial data (pairwise
# #'   correlations)"}, and \code{"Imputation (replace missing values with
# #'   estimates)"}.
# #' @param use.correlation A logical value specifying whether to use the
# #'   correlation matrix (\code{TRUE}), or the covariance matrix (\code{FALSE}).
# #' @param type A string specifying the type of analysis to do. Valid options are
# #'   \code{"PCA"}, \code{"Unweighted least squares"}, \code{"Generalized least
# #'   squares"}, and \code{"Maximum likelihood"}.
# #' @param rotation A string specifying the type of rotation to be used. Valid
# #'   options are  \code{"none"}, \code{"varimax"}, \code{"quartimax"},
# #'   \code{"bentlerT"}, \code{"equamax"}, \code{"varimin"}, \code{"geominT"},
# #'   \code{"bifactor"}, \code{"promax"}, \code{"oblimin"}, \code{"simplimax"},
# #'   \code{"bentlerQ"}, \code{"geominQ"}, \code{"biquartimin"}, and
# #'   \code{"cluster"}. More details are found in package \code{psych}.
# #' @param n.factors TODO
# #' @param sort.coefficients.by.size A logical value determining whether loadings
# #'   should be sorted when printed.
# #' @param suppress.small.coefficients TODO
# #' @param min.display.loading.value Loadings smaller than this value will not be
# #'   displayed when printed.
# #' @param print.type A string specifying the type of printing that should be
# #'   done. Valid options are \code{"table"} to display a loading table,
# #'   \code{"scree"} to display a Scree Plot, and \code{"scatter"} to display a
# #'   plot of the first two dimensions of the final loadings. The latter two
# #'   options make use of HTML widgets.
# #' @param plot.labels A logical value which determines whether or not the
# #'   scatter plot will show the labels of the input data, or just integers
# #'   specifying the column number of each variable.
# #'
# #' @details This function is a wrapper for the functions \code{\link[psych]{fa}}
# #'   and \code{\link[psych]{principal}} from package \code{psych}. It adds
# #'   options for handling of missing data, weighting, filtering, and printing.
# #' @importFrom flipStatistics CovarianceAndCorrelationMatrix
# #' @importFrom psych principal fa
# #' @export
# FactorAnalysis <- function(data,
#                            weights = NULL,
#                            subset = NULL,
#                            missing = "Exclude cases with missing data",
#                            use.correlation = TRUE,
#                            type = "PCA",
#                            rotation = "none",
#                            n.factors = 1, #Need a new option to allow the user to do this with eigenvalues
#                            sort.coefficients.by.size = FALSE,
#                            suppress.small.coefficients = FALSE,
#                            min.display.loading.value = 0.1,
#                            print.type = "table",
#                            plot.labels = TRUE)
# {
#
#     # Generate the data that will be input to the correlation/covariance
#     # matrix by filtering and imputing if specified.
#     prepared.data <- prepareDataForFactorAnalysis(data, weights, subset, missing)
#
#     # Work out the number of observations to supply to psych package functions for
#     # calculation of goodness of fit statistics.
#     if (!is.null(weights))
#     {
#         n.obs <- sum(prepared.data$weights)
#     } else {
#         n.obs <- nrow(prepared.data$subset.data)
#     }
#
#     input.matrix <- CovarianceAndCorrelationMatrix(
#         data = prepared.data$subset.data,
#         weights = prepared.data$subset.weights,
#         pairwise = missing == "Use partial data (pairwise correlations)",
#         use.correlation = use.correlation)
#
#     row.names(input.matrix) <- colnames(data)
#     colnames(input.matrix) <- colnames(data)
#
#     # Work out which rotation to use
#     # Convert from the strings that are to be used in the menus, which begin with upppercase letters
#     substr(rotation, 1, 1) <- tolower(substr(rotation, 1, 1))
#
#     # Call the appropriate method from psych
#
#     if (type == "PCA")
#     {
#         results <- principal(input.matrix,
#                                       nfactors = n.factors,
#                                       rotate = rotation,
#                                       covar = !use.correlation,
#                                       scores = TRUE)
#
#     }
#     else
#     {
#         # Map SPSS options to psych options
#         method <- type
#         # method <- switch(type,
#         #                  "Unweighted least squares" = "pa",
#         #                  "Generalized least squares" = "gls",
#         #                  "Maximum likelihood" = "ml")
#
#         if (is.null(method))
#         {
#             stop(paste0("Do not recognize factor analysis type: ", type))
#         }
#
#         results <- fa(input.matrix,
#                                nfactors = n.factors,
#                                rotate = rotation,
#                                covar = !use.correlation,
#                                fm = method,
#                                scores = TRUE)
#
#     }
#
#
#     # Transform original variables to a new set of variables if needed
#
#
#
#     # Add additional details to the output object
#     results$sort.coefficients.by.size <- sort.coefficients.by.size
#     results$suppress.small.coefficients <- suppress.small.coefficients
#     results$original.data <- data
#     results$original.weights <- weights
#     results$data.used <- prepared.data
#     results$use.correlation <- use.correlation
#     results$print.type <- print.type
#     results$plot.labels <- plot.labels
#     if (use.correlation)
#     {
#         results$correlation.matrix <- input.matrix
#     }
#     else
#     {
#         results$covariance.matrix <- input.matrix
#     }
#
#     class(results) <- "flipFactorAnalysis"
#     # Return
#     return(results)
# }


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
#' @param n.factors An integer specifying the number of principal components to keep.
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
#'   multiplied by the component correlations),
#'   \code{"scree"} to display a Scree Plot, and \code{"scatter"} to display a
#'   plot of the first two dimensions of the final loadings. The latter two
#'   options make use of HTML widgets.
#' @param plot.labels A logical value which determines whether or not the
#'   scatter plot will show the labels of the input data, or just integers
#'   specifying the column number of each variable.
#'
#' @details This uses \code{\link[psych]{principal}} from package \code{psych} to compute the unrotated
#' PCA, and uses package \code{GPArotation} to find a rotated solution if required, to match SPSS' PCA. The
#' rotation includes a Kaiser normalization and a method of Promax which matches what SPSS does.
#' Includes handling of missing data, weighting, and filtering.
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
                               n.factors = 1,
                               sort.coefficients.by.size = FALSE,
                               suppress.small.coefficients = FALSE,
                               min.display.loading.value = 0.1,
                               print.type = "loadings",
                               plot.labels = TRUE)
{

    # Generate the data that will be input to the correlation/covariance
    # matrix by filtering and imputing if specified.
    prepared.data <- prepareDataForFactorAnalysis(data, weights, subset, missing)

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
    stddevs <- StandardDeviation(prepared.data$subset.data, weights = prepared.data$subset.weights)
    stdmat <- matrix(rep(stddevs, n.factors), ncol = n.factors)
    if (!use.correlation)
    {
        input.matrix <- correlation.matrix * stddevs %o% stddevs
    } else {
        input.matrix <- correlation.matrix
    }

    row.names(input.matrix) <- colnames(data)
    colnames(input.matrix) <- colnames(data)

    # Unrotated loadings
    # Don't return all of the properties returned by
    # principal() as they are not designed to account
    # for weighting
    initial.results <- principal(input.matrix,
                                 nfactors = n.factors,
                                 rotate = "none",
                                 covar = !use.correlation,
                                 scores = FALSE)
    unrotated.loadings <- initial.results$loadings
    loadings <- unrotated.loadings

    # Work out which rotation to use
    # Convert from the strings that are to be used in the menus, which begin with upppercase letters
    substr(rotation, 1, 1) <- tolower(substr(rotation, 1, 1))

    oblique.rotation <- rotation == "oblimin" || rotation == "promax"

    # Rotate the loadings
    if (rotation != "none")
    {
        rotation.results <- RotateLoadings(unrotated.loadings,
                                           rotation = rotation,
                                           delta = oblimin.delta,
                                           kappa = promax.kappa,
                                           covar = !use.correlation,
                                           stds = stddevs)
        rotated.loadings <- rotation.results$rotated.loadings
        loadings <- rotated.loadings
        if (oblique.rotation)
        {
            structure.matrix <- rotation.results$structure.matrix
        } else {
            structure.matrix <- rotated.loadings
        }
        component.correlations <- rotation.results$component.correlations
        colnames(structure.matrix) <- colnames(loadings)

    } else {
        loadings <- unrotated.loadings
        rotated.loadings <- unrotated.loadings
        structure.matrix <- loadings
        component.correlations <- NULL
    }
    comp.names <- colnames(unrotated.loadings)
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

    score.weights <- solve(correlation.matrix, S)

    # Original data is scaled befor generating scores
    if (!is.null(weights))
    {
        scaled.data <- scaleDataUsingWeights(data = prepared.data$subset.data, weights = prepared.data$subset.weights)
    } else
    {
        scaled.data <- scale(prepared.data$subset.data)
    }

    # Multiply the scaled data by the weights to produce scores
    scores <- as.matrix(scaled.data) %*% score.weights

    # Fill out any additional cases with missing values, so that the size of the output scores
    # matches the number of respondents in the original data set, and that the cases are
    # matched up correctly
    new.data <- matrix(NA, nrow = nrow(data), ncol = ncol(scores))
    row.names(new.data) <- row.names(data)
    colnames(new.data) <- colnames(scores)
    new.data[which(row.names(new.data) %in% row.names(scores)), ] <- scores
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



    results$initial.communalities <- initial.communalities
    results$extracted.communalities <- extracted.communalities
    if (!use.correlation)
    {
        results$rescaled.initial.communalities <- rescaled.initial.communalities
        results$rescaled.extracted.communalities  <- rescaled.extracted.communalities
    }


    class(results) <- "flipFactorAnalysis"
    return(results)
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
#' @inheritParams PrincipalComponentsAnalysis
#'
#' @return An HTML widget object from plotly containing the Scree Plot.
#' @importFrom flipStatistics CovarianceAndCorrelationMatrix
#' @importFrom plotly plot_ly layout
#' @export
ScreePlot <- function(x, weights = NULL, subset = NULL, missing = "Exclude cases with missing data", use.correlation = TRUE)
{
    if (class(x) == "data.frame")
    {
        prepared.data <- prepareDataForFactorAnalysis(data = x, weights = weights, subset = subset, missing = missing)
        input.matrix <- CovarianceAndCorrelationMatrix(
            data = prepared.data$subset.data,
            weights = prepared.data$subset.weights,
            pairwise = missing == "Use partial data (pairwise correlations)",
            use.correlation = use.correlation)
        input.values <- eigen(input.matrix)$values
    }
    else if (class(x) == "numeric")
    {
        input.values <- x
    }
    else if (class(x) == "flipFactorAnalysis" || class(x) == "fa" || class(x) == "principal")
    {
        input.values <- x$values
    }
    else
    {
        stop(paste0("Can't make a Scree Plot for object of class", class(x)))
    }

    input.values <- sort(input.values, decreasing = TRUE)

    df = data.frame(eig.num = 1:length(input.values), eig.vals = input.values)

    `Component Number` <- 1:length(input.values)
    Eigenvalue <- input.values

    my.plot <- plot_ly(x = `Component Number`,
                       y = Eigenvalue,
                       mode = "lines+markers")
    layout(plot = my.plot, title = "Scree Plot", yaxis = list(range = c(0, max(input.values) + 1)))
    return(my.plot)
}


#' \code{ComponentPlot}
#'
#' @description Create a scatter plot showing the loadings of each variable on
#'   the first two principal components.
#'
#' @param x An object of class \code{flipFactorAnalysis}.
#' @param show.labels Label the points with the row names.
#' @importFrom flipPlots LabeledScatterPlot
#' @export
ComponentPlot <- function(x, show.labels = TRUE)
{
    print(paste0("show.labels: ", show.labels))
    if (is.null(x$loadings))
    {
        stop("Input should be created by Data Reduction - Factor Analysis or Data Reduction - Principal Components Analysis")
    }

    if (ncol(x$loadings) < 2)
    {
        stop("There aren't enough components to plot.")
    }

    ### Wait for update to flipPlots

    labels <- as.character(1:nrow(x$loadings))
    if (show.labels)
    {
        if (is.null(row.names(x$loadings)))
        {
            warning("The loadings do not contain labels.")
        }
        labels <- row.names(x$loadings)
    }
    print("labels:")
    print(labels)
    LabeledScatterPlot(x$loadings[, 1:2], row.labels = labels, title = "Component Plot", fixed.aspect = TRUE)
}


#' \code{prepareDataForFactorAnalysis}
#' @description Filter data, remove cases with missing values, and impute where requested.
#' @inheritParams PrincipalComponentsAnalysis
#' @return A list containing \code{subset.data}, which is a data frame which has had subset applied
#' and missing values removed or imputed as specified by the parameter \code{missing}, and \code{prepared.weights}
#' which is a nuneric vector containing the weight values that correspond to the remaining cases (or NULL when
#' the input weight is NULL).
#' @importFrom  flipImputation Imputation
#' @importFrom  flipData ExcludeCasesWithCompletelyMissingData ExcludeCasesWithAnyMissingData ErrorIfMissingDataFound
prepareDataForFactorAnalysis <- function(data, weights, subset, missing)
{

    row.names <- rownames(data)

    # Create the input data by filtering and removing missing values or
    # imputing where specified.

    # If no filter specified, create a subset containing all rows
    if (is.null(subset))
    {
        subset <- rep(TRUE, nrow(data))
    }

    # Check filtered rows for missing data
    subset.data <- data[subset, ]
    if (missing == "Error if missing data")
    {
        ErrorIfMissingDataFound(subset.data)
    } else if (missing == "Imputation (replace missing values with estimates)") {
        imputed.data <- Imputation(data)[[1]]
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
        subset.weights <- weights[row.names %in% rownames(subset.data)]
    }

    return(list(subset.data = subset.data, subset.weights = subset.weights))
}


# This function has not been tested for factor analysis.
# It gives the wrong results for Principal Components Analysis,
# where the factor scores should be the same for the three
# methods "Regression", "Bartlett", and "Anderson-Rubin". The
# function I used, psych::factor.scores gives different results
# for the three methods, leading me to think that something is
# wrong./
# #' \code{GenerateScoresFromFactorAnalysis}
# #'
# #' @description Generates a set of scores from a factor analysis or principal
# #'   component analysis, which is a new set of data corresponding to the
# #'   principal components or factors.
# #'
# #' @param factor.analysis.object The results of a factor analysis or principal
# #'   component analysis that was done using \code{\link{FactorAnalysis}}. This
# #'   provides the loadings, the raw data to transform, as well as weights and
# #'   subset information.
# #' @param method A string describing the method to use to obtain the scores. The
# #'   allowed options are \code{Regression}, \code{Bartlett}, and
# #'   \code{Anderson-Rubin}, and these map to the options used by package psych
# #'   \code{Thurstone}, \code{Bartlett}, and \code{Anderson} respectively.
# #'
# #' @details This function provides a wrapper for the function
# #'   \code{factor.scores} from package psych. It adds functionality to handle
# #'   the presence of weights. Where weights are specified the input data is
# #'   standardized using the weighted standard deviation and mean. The data that
# #'   is used to generate the scores is the same as that used to generate the
# #'   factor analysis or PCA. That is, the original subset is is used, if
# #'   imputation was specified originally, then the imputed data is used,
# #'   otherwise all cases which are not completely missing are used.
# #'
# #' @return A data frame with the same dimensions as the data which was
# #'   originally supplied to \code{\link{FactorAnalysis}}.
# #' @importFrom psych factor.scores
# #' @export
# GenerateScoresFromFactorAnalysis <- function(factor.analysis.object, method = "Regression")
# {
#     if (class(factor.analysis.object) != "flipFactorAnalysis")
#     {
#         stop("Input should be an object generated by FactorAnalysis")
#     }
#
#     # Convert the method names that we are offering, which match those in SPSS, to the equivalent
#     # names used by psych
#     translated.method <- switch(method, "Regression" = "Thurstone", "Bartlett" = "Bartlett", "Anderson-Rubin" = "Anderson")
#
#     # 1 Scale the data. Weight is used if present
#
#     subset.data <- factor.analysis.object$data.used$subset.data
#     subset.weights <- factor.analysis.object$data.used$subset.weights
#
#     if (!is.null(factor.analysis.object$original.weights))
#     {
#         scaled.data <- scaleDataUsingWeights(data = subset.data, weights = subset.weights)
#     } else
#     {
#         scaled.data <- scale(subset.data)
#     }
#
#     # 2 Generate the weights using factor.scores, using the correlation or covariance matrix and the loadings
#     #   from the factor analysis object
#
#     input.matrix <- factor.analysis.object$input.matrix
#
#     weights.matrix <- factor.scores(input.matrix, f = factor.analysis.object, method = translated.method)$weights
#
#     # 3 Multiply the scaled data by the weights to produce scores
#
#     scores <- as.matrix(scaled.data) %*% weights.matrix
#
#     # 4 Fill out any additional cases with missing values, so that the size of the output scores
#     #   matches the number of respondents in the original data set, and that the cases are
#     #   matched up correctly
#
#     original.data <- factor.analysis.object$original.data
#     new.data <- matrix(NaN, nrow = nrow(original.data), ncol = ncol(scores))
#     row.names(new.data) <- row.names(original.data)
#     colnames(new.data) <- colnames(scores)
#     new.data[which(row.names(new.data) %in% row.names(scores)), ] <- scores
#
#     return(as.data.frame(new.data))
# }

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
    } else if (!is.null(weights))
    {
        sample.size <- sum(prepared.data$subset.weights)

    } else {
        sample.size <- nrow(prepared.data$subset.data)
    }
    test.results <- cortest.bartlett(correlation.matrix, n = sample.size)
    class(test.results) <- "flipBartlett"

    return(test.results)
}

print.flipBartlett <- function(x, ...)
{
    v <- unlist(x)
    print(v)
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
convertVariableForFactorAnalysis <- function(variable)
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
    vn <- attr(variable, "question")
    if (is.null(vn))
    {
        vn <- deparse(substitute(x))
    }
    indicator.matrix <- FactorToNumeric(variable, variable.name = vn)
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

#' \code{ConvertVariablesForFactorAnalysis}
#' @description Convert variables to the appropriate form for use in PCA or Factor Analysis
#' @param variables A list of variables that you want to include in your analysis.
#' @details Numeric variables are unchanged. Ordered factors are replaced with a numeric
#' variable containing their levels. Non-ordered factors are converted to a set of indicator
#' (binary) variables with one variable for each level but the first.
ConvertVariablesForFactorAnalysis <- function(variables) {
    .is.non.ordered.factor <- function(x) { return(is.factor(x) && !is.ordered(x))}
    if (any(sapply(variables, .is.non.ordered.factor)))
    {
        warning("One or more of the inputs are non-ordered factors. They will be converted to binary variables and the first level will be excluded. Consider supplying numeric variables.")
    }
    return(as.data.frame(lapply(variables, convertVariableForFactorAnalysis), optional = TRUE))
}
