#' \code{MultipleCorrespondanenceAnalysis}
#' @description Calculate multiple correspondence analysis of categorical variables
#' @param formula Symbolic description of the model to be fitted
#' @param data A data frame containing factor variables.
#' @param output Specify output generated. May be one of \code{"Scatterplot"} or \code{"Text"}.
#' @param scatter.palette Color palette used for Scatterplot output.
#' @param weights A numeric vector containing the weight for each case in \code{data}.
#' @param subset A logical vector which describes the subset of \code{data} to be analysed.
#' @param missing A string specifiying what to do when the data contains missing values. This should be one of \code{"Error if missing data", "Exclude cases with missing data"}, and \code{"Imputation (replace missing values with estimates)"}.
#' @param auxiliary.data A \code{data.frame} containing additional variables
#'  to be used in imputation (if required). While adding more variables will improve
#'  the quality of the imputation, it will dramatically slow down the time to estimate.
#'  Factors and Character variables with a large number of categories should not be included,
#'  as they will both slow down the data and are unlikely to be useful
#' @param chart.title String used as the title of the Scatterplot.
#' @param max.labels.plot A number specifying the maximum of labels of show in bubble or scatterplots. The remaining points will be shown without labels.
#' @param show.labels A logical indicating whether \code{"Label"} attribute should be used for reporting results
#' @importFrom flipData EstimationData GetData
#' @importFrom flipFormat Labels
#' @importFrom flipTransformations Factor
#' @importFrom flipStatistics WeightedTable
#' @importFrom ca mjca
#' @importFrom verbs Sum
#' @export

MultipleCorrespondenceAnalysis <- function(formula,
                                           data = NULL,
                                           output = c("Text", "Scatterplot")[1],
                                           scatter.palette = "Default colors",
                                           weights = NULL,
                                           subset = NULL,
                                           missing = "Exclude cases with missing data",
                                           auxiliary.data = NULL,
                                           chart.title = "Multiple correspondence analysis",
                                           max.labels.plot = 200,
                                           show.labels = FALSE)
{
    if (output != "Scatterplot")
    {
        scatter.palette <- NA
        max.labels.plot <- 0
        chart.title <- ""
    }

    # Data cleaning
    cl <- match.call()
    .formula <- formula # Hack to work past scoping issues in car package: https://cran.r-project.org/web/packages/car/vignettes/embedding.pdf.
    subset.description <- try(deparse(substitute(subset)), silent = TRUE) #We don't know whether subset is a variable in the environment or in data.
    subset <- eval(substitute(subset), data, parent.frame())
    if (!is.null(subset))
    {
        if (is.null(subset.description) | (class(subset.description) == "try-error") | !is.null(attr(subset, "name")))
            subset.description <- Labels(subset)
        if (is.null(attr(subset, "name")))
            attr(subset, "name") <- subset.description
    }
    weights <- eval(substitute(weights), data, parent.frame())
    data <- GetData(.formula, data, auxiliary.data)

    # Turn binary variables into proper factors
    data <- as.data.frame(lapply(data, function(x){
        if (class(x) == "integer")
        {
            x.lev <- sort(unique(as.character(x)))
            if (all(x.lev == c("0", "1")))
            {
                x.lev <- c("No", "Yes")
            }
            x <- Factor(x, labels = x.lev)
        }
        return(x)
    }), check.names=F)

    if (!is.null(weights) && length(weights) != nrow(data))
        stop("length of weights does not match the number of observations in data\n")
    if (!is.null(subset) && length(subset) > 1 && length(subset) != nrow(data))
        stop("length of subset does not match the number of observations in data\n")

    # MCA does not do prediction so no need to retain filtered data
    processed.data <- EstimationData(formula, data, subset, weights, missing)
    is.data.used <- processed.data$post.missing.data.estimation.sample
    data.used <- processed.data$estimation.data
    weights.used <- processed.data$weights
    if (!is.null(weights.used))
    {
        # Rescale weights as mjca gives rounding errors when weights are small
        w.min <- min(1, min(weights.used[weights.used > 0]))
        weights.used <- weights.used/w.min
        datfreq <- WeightedTable(data.used, weights=weights.used)
        dd <- dim(datfreq)
        if (length(dd) <= 2 && min(dd) <= 2)
            stop("Input data does not contain enough variables. Try using Correspondence Analysis instead.\n")
        obj <- try(mjca(datfreq, nd=NA), silent = TRUE)
    }
    else
    {
        obj <- try(mjca(data.used, nd=NA), silent = TRUE)
    }

    # Try to make error message more informative if possible
    if (inherits(obj, "try-error"))
    {
        tmp.dat <- if (!is.null(weights.used)) datfreq else data.used
        tmp.obj <- try(mjca(tmp.dat, nd = NA, lambda = "Burt"), silent = TRUE)
        err.msg <- "Could not perform Multiple Correspondence Analysis. "

        # In the second case here, we still don't know exactly why the Burt matrix can
        # be computed but the adjusted inertias can't but it is helpful to know that
        # the data is not completely wrong
        if (!inherits(tmp.obj, "try-error"))
        {
            if (Sum(tmp.obj$inertia.e > 1/tmp.obj$nd.max, remove.missing = FALSE) <= 1)
                stop (err.msg, "Input data reduces to 1 standard coordinate. Try including additional variables in the analysis.")
            else
                stop(err.msg, "Could not compute adjusted inertia.")
        }
        # Error for some other reason
        stop(err.msg)
    }

    # Label data output
    # levelnames.ord always use colnames(data) and should match levelnames given by mjca
    # the only difference is that they keep the order of the original factor levels
    # variable names may use Label - they are for display only
    cnames <- make.names(colnames(data))
    obj$levelnames.ord <- sprintf("%s:%s",
                                  rep(cnames, lapply(data.used, nlevels)),
                                  unlist(lapply(data.used, levels)))
    obj$variablenames <- if(!show.labels) obj$levelnames.ord
                         else sprintf("%s:%s",
                                        rep(Labels(data), lapply(data.used, nlevels)),
                                        unlist(lapply(data.used, levels)))
    empty.levels <- which(!obj$levelnames.ord %in% obj$levelnames)
    if (length(empty.levels) > 0)
    {
        obj$levelnames.ord <- obj$levelnames.ord[-empty.levels]
        obj$variablenames  <- obj$variablenames[-empty.levels]
    }

    rownames(obj$colcoord) <- obj$levelnames
    rownames(obj$colpcoord) <- obj$levelnames
    obj$colcoord <- obj$colcoord[obj$levelnames.ord,]
    obj$colpcoord <- obj$colpcoord[obj$levelnames.ord,]
    rownames(obj$colcoord) <- obj$variablenames
    rownames(obj$colpcoord) <- obj$variablenames
    colnames(obj$colcoord) <- sprintf("Dimension %d", 1:ncol(obj$colcoord))
    colnames(obj$colpcoord) <- sprintf("Dimension %d", 1:ncol(obj$colpcoord))

    # Save object
    if (show.labels)
        obj$colnames <- Labels(data)
    obj$output <- output
    obj$scatter.palette <- scatter.palette
    obj$data.used <- data.used
    obj$is.data.used <- is.data.used
    obj$data.description <- processed.data$description
    obj$chart.title <- chart.title
    obj$max.labels.plot <- max.labels.plot

    groups <- rep(obj$colnames, obj$levels.n)
    coords <- data.frame(obj$colpcoord[,1:2], Group = groups, stringsAsFactors = FALSE,
                    check.names = FALSE, check.rows = FALSE)
    colnames(coords)[1:2] <- sprintf("Dimension %d (%.1f%%)", 1:2, obj$inertia.e[1:2] * 100)
    attr(coords, "scatter.variable.indices") <- c(x = 1, y = 2, sizes = NA, colors = 3)
    attr(obj, "ChartType") <- "X Y Scatter"
    attr(obj, "ChartData") <- coords
    class(obj) <- c("mcaObj", "visualization-selector")
    return(obj)
}


#' \code{print.mcaObj}
#' @param x The multiple correspondance analysis object to be analysed.
#' @param digits Integer indicating number of decimal places to be used.
#' @param ... Not used
#' @importFrom rhtmlLabeledScatter LabeledScatter
#' @importFrom flipChartBasics ChartColors
#' @export
#' @method print mcaObj
print.mcaObj <- function(x, digits = 3, ...)
{
    if (x$output == "Text")
    {
        ndim <- ncol(x$colpcoord)
        cat("Multiple correspondence analysis\n")
        cat(x$data.description, "\n\n")
        sum.tab <- data.frame('Canonical correlation' = x$sv,
                              'Inertia' = x$sv^2,
                              'Proportion explained' = x$inertia.e,
                              check.names = FALSE)
        rownames(sum.tab) <- sprintf("Dimension %d", 1:nrow(sum.tab))
        print(round(sum.tab[1:ndim,], digits=digits))
        cat("\n\nStandard Coordinates\n")
        print(round(x$colcoord, digits=digits))
        cat("\n\nPrincipal Coordinates\n")
        print(round(x$colpcoord, digits=digits))
    } else
    {
        coords <- attr(x, "ChartData")
        max.length <- max(nchar(x$variablenames))
        if (max.length > 20)
            warning("The labels of the variables and/or values are long. Edit the labels in the input variables to improve the look of this map.")
        gcolors <- ChartColors(length(x$colnames), x$scatter.palette)

        lab <- x$variablenames
        if (x$max.labels.plot > 0 && length(lab) > x$max.labels.plot)
            lab[-(1:x$max.labels.plot)] <- ""
        print(LabeledScatter(X = coords[,1],
                       Y = coords[,2],
                       label = lab,
                       label.alt = x$variablenames,
                       group = coords[,3],
                       colors = gcolors,
                       fixed.aspect = TRUE,
                       title = x$chart.title,
                       x.title = colnames(coords)[1],
                       y.title = colnames(coords)[2],
                       axis.font.size = 10,
                       labels.font.size = 12,
                       title.font.size = 20,
                       y.title.font.size = 16,
                       x.title.font.size = 16))
    }

}

#' @rdname MultipleCorrespondenceAnalysis
#' @param object Object of class \code{"mcaObj"} created using \code{MultipleComponentsAnalysis}.
#' @param ... Not used.
#' @importFrom flipTransformations FactorToIndicators
#' @importFrom flipFormat Labels
#' @importFrom verbs SumEachRow SumEachColumn
#' @export

fitted.mcaObj <- function(object, ...)
{
    newdata <- object$data.used
    tab.newdata <- as.data.frame(lapply(newdata, FactorToIndicators))

    # Checking that data is compatible - may not be needed since we do not predict for new data
    colnames(tab.newdata) <- sprintf("%s:%s", rep(make.names(colnames(newdata)), unlist(lapply(newdata, nlevels))),
                                     unlist(lapply(newdata, levels)))
    extra.levels <- setdiff(colnames(tab.newdata), object$levelnames.ord)
    if (length(extra.levels) > 0 && SumEachRow(tab.newdata[,extra.levels], remove.missing = FALSE) > 0)
        warning("Factor levels of new data contains levels not in estimation data: ",
             paste(extra.levels, collapse=", "), "\n")
    tab.newdata <- tab.newdata[,object$levelnames.ord]

    ndim <- ncol(object$colcoord)
    ndata <- nrow(tab.newdata)
    nq <- ncol(newdata)
    csum <- SumEachColumn(tab.newdata, remove.missing = FALSE)
    denom <- sqrt(csum * nq)
    zx <- sweep(tab.newdata, 2, denom, "/")
    x.svd <- svd(zx)

    fac <- matrix(NA, ndata, ndim)
    for (r in 1:ndata)
        for (c in 1:ndim)
            fac[r,c] <- x.svd$u[r,c+1]/(nq*x.svd$d[c+1])

    ind.prop <- prop.table(as.matrix(tab.newdata), 2)
    pred <- t(ind.prop) %*% fac

    nfull <- length(object$is.data.used)
    index <- which(object$is.data.used) # length should be ndata
    results <- c()
    for (c in 1:ndim)
    {
        vec <- rep(NA, nfull)
        fmult <- object$colcoord[1,c]/pred[1,c]
        for (i in 1:ndata)
            vec[index[i]] <- fac[i,c] * fmult * object$sv[c]
        results <- cbind(results, vec)
    }
    colnames(results) <- sprintf("Dimension %d", 1:ndim)
    return(results)
}
