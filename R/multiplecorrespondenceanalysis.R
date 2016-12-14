#' \code{WeightedTable}
#' @description Generalisation of the \code{table} function in base to handle weights
#' @param ... one or more objects which can be interpretated as factors, or a list or dataframe whose components can be so interpreted
#' @param weights numeric vector of sampling weights
#' @param exclude levels to remove for all factors in \code{...}
#' @param useNA whether to include \code{NA} values in the table
#' @param dnn the names to given to the dimensions in the result
#' @param deparse.level controls how the default \code{dnn} is constructed.
#' @export

WeightedTable <- function (...,
                           weights = NULL,
                           exclude = if (useNA == "no") c(NA, NaN),
                           useNA = c("no", "ifany", "always"),
                           dnn = list.names(...),
                           deparse.level = 1)
{
    list.names <- function(...) {
        l <- as.list(substitute(list(...)))[-1L]
        nm <- names(l)
        fixup <- if (is.null(nm))
            seq_along(l)
        else nm == ""
        dep <- vapply(l[fixup], function(x) switch(deparse.level +
            1, "", if (is.symbol(x)) as.character(x) else "",
            deparse(x, nlines = 1)[1L]), "")
        if (is.null(nm))
            dep
        else {
            nm[fixup] <- dep
            nm
        }
    }
    if (!missing(exclude) && is.null(exclude))
        useNA <- "always"
    useNA <- match.arg(useNA)
    args <- list(...)
    if (!length(args))
        stop("nothing to tabulate")
    if (length(args) == 1L && is.list(args[[1L]])) {
        args <- args[[1L]]
        if (length(dnn) != length(args))
            dnn <- if (!is.null(argn <- names(args)))
                argn
            else paste(dnn[1L], seq_along(args), sep = ".")
    }
    bin <- 0L
    lens <- NULL
    dims <- integer()
    pd <- 1L
    dn <- NULL
    for (a in args) {
        if (is.null(lens))
            lens <- length(a)
        else if (length(a) != lens)
            stop("all arguments must have the same length")
        cat <- if (is.factor(a)) {
            if (any(is.na(levels(a))))
                a
            else {
                if (is.null(exclude) && useNA != "no")
                  addNA(a, ifany = (useNA == "ifany"))
                else {
                  if (useNA != "no")
                    a <- addNA(a, ifany = (useNA == "ifany"))
                  ll <- levels(a)
                  a <- factor(a, levels = ll[!(ll %in% exclude)],
                    exclude = if (useNA == "no")
                      NA)
                }
            }
        }
        else {
            a <- factor(a, exclude = exclude)
            if (useNA != "no")
                addNA(a, ifany = (useNA == "ifany"))
            else a
        }
        nl <- length(ll <- levels(cat))
        dims <- c(dims, nl)
        if (prod(dims) > .Machine$integer.max)
            stop("attempt to make a table with >= 2^31 elements")
        dn <- c(dn, list(ll))
        bin <- bin + pd * (as.integer(cat) - 1L)
        pd <- pd * nl
    }

    names(dn) <- dnn
    bin <- bin[!is.na(bin)]
    if (length(bin))
        bin <- bin + 1L
    wbins <- 1
    if (!is.null(weights))
    {
        wbins <- rep(0, pd)
        for (i in 1:length(weights))
           wbins[bin[i]] <- wbins[bin[i]] + weights[i]
        #cat("wbins:", wbins, "\n")
        y <- array(wbins, dims, dimnames = dn)
    }
    else
    {
        y <- array(tabulate(bin, pd), dims, dimnames = dn)
    }
    class(y) <- "table"
    y
}

#' \code{MultipleCorrespondanenceAnalysis}
#' @description Calculate multiple correspondence analysis of categorical variables
#' @param formula Symbolic description of the model to be fitted
#' @param data A data frame containing factor variables.
#' @param output Specify output generated. May be one of \code{"Scatterplot"} or \code{"Text"}.
#' @param weights A numeric vector containing the weight for each case in \code{data}.
#' @param subset A logical vector which describes the subset of \code{data} to be analysed.
#' @param missing A string specifiying what to do when the data contains missing values. This should be one of \code{"Error if missing data", "Exclude cases with missing data"}, and \code{"Imputation (replace missing values with estimates)"}.
#' @param auxiliary.data A \code{data.frame} containing additional variables
#'  to be used in imputation (if required). While adding more variables will improve
#'  the quality of the imputation, it will dramatically slow down the time to estimate.
#'  Factors and Character variables with a large number of categories should not be included,
#'  as they will both slow down the data and are unlikely to be useful
#' @param show.labels A logical indicating whether \code{"Label"} attribute should be used for reporting results
#' @importFrom flipData EstimationData
#' @importFrom flipFormat Labels
#' @importFrom ca mjca
#' @export

MultipleCorrespondenceAnalysis <- function(formula,
                                           data = NULL,
                                           output = c("Text", "Scatterplot")[1],
                                           weights = NULL,
                                           subset = NULL,
                                           missing = "Exclude cases with missing data",
                                           auxiliary.data = NULL,
                                           show.labels = FALSE)
{
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
    }

    # MCA
    datfreq <- WeightedTable(data.used, weights=weights.used)
    dd <- dim(datfreq)
    if (length(dd) <= 2 && min(dd) <= 2)
        stop("Cannot perform SVD on data matrix. Try including more variables in the analysis\n")
    obj <- mjca(datfreq, nd=NA)

    # Label data output
    # levelnames.ord always use colnames(data) and should match levelnames given by mjca
    # the only difference is that they keep the order of the original factor levels
    # variable names may use Label - they are for display only
    obj$levelnames.ord <- sprintf("%s:%s",
                                  rep(colnames(data), lapply(data.used, nlevels)),
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
    obj$data.used <- data.used
    obj$is.data.used <- is.data.used
    obj$data.description <- processed.data$description
    class(obj) <- "mcaObj"
    return(obj)
}

#' \code{plot.mcaObj}
#' @description Plots scatterplot of two largest principal coordinates from MCA analysis
#' @param x The multiple correspondance analysis object to be analysed
#' @importFrom rhtmlLabeledScatter LabeledScatter
#' @export
#'
plot.mcaObj <- function(x, ...)
{
    if (!inherits(x, "mcaObj"))
        stop("object must be an mca object created using MultipleCorrespondence Analysis()\n")

     groups <- rep(x$colnames, x$levels.n)
        print(LabeledScatter(X = x$colpcoord[,1],
                       Y = x$colpcoord[,2],
                       label = x$variablenames,
                       group = groups,
                       fixed.aspect = TRUE,
                       title = "Multiple correspondence analysis",
                       x.title = "Dimension 1",
                       y.title = "Dimension 2",
                       axis.font.size = 8,
                       labels.font.size = 12,
                       title.font.size = 20,
                       y.title.font.size = 16,
                       x.title.font.size = 16))
}


#' \code{print.mcaObj}
#' @param x The multiple correspondance analysis object to be analysed.
#' @param digits Integer indicating number of decimal places to be used.
#' @importFrom rhtmlLabeledScatter LabeledScatter
#' @export

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
        #if (x$output == "Scatterplot")
        groups <- rep(x$colnames, x$levels.n)
        print(LabeledScatter(X = x$colpcoord[,1],
                       Y = x$colpcoord[,2],
                       label = x$variablenames,
                       group = groups,
                       fixed.aspect = TRUE,
                       title = "Multiple correspondence analysis",
                       x.title = "Dimension 1",
                       y.title = "Dimension 2",
                       axis.font.size = 8,
                       labels.font.size = 12,
                       title.font.size = 20,
                       y.title.font.size = 16,
                       x.title.font.size = 16))
    }

}

#' @rdname MultipleCorrespondenceAnalysis
#' @param object Object of class \code{"mcaObj"} created using \code{MultipleComponentsAnalysis}.
#' @importFrom flipTransformations FactorToIndicators
#' @importFrom flipFormat Labels
#' @export

fitted.mcaObj <- function(object, ...)
{
    newdata <- object$data.used
    tab.newdata <- as.data.frame(lapply(newdata, FactorToIndicators))

    # Checking that data is compatible - may not be needed since we do not predict for new data
    colnames(tab.newdata) <- sprintf("%s:%s", rep(colnames(newdata), unlist(lapply(newdata, nlevels))),
                                     unlist(lapply(newdata, levels)))
    extra.levels <- setdiff(colnames(tab.newdata), object$levelnames.ord)
    if (length(extra.levels) > 0 && rowSums(tab.newdata[,extra.levels]) > 0)
        stop("Factor levels of new data contains levels not in estimation data: ",
             paste(extra.levels, collapse=", "), "\n")
    tab.newdata <- tab.newdata[,object$levelnames.ord]

    ndim <- ncol(object$colcoord)
    ndata <- nrow(tab.newdata)
    nq <- ncol(newdata)
    csum <- colSums(tab.newdata)
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
