#' \code{WeightedTable}
#' @description Generalisation of the \code{table} function in base to handle weights
#' @param ... one or more objects which can be interpretated as factors, or a list or dataframe whose components can be so interpreted
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
#' @param data A data frame containing factor variables.
#' @param output Specify output generated. May be one of \code{"Scatterplot", "Moonplot"} or \code{"Text"}.
#' @param weights A numeric vector containing the weight for each case in \code{data}.
#' @param subset A logical vector which describes the subset of \code{data} to be analysed.
#' @param missing A string specifiying what to do when the data contains missing values. This should be one of \code{"Error if missing data", "Exclude cases with missing data"}, and \code{"Imputation (replace missing values with estimates)"}.
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
    processed.data <- EstimationData(formula, data, subset, weights, missing)


    # MCA
    w.est <- processed.data$weights
    if (!is.null(w.est))
    {
        # Rescale weights as mjca gives rounding errors when weights are small
        w.min <- min(1, min(w.est[w.est > 0]))
        w.est <- w.est/w.min
    }
    datfreq <- WeightedTable(processed.data$estimation.data, weights=w.est)
    dd <- dim(datfreq)
    if (length(dd) <= 2 && min(dd) <= 2)
        stop("Not enough category combinations. Try including more variables in the analysis\n")

    obj <- mjca(datfreq, nd=NA)

    # Label data output
    # levelnames.ord always use colnames(data) and should match levelnames given by mjca
    # the only difference is that they keep the order of the original factor levels
    # variable names may use Label - they are for display only
    obj$levelnames.ord <- sprintf("%s:%s",
                                  rep(colnames(data), lapply(processed.data$estimation.data, nlevels)),
                                  unlist(lapply(processed.data$estimation.data, levels)))
    obj$variablenames <- if(!show.labels) obj$levelnames.ord
                         else sprintf("%s:%s",
                                        rep(Labels(data), lapply(processed.data$estimation.data, nlevels)),
                                        unlist(lapply(processed.data$estimation.data, levels)))
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
    if (missing == "Imputation (replace missing values with estimates)")
        data <- processed.data$data
    obj$data <- data
    obj$processed.data <- processed.data
    class(obj) <- "mcaObj"
    return(obj)
}

#' \code{print.mcaObj}
#' @param object The multiple correspondance analysis object to be analysed
#' @importFrom rhtmlLabeledScatter LabeledScatter
#' @export

print.mcaObj <- function(x, ...)
{
    if (x$output == "Scatterplot")
    {
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

    if (x$output == "Text")
    {
        oo <- options(scipen = 5)
        ndim <- ncol(x$colpcoord)
        cat("Multiple correspondence analysis\n")
        cat(x$processed.data$description, "\n\n")
        sum.tab <- data.frame('Canonical correlation' = x$sv,
                              'Inertia' = x$sv^2,
                              'Proportion explained' = x$inertia.e,
                              check.names = FALSE)
        rownames(sum.tab) <- sprintf("Dimension %d", 1:nrow(sum.tab))
        print(sum.tab[1:ndim,], digits=2)
        cat("\n\nStandard Coordinates\n")
        print(x$colcoord, digits=2)
        cat("\n\nPrincipal Coordinates\n")
        print(x$colpcoord, digits=2)
        on.exit(options(oo))
    }
}

#' \code{predict.mca}
#' @description Predict coordinates of new data with multiple correspondance analysis
#' @param object A mca object created using \code{MultipleCorrespondenceAnalysis}.
#' @param newdata A data frame containing factor variables. Levels must match the factors used to construct the model
#' @importFrom flipTransformations FactorToIndicators
#' @importFrom flipFormat Labels
#' @export

predict.mcaObj <- function(object, newdata = NULL)
{
    if (!inherits(object, "mcaObj"))
        stop("object must be an mca object created using MultipleCorrespondence Analysis()\n")

    if (is.null(newdata))
    {
        newdata <- object$data
        ind.ret <- 1:nrow(newdata)
    }
    else
    {
        cnames <- intersect(colnames(object$data), colnames(newdata))
        if (length(cnames) < ncol(object$data))
            stop("newdata is missing variables in the model\n")
        ind.ret <- 1:nrow(newdata)
        newdata <- rbind(newdata[,cnames], object$data)
    }

    tab.newdata <- as.data.frame(lapply(newdata, FactorToIndicators))
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

    results <- c()
    for (c in 1:ndim)
    {
        vec <- rep(0, ndata)
        fmult <- object$colcoord[1,c]/pred[1,c]
        for (i in 1:ndata)
            vec[i] <- fac[i,c] * fmult * object$sv[c]
        results <- cbind(results, vec)
    }
    colnames(results) <- sprintf("Dimension %d", 1:ndim)

    #coords <- crossprod(t(tab.newdata), object$colpcoord)
    #coords2 <- sweep(coords, 2, object$colcoord[1,]/pred[1,]*object$sv[1:ndim], "*")
    #return(list(results=results, coords=coords2))
    return(results[ind.ret,,drop=FALSE])
}
