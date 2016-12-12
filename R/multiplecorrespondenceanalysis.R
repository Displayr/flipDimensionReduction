#' \code{WWTable}
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
        #cat("bin", bin, "\n")
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
                               output = c("Scatterplot", "Moonplot", "Text"),
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

    if (!is.null(weights) & length(weights) != nrow(data))
        stop("length of weights does not match the number of observations in data\n")
    if (!is.null(subset) & length(subset) != nrow(data))
        stop("length of subset does not match the number of observations in data\n")
    processed.data <- EstimationData(formula, data, subset, weights, missing)


    # MCA
    w.est <- processed.data$weights
    if (!is.null(w.est))
    {
        w.min <- max(1, min(w.est[w.est > 0]))
        w.est <- w.est/w.min
    }
    datfreq <- WeightedTable(processed.data$estimation.data,
                             weights=w.est)
    obj <- mjca(datfreq, nd=NA)

    obj$levelnames.ord <- sprintf("%s:%s", rep(colnames(processed.data$estimation.data), lapply(processed.data$estimation.data, nlevels)),
                                     unlist(lapply(data, levels)))
    obj$levelnames.show <- ifelse(!show.labels, obj$levelnames.ord,
                                  sprintf("%s:%s", rep(Labels(processed.data$estimation.data),
                                                lapply(processed.data$estimation.data, nlevels)),
                                                unlist(lapply(data, levels))))


    colnames(obj$colcoord) <- sprintf("Dimension %d", 1:ncol(obj$colcoord))
    colnames(obj$colpcoord) <- sprintf("Dimension %d", 1:ncol(obj$colpcoord))
    rownames(obj$colcoord) <- obj$levelnames       # out of order
    rownames(obj$colpcoord) <- obj$levelnames
    obj$colcoord <- obj$colcoord[obj$levelnames.ord,]
    obj$colpcoord <- obj$colpcoord[obj$levelnames.ord,]

    obj$output <- output
    obj$original.data <- data
    obj$processed.data <- processed.data
    class(obj) <- "mca"
    return(obj)
}

#' \code{print.mca}
#' @param object The multiple correspondance analysis object to be analysed
#' @importFrom rhtmlMoonPlot moonplot
#' @importFrom rhtmlLabeledScatter LabeledScatter
#' @export

print.mca <- function(object)
{
    # Print output
    if (object$output == "Scatterplot")
    {
        groups <- rep(object$colnames, object$levels.n)
        print(LabeledScatter(X = object$colpcoord[,1],
                       Y = object$colpcoord[,2],
                       label = object$levelnames.ord,
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

    if (object$output == "Moonplot")
    {

    }

    if (object$output == "Text")
    {
        sum.tab <- data.frame('Canonical correlation' = object$sv,
                              'Inertia' = object$sv^2,
                              'Proportion explained' = object$inertia.e)
        print(sum.tab)
        print(object$colcoord)
        print(object$colpcoord)
    }
}

#' \code{predict.mca}
#' @description Predict coordinates of new data with multiple correspondance analysis
#' @param object A mjca object created using \code{MultipleCorrespondenceAnalysis}.
#' @param newdata A data frame containing factor variables. Levels must match the factors used to construct the model
#' @importFrom flipTransformations FactorToIndicators
#' @export

predict.mca <- function(object, newdata = NULL)
{
    if (!inherits(object, "mca"))
        stop("object must be an mca object created using MultipleCorrespondence Analysis()\n")

    if (is.null(newdata))
        newdata <- object$orig.data

    tab.newdata <- as.data.frame(lapply(newdata, FactorToIndicators))
    # remove empty factors?
    colnames(tab.newdata) <- sprintf("%s:%s", rep(colnames(newdata), lapply(newdata, nlevels)),
                                     unlist(lapply(newdata, levels)))
    if (length(setdiff(colnames(tab.newdata), object$levelnames.ord)) > 0)
        stop("Levels of new data does not match estimation data\n")
    tab.newdata <- tab.newdata[object$levelnames.ord,]   # in case levels have changed order

    coord <- crossprod(t(tab.newdata), object$colpcoord)
    #dist2.row <- rowSums(t((t(tab.newdata) - object$colmass)^2/object$colmass))
    #cos2 <- coord^2/dist2.row
    #coord <- coord[, 1:ncp, drop=FALSE]
    colnames(coord) <- paste("Dim", 1:ncp)
    return(coord)
}
