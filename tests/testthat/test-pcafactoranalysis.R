context("Principal Components Analysis")

library(flipStatistics)

data(pcaPhoneTestData, package = "flipExampleData")
test.data.1 <- pcaPhoneTestData$data.set # Most cases have some missing observations (named "q23 20% Missing" in SPSS file)
test.data.2 <- pcaPhoneTestData$data.set.original # Most cases do not have missing observations (named "q23" in SPSS file)
test.weight <- pcaPhoneTestData$weight
test.calibrated.weight <- pcaPhoneTestData$calibrated.weight
data(cola, package = "flipExampleData")

test_that("Smoothing of correlation matrix warnings", {
    # Generic smoothing warning
    expect_warning(PrincipalComponentsAnalysis(test.data.1[,c(1,3,5,1)]),
                   "at least one variable is perfectly correlated with a combination")
    # Missing data removal warning
    expect_warning(PrincipalComponentsAnalysis(test.data.1),
                   "after removing the cases with missing data")
    # More variables than there are cases
    expect_warning(PrincipalComponentsAnalysis(replicate(10, runif(5))),
                   "more observations than the number of variables to be positive definite")
    # Pairwise missing data calculation
    set.seed(12321)
    x <- rnorm(30)
    y <- rnorm(30)
    z <- x + y + rnorm(30)/50
    dat <- data.frame(x, y, z)
    # Set 5 missing at random
    dat <- as.data.frame(lapply(dat, function(x) { is.na(x) <- sample.int(30L, size = 5L); x}))
    expect_warning(PrincipalComponentsAnalysis(dat, missing = "Use partial data (pairwise correlations)"),
                   paste0("Consider using an alternative ", sQuote("missing"),
                          " argument instead"))
})

test_that("PCA: binary", {
    zd <- cola[, match("Q24_1", names(cola)):match("Q24_10", names(cola))]
    z1 <- suppressWarnings(flipTransformations::AsNumeric(zd, binary = FALSE, remove.first = TRUE))
    expect_error(z <- PrincipalComponentsAnalysis(data = z1), NA)
    z2 <- flipTransformations::AsNumeric(zd, binary = TRUE, remove.first = TRUE)
    expect_error(z <- PrincipalComponentsAnalysis(data = z2), NA)
})


test_that("PCA: Select components with Kaiser rule", {
    test.pca <- PrincipalComponentsAnalysis(data=test.data.1, use.correlation = T, missing = "Use partial data (pairwise correlations)", select.n.rule = "Kaiser rule")
    expect_equal(ncol(test.pca$loadings), 8)
})

test_that("PCA: Select components by eigenvalue", {
    test.pca <- PrincipalComponentsAnalysis(data=test.data.1, use.correlation = T, missing = "Use partial data (pairwise correlations)", select.n.rule = "Eigenvalues over", eigen.min = 2)
    expect_equal(ncol(test.pca$loadings), 2)
})

test_that("PCA: show.labels", {

    z <- PrincipalComponentsAnalysis(data = test.data.1, show.labels = TRUE, missing = "Use partial data (pairwise correlations)")
    z2 <- PrincipalComponentsAnalysis(data = test.data.1, n.factors = 2, show.labels = FALSE, print.type = "Detailed Output", missing = "Use partial data (pairwise correlations)")
    expect_equal(rownames(z$loadings)[1], rownames(z$loadings)[1])
})

# Compare the first eigenvalue of the input matrix with that reported in SPSS
test_that("Compare Eigenvalues with SPSS Results", {

    # Pairwise correlation matrix, unweighted
    test.pca <- PrincipalComponentsAnalysis(data = test.data.1,
                                            use.correlation = TRUE,
                                            missing = "Use partial data (pairwise correlations)")

    expect_equal(round(test.pca$values[1], 6), 4.440349)

    # Pairwise covariance matrix, unwieghted
    test.pca <- PrincipalComponentsAnalysis(data = test.data.1,
                                            use.correlation = FALSE,
                                            missing = "Use partial data (pairwise correlations)")
    expect_equal(round(test.pca$values[1], 6), 7.858482)

    # Pairwise correlation matrix, weighted
    test.pca <- PrincipalComponentsAnalysis(data = test.data.1,
                                            use.correlation = TRUE,
                                            weights = test.calibrated.weight,
                                            missing = "Use partial data (pairwise correlations)")
    expect_equal(round(test.pca$values[1], 6), 4.566926)

    # Pairwise covariance matrix, weighted
    test.pca <- PrincipalComponentsAnalysis(data = test.data.1,
                                            use.correlation = FALSE,
                                            weights = test.calibrated.weight,
                                            missing = "Use partial data (pairwise correlations)")
    expect_equal(round(test.pca$values[1], 6), 7.394944)

    # Listwise correlation matrix, weighted
    test.pca <- PrincipalComponentsAnalysis(data = test.data.2,
                                            use.correlation = TRUE,
                                            weights = test.weight,
                                            missing = "Exclude cases with missing data")

    expect_equal(round(test.pca$values[1], 6), 4.540077)

    # Listwise covariance matrix, weighted
    test.pca <- PrincipalComponentsAnalysis(data = test.data.2,
                                            use.correlation = FALSE,
                                            weights = test.weight,
                                            missing = "Exclude cases with missing data")

    expect_equal(round(test.pca$values[1], 6), 7.285815)

})

# Compare unrotated loadings with SPSS
test_that("Compare Loadings with SPSS Results", {

    # Replicate SPSS PCA Results using loading value (4, 3) from the Component Matrix

    # Options:
    # - Weighted data
    # - Computed using correlation matrix
    # - Use complete observations (listwise)
    # This is the SPSS default option
    test.pca <- PrincipalComponentsAnalysis(data = test.data.2,
                                            weights = test.weight,
                                            n.factors = 7)
    expect_equal(round(test.pca$loadings[4,3], 13) , round(-0.11674422568134, 13))

    # Options:
    # - No weight
    # - Computed using correlation matrix
    # - Use pairwise complete observations
    test.pca <- PrincipalComponentsAnalysis(data = test.data.1,
                                            use.correlation = TRUE,
                                            n.factors = 7,
                                            missing = "Use partial data (pairwise correlations)")

    expect_equal(test.pca$loadings[4,3], 0.191794614246439)

    # Options:
    # - Weighted
    # - Covariance matrix
    # - Pairwise complete observations
    # For the covaraiance matrix these correspond to the initial, non-standardized loadings.
    test.pca <- PrincipalComponentsAnalysis(data = test.data.1,
                                            weights = test.calibrated.weight,
                                            use.correlation = FALSE,
                                            n.factors = 7,
                                            missing = "Use partial data (pairwise correlations)")
    expect_equal(test.pca$raw.loadings[4,3], -0.184068852456745)
    expect_equal(test.pca$loadings[4,3], -0.138042266103701)


})


test_that("Rotations", {

    # Small Case 1
    dd <- test.data.1
    use.correlation <- FALSE
    missing <- "Use partial data (pairwise correlations)"
    n.factors <- 5
    test.pca <- PrincipalComponentsAnalysis(data = dd, n.factors = n.factors, use.correlation = use.correlation, missing = missing)
    test.loadings <- test.pca$raw.loadings
    stddevs <- StandardDeviation(test.pca$data.used$subset.data, weights = test.pca$data.used$subset.weights)

    # Varimax
    varimax.results <- RotateLoadings(test.loadings, rotation = "varimax")
    expect_equal(varimax.results$rotated.loadings[4,3], 0.0108629573029305)
    expect_equal(varimax.results$objective, -2.65301564743348)


    # Quartimax
    quartimax.results <- RotateLoadings(test.loadings, rotation = "quartimax")
    expect_equal(quartimax.results$rotated.loadings[4,3], 0.0104355733910576)
    expect_equal(quartimax.results$objective, -4.09250665459443)

    # Equamax
    equamax.results <- RotateLoadings(test.loadings, rotation = "equamax")
    expect_equal(equamax.results$rotated.loadings[4,3], 0.00875627452122685)
    expect_equal(equamax.results$objective, 4.99779933584178)

    # Oblimin with delta = 0
    oblimin.results <- RotateLoadings(test.loadings, rotation = "oblimin", delta = 0)
    expect_equal(oblimin.results$rotated.loadings[4,3], 0.466006989622174)
    expect_equal(oblimin.results$objective, 1.73906924666269)

    # Oblimin with delta = -1
    oblimin.results <- RotateLoadings(test.loadings, rotation = "oblimin", delta = -1)
    expect_equal(oblimin.results$rotated.loadings[4,3], 0.401050216217268)
    expect_equal(oblimin.results$objective, 6.11495048067651)

    # Promax with kappa = 4
    promax.results <- RotateLoadings(test.loadings, rotation = "promax", kappa = 4, covar = TRUE, stds = stddevs)

    expect_equal(promax.results$rotated.loadings[4,3], -0.00112202944507435)
    expect_equal(promax.results$rotated.loadings[1,1], -0.0466529994996703)


    # Small Test Case 2
    dd <- test.data.2
    use.correlation <- TRUE
    missing <- "Exclude cases with missing data"
    n.factors <- 5
    test.pca <- PrincipalComponentsAnalysis(data = dd, n.factors = n.factors, use.correlation = use.correlation, missing = missing)
    test.loadings <- test.pca$loadings
    stddevs <- StandardDeviation(test.pca$data.used$subset.data, weights = test.pca$data.used$subset.weights)

    # Varimax
    varimax.results <- RotateLoadings(test.loadings, rotation = "varimax")
    expect_equal(varimax.results$rotated.loadings[4,3], -0.222446034144783)
    expect_equal(varimax.results$objective, -2.77562018264623)


    # Quartimax
    quartimax.results <- RotateLoadings(test.loadings, rotation = "quartimax")
    expect_equal(quartimax.results$rotated.loadings[4,3], -0.218988274450239)
    expect_equal(quartimax.results$objective, -4.06874788859416)

    # Equamax
    equamax.results <- RotateLoadings(test.loadings, rotation = "equamax")
    expect_equal(equamax.results$rotated.loadings[4,3], -0.225579835503491)
    expect_equal(equamax.results$objective, 4.77987055008378)

    # Oblimin with delta = 0
    oblimin.results <- RotateLoadings(test.loadings, rotation = "oblimin", delta = 0)
    expect_equal(oblimin.results$rotated.loadings[4,3], 0.0798833587959736)
    expect_equal(oblimin.results$objective, 1.82172697535417)

    # Oblimin with delta = -1
    oblimin.results <- RotateLoadings(test.loadings, rotation = "oblimin", delta = -1)
    expect_equal(oblimin.results$rotated.loadings[4,3], 0.0813449976456852)
    expect_equal(oblimin.results$objective, 6.43527779337301)

    # Promax with kappa = 4
    promax.results <- RotateLoadings(test.loadings, rotation = "promax", kappa = 4, covar = FALSE, stds = stddevs)
    expect_equal(promax.results$rotated.loadings[4,3], -0.233120853002891)




    # Large Test Case 1
    dd <- test.data.2
    use.correlation <- FALSE
    missing <- "Exclude cases with missing data"
    n.factors <- 15
    test.pca <- PrincipalComponentsAnalysis(data = dd, n.factors = n.factors, use.correlation = use.correlation, missing = missing)
    test.loadings <- test.pca$raw.loadings
    stddevs <- StandardDeviation(test.pca$data.used$subset.data, weights = test.pca$data.used$subset.weights)


    # Varimax
    varimax.results <- RotateLoadings(test.loadings, rotation = "varimax")
    expect_equal(varimax.results$rotated.loadings[4,3], -0.199118942590853)
    expect_equal(varimax.results$objective, -3.82370035508231)


    # Quartimax
    quartimax.results <- RotateLoadings(test.loadings, rotation = "quartimax")
    expect_equal(quartimax.results$rotated.loadings[4,3], -0.192735775676314)
    expect_equal(quartimax.results$objective, -4.35565604315862)

    # Equamax
    equamax.results <- RotateLoadings(test.loadings, rotation = "equamax")
    expect_equal(equamax.results$rotated.loadings[4,3], -0.15997219890279)
    expect_equal(equamax.results$objective, 3.66041181216845)

    # Oblimin with delta = 0
    oblimin.results <- RotateLoadings(test.loadings, rotation = "oblimin", delta = 0)
    expect_equal(oblimin.results$rotated.loadings[4,3], 0.359332841701882)
    expect_equal(oblimin.results$objective, 1.14431489879381)

    # Oblimin with delta = -1
    oblimin.results <- RotateLoadings(test.loadings, rotation = "oblimin", delta = -1)
    expect_equal(oblimin.results$rotated.loadings[4,3], 0.393949682001849)
    expect_equal(oblimin.results$objective, 6.00236160840857)

    # Promax with kappa = 4
    promax.results <- RotateLoadings(test.loadings, rotation = "promax", kappa = 4, covar = TRUE, stds = stddevs)
    expect_equal(promax.results$rotated.loadings[4,3], -0.100589900441283)


    # Objective functions for SPSS Rotations
    #Small Case 1
    spss.varimax.loadings <- data.matrix(read.csv(system.file("extdata", "small_case_1_varimax_loadings.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.quartimax.loadings <- data.matrix(read.csv(system.file("extdata", "small_case_1_quartimax_loadings.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.equamax.loadings <- data.matrix(read.csv(system.file("extdata", "small_case_1_equamax_loadings.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.oblimin.loadings.0 <- data.matrix(read.csv(system.file("extdata", "small_case_1_oblimin_0.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.oblimin.loadings.1 <- data.matrix(read.csv(system.file("extdata", "small_case_1_oblimin_-1.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.varimax.objective <- get.objective.for.loadings(spss.varimax.loadings, method = "varimax")
    spss.quartimax.objective <- get.objective.for.loadings(spss.quartimax.loadings, method = "quartimax")
    spss.equamax.objective <- get.objective.for.loadings(spss.equamax.loadings, method = "equamax")
    spss.oblimin.objective.0 <- get.objective.for.loadings(spss.oblimin.loadings.0, method = "oblimin", gam = 0)
    spss.oblimin.objective.1 <- get.objective.for.loadings(spss.oblimin.loadings.1, method = "oblimin", gam = -1)
    expect_equal(spss.varimax.objective, -2.6530156416783)
    expect_equal(spss.quartimax.objective, -4.09250650536889)
    expect_equal(spss.equamax.objective, 4.99779934971988)
    expect_equal(spss.oblimin.objective.0, 2.05513700318594)
    expect_equal(spss.oblimin.objective.1, 6.98200686859762)

    # Small Case 2

    spss.varimax.loadings <- data.matrix(read.csv(system.file("extdata", "small_case_2_varimax_loadings.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.quartimax.loadings <- data.matrix(read.csv(system.file("extdata", "small_case_2_quartimax_loadings.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.equamax.loadings <- data.matrix(read.csv(system.file("extdata", "small_case_2_equamax_loadings.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.oblimin.loadings.0 <- data.matrix(read.csv(system.file("extdata", "small_case_2_oblimin_0.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.oblimin.loadings.1 <- data.matrix(read.csv(system.file("extdata", "small_case_2_oblimin_-1.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.varimax.objective <- get.objective.for.loadings(spss.varimax.loadings, method = "varimax")
    spss.quartimax.objective <- get.objective.for.loadings(spss.quartimax.loadings, method = "quartimax")
    spss.equamax.objective <- get.objective.for.loadings(spss.equamax.loadings, method = "equamax")
    spss.oblimin.objective.0 <- get.objective.for.loadings(spss.oblimin.loadings.0, method = "oblimin", gam = 0)
    spss.oblimin.objective.1 <- get.objective.for.loadings(spss.oblimin.loadings.1, method = "oblimin", gam = -1)
    expect_equal(spss.varimax.objective, -2.77562017624509)
    expect_equal(spss.quartimax.objective, -4.06874755671149)
    expect_equal(spss.equamax.objective, 4.77987055356406)
    expect_equal(spss.oblimin.objective.0, 2.0692239107895)
    expect_equal(spss.oblimin.objective.1, 7.04195048947717)


    # Large Case 1

    spss.varimax.loadings <- data.matrix(read.csv(system.file("extdata", "large_case_1_varimax_loadings.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.quartimax.loadings <- data.matrix(read.csv(system.file("extdata", "large_case_1_quartimax_loadings.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.equamax.loadings <- data.matrix(read.csv(system.file("extdata", "large_case_1_equamax_loadings.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.oblimin.loadings.0 <- data.matrix(read.csv(system.file("extdata", "large_case_1_oblimin_0.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.oblimin.loadings.1 <- data.matrix(read.csv(system.file("extdata", "large_case_1_oblimin_-1.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.varimax.objective <- get.objective.for.loadings(spss.varimax.loadings, method = "varimax")
    spss.quartimax.objective <- get.objective.for.loadings(spss.quartimax.loadings, method = "quartimax")
    spss.equamax.objective <- get.objective.for.loadings(spss.equamax.loadings, method = "equamax")
    spss.oblimin.objective.0 <- get.objective.for.loadings(spss.oblimin.loadings.0, method = "oblimin", gam = 0)
    spss.oblimin.objective.1 <- get.objective.for.loadings(spss.oblimin.loadings.1, method = "oblimin", gam = -1)
    expect_equal(spss.varimax.objective, -3.82370028777476)
    expect_equal(spss.quartimax.objective, -4.35565049292685)
    expect_equal(spss.equamax.objective, 3.66041249247838)
    expect_equal(spss.oblimin.objective.0, 1.56545745595947)
    expect_equal(spss.oblimin.objective.1, 7.57197382710856)

})

test_that("Compare Scores with SPSS", {


    ###
    # Replicate SPSS PCA Scores - Variables produced based on the factors identified
    # Look at the value of component 1 in the 6th case
    ###



    # Options:
    # - No weight
    # - Computed using correlation matrix
    # - No rotation
    # - Use only complete observations
    test.pca <- PrincipalComponentsAnalysis(data = test.data.2,
                                            use.correlation = TRUE,
                                            n.factors = 7)
    expect_equal(test.pca$scores[6,1], -.88813361129142)




    # Options:
    # - Weighted by test.weight
    # - Computed using correlation matrix
    # - No rotation
    # - Use only complete observations
    test.pca <- PrincipalComponentsAnalysis(data = test.data.2,
                                            use.correlation = TRUE,
                                            n.factors = 7,
                                            weights = test.weight)
    expect_equal(test.pca$scores[6,1], -1.16828517195651)


    # Options:
    # - Unweighted
    # - Correlation matrix
    # - no rotation
    # - pairwise complete observations
    test.pca <- PrincipalComponentsAnalysis(data = test.data.1,
                                            use.correlation = TRUE,
                                            n.factors = 5,
                                            missing = "Use partial data (pairwise correlations)")
    expect_equal(test.pca$scores[145,1], -.96405711314469)

    # Options:
    # - Unweighted
    # - Covariance matrix
    # - No rotation
    # - Listwise complete
    test.pca <- PrincipalComponentsAnalysis(data = test.data.2,
                                            use.correlation = FALSE,
                                            n.factors = 5)
    expect_equal(test.pca$scores[6,1], -.92965574784272)

    # Options:
    # - weighted
    # - covariance matrix
    # - no rotation
    # - Listwise complete
    test.pca <- PrincipalComponentsAnalysis(data = test.data.2,
                                            use.correlation = FALSE,
                                            weights = test.weight,
                                            n.factors = 7)
    expect_equal(test.pca$scores[6,1], -1.26176530793757)

    # Options:
    # - correlation matrix
    # - varimax rotation
    # - listwise complete
    test.pca <- PrincipalComponentsAnalysis(data = test.data.2,
                                            use.correlation = TRUE,
                                            n.factors = 7,
                                            rotation = "varimax")
    expect_equal(round(test.pca$scores[6,1],4), round(-.96769594035589, 4))

    # Options:
    # - correlation matrix
    # - promax rotation
    # - listwise complete
    test.pca <- PrincipalComponentsAnalysis(data = test.data.2,
                                            use.correlation = TRUE,
                                            n.factors = 7,
                                            rotation = "promax")
    expect_equal(unname(test.pca$scores[6,1]), -0.967004475228648)

    # Options:
    # - Covariance matrix
    # - varimax rotation
    # - weighted
    # - listwise complete
    test.pca <- PrincipalComponentsAnalysis(data = test.data.2,
                                            use.correlation = FALSE,
                                            n.factors = 7,
                                            rotation = "varimax",
                                            weights = test.weight)
    expect_equal(round(test.pca$scores[6,1], 4), round(-1.10861281521457, 4))




})


# Test that the print options do not generate errors in any combination
for (print.type in c("loadings", "structure", "Component Plot", "Scree Plot", "details", "variance", "2d"))
{
    for (rotation in c("none", "varimax", "promax"))
    {
        for (missing in c("Use partial data (pairwise correlations)", "Exclude cases with missing data"))
        {
            for (use.correlation in c(TRUE, FALSE))
            {

                test_that(paste("Print options do not generate errors:", print.type, rotation, missing, use.correlation), {
                    expect_error(test.pca <- PrincipalComponentsAnalysis(data = test.data.2,
                                                                         weights = test.weight,
                                                                         n.factors = 5,
                                                                         print.type = print.type,
                                                                         rotation = rotation,
                                                                         missing = missing,
                                                                         use.correlation = use.correlation,
                                                                         suppress.small.coefficients = TRUE,
                                                                         sort.coefficients.by.size = TRUE), NA)
                    expect_error(capture.output(suppressWarnings(print(test.pca))), NA) #capture.output is used to suppress the contents of the printing as I only want to check that the prints don't generate an error
                })
            }

        }

    }
}




test_that("Compare other measures with SPSS", {

    # Replicate SPSS Bartlet test results

    expect_equal(BartlettTestOfSphericity(test.data.1, missing = "Use partial data (pairwise correlations)")$chisq, 2254.76019036933)
    expect_equal(BartlettTestOfSphericity(test.data.2, missing = "Exclude cases with missing data")$chisq, 3059.91481376238)
    expect_equal(BartlettTestOfSphericity(test.data.1, missing = "Use partial data (pairwise correlations)", weights = test.weight)$chisq, 2435.76130608532)
    expect_equal(BartlettTestOfSphericity(test.data.2, missing = "Exclude cases with missing data", weights = test.weight)$chisq, 3371.42755825416)

    # Communalities and Sum of Squared Loadings
    test.pca <- PrincipalComponentsAnalysis(data = test.data.2,
                                            weights = test.weight,
                                            n.factors = 5,
                                            rotation = "promax",
                                            missing = "Use partial data (pairwise correlations)",
                                            use.correlation = FALSE,
                                            print.type = "details")
    expect_equal(unname(round(test.pca$extracted.communalities[1], 6)), 0.044206)
    expect_equal(unname(round(test.pca$rescaled.extracted.communalities[1], 6)), 0.092503)
    expect_equal(unname(round(colSums(test.pca$structure.matrix^2)[1], 4)), round(3.943949,4))

    test.pca <- PrincipalComponentsAnalysis(data = test.data.2,
                                            n.factors = 5,
                                            rotation = "varimax",
                                            missing = "Use partial data (pairwise correlations)",
                                            use.correlation = TRUE,
                                            print.type = "details")
    expect_equal(unname(round(test.pca$extracted.communalities[1], 6)), 0.277254)
    expect_equal(unname(round(colSums(test.pca$loadings^2)[1], 5)), round(3.430480, 5))
})


test_that("Imputation", {


    expect_error(test.pca <- PrincipalComponentsAnalysis(data = test.data.1,
                                              missing = "Imputation (replace missing values with estimates)",
                                              print.type = "loadings",
                                              n.factors = 5,
                                              suppress.small.coefficients = TRUE), NA)
    #expect_equal(round(test.pca$loadings[4,3],3), 0.133)

})


test_that("Filters", {

    filt <- (1:nrow(test.data.1)) > 300
    test.pca <- PrincipalComponentsAnalysis(data = test.data.1,
                                              missing = "Imputation (replace missing values with estimates)",
                                              subset = filt,
                                              print.type = "loadings",
                                              n.factors = 5,
                                              suppress.small.coefficients = TRUE)
    sc.pca <- fitted(test.pca)
    expect_equal(all(!is.na(sc.pca[which(filt),1])), TRUE)
    expect_equal(all(is.na(sc.pca[which(!filt),1])), TRUE)
    expect_equal(nrow(sc.pca), nrow(test.data.1))

    # check ambiguous rownames are handled correctly for use in TextPrincipalComponentAnalysis
    ambiguous.row.name.data <- read.csv(system.file("extdata", "toy_encoding_example.csv",
                                                    package = "flipDimensionReduction"),
                                        header = TRUE)
    ambiguous.rows <- ambiguous.row.name.data[, 1]
    ambiguous.row.name.data <- ambiguous.row.name.data[, -1]
    ambiguous.row.name.data <- as.matrix(ambiguous.row.name.data)
    row.names(ambiguous.row.name.data) <- ambiguous.rows
    expect_error(test.prep <- flipDimensionReduction:::prepareDataForFactorAnalysis(data = ambiguous.row.name.data,
                                                                                    subset = NULL, weights = NULL,
                                                                                    missing = "Exclude cases with missing data"),
                 NA)
    test.subset <- rep(FALSE, nrow(ambiguous.row.name.data))
    test.subset[15:nrow(ambiguous.row.name.data)] <- TRUE
    test.weights <- rnorm(nrow(ambiguous.row.name.data))
    expect_error(test.prep <- flipDimensionReduction:::prepareDataForFactorAnalysis(data = ambiguous.row.name.data,
                                                                                    subset = test.subset, weights = test.weights,
                                                                                    missing = "Exclude cases with missing data"),
                 NA)
    expect_equal(ambiguous.row.name.data[test.subset, ], test.prep$subset.data)
    expect_equal(test.weights[test.subset], test.prep$subset.weights)
})


test_that("Converting factors for use in PCA", {

    test.data <- test.data.2[,1:10]
    test.data[,1] <- as.factor(test.data[,1])
    test.data[,2] <- as.ordered(test.data[,2])
    converted.data <- suppressWarnings(flipTransformations::AsNumeric(test.data, binary = TRUE))#ConvertVariablesForFactorAnalysis(test.data)
    expect_error(test.pca <- PrincipalComponentsAnalysis(data = converted.data[, -1],
                                                         missing = "Exclude cases with missing data",
                                                         n.factors = 5,
                                                         print.type = "details"), NA)
    expect_equal(test.pca$loadings[1,1], 0.2634458429558) #Value when we conduct this analysis in Q with the first variable exploded as binary variables (excluding the first level)
})


test_that("Component Plot works for PCA objects created by princomp and psych", {
    dd <- test.data.2[!is.na(rowSums(test.data.2)),]
    test.princomp <- princomp(dd)
    expect_error(ComponentPlot(test.princomp), NA)
    test.psych <- psych::principal(test.data.1, nfactors = 5)
    expect_error(ComponentPlot(test.psych), NA)
})

test_that("PCA contains attribute data for exporting to Excel",
{
    pca <- PrincipalComponentsAnalysis(test.data.2, n.factors = 7, print.type = "Loadings Table")
    expect_equal(dim(attr(pca, "ChartData")), c(25, 7))
    pca <- PrincipalComponentsAnalysis(test.data.2, n.factors = 7, print.type = "Structure Matrix")
    expect_equal(dim(attr(pca, "ChartData")), c(25, 7))
    pca <- PrincipalComponentsAnalysis(test.data.2, n.factors = 7, print.type = "Variance Explained")
    expect_equal(dim(attr(pca, "ChartData")), c(25, 3))
    pca <- PrincipalComponentsAnalysis(test.data.2, n.factors = 7, print.type = "2D Scatterplot")
    expect_equal(attr(pca, "ChartType"), "X Y Scatter")
    expect_equal(dim(attr(pca, "ChartData")), c(623, 2))
    expect_equal(length(attr(attr(pca, "ChartData"), "scatter.variable.indices")), 4)
    pca <- PrincipalComponentsAnalysis(test.data.2, n.factors = 7, print.type = "Scree Plot")
    expect_equal(length(attr(pca, "ChartData")), 25)
    pca <- PrincipalComponentsAnalysis(test.data.2, n.factors = 7, print.type = "Component Plot")
    expect_equal(attr(pca, "ChartType"), "X Y Scatter")
    expect_equal(dim(attr(pca, "ChartData")), c(25, 2))
    expect_equal(length(attr(attr(pca, "ChartData"), "scatter.variable.indices")), 4)
    pca <- PrincipalComponentsAnalysis(test.data.2, n.factors = 7, print.type = "Detailed Output")
    expect_equal(dim(attr(pca, "ChartData")), c(119, 8))

})


test_that("Duplciate row-names handled", {
    duplicate.name.data <- structure(c(-0.514, -0.379, -0.514, -0.514, -0.514, -0.541, -0.966,
                                       -0.966, -0.966, -0.966, -0.966, -0.966, -0.966, -0.966, -0.966,
                                       -0.966, -0.966, -0.559, -0.863, -0.756, -0.607, -0.618, -0.465,
                                       -0.543, -0.54, -0.593, -0.554, -0.544, -0.577, -0.523, -0.256,
                                       -0.57, -0.472, -0.461, -0.353, -0.25, -0.353, -0.353, -0.353,
                                       -0.273, 0.196, 0.196, 0.196, 0.196, 0.196, 0.196, 0.196, 0.196,
                                       0.196, 0.196, 0.196, 0.228, 0.111, 0.094, -0.085, -0.051, 0.11,
                                       -0.092, -0.072, 0.003, -0.203, -0.141, -0.205, -0.2, -0.048,
                                       -0.214, -0.25, -0.331, 0.502, 0.346, 0.502, 0.502, 0.502, 0.285,
                                       -0.082, -0.082, -0.082, -0.082, -0.082, -0.082, -0.082, -0.082,
                                       -0.082, -0.082, -0.082, -0.072, 0.045, 0.027, 0.016, -0.082,
                                       -0.063, -0.183, -0.064, -0.237, 0.275, 0.336, 0.33, 0.385, 0.154,
                                       0.21, 0.252, 0.164, -0.437, -0.34, -0.437, -0.437, -0.437, -0.137,
                                       -0.114, -0.114, -0.114, -0.114, -0.114, -0.114, -0.114, -0.114,
                                       -0.114, -0.114, -0.114, 0.001, 0.175, 0.303, 0.279, 0.336, 0.47,
                                       0.093, 0.017, 0.102, 0.039, 0.058, 0.284, 0.363, 0.392, 0.201,
                                       0.131, 0.085, 0.337, 0.28, 0.337, 0.337, 0.337, 0.149, -0.083,
                                       -0.083, -0.083, -0.083, -0.083, -0.083, -0.083, -0.083, -0.083,
                                       -0.083, -0.083, -0.06, 0.213, 0.245, 0.267, 0.287, 0.453, 0.288,
                                       0.272, 0.232, -0.187, -0.255, -0.347, -0.268, -0.204, -0.045,
                                       -0.182, 0.054, -0.066, -0.043, -0.066, -0.066, -0.066, 0.013,
                                       -0.024, -0.024, -0.024, -0.024, -0.024, -0.024, -0.024, -0.024,
                                       -0.024, -0.024, -0.024, -0.179, -0.138, -0.291, 0.027, 0.028,
                                       -0.267, 0.494, 0.519, 0.427, 0.206, 0.222, 0.197, 0.213, -0.133,
                                       -0.171, -0.225, -0.115, 0.111, 0.019, 0.111, 0.111, 0.111, 0.047,
                                       0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001,
                                       0.001, 0.001, 0.079, 0.059, 0.069, 0.112, 0.159, 0.181, -0.203,
                                       -0.22, -0.148, 0.114, 0.143, 0.259, 0.216, 0.012, -0.401, -0.558,
                                       -0.44, -0.095, -0.131, -0.095, -0.095, -0.095, -0.146, -0.009,
                                       -0.009, -0.009, -0.009, -0.009, -0.009, -0.009, -0.009, -0.009,
                                       -0.009, -0.009, -0.216, 0.069, 0.084, 0.131, 0.031, 0.049, -0.048,
                                       -0.034, -0.139, 0.525, 0.382, -0.227, -0.225, -0.436, 0.033,
                                       0.032, 0.083, 0.006, 0.016, 0.006, 0.006, 0.006, -0.163, -0.009,
                                       -0.009, -0.009, -0.009, -0.009, -0.009, -0.009, -0.009, -0.009,
                                       -0.009, -0.009, 0.247, -0.031, 0.105, -0.407, -0.314, 0.287,
                                       -0.018, 0.196, 0.034, -0.049, 0.275, -0.03, 0.025, 0.221, -0.021,
                                       -0.198, 0.224, 0.051, 0.002, 0.051, 0.051, 0.051, -0.053, -0.021,
                                       -0.021, -0.021, -0.021, -0.021, -0.021, -0.021, -0.021, -0.021,
                                       -0.021, -0.021, 0.048, 0.061, 0.165, -0.204, -0.004, -0.033,
                                       0.026, 0.126, 0.079, 0.042, 0.136, -0.098, -0.112, 0.15, 0.1,
                                       0.262, -0.584), .Dim = c(34L, 10L),
                                     .Dimnames = list(c("na",
                                                        "n a", "na", "na", "na", "nan", "cat", "cat", "cat", "cat", "cat",
                                                        "cat", "cat", "cat", "cat", "cat", "cat", "i have a cat", "dog",
                                                        "dogs", "hound", "poodle", "dogs rock", "giraffe", "zebra", "lemur",
                                                        "cup", "plate", "stove", "oven", "i like to cook", "love", "like",
                                                        "awesome"), NULL))
    expect_error(pca <- PrincipalComponentsAnalysis(duplicate.name.data, select.n.rule = "Number of components",
                                                    n.factors = 4, rotation = "None"),
                 NA)
    expect_false(any(is.na(pca$scores)))
    subset <- c(rep(FALSE, 16), rep(TRUE, 18))
    expect_error(subset.pca <- PrincipalComponentsAnalysis(duplicate.name.data, select.n.rule = "Number of components",
                                                           n.factors = 4, rotation = "None", subset = subset),
                 NA)
    expect_true(all(is.na(subset.pca$scores[1:16, ])))
    expect_true(all(!is.na(subset.pca$scores[17:34, ])))
    expect_true(all(is.numeric(subset.pca$scores[17:34, ])))
    # Check edge case where there are unique row-names with a single rowname that is empty ("")
    unique.name.with.empty.data <- structure(c(-0.514, -0.514, -0.514, -0.514, -0.541, -0.966, -0.966, -0.966,
                                               -0.966, -0.966, -0.966, -0.966, -0.966, -0.966, -0.966, -0.559,
                                               -0.863, -0.756, -0.607, -0.618, -0.543, -0.54, -0.593, -0.554,
                                               -0.544, -0.577, -0.523, -0.57, -0.472, -0.461, -0.353, -0.25,
                                               -0.353, -0.353, -0.273, 0.196, 0.196, 0.196, 0.196, 0.196, 0.19,
                                               0.196, 0.196, 0.196, 0.228, 0.111, 0.094, -0.085), .Dim = c(8L, 6L),
                                             .Dimnames = list(c(LETTERS[1:7], ""), NULL))
    expect_error(PrincipalComponentsAnalysis(unique.name.with.empty.data, select.n.rule = "Number of components",
                                             n.factors = 4, rotation = "None"),
                 NA)


})

test_that("Output contains the right class for extension buttons", {
    # NOTE: if any of the tests below fail due to class names changing, ALL
    #       extension buttons in the wiki that refer to this class name should
    #       be updated with the new class name.

    result <- suppressWarnings(PrincipalComponentsAnalysis(test.data.1))

    expect_true(inherits(result, "flipFactorAnalysis"))
})

#     # Comparisons with results from Applied Multivariate Statistics for the Social Sciences
#
#     table.11.2.data <- c(1,0.467,0.681,0.447,0.61,0.236,0.401,0.214,-0.062,0.227,0.238,0.189,0.401,0.075,0.314,0.167,0.148,0.099,
#                          0.467,1,0.6,0.585,0.466,0.324,0.346,0.179,0.105,0.465,0.392,0.146,0.374,0.4,0.59,0.337,0.203,0.061,
#                          0.681,0.6,1,0.643,0.673,0.339,0.344,0.242,-0.001,0.295,0.367,0.227,0.479,0.14,0.451,0.239,-0.028,-0.069,
#                          0.447,0.585,0.643,1,0.612,0.357,0.081,0.003,-0.13,0.33,0.178,0.159,0.296,0.289,0.457,0.336,0.236,-0.158,
#                          0.61,0.466,0.673,0.612,1,0.077,0.056,-0.029,-0.352,0.004,0.023,0.117,0.154,-0.027,0.192,0.011,0.037,-0.097,
#                          0.236,0.324,0.339,0.357,0.077,1,0.518,0.517,0.619,0.698,0.542,0.336,0.676,0.513,0.671,0.463,0.051,-0.038,
#                          0.401,0.346,0.344,0.081,0.056,0.518,1,0.632,0.476,0.502,0.381,0.38,0.567,0.369,0.5,0.217,-0.155,0.275,
#                          0.214,0.179,0.242,0.003,-0.029,0.517,0.632,1,0.544,0.517,0.367,0.384,0.589,0.28,0.442,0.182,-0.3,0.159,
#                          -0.062,0.105,-0.001,-0.13,-0.352,0.619,0.476,0.544,1,0.575,0.697,0.084,0.633,0.464,0.456,0.41,-0.043,0.215,
#                          0.227,0.465,0.295,0.33,0.004,0.698,0.502,0.517,0.575,1,0.501,0.192,0.588,0.72,0.716,0.502,0.218,0.032,
#                          0.238,0.392,0.367,0.178,0.023,0.542,0.381,0.367,0.697,0.501,1,-0.001,0.61,0.359,0.46,0.397,0.079,0.091,
#                          0.189,0.146,0.227,0.159,0.117,0.336,0.38,0.384,0.084,0.192,-0.001,1,0.307,0.175,0.333,-0.06,-0.149,0.139,
#                          0.401,0.374,0.479,0.296,0.154,0.676,0.567,0.589,0.633,0.588,0.61,0.307,1,0.465,0.616,0.393,-0.12,0.071,
#                          0.075,0.4,0.14,0.289,-0.027,0.513,0.369,0.28,0.464,0.72,0.359,0.175,0.465,1,0.688,0.519,0.444,0.033,
#                          0.314,0.59,0.451,0.457,0.192,0.671,0.5,0.442,0.456,0.716,0.46,0.333,0.616,0.688,1,0.466,0.199,-0.031,
#                          0.167,0.337,0.239,0.336,0.011,0.463,0.217,0.182,0.41,0.502,0.397,-0.06,0.393,0.519,0.466,1,0.276,-0.145,
#                          0.148,0.203,-0.028,0.236,0.037,0.051,-0.155,-0.3,-0.043,0.218,0.079,-0.149,-0.12,0.444,0.199,0.276,1,-0.344,
#                          0.099,0.061,-0.069,-0.158,-0.097,-0.038,0.275,0.159,0.215,0.032,0.091,0.139,0.071,0.033,-0.031,-0.145,-0.344,1)
#
#     table.11.2.data <- matrix(table.11.2.data, nrow = 18)
#     names <- c("DOM", "CAPSTAT", "SOCIAL", "SOCRPRES", "SELFACP", "WELLBEG", "RESPON", "SOCLIZ",
#                "SELFCTRL", "TOLER", "GOODIMP", "COMMUNAL", "ACHCONF", "ACHINDEP", "INTELEFF",
#                "PSYMIND", "FLEX", "FEMIN")
#     rownames(table.11.2.data) <- names
#     colnames(table.11.2.data) <- names
#
#     # Unrotated PCA with 3 compnents
#     mf <- psych::principal(table.11.2.data, nfactors = 3, rotate = "none")
#     # Loading for DOM in component 1
#     expect_equal(round(mf$loadings[1,1], 5), 0.50137)
#     # Loading for FEMIN in component 3
#     expect_equal(round(mf$loadings[18,3], 5), 0.49437)
#
#     # PCA with 3 components varimax rotation
#     # These results are close to those in the text, but they do not match exactly.
#     # The authors don't provide much info about their varimax rotation
#     mf2 <- psych::principal(table.11.2.data, nfactors = 3, rotate = "varimax")
#     # Loading for TOLER in component 1
#     testthat::expect_equal(mf2$loadings[10,1], 0.85516, 2e-5)
#     # Loading for FEMIN in component 3
#     testthat::expect_equal(mf2$loadings[18,3], 0.56029, 2e-5)
