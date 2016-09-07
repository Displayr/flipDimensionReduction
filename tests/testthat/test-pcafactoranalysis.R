context("Principal Components Analysis")

library(flipStatistics)

data(pcaPhoneTestData, package = "flipExampleData")
test.data.1 <- pcaPhoneTestData$data.set # Most cases have some missing observations (named "q23 20% Missing" in SPSS file)
test.data.2 <- pcaPhoneTestData$data.set.original # Most cases do not have missing observations (named "q23" in SPSS file)
test.weight <- pcaPhoneTestData$weight
test.calibrated.weight <- pcaPhoneTestData$calibrated.weight
data(cola, package = "flipExampleData")

test_that("PCA: binary", {
    zd <- cola[, match("Q24_1", names(cola)):match("Q24_10", names(cola))]
    z1 <- flipTransformations::AsNumeric(zd, binary = FALSE, remove.first = TRUE)
    z <- PrincipalComponentsAnalysis(data = z1)
    z2 <- flipTransformations::AsNumeric(zd, binary = TRUE, remove.first = TRUE)
    z <- PrincipalComponentsAnalysis(data = z2)
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
    spss.varimax.loadings <- data.matrix(read.csv(system.file("extdata", "small case 1 varimax loadings.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.quartimax.loadings <- data.matrix(read.csv(system.file("extdata", "small case 1 quartimax loadings.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.equamax.loadings <- data.matrix(read.csv(system.file("extdata", "small case 1 equamax loadings.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.oblimin.loadings.0 <- data.matrix(read.csv(system.file("extdata", "small case 1 oblimin 0.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.oblimin.loadings.1 <- data.matrix(read.csv(system.file("extdata", "small case 1 oblimin -1.csv", package = "flipDimensionReduction"), header=FALSE))
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

    spss.varimax.loadings <- data.matrix(read.csv(system.file("extdata", "small case 2 varimax loadings.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.quartimax.loadings <- data.matrix(read.csv(system.file("extdata", "small case 2 quartimax loadings.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.equamax.loadings <- data.matrix(read.csv(system.file("extdata", "small case 2 equamax loadings.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.oblimin.loadings.0 <- data.matrix(read.csv(system.file("extdata", "small case 2 oblimin 0.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.oblimin.loadings.1 <- data.matrix(read.csv(system.file("extdata", "small case 2 oblimin -1.csv", package = "flipDimensionReduction"), header=FALSE))
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

    spss.varimax.loadings <- data.matrix(read.csv(system.file("extdata", "large case 1 varimax loadings.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.quartimax.loadings <- data.matrix(read.csv(system.file("extdata", "large case 1 quartimax loadings.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.equamax.loadings <- data.matrix(read.csv(system.file("extdata", "large case 1 equamax loadings.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.oblimin.loadings.0 <- data.matrix(read.csv(system.file("extdata", "large case 1 oblimin 0.csv", package = "flipDimensionReduction"), header=FALSE))
    spss.oblimin.loadings.1 <- data.matrix(read.csv(system.file("extdata", "large case 1 oblimin -1.csv", package = "flipDimensionReduction"), header=FALSE))
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
for (print.type in c("loadings", "structure", "Component Plot", "details"))
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
                    expect_error(capture.output(print(test.pca)), NA) #capture.output is used to suppress the contents of the printing as I only want to check that the prints don't generate an error
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


    test.pca <- PrincipalComponentsAnalysis(data = test.data.1,
                                              missing = "Imputation (replace missing values with estimates)",
                                              print.type = "loadings",
                                              n.factors = 5,
                                              suppress.small.coefficients = TRUE)
    expect_equal(test.pca$loadings[4,3], 0.172050972343564)

})

test_that("Converting factors for use in PCA", {

    test.data <- test.data.2[,1:10]
    test.data[,1] <- as.factor(test.data[,1])
    test.data[,2] <- as.ordered(test.data[,2])
    converted.data <- flipTransformations::AsNumeric(test.data, binary = TRUE)#ConvertVariablesForFactorAnalysis(test.data)
    expect_error(test.pca <- PrincipalComponentsAnalysis(data = converted.data[, -1],
                                                         missing = "Exclude cases with missing data",
                                                         n.factors = 5,
                                                         print.type = "details"), NA)
    expect_equal(test.pca$loadings[1,1], 0.2634458429558) #Value when we conduct this analysis in Q with the first variable exploded as binary variables (excluding the first level)
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
