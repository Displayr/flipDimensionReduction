context("Dimension Reduction Scatterplot")

data(hbatwithsplits, package = "flipExampleData")

# numerical and factor inputs
input.data <- hbatwithsplits[, c("x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12")]

subset <- hbatwithsplits$x3 != "Large (500+)"

test_that("Dim Redn Scatterplot: algorithm and groups", {
    for (algo in c("t-SNE", "MDS - Metric", "MDS - Non-metric", "PCA")) {
        for (groups in list(hbatwithsplits$x1, hbatwithsplits$x19, NULL)) {
            expect_error(d <- DimensionReductionScatterplot(data = input.data, data.groups = groups, algorithm = algo,
                                                       perplexity = 10, binary = TRUE, subset = subset), NA)
            expect_error(GoodnessOfFitPlot(d, max.points = 100), NA)

        }
    }
})
