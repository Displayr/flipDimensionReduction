context("Dimension Reduction Scatterplot and GoodnessOfFit")

data(hbatwithsplits, package = "flipExampleData")
# numerical and factor inputs
input.data <- hbatwithsplits[, c("x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12")]
subset <- hbatwithsplits$x3 != "Large (500+)"

# distance matrices
middle.east <- c("", "Al-Qaida", "Egypt", "Hamas", "Hezbollah", "Iran", "Iraq",
                  "ISIS", "Israel", "Palestinian Authority", "Saudi Arabia", "Syria",
                  "Turkey", "United States", "Al-Qaida", "0", "3", "2", "3", "2",
                  "3", "3", "3", "3", "3", "3", "3", "3", "Egypt", "3", "0", "3",
                  "3", "2", "1", "3", "1", "2", "1", "3", "3", "1", "Hamas", "2",
                  "3", "0", "2", "2", "2", "3", "3", "2", "2", "3", "1", "3", "Hezbollah",
                  "3", "3", "2", "0", "1", "1", "3", "3", "2", "3", "1", "2", "3",
                  "Iran", "2", "2", "2", "1", "0", "1", "3", "3", "2", "3", "1",
                  "3", "3", "Iraq", "3", "1", "2", "1", "1", "0", "3", "3", "1",
                  "3", "1", "3", "1", "ISIS", "3", "3", "3", "3", "3", "3", "0",
                  "3", "3", "3", "3", "3", "3", "Israel", "3", "1", "3", "3", "3",
                  "3", "3", "0", "3", "2", "3", "2", "1", "Palestinian Authority",
                  "3", "2", "2", "2", "2", "1", "3", "3", "0", "1", "2", "1", "2",
                  "Saudi Arabia", "3", "1", "2", "3", "3", "3", "3", "2", "1",
                  "0", "3", "2", "1", "Syria", "3", "3", "3", "1", "1", "1", "3",
                  "3", "2", "3", "0", "1", "3", "Turkey", "3", "3", "1", "2", "3",
                  "3", "3", "2", "1", "2", "1", "0", "2", "United States", "3",
                  "1", "3", "3", "3", "1", "3", "1", "2", "1", "3", "2", "0")
middle.east <- matrix(middle.east, 14, 14)

data(breakfast.dissimilarities, package = "flipExampleData")
breakfast = breakfast.dissimilarities[[1]]


test_that("Dimension Reduction Scatterplot and GoodnessOfFit: data.frame", {
    for (algo in c("t-SNE", "MDS - Metric", "MDS - Non-metric", "PCA")) {
        for (groups in list(hbatwithsplits$x1, hbatwithsplits$x19, NULL)) {
            expect_error(d <- DimensionReductionScatterplot(data = input.data, data.groups = groups, algorithm = algo,
                                                       perplexity = 10, binary = TRUE, subset = subset), NA)
            expect_error(GoodnessOfFitPlot(d, max.points = 100), NA)

        }
    }
})

test_that("Dimension Reduction Scatterplot and GoodnessOfFit: raw matrix", {
    for (algo in c("t-SNE", "MDS - Metric", "MDS - Non-metric")) {
        expect_error(d <- DimensionReductionScatterplot(table = middle.east, algorithm = algo, raw.table = TRUE,
                                                        perplexity = 3), NA)
        expect_error(GoodnessOfFitPlot(d, max.points = 100), NA)
    }
})

test_that("Dimension Reduction Scatterplot and GoodnessOfFit: distance matrix", {
    for (algo in c("t-SNE", "MDS - Metric", "MDS - Non-metric")) {
        expect_error(d <- DimensionReductionScatterplot(table = breakfast, algorithm = algo, raw.table = FALSE,
                                                        perplexity = 3), NA)
        expect_error(GoodnessOfFitPlot(d, max.points = 100), NA)
    }
})
