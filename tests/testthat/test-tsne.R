context("t-SNE")

data(hbatwithsplits, package = "flipExampleData")

# numerical and factor inputs
input.data <- hbatwithsplits[, c("x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12")]

subset <- hbatwithsplits$x3 == "Large (500+)"

test_that("tSNE: perplexity, subset with binary = TRUE", {
    for (perplexity in c(2, 5, 10, 20)) {
        for (subset in list(subset, NULL)) {
            expect_error(tSNE(input.data, perplexity = perplexity, is.distance = FALSE, binary = TRUE, subset = subset), NA)
        }
    }
})

test_that("tSNE: perplexity, subset with binary = FALSE", {
    for (perplexity in c(2, 5, 10, 20)) {
        for (subset in list(subset, NULL)) {
            expect_warning(tSNE(input.data, perplexity = perplexity, is.distance = FALSE, binary = FALSE, subset = subset))
        }
    }
})


data(breakfast.dissimilarities, package = "flipExampleData")
breakfast = breakfast.dissimilarities[[1]]

test_that("tSNE: distance", {
    expect_error(tSNE(breakfast, perplexity = 3, is.distance = TRUE), NA)
})
