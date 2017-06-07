context("t-SNE")

data(hbatwithsplits, package = "flipExampleData")

# numerical and factor inputs
input.data <- hbatwithsplits[, c("x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12")]

test_that("tSNE: perplexity", {
    for (perplexity in c(2, 5, 10, 20)) {
        expect_error(tSNE(input.data, perplexity = perplexity, is.distance = FALSE), NA)
    }
})

