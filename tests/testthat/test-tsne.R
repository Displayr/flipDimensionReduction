context("t-SNE")

data(hbatwithsplits, package = "flipExampleData")

# all numerical inputs
input.data <- hbatwithsplits[, c("x6", "x7", "x8", "x9", "x10", "x11", "x12")]

test_that("tSNE: perplexity", {
    for (perplexity in c(2, 5, 10, 20)) {
        expect_error(tSNE(input.data, data.groups = NULL, perplexity = perplexity, is.distance = FALSE), NA)
    }
})


# inputs include factors
input.data <- hbatwithsplits[, c("x4", "x5", "x6", "x7", "x8", "x9", "x10")]

test_that("tSNE: label types", {
    expect_error(tSNE(input.data, data.groups = hbatwithsplits[, c("x3")], perplexity = 10, is.distance = FALSE), NA)
    expect_error(tSNE(input.data, data.groups = hbatwithsplits[, c("x22")], perplexity = 10, is.distance = FALSE), NA)
    expect_error(tSNE(input.data, data.groups = as.integer(hbatwithsplits[, c("x22")]), perplexity = 10, is.distance = FALSE), NA)
})

