context("t-SNE")

data(hbatwithsplits, package = "flipExampleData")

# all numerical inputs
input.data <- hbatwithsplits[, c("x6", "x7", "x8", "x9", "x10", "x11", "x12")]

test_that("tSNE: models and perplexity", {
    for (model in c("tsne", "Rtsne")) {
        for (perplexity in c(2, 5, 10, 20)) {
            expect_error(tSNE(input.data, algorithm = model, data.labels = NULL, perplexity = perplexity), NA)
        }
    }
})


# inputs include factors
input.data <- hbatwithsplits[, c("x4", "x5", "x6", "x7", "x8", "x9", "x10")]

test_that("tSNE: label types", {
    expect_error(tSNE(input.data, algorithm = "Rtsne", data.labels = hbatwithsplits[, c("x3")], perplexity = 10), NA)
    expect_error(tSNE(input.data, algorithm = "Rtsne", data.labels = hbatwithsplits[, c("x22")], perplexity = 10), NA)
    expect_error(tSNE(input.data, algorithm = "Rtsne", data.labels = as.integer(hbatwithsplits[, c("x22")]), perplexity = 10), NA)
})

