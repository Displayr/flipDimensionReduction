context("t-SNE and UMAP")

data(hbatwithsplits, package = "flipExampleData")

input.data <- hbatwithsplits[, c("x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12")]
input.data <- AsNumeric(input.data)
subset <- hbatwithsplits$x3 == "Large (500+)"

test_that("tSNE: perplexity", {
    for (perplexity in c(2, 5, 10, 20)) {
        expect_error(tSNE(input.data, perplexity = perplexity, is.distance = FALSE), NA)
    }
})


#test_that("UMAP: n.neighbours", {
#    for (n.neighbours in c(2, 5, 10, 20)) {
#        expect_error(UMAP(input.data, n.neighbours = n.neighbours), NA)
#    }
#})


data(breakfast.dissimilarities, package = "flipExampleData")
breakfast = breakfast.dissimilarities[[1]]

test_that("tSNE: distance", {
    expect_error(tSNE(breakfast, perplexity = 3, is.distance = TRUE), NA)
})
