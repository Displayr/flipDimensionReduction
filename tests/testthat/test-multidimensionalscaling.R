context("Multidimensional scaling")

data(breakfast.dissimilarities, package = "flipExampleData")
breakfast = breakfast.dissimilarities[[1]]

test_that("MDS", {
    expect_error(MultiDimesnsionalScaling(breakfast, metric = TRUE), NA)
    expect_error(MultiDimesnsionalScaling(breakfast, metric = FALSE), NA)
})
