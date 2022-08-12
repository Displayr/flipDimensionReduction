context("Distance Matrix")

library(flipStatistics)

data(pcaPhoneTestData, package = "flipExampleData")
test.data <- pcaPhoneTestData$data.set.original # Most cases do not have missing observations (named "q23" in SPSS file)
test.weight <- pcaPhoneTestData$weight
data(cola, package = "flipExampleData")

test_that("Distance: binary", {
    z <- cola[, match("Q24_1", names(cola)):match("Q24_10", names(cola))]
    expect_error(suppressWarnings(print(DistanceMatrix(z, binary = FALSE)), NA))
    expect_error(print(DistanceMatrix(z, binary = TRUE)), NA)
})


z <- test.data

for (dm in c("Euclidean", "Squared Euclidean", "Maximum", "Manhattan", "Minkowski"))
    test_that(paste0("DistanceMatrix distance.measure: ", dm), {
        expect_error(print(DistanceMatrix(z, measure = "Dissimilarities", distance.measure = dm)), NA)
})

for (sm in c("Correlation", "Cosine"))
    test_that(paste0("DistanceMatrix similarity.measure: ", sm), {
        expect_error(print(DistanceMatrix(z, measure = "Similarities", similarity.measure = sm)), NA)
})


for (sb in c("Variable", "Case"))
    for (s in c("None", "z-scores", "Range [-1,1]", "Range [0,1]", "Mean of 1", "Standard deviation of 1"))
        test_that(paste0("DistanceMatrix standardization: ", sb, " : ", s), {
            expect_error(print(DistanceMatrix(z, standardize.by = sb, standardization = s)), NA)
})

test_that(paste0("DistanceMatrix outputs"), {
    expect_error(print(DistanceMatrix(z, show.cell.values = "Yes")), NA)
    expect_error(print(DistanceMatrix(z, show.cell.values = "No")), NA)
    expect_error(print(DistanceMatrix(z, show.cell.values = "Automatic")), NA)
    expect_error(print(DistanceMatrix(z, show.row.labels = "Yes")), NA)
    expect_error(print(DistanceMatrix(z, show.row.labels = "No")), NA)
    expect_error(print(DistanceMatrix(z, show.column.labels = "Yes")), NA)
    expect_error(print(DistanceMatrix(z, show.column.labels = "No")), NA)
})

test_that(paste0("DistanceMatrix others"), {
    expect_error(print(DistanceMatrix(z, weights = test.weight)), NA)
    expect_error(print(DistanceMatrix(z, distance.measure = "Minkowski", minkowski = 0)),
                 "Minkowski power must be positive *")
    expect_error(print(DistanceMatrix(z, distance.measure = "Minkowski", minkowski = 0.5)), NA)
    expect_error(print(DistanceMatrix(z, distance.measure = "Minkowski", minkowski = 1)), NA)
    expect_error(print(DistanceMatrix(z, distance.measure = "Minkowski", minkowski = 1000)),
                 "Minkowski power must be positive and less than 100")
    expect_error(print(DistanceMatrix(z, compare = "Cases")), "There are more than 100 cases *")
    expect_error(print(DistanceMatrix(z[1:50, ], compare = "Cases")), NA)
    expect_error(print(DistanceMatrix(z, measure.transformation = "None")), NA)
    expect_error(print(DistanceMatrix(z, measure.transformation = "Absolute values")), NA)
    expect_error(print(DistanceMatrix(z, measure.transformation = "Reverse sign")), NA)
    expect_error(print(DistanceMatrix(z, measure.transformation = "Range [0,1]")), NA)
})

test_that("DS-3881: Distance matrix correctly applies filter", {
    set.seed(12321)
    n <- nrow(z)
    subset <- sample(c(TRUE, FALSE), size = n, replace = TRUE)
    unit.weights <- rep(1L, n)
    d <- DistanceMatrix(z, subset = subset)
    d.explicit.weights <- DistanceMatrix(z, subset = subset, weights = unit.weights)
    expect_equal(d, d.explicit.weights)
    output <- d[["distance"]]
    expect_true(identical(dim(output), c(25L, 25L)))
    expect_true(!anyNA(output))
    expected.labels <- vapply(z, attr, character(1L), "label", USE.NAMES = FALSE)
    expected.labels <- sub("q23: ", "", expected.labels, fixed = TRUE)
    expect_equal(dimnames(output), replicate(2L, expected.labels, simplify = FALSE))
})
