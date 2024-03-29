context("Scree and Component plots")

data(adult.2000, package = "flipExampleData")

test_that("Scree and Component", {
    z <- PrincipalComponentsAnalysis(data = adult.2000[, c("age", "education_num", "hrs_per_week",
                                                           "capital_gain", "capital_loss")],
                                     n.factors = 3)
    expect_error(ComponentPlot(z), NA)
    expect_error(ScreePlot(z), NA)
    expect_error(ScreePlot(adult.2000[, c("age", "education_num", "hrs_per_week",
                             "capital_gain", "capital_loss")]), NA)
})
