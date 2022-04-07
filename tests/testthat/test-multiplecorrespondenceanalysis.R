context("Multiple Correspondence Analysis")

data("cola", package="flipExampleData")
output = "Scatterplot"
wg <- as.numeric(cola$LastResp)/4

test_that("Attributes for charting", {
    expect_error(default.res <- MultipleCorrespondenceAnalysis(~Q12+Q13+Q14+Q15+Q16,
        data=cola, output = "Scatterplot"), NA)
    expect_equal(attr(default.res, "ChartType"), "X Y Scatter")
    expect_equal(dimnames(attr(default.res, "ChartData")),
                 list(c("Q12:Every or nearly every day", "Q12:4 to 5 days a week",
                    "Q12:2 to 3 days a week", "Q12:Once a week", "Q12:Once every 2 weeks",
                    "Q12:Once a month", "Q12:Once every 3 months", "Q12:Once or twice a year",
                    "Q12:Never", "Q13:Never", "Q13:Rarely (once or twice in a year)",
                    "Q13:Quite often (about once every month)",
                    "Q13:Every chance I get (every week I look for new competitions t",
                    "Q14:To be admired", "Q14:To be appreciated", "Q15:To be selfish",
                    "Q15:To be dependent", "Q16:To be in charge", "Q16:To be successful"),
                    c("Dimension 1 (42.4%)", "Dimension 2 (12.3%)", "Group")))
    expect_equal(attr(attr(default.res, "ChartData"), "scatter.variable.indices"),
        c(x = 1, y = 2, sizes = NA, colors = 3))
})

test_that("MCA canonical correlation with no weighting", {
          expect_error(default.res <- MultipleCorrespondenceAnalysis(~Q12+Q13+Q14+Q15+Q16,
                       data=cola, output = "Scatterplot"), NA)
          expect_equal(round(default.res$sv[1:6], 3),
                       c(0.11,0.06,0.04,0.027,0.018,0.011))
          expect_equal(round(default.res$inertia.e[1:6], 3),
                       c(0.424,0.123,0.055,0.026,0.011,0.004))
          coord <- fitted(default.res)
          expect_equal(abs(round(coord[1:10,1], 3)),
                       abs(c(-0.193,-0.077,0.189,-0.009,-0.077,0.098,0.039,0.128,0.161,-0.180)))
})

test_that("MCA canonical correlation with weighting", {
          expect_error(weighted.res <- MultipleCorrespondenceAnalysis(~Q12+Q13+Q14+Q15+Q16,
                       data=cola, weights=wg), NA)
          expect_equal(round(weighted.res$sv[1:6], 3),
                       c(0.11,0.063,0.048,0.038,0.023,0.012))
          expect_equal(round(weighted.res$inertia.e[1:6], 3),
                       c(0.356,0.118,0.067,0.044,0.016,0.004))
          expect_equal(rownames(weighted.res$colpcoord)[1],
                       "Q12:Every or nearly every day")
})

test_that("Show names", {
          expect_error(label.res <- MultipleCorrespondenceAnalysis(~Q12+Q13+Q14+Q15+Q16, data=cola, show.labels=T), NA)
          expect_equal(rownames(label.res$colpcoord)[1],
                       "Q12. How  often do you drink cola with alcohol:Every or nearly every day")
})

test_that("Missing data", {
    captured.warnings <- capture_warnings(miss.res <- MultipleCorrespondenceAnalysis(~Q7_1+Q7_2+Q7_3+Q7_4, data=cola))
    expect_setequal(captured.warnings,
                    c(paste0("Some categories do not appear in the data: 'Q7. [Pref 2] Specify preference (Q7_1): ",
                             "Coke Zero'. This may be because they are empty in the raw data, or because they are ",
                             "empty after any weights, filters/subsets, or missing data settings are applied. ",
                             "This may cause an error. It is recommended that you merge categories prior to ",
                             "estimating the model, use an alternative missing data method, filter the data, or ",
                             "make the data numeric."),
                      paste0("96% of the data is missing and has been excluded from the analysis. Consider either ",
                             "filters to ensure that the data that is missing is in-line with your expectations, ",
                             "or, set 'Missing Data' to another option.")))
    miss.coord <- fitted(miss.res)
    expect_equal(nrow(miss.res$data.used), 13)
    expect_equal(nrow(miss.res$colpcoord), 23)
    expect_true(abs(unname(round(miss.res$colpcoord[23,1],3))/0.943) - 1L < sqrt(.Machine$double.eps))
    expect_equal(nrow(miss.coord), 327)
    expect_equal(sum(!is.na(miss.coord[,1])), 13)
})

impute.res <- suppressWarnings(MultipleCorrespondenceAnalysis(~Q7_1+Q7_2+Q7_3+Q7_4, data=cola,
                            missing = "Imputation (replace missing values with estimates)"))
test_that("Impute missing data", {
          expect_equal(nrow(impute.res$colpcoord), 24)
})

fcond <- as.numeric(cola$URLID) <= 300
filt.res <- MultipleCorrespondenceAnalysis(~Q12+Q13+Q14+Q15+Q16, data=cola, subset=fcond)
filt.coord <- fitted(filt.res)
test_that("Filters", {
          expect_equal(nrow(filt.res$data.used), 300)
          expect_equal(nrow(filt.coord), 327)
          expect_equal(all(is.na(filt.coord[which(!fcond),1])), TRUE)
          expect_equal(all(!is.na(filt.coord[which(fcond),1])), TRUE)
})

test_that("Error",
{
    expect_error(suppressWarnings(MultipleCorrespondenceAnalysis(~Q2 + Q3, data = cola)), fixed = TRUE,
        "Could not perform Multiple Correspondence Analysis. Input data reduces to 1 standard coordinate. Try including additional variables in the analysis.")
})
