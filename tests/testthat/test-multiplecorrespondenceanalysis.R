context("Multiple Correspondence Analysis")

data("cola", package="flipExampleData")
output = "Scatterplot"
wg <- as.numeric(cola$LastResp)/4

default.res <- MultipleCorrespondenceAnalysis(~Q12+Q13+Q14+Q15+Q16, data=cola, output = "Scatterplot")
weighted.res <- MultipleCorrespondenceAnalysis(~Q12+Q13+Q14+Q15+Q16, data=cola, weights=wg)
label.res <- MultipleCorrespondenceAnalysis(~Q12+Q13+Q14+Q15+Q16, data=cola, show.labels=T)

test_that("Attributes for charting", {
    expect_equal(attr(default.res, "ChartType"), "X Y Scatter")
    expect_equal(attr(default.res, "ChartData"),
                 structure(list(`Dimension 1 (42.4%)` = c(0.229897544745244, 0.489577115527352,
                    0.0299694565465756, 0.135359694366915, -0.0361801431005296, 0.0922410013858042,
                    -0.00178665113291533, -0.123829363169976, -0.0737649701593521,
                    0.16079588600786, 0.017425452400396, -0.0518742732727303, -0.0413660094206187,
                    0.431759235446547, -0.060175503198125, -0.0200795636559426, 0.111240782653922,
                    0.518575919754892, -0.0266791469970362), `Dimension 2 (12.3%)` = c(-0.188848870632389,
                    0.172685637154292, -0.112704450766059, 0.0860330180978322, -0.00258087847909523,
                    0.0732298559215832, 0.0121108161324338, 0.111366520900892, -0.0988922026001848,
                    -0.0457144307717477, 0.0452486878877055, -0.0252893485414556,
                    -0.0145090231807679, 0.0394345624086789, -0.00549610625904932,
                    0.0367136405461155, -0.20339356862548, -0.0329415776993558, 0.00169474354723375
                    ), Group = c("Q12", "Q12", "Q12", "Q12", "Q12", "Q12", "Q12",
                    "Q12", "Q12", "Q13", "Q13", "Q13", "Q13", "Q14", "Q14", "Q15",
                    "Q15", "Q16", "Q16")), class = "data.frame", row.names = c("Q12:Every or nearly every day",
                    "Q12:4 to 5 days a week", "Q12:2 to 3 days a week", "Q12:Once a week",
                    "Q12:Once every 2 weeks", "Q12:Once a month", "Q12:Once every 3 months",
                    "Q12:Once or twice a year", "Q12:Never", "Q13:Never",
                    "Q13:Rarely (once or twice in a year)",
                    "Q13:Quite often (about once every month)",
                    "Q13:Every chance I get (every week I look for new competitions t",
                    "Q14:To be admired", "Q14:To be appreciated", "Q15:To be selfish",
                    "Q15:To be dependent", "Q16:To be in charge", "Q16:To be successful"
                    ), scatter.variable.indices = c(x = 1, y = 1, sizes = NA, colors = 3)))
})

test_that("MCA canonical correlation with no weighting", {
          expect_equal(round(default.res$sv[1:6], 3),
                       c(0.11,0.06,0.04,0.027,0.018,0.011))
          expect_equal(round(default.res$inertia.e[1:6], 3),
                       c(0.424,0.123,0.055,0.026,0.011,0.004))
          coord <- fitted(default.res)
          expect_equal(abs(round(coord[1:10,1], 3)),
                       abs(c(-0.193,-0.077,0.189,-0.009,-0.077,0.098,0.039,0.128,0.161,-0.180)))
})

test_that("MCA canonical correlation with weighting", {
          expect_equal(round(weighted.res$sv[1:6], 3),
                       c(0.11,0.063,0.048,0.038,0.023,0.012))
          expect_equal(round(weighted.res$inertia.e[1:6], 3),
                       c(0.356,0.118,0.067,0.044,0.016,0.004))
})

test_that("No label names", {
          expect_equal(rownames(weighted.res$colpcoord)[1],
                       "Q12:Every or nearly every day")
})

test_that("Show names", {
          expect_equal(rownames(label.res$colpcoord)[1],
                       "Q12. How  often do you drink cola with alcohol:Every or nearly every day")
})

miss.res <- suppressWarnings(MultipleCorrespondenceAnalysis(~Q7_1+Q7_2+Q7_3+Q7_4, data=cola))
miss.coord <- fitted(miss.res)
test_that("Missing data", {
          expect_equal(nrow(miss.res$data.used), 13)
          expect_equal(nrow(miss.res$colpcoord), 23)
          expect_equal(unname(round(miss.res$colpcoord[23,1],3)), -0.943)
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
