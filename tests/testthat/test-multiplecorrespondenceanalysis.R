context("Multiple correspondence Analysis")

data("cola", package="flipExampleData")
output = "Scatterplot"
wg <- as.numeric(cola$URLID)/4

default.res <- MultipleCorrespondenceAnalysis(~Q12+Q13+Q14+Q15+Q16, data=cola)
weighted.res <- MultipleCorrespondenceAnalysis(~Q12+Q13+Q14+Q15+Q16, data=cola, weights=wg)
label.res <- MultipleCorrespondenceAnalysis(~Q12+Q13+Q14+Q15+Q16, data=cola, show.labels=T)

test_that("MCA canonical correlation with no weighting", {
          expect_equal(round(default.res$sv[1:6], 3),
                       c(0.11,0.06,0.04,0.027,0.018,0.011))
          expect_equal(round(default.res$inertia.e[1:6], 3),
                       c(0.424,0.123,0.055,0.026,0.011,0.004))
          pred <- predict(default.res)
          expect_equal(round(pred[1:10,1], 3),
                       c(-0.193,-0.077,0.189,-0.009,-0.077,0.098,0.039,0.128,0.161,-0.180))
})

#test_that("MCA canonical correlation with weighting", {
#          expect_equal(round(weighted.res$sv[1:6], 3),
#                       c(0.11,0.063,0.048,0.038,0.023,0.012))
#          expect_equal(round(weighted.res$inertia.e[1:6], 3),
#                       c(0.356,0.118,0.067,0.044,0.016,0.00))
#})

test_that("No label names", {
          expect_equal(rownames(weighted.res$colpcoord)[1],
                       "Q12:Every or nearly every day")
})

test_that("Show names", {
          expect_equal(rownames(label.res$colpcoord)[1],
                       "Q12. How  often do you drink cola with alcohol:Every or nearly every day")
})

miss.res <- MultipleCorrespondenceAnalysis(~Q7_1+Q7_2+Q7_3+Q7_4, data=cola)
test_that("Missing data", {
          expect_equal(nrow(miss.res$processed.data$estimation.data), 13)
          expect_equal(nrow(miss.res$colpcoord), 23)
          expect_equal(unname(round(miss.res$colpcoord[23,1],3)), -0.943)
})

impute.res <- MultipleCorrespondenceAnalysis(~Q7_1+Q7_2+Q7_3+Q7_4, data=cola,
                            missing= "Imputation (replace missing values with estimates)")
test_that("Missing data", {
          expect_equal(nrow(impute.res$processed.data$estimation.data), 327)
          expect_equal(nrow(impute.res$colpcoord), 24)
})
