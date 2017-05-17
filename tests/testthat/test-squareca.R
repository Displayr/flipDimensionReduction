context("CA for square matrices")

x1 <- structure(c(0.826446280991736, 2.47933884297521, 0, 2.20385674931129,
0.826446280991736, 1.10192837465565, 19.8347107438017, 0.826446280991736,
3.30578512396694, 6.61157024793388, 0, 4.6831955922865, 0.826446280991736,
0.550964187327824, 1.37741046831956, 0.550964187327824, 5.78512396694215,
0.275482093663912, 20.6611570247934, 3.30578512396694, 1.10192837465565,
4.6831955922865, 0.826446280991736, 2.20385674931129, 15.1515151515152
), .Dim = c(5L, 5L), statistic = "Total %", .Dimnames = list(
    c("Other", "Optus", "Orange (Hutchison)", "Telstra (Mobile Net)",
    "Vodafone"), c("Other", "Optus", "Orange (Hutchison)", "Telstra (Mobile Net)",
    "Vodafone")), name = "x1", questions = c("Company currently with",
"Company for previous contract - if on contract [Phone.sav]"))

x2 <- structure(c(0.710227272727273, 1.13636363636364, 0.284090909090909,
0.852272727272727, 0.710227272727273, 6.39204545454545, 14.0625,
4.11931818181818, 7.24431818181818, 6.67613636363636, 0.142045454545455,
1.27840909090909, 0.852272727272727, 0.568181818181818, 0, 3.40909090909091,
4.97159090909091, 2.98295454545455, 11.9318181818182, 3.26704545454545,
4.26136363636364, 5.11363636363636, 2.84090909090909, 4.40340909090909,
11.7897727272727), .Dim = c(5L, 5L), statistic = "Total %", .Dimnames = list(
    c("Other", "Optus", "Orange", "Telstra", "Vodafone"), c("Other",
    "Optus", "Orange", "Telstra", "Vodafone")), name = "x2", questions = c("Company choosen in choice set A",
"Company currently with [Phone.sav]"))

res1 <- CorrespondenceAnalysis(x1, square=T, output="Text")
res2 <- CorrespondenceAnalysis(x2, square=T, output="Text")

test_that("High symmetry",
          {
              expect_equal(round(res1$original$sv, 3), c(0.582,0.410,0.170,0.170,0.153,0.060,0.060,0.043,0.0))
              # only eigenvectors of symmetric dimensions are unique determined
              expect_equal(round(abs(unname(res1$original$rowcoord[1:5,2])),2), c(0.21,1.06,0.98,0.18, 1.46))
              expect_equal(round(abs(unname(res1$original$rowcoord[1:5,8])),2), c(2.32,0.67,3.35,0.06,0.14))
          })

test_that("Low symmetry",
          {
              expect_equal(round(res2$original$sv, 3), c(0.316,0.271,0.271,0.227,0.079,0.065,0.028,0.028,0.0))
              expect_equal(round(abs(unname(res2$original$rowcoord[1:5,4])),2), c(0.37,1.25,0.58,1.00,0.88))
              expect_equal(round(abs(unname(res2$original$rowcoord[1:5,6])),2), c(2.61,0.68,1.61,0.23,0.29))
          })
