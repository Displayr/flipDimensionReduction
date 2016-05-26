context("Principal Component and Factor Analysis")

# Tests which replicate options available in SPSS
data(pcaPhoneTestData, package = "flipData")
test.data.1 <- pcaPhoneTestData$data.set # Most cases have some missing observations (named "q23 20% Missing" in SPSS file)
test.data.2 <- pcaPhoneTestData$data.set.original # Most cases do not have missing observations (named "q23" in SPSS file)
test.weight <- pcaPhoneTestData$weight
test.calibrated.weight <- pcaPhoneTestData$q.calibrated.weight

test_that("PCA and factor analysis", {


####
# Replicate SPSS PCA Results using loading value (4, 3) from the Component Matrix
####

# Options:
# - Weighted data
# - Computed using correlation matrix
# - No Rotation
# - Use complete observations (listwise)
# This is the SPSS default option
testthat::expect_equal(
    FactorAnalysis(data = test.data.2, weights = test.weight,
                   n.factors = 7)$loadings[4,3] , -0.11674422568134, 1e-8)

# Options:
# - No weight
# - Computed using correlation matrix
# - No rotation
# - Use pairwise complete observations
testthat::expect_equal(
    FactorAnalysis(data = test.data.1,
                   use.correlation = TRUE,
                   n.factors = 7,
                   missing = "Use partial data (pairwise correlations)")$loadings[4,3],
    0.191794614246439
)





###
# Replicate SPSS PCA Scores - Variables produced based on the factors identified
# Look at the value of component 1 in the 6th case
###

# Options:
# - No weight
# - Computed using correlation matrix
# - No rotation
# - Use only complete observations
test.pca <- FactorAnalysis(data = test.data.2,
                           use.correlation = TRUE,
                           n.factors = 7)
testthat::expect_equal(
    GenerateScoresFromFactorAnalysis(test.pca)[6,1], -.88813361129142
)

# Options:
# - Weighted by test.weight
# - Computed using correlation matrix
# - No rotation
# - Use only complete observations
test.pca <- FactorAnalysis(data = test.data.2,
                           use.correlation = TRUE,
                           n.factors = 7,
                           weights = test.weight)
testthat::expect_equal(
    GenerateScoresFromFactorAnalysis(test.pca)[6,1], -1.16828517195651
)


###
# Replicate SPSS Factor Analysis Results by comparing loadings
###

#test.fa <- FactorAnalysis(data = test.data.2, use.correlation = TRUE, n.factors = 7, type = "Unweighted least squares")
#test.fa <- FactorAnalysis(data = test.data.2, use.correlation = TRUE, n.factors = 7, type = "Generalized least squares")

###
# Replicate SPSS Factor Analysis Scores
###



###
# Replicate SPSS Rotations by comparing rotated loadings
###


###
# Replicate SPSS goodness of fit values for FA


###
# Replicate SPSS Bartlet test results
###

testthat::expect_equal(BartlettTestOfSphericity(test.data.1, missing = "Use partial data (pairwise correlations)")$chisq, 2254.76019036933)
testthat::expect_equal(BartlettTestOfSphericity(test.data.2, missing = "Exclude cases with missing data")$chisq, 3059.91481376238)


# Comparisons with results from Applied Multivariate Statistics for the Social Sciences

table.11.2.data <- c(1,0.467,0.681,0.447,0.61,0.236,0.401,0.214,-0.062,0.227,0.238,0.189,0.401,0.075,0.314,0.167,0.148,0.099,
                     0.467,1,0.6,0.585,0.466,0.324,0.346,0.179,0.105,0.465,0.392,0.146,0.374,0.4,0.59,0.337,0.203,0.061,
                     0.681,0.6,1,0.643,0.673,0.339,0.344,0.242,-0.001,0.295,0.367,0.227,0.479,0.14,0.451,0.239,-0.028,-0.069,
                     0.447,0.585,0.643,1,0.612,0.357,0.081,0.003,-0.13,0.33,0.178,0.159,0.296,0.289,0.457,0.336,0.236,-0.158,
                     0.61,0.466,0.673,0.612,1,0.077,0.056,-0.029,-0.352,0.004,0.023,0.117,0.154,-0.027,0.192,0.011,0.037,-0.097,
                     0.236,0.324,0.339,0.357,0.077,1,0.518,0.517,0.619,0.698,0.542,0.336,0.676,0.513,0.671,0.463,0.051,-0.038,
                     0.401,0.346,0.344,0.081,0.056,0.518,1,0.632,0.476,0.502,0.381,0.38,0.567,0.369,0.5,0.217,-0.155,0.275,
                     0.214,0.179,0.242,0.003,-0.029,0.517,0.632,1,0.544,0.517,0.367,0.384,0.589,0.28,0.442,0.182,-0.3,0.159,
                     -0.062,0.105,-0.001,-0.13,-0.352,0.619,0.476,0.544,1,0.575,0.697,0.084,0.633,0.464,0.456,0.41,-0.043,0.215,
                     0.227,0.465,0.295,0.33,0.004,0.698,0.502,0.517,0.575,1,0.501,0.192,0.588,0.72,0.716,0.502,0.218,0.032,
                     0.238,0.392,0.367,0.178,0.023,0.542,0.381,0.367,0.697,0.501,1,-0.001,0.61,0.359,0.46,0.397,0.079,0.091,
                     0.189,0.146,0.227,0.159,0.117,0.336,0.38,0.384,0.084,0.192,-0.001,1,0.307,0.175,0.333,-0.06,-0.149,0.139,
                     0.401,0.374,0.479,0.296,0.154,0.676,0.567,0.589,0.633,0.588,0.61,0.307,1,0.465,0.616,0.393,-0.12,0.071,
                     0.075,0.4,0.14,0.289,-0.027,0.513,0.369,0.28,0.464,0.72,0.359,0.175,0.465,1,0.688,0.519,0.444,0.033,
                     0.314,0.59,0.451,0.457,0.192,0.671,0.5,0.442,0.456,0.716,0.46,0.333,0.616,0.688,1,0.466,0.199,-0.031,
                     0.167,0.337,0.239,0.336,0.011,0.463,0.217,0.182,0.41,0.502,0.397,-0.06,0.393,0.519,0.466,1,0.276,-0.145,
                     0.148,0.203,-0.028,0.236,0.037,0.051,-0.155,-0.3,-0.043,0.218,0.079,-0.149,-0.12,0.444,0.199,0.276,1,-0.344,
                     0.099,0.061,-0.069,-0.158,-0.097,-0.038,0.275,0.159,0.215,0.032,0.091,0.139,0.071,0.033,-0.031,-0.145,-0.344,1)

table.11.2.data <- matrix(table.11.2.data, nrow = 18)
names <- c("DOM", "CAPSTAT", "SOCIAL", "SOCRPRES", "SELFACP", "WELLBEG", "RESPON", "SOCLIZ",
              "SELFCTRL", "TOLER", "GOODIMP", "COMMUNAL", "ACHCONF", "ACHINDEP", "INTELEFF",
              "PSYMIND", "FLEX", "FEMIN")
rownames(table.11.2.data) <- names
colnames(table.11.2.data) <- names

# Unrotated PCA with 3 compnents
mf <- psych::principal(table.11.2.data, nfactors = 3, rotate = "none")
# Loading for DOM in component 1
testthat::expect_equal(round(mf$loadings[1,1], 5), 0.50137)
# Loading for FEMIN in component 3
testthat::expect_equal(round(mf$loadings[18,3], 5), 0.49437)

# PCA with 3 components varimax rotation
# These results are close to those in the text, but they do not match exactly.
# The authors don't provide much info about their varimax rotation
mf2 <- psych::principal(table.11.2.data, nfactors = 3, rotate = "varimax")
# Loading for TOLER in component 1
testthat::expect_equal(mf2$loadings[10,1], 0.85516, 2e-5)
# Loading for FEMIN in component 3
testthat::expect_equal(mf2$loadings[18,3], 0.56029, 2e-5)


# Testing imputation function
library(MASS)
set.seed(333)
# Specify a covariance matrix
my.cov <- matrix(c(1,    0.9,  0.6,  0.01, 0.02, 0.05,
                   0.9,  1  ,  0.8,  0.06, 0.03, 0.01,
                   0.6,  0.8,  1  ,  0.09, 0.01, 0.02,
                   0.01, 0.06, 0.09, 1   , 0.75, 0.69,
                   0.02, 0.03, 0.01, 0.75, 1   , 0.8,
                   0.05, 0.01,  0.02, 0.69, 0.8,  1), nrow = 6)

# Generate some data
n.rows <- 1000
initial.data <- mvrnorm(n = n.rows, mu = rep(0,6), Sigma = my.cov)

# Create some missing data completely at random
set.seed(333)
missing.indicator <- matrix(rbinom(n = n.rows * ncol(my.cov), size = 1, prob = 0.1) == 1, ncol = ncol(my.cov))

mcar.data <- initial.data
mcar.data[missing.indicator] <- NaN
imputed.data <- flipImputation:::Imputation(mcar.data, m = 1)[[1]]

# max(abs(cor(initial.data) - cor(mcar.data, use = "pairwise.complete.obs")))
# max(abs(cor(initial.data) - cor(imputed.data)))

norm(my.cov - cor(mcar.data, use = "pairwise.complete.obs"), type = "F")
norm(my.cov - cor(imputed.data), type = "F")


# Create some data which is missing not completely at random
set.seed(333)
nmar.data <- initial.data
probs <- initial.data[,1] + abs(min(initial.data[,1]))
probs <- probs/max(probs)
for (j in 1L:nrow(initial.data))
{
    draw <- rbinom(n = ncol(my.cov), size = 1, prob = probs[j]/2)
    nmar.data[j, which(draw == 1)] <- NaN
}

imputed.data <- flipImputation::Imputation(nmar.data)[[1]]
norm(my.cov - cor(nmar.data, use = "pairwise.complete.obs"), type = "F")
norm(my.cov - cor(imputed.data), type = "F")


nmar.data.2 <- initial.data
nmar.data.2[nmar.data.2 > 0.9] <- NaN
imputed.data <- flipImputation::Imputation(nmar.data.2)[[1]]
norm(my.cov - cor(nmar.data.2, use = "pairwise.complete.obs"), type = "F")
norm(my.cov - cor(imputed.data), type = "F")

})
