# PCA <- function(PCA)
#
#
#
#
#
#
#
#
#
#
#
#
#
# # Load the base dataset attitude to work with.
# require(psych)
# require(GPArotation)
# require(Matrix)
# psych::principal(attitude,2, rotate = "none")
#
# data(attitude)
#
#
#
# # Compute eigenvalues and eigen vectors of the correlation matrix.
# pfa.eigen<-eigen(cor(attitude))
# # Print and note that eigen values are those produced by SPSS.
# # Also note that SPSS will extract 2 components as eigen values > 1 = 2
# pfa.eigen$values
# # set a value for the number of factors (for clarity)
# factors<-2
# # Extract and transform two components.
# pfa.eigen$vectors [ , 1:factors ]  %*%  diag ( sqrt (pfa.eigen$values [ 1:factors ] ),factors,factors )
#
#
#
# cur_svd <- svd(scale(attitude))
# cur_svd.TrimVectors(tol);  //checking to see if redundant components need removing if tol > 0 (NB: by default, tol probably will be 0)
# d <- cur_svd$d
# Eigenvalues <- d * d;
# nComponents <- length(d)
# svd(attitude)
# Eigenvalues = Eigenvalues / sum(Eigenvalues) * nComponents
# StandardDeviations = sqrt(Eigenvalues)
# Coefficients = cur_svd$v
# MakePredominantlyPositive(ref Coefficients)
# Loadings <- sweep(Coefficients, 2, StandardDeviations, "*")
# Loadings
# Coefficients = sweep(Coefficients, 2, StandardDeviations, "Operation.Divide"/"") #modifying coefficients so that the factors have a st deviation of 1
# Coefficients
#
#
#
# selectedComponents = new bool[nComponentsInitial];
#   for (var i = 0; i < nComponentsInitial; i++)
#     selectedComponents[i] = true;
# }
#
#
# svd(attitude)
