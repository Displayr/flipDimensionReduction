[![](https://travis-ci.org/Displayr/flipDimensionReduction.svg?branch=master)](https://travis-ci.org/Displayr/flipDimensionReduction/)
[![Coverage Status](https://coveralls.io/repos/github/Displayr/flipDimensionReduction/badge.svg?branch=master)](https://coveralls.io/github/Displayr/flipDimensionReduction?branch=master)
# flipDimensionReduction

Algorithms for dimension reduction

## Installation

To install from GitHub:
```
require(devtools)
install_github("Displayr/flipDimensionReduction", dependencies = NA)
```

If you have not set up a GitHub Personal Access Token, you will likely need to do so to avoid 
GitHub rate limits, which will manifest as 403 errors when downloading packages via
`install_github`. Please see the documentation in the `usethis` package or see the 
instructions [here](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token) and [here](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token).

If you are using Windows, you will need to have a version of Rtools installed that matches your
version of R in order to build packages from source. Rtools can be downloaded from
[here](https://cran.r-project.org/bin/windows/Rtools/).

Specifying `dependencies = NA` in `install_github` will not install packages listed
in `Suggests` in the `DESCRIPTION` file (some of which may be proprietary and unavailable for download).

## Submitting a bug report

If you encounter a problem using the package, please open an [issue](https://github.com/Displayr/flipDimensionReduction/issues). To achieve a resolution as quickly as possible, please include a minimal, reproducible example of the bug, along with the exact error message or output you receive and the behavior you expect. Including the output of `sessionInfo()` in R can be helpful to reproduce the issue. Please see this [FAQ](https://community.rstudio.com/t/faq-whats-a-reproducible-example-reprex-and-how-do-i-create-one/5219), which has a number of useful tips on creating great reproducible examples. 

[![Displayr logo](https://mwmclean.github.io/img/logo-header.png)](https://www.displayr.com)
