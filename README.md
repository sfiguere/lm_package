
# lmpackage

<!-- badges: start -->
<!-- badges: end -->

The goal of lmpackage is to simulate the lm function in Base R. It leverages
ordinary least squares as an estimation method to fit a linear regression model 
by minimizing the sum of the squared vertical distances between the observed 
data points and the predicted data.

## Installation

You can install the development version of lmpackage from [GitHub](https://github.com/sfiguere/lmpackage) with:

``` r
# install.packages("pak")
pak::pak("sfiguere/lmpackage")
```

## Example

```r
library(lmpackage)

lm_function(mpg ~ wt, data = mtcars)
#> (Intercept)          wt 
#>   37.285126   -5.344472
```

## Getting Help
If you encounter a clear bug, please file an issue with a minimal reproducible example on [GitHub](https://github.com/sfiguere/lmpackage/issues). For questions and other discussion, please use email sfiguere@umich.edu






