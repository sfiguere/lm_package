
# lm_package

<!-- badges: start -->
<!-- badges: end -->

The goal of lm_package is to simulate the lm function in Base R. It leverages
ordinary least squares as an estimation method to fit a linear regression model 
by minimizing the sum of the squared vertical distances between the observed 
data points and the predicted data.

## Installation

You can install the development version of lm_package from [GitHub](https://github.com/sfiguere/lm_package) with:

``` r
# install.packages("pak")
pak::pak("sfiguere/lm_package")
```

## Example

```r
library(lm_package)

lm_function(mpg ~ wt, data = mtcars)
#> (Intercept)          wt 
#>   37.285126   -5.344472
```

## Getting Help
If you encounter a clear bug, please file an issue with a minimal reproducible example on [GitHub](https://github.com/sfiguere/lm_package/issues). For questions and other discussion, please use email sfiguere@umich.edu






