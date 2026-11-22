
# lmpackage

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/sfiguere/lm_package/graph/badge.svg)](https://app.codecov.io/gh/sfiguere/lm_package)
[![R-CMD-check](https://github.com/sfiguere/lm_package/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sfiguere/lm_package/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of lmpackage is to simulate the lm function in Base R. It leverages
ordinary least squares as an estimation method to fit a linear regression model 
by minimizing the sum of the squared vertical distances between the observed 
data points and the predicted data.

## Navigation

* [Installation](#installation)
* [Basic Usage](#basic-usage)
* [Example: mtcars](#example-mtcars)
* [Example: iris](#example-iris)
* [Getting Help](#getting-help)


## Installation

You can install the development version of lmpackage from [GitHub](https://github.com/sfiguere/lmpackage) with:

```r
# install.packages("remotes")
remotes::install_github("sfiguere/lm_package")
```

## Basic Usage

```r
library(lmpackage)

model = lm_function(y ~ x, data = my_data)

model             
summary(model)
model$coefficients
```

## Example: mtcars

```r
library(lmpackage)

lm_function(mpg ~ wt, data = mtcars)

# Call:
# lm_function(formula = mpg ~ wt, data = mtcars)

# Coefficients:
# (Intercept)           wt  
#      37.285       -5.344 

summary(lm_function(mpg ~ wt, data = mtcars))

# Call:
# lm_function(formula = mpg ~ wt, data = mtcars)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -4.5432 -2.3647 -0.1252  1.4096  6.8727 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  37.2851     1.8776  19.858  < 2e-16 ***
# wt           -5.3445     0.5591  -9.559 1.29e-10 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.046 on 30 degrees of freedom
# Multiple R-squared: 0.7528,	Adjusted R-squared:  0.7446 
# F-statistic: 91.38 on 1 and 30 DF, p-value: 1.294e-10
```

## Example: iris
```r
lm_function(Sepal.Length ~ Petal.Length, data = iris)

# Call:
# lm_function(formula = Sepal.Length ~ Petal.Length, data = iris)
# 
# Coefficients:
#  (Intercept) Petal.Length 
#      "4.307"      "0.409" 

summary(lm_function(Sepal.Length ~ Petal.Length, data = iris))

# Call:
# lm_function(formula = Sepal.Length ~ Petal.Length, data = iris)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.24675 -0.29657 -0.01515  0.27676  1.00269 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   4.30660    0.07839   54.94   <2e-16 ***
# Petal.Length  0.40892    0.01889   21.65   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4071 on 148 degrees of freedom
# Multiple R-squared: 0.76,	Adjusted R-squared:  0.7583 
# F-statistic: 468.55 on 1 and 148 DF, p-value: < 2.2e-16
```

## Getting Help
If you encounter a clear bug, please file an issue with a minimal reproducible example on [GitHub](https://github.com/sfiguere/lm_package/issues). For questions and other discussion, please use email sfiguere@umich.edu






