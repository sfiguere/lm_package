test_that("print methods run with no errors", {
  model = lm_function(mpg ~ wt, data = mtcars)
  expect_output(print(model), "Coefficients")
  expect_output(print(summary(model)), "Residuals")
  expect_output(print(summary(model)), "Residual standard error")
  expect_output(print(summary(model)), "Multiple R-squared")
  expect_output(print(summary(model)), "F-statistic")
})
