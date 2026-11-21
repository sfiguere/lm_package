test_that("print methods run with no errors", {
  model = lm_function(mpg ~ wt, data = mtcars)
  expect_output(print(model), "Coefficients")
  expect_output(print(summary(model)), "Residuals")
  expect_output(print(summary(model)), "Residual standard error")
  expect_output(print(summary(model)), "Multiple R-squared")
  expect_output(print(summary(model)), "F-statistic")
})

test_that("coefficients match lm()", {
  model = lm_function(mpg ~ wt, data = mtcars)
  base_model = lm(mpg ~ wt, data = mtcars)
  expect_equal(model$coefficients, coef(base_model))
})

test_that("summary table matches lm()", {
  summary_model = summary(lm_function(mpg ~ wt, data = mtcars))
  summary_base_model = summary(lm(mpg ~ wt, data = mtcars))
  expect_equal(summary_model$coefficients, coef(summary_base_model),
               ignore_attr = TRUE)
  expect_equal(summary_model$r.squared,
               summary_base_model$r.squared)
  expect_equal(unname(summary_model$fstatistic),
               unname(summary_base_model$fstatistic))
})

test_that("works with multiple predictors and factors", {
  model = lm_function(mpg ~ wt + hp + factor(am), data = mtcars)
  base_model = lm(mpg ~ wt + hp + factor(am), data = mtcars)
  expect_equal(model$coefficients, coef(base_model))
})

test_that("perfectly collinear predictors throw an error and exits", {
  mtcars$kpl = mtcars$mpg * 0.425144
  expect_error(lm_function(wt ~ mpg + kpl, data = mtcars))
})

test_that("perfectly collinear predictors throw an error and exits", {
  expect_error(lm_function(mpg ~ wt + I(wt * 2), data = mtcars),
               "collinear")
})



