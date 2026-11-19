#' Fit a linear regression model using a formula and data set
#'
#' @param call_formula a formula
#' @param data a data frame containing the variables in the formula
#'
#' @return a named numeric vector of estimated coefficients
#'
#' @examples
#' lm_function(mpg ~ wt, mtcars)
#'
#' @export
lm_function = function(call_formula, data) {
  call = match.call()

  mf = stats::model.frame(formula = call_formula, data = data)
  # allow model.frame to update it
  mt = attr(mf, "terms")

  x_data = stats::model.matrix(mt, mf)
  y_data = stats::model.response(mf, "numeric")

  x_data_transpose = t(x_data)
  predictor_mat = x_data_transpose %*% x_data

  # calculate how predictors move with outcome
  outcome_mat = x_data_transpose %*% y_data

  coefs = drop(solve(predictor_mat, outcome_mat))

  obj = list(call = call, coefficients = coefs)
  class(obj) = "lm_sfiguere"

  return(obj)
}

print.lm_sfiguere <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n",
      sep = "")

  cat("Coefficients:\n")
  print.default(format(x$coefficients, digits = 3, quote = FALSE))
  cat("\n")

  invisible(x)
}
