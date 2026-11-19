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
lm_function = function(call_formula, data)
{
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

  # additions to object for summary method
  fitted = drop(x_data %*% coefs)
  resids = y_data - fitted

  obj = list(
    call = call,
    coefficients = coefs,
    residuals = resids
    )
  class(obj) = "lm_sfiguere"

  return(obj)
}

#' Print an lm_sfiguere object
#'
#' @param x An object of class "lm_sfiguere"
#' @param digits Number of significant digits to show
#' @param ... Unused, for further arguments passed or from other methods
#' @return Invisibly returns the input object \code{x}.
#'
#' @examples
#' print(lm_function(mpg ~ wt, mtcars))
#'
#' @export
print.lm_sfiguere = function(x, digits = max(3L, getOption("digits") - 3L), ...)
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

#' Summarize an lm_sfiguere object
#'
#' @param object An object of class "lm_sfiguere"
#' @param ... Unused, for further arguments passed or from other methods
#' @return An object of class "summary.lm_sfiguere"
#' @export
summary.lm_sfiguere = function(object, ...) {
  est = object$coefficients
  res = object$residuals

  structure(
    list(call = object$call,
         residuals = res),
    class = "summary.lm_sfiguere"
  )
}

#' Print a summary of an lm_sfiguere object
#'
#' @param x An object of class "summary.lm_sfiguere"
#' @param digits Number of significant digits to show
#' @param ... Unused, for further arguments passed or from other methods
#' @return Invisibly returns the input object \code{x}.
#' @export
print.summary.lm_sfiguere = function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n",
      sep = "")

  cat("Residuals:\n")
  five_summary = c("Min", "1Q", "Median", "3Q", "Max")
  res  = structure(zapsmall(stats::quantile(x$residuals), digits + 1L),
                   names = five_summary)

  print(res, digits = digits)

  cat("Coefficients:\n")


}


