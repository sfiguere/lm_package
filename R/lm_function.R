#' Fit a linear regression model using a formula and data set
#'
#' @param formula a formula
#' @param data a data frame containing the variables in the formula. It can be
#'  used to carry out regression, and used for hypothesis testing.
#'
#' @details
#' Models for lm_function are specified symbolically. Normal usage has the form
#' response ~ terms where the response is the (numeric) response vector and the
#' terms is a series of variables which specifies a linear predictor for the
#' response. Specifically, the syntax of terms is in the form first + second +
#' ... + indicates all the terms in the first together with all the terms in the
#' second and so forth with duplicates removed. The specification first * second
#' indicates the cross of first and second used in interaction modification.
#' This notation is the same as first + second + first:second.
#' The function stops with an error if the predictors are perfectly collinear.
#'
#' @return lm_function returns an object of class "lm_sfiguere". The print
#' function returns the call made along with coefficients. The summary function
#' is used to print the results of the regression.
#'
#' \describe{
#'   \item{call}{the matched call formula supplied.}
#'   \item{coefficients}{a named vector of coefficients.}
#'   \item{residuals}{the residuals, that is response minus fitted values.}
#'   \item{fitted.values}{the fitted mean values.}
#'   \item{rank}{the numeric rank of the fitted linear model.}
#'   \item{x_obj}{Model matrix used.}
#'   \item{df.residual}{the residual degrees of freedom.}
#' }
#'
#' @author Samantha Figueredo Botello \email{sfiguere@@umich.edu}
#' @references
#' \url{https://github.com/wch/r-source/blob/trunk/src/library/stats/R/lm.R}
#'
#' @examples
#' lm_function(mpg ~ wt, mtcars)
#'
#' @importFrom stats model.frame model.matrix model.response pt pf
#' @export
lm_function = function(formula, data)
{
  # check for proper usage of function
  usage = "Usage: lm_function(y ~ x, data = my_data_frame)"
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame containing the variables in the formula.\n",
         usage, call. = FALSE)
  }

  mf = model.frame(formula, data)

  x_data = model.matrix(formula, data)
  y_data = model.response(mf)

  # check for perfectly collinear predictors before solving
  rank = qr(x_data)$rank
  if (rank < ncol(x_data)) {
    stop("Predictors are perfectly collinear.
         The model cannot be fit. Exiting...",
         call. = FALSE)
  }

  x_data_transpose = t(x_data)
  predictor_mat = x_data_transpose %*% x_data

  # calculate how predictors move with outcome
  outcome_mat = x_data_transpose %*% y_data

  coefs = drop(solve(predictor_mat, outcome_mat))

  # additions to object for summary method
  fitted = drop(x_data %*% coefs)
  resids = y_data - fitted

  data_name = deparse(substitute(data))
  call = paste0("lm_function(formula = ",
                deparse(formula), ", data = ", data_name, ")")


  obj = list(
    call = call,
    coefficients = coefs,
    residuals = resids,
    fitted.values = fitted,
    rank = rank,
    x_obj = x_data,
    df.residual = nrow(x_data) - rank
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
  cat("\nCall:\n")
  cat(x$call)
  cat("\n\n")
  cat("Coefficients:\n")
  print.default(format(x$coefficients, digits = digits),
                print.gap = 2, quote = FALSE)
  cat("\n")
  invisible(x)

}

#' Summarize an lm_sfiguere object
#'
#' @param object An object of class "lm_sfiguere"
#' @param ... Unused, for further arguments passed or from other methods
#' @return An object of class "summary.lm_sfiguere"
#'
#' @examples
#' model = lm_function(mpg ~ wt, data = mtcars)
#' summary(model)
#'
#' @export
summary.lm_sfiguere = function(object, ...) {

  call = object$call
  est = object$coefficients
  res = object$residuals
  x_data = object$x_obj
  df = object$df.residual

  # build table of coefficients
  rss = sum(res^2)
  resvar = rss/df
  se = sqrt(diag(resvar * solve(t(x_data) %*% x_data)))
  tval = est/se
  pval = 2*pt(abs(tval), df, lower.tail = FALSE)

  coef_table = cbind(
    `Estimate` = est,
    `Std. Error` = se,
    `t value` = tval,
    `Pr(>|t|)` = pval
    )

  # Format residual standard error, Multiple R-squared, and F-statistic section
  sigma = sqrt(resvar)
  mss = sum((object$fitted.values - mean(object$fitted.values))^2)
  r_squared = mss / (mss + rss)
  rank = qr(x_data)$rank
  p = object$rank
  f_statistic = (mss / (p - 1)) / resvar
  n = length(res)

  structure(
    list(
      call = object$call,
      residuals = res,
      coefficients  = coef_table,
      sigma = sqrt(resvar),
      df = df,
      r.squared = r_squared,
      adj.r.squared = 1 - (1 - r_squared) * (n - 1) / df,
      fstatistic = c(value = f_statistic, numdf = p - 1, dendf = df)
    ),
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
  # call section
  cat("\nCall:\n")
  cat(x$call)
  cat("\n")
  cat("\n")

  # residuals with five number summary
  cat("Residuals:\n")
  five_summary = c("Min", "1Q", "Median", "3Q", "Max")
  res = structure(zapsmall(stats::quantile(x$residuals), digits + 1L),
                   names = five_summary)
  print(res, digits = digits)

  # coefficient table with significance codes
  cat("\nCoefficients:\n")
  stats::printCoefmat(x$coefficients, digits = digits)

  # Goodness of fit section
  cat("\nResidual standard error:",
      format(signif(x$sigma, digits)), "on", x$df, "degrees of freedom")
  cat("\n")
  cat(paste0("Multiple R-squared: ", round(x$r.squared, digits)))
  cat(",\tAdjusted R-squared: ", round(x$adj.r.squared, digits), "\n")
  # calculate the upper-tail cumulative distribution function for F-stat pval
  pval = pf(x$fstatistic[1L],
            x$fstatistic[2L],
            x$fstatistic[3L],
            lower.tail = FALSE)

  cat(sprintf("F-statistic: %s on %s and %s DF, p-value: %s\n",
               round(x$fstatistic[1L], 2),
               x$fstatistic[2L],
               x$fstatistic[3L],
               format.pval(pval, digits = digits)
               ))
  cat("\n")
  invisible(x)
}


