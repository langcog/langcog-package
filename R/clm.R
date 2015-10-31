#' Fitting linear models under constraints
#'
#' \code{clm} (constrained linear model) is used to fit linear models under
#' constraints on the coefficients. It uses quadratic programming to run a
#' regression on data with a specified formula, subject to the constraint that
#' the coefficients of the regression sum to 1 (in the future could support
#' arbitrary constraints on the coefficients).
#'
#' @param formula An object of class "\code{\link[stats]{formula}}" (or one that
#'   can be coerced to that class): a symbolic description of the model to be
#'   fitted.
#' @param data A data frame (or object coercible by
#'   \code{\link[base]{as.data.frame}} to a data frame) containing the variables
#'   in the model.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return \code{clm} returns an object of \code{\link[base]{class}}
#'   "\code{clm}". An object of class "\code{clm}" is a list containing at least
#'   the following components:
#'   \tabular{ll}{
#'     \code{solution} \tab a vector of coefficients for the constrained
#'                          solution \cr
#'     \code{unconstrined.solution} \tab a vector of coefficients for the
#'                          unconstrained solution \cr
#'     \code{formula} \tab the formula passed \code{clm} \cr
#'   }
#'
#' @seealso \code{\link[stats]{lm}}, \code{\link[quadprog]{solve.QP}}
#'
#' @examples
#' ## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
#' ## Page 9: Plant Weight Data.
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' df <- data.frame(weight = c(ctl, trt), group = c(rep(0, 10), rep(1, 10)))
#' lm.D9 <- clm(weight ~ group, df)
clm <- function(formula, data, ...) {
  M <- stats::model.frame(stats::as.formula(formula), as.data.frame(data))
  y <- M[,1]
  X <- as.matrix(M[,-1])
  s <- quadprog::solve.QP(t(X) %*% X, t(y) %*% X,
                          matrix(1, nrow = ncol(X), ncol = 1), 1, meq = 1)
  class(s) <- "clm"
  s$formula <- formula
  return(s)
}

#' Predict method for constrained linear model fits
#'
#' Predicted values based on constrained linear model object.
#'
#' @param object Object of class inheriting from "\code{clm}".
#' @param newdata A data frame in which to look for variables with which to
#'   predict.
#' @param ... Further arguments passed to or from other methods.
## S3 method for class 'clm'
predict.clm <- function(object, newdata, ...) {
  M <- as.matrix(stats::model.frame(object$formula[-2], newdata))
  s <- object$solution
  p <- (M %*% s)
  rownames(p) <- NULL
  p[,1]
}

#' Prediction data frame for constrained linear model fits
#'
#' Get predicted values based on constrained linear model object into data
#' frame. This method is called by \code{\link[ggplot2]{stat_smooth}}.
#'
#' @param model Object of class inheriting from "\code{clm}".
#' @param xseq See \code{\link[ggplot2]{stat_smooth}}.
#' @param se See \code{\link[ggplot2]{stat_smooth}}.
#' @param level See \code{\link[ggplot2]{stat_smooth}}.
## S3 predictdf method for 'clm'
predictdf.clm <- function(model, xseq, se, level) {
  pred <- stats::predict(model, newdata = data.frame(x = xseq))
  data.frame(x = xseq, y = as.vector(pred))
}
