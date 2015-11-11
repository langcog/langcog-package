#' Standard error of the mean
#' 
#' Computes the standard error of the mean of the values in x. If \code{na.rm} 
#' is \code{TRUE} then missing values are removed before computation proceeds.
#' 
#' @param x A numeric vector
#' @param na.rm Logical indicating whether missing values should be removed.
#'   
#' @examples
#' x <- rnorm(100)
#' sem(x)
#' @export
sem <- function(x, na.rm = FALSE) {
  if (na.rm) {
    n <- sum(!is.na(x))
  } else {
    n <- length(x)
  }
  sd(x, na.rm = na.rm) / sqrt(n)
}

#' Confidence interval (lower)
#' 
#' Gives the lower bound of the 95\% confidence interval of an empirical
#' distribution.
#' 
#' @param x Numeric vector whose confidence interval is wanted.
#' @param na.rm Logical indicating whether NAs should be removed
#'   
#' @examples
#' x <- rnorm(1000)
#' ci_lower(x)
#' @export
ci_lower <- function(x, na.rm = FALSE) {
  quantile(x, 0.025, na.rm)
}

#' Confidence interval (upper)
#' 
#' Gives the upper bound of the 95\% confidence interval of an empirical
#' distribution.
#' 
#' @param x Numeric vector whose confidence interval is wanted.
#' @param na.rm Logical indicating whether NAs should be removed
#'   
#' @examples
#' x <- rnorm(1000)
#' ci_upper(x)
#' @export
ci_upper <- function(x, na.rm = FALSE) {
  quantile(x, 0.975, na.rm)
}

#' Significance stars
#' 
#' Gives the statistical significance level of a \emph{p}-value.
#' 
#' @param p A numeric \emph{p}-value from a hypothesis test.
#'   
#' @return A string representing the significance level correponding to this 
#'   \emph{p}-value ("\code{***}", "\code{**}", "\code{*}", "\code{.}", or "").
#'   
#' @examples
#' get_stars(0.03)
#' @export
get_stars <- function(p) {
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  if (p < 0.1) return(".")
  return("")
}
