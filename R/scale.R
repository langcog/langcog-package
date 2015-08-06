#' Scale a matrix with no attributes
#' 
#' From \link[here]{http://r.789695.n4.nabble.com/How-to-remove-attributes-from-scale-in-a-matrix-td4671715.html}
#' 
#' \code{scale} is exactly the same as normal scale, except without the attributes 
#' that make \code{\link[base]{scale}} so terrible to work with in functional 
#' environments.
#' 
#' @keywords statistics
#' @export
#' @examples
#' scale(c(1,2,3,4,5))
#' 
#' @seealso \code{\link[base]{scale}}

scale <- function (x, center = TRUE, scale = TRUE) 
{
  x <- as.matrix(x)
  nc <- ncol(x)
  if (is.logical(center)) {
    if (center) {
      center <- colMeans(x, na.rm = TRUE)
      x <- sweep(x, 2L, center, check.margin = FALSE)
    }
  }
  else if (is.numeric(center) && (length(center) == nc)) 
    x <- sweep(x, 2L, center, check.margin = FALSE)
  else stop("length of 'center' must equal the number of columns of 'x'")
  if (is.logical(scale)) {
    if (scale) {
      f <- function(v) {
        v <- v[!is.na(v)]
        sqrt(sum(v^2)/max(1, length(v) - 1L))
      }
      scale <- apply(x, 2L, f)
      x <- sweep(x, 2L, scale, "/", check.margin = FALSE)
    }
  }
  else if (is.numeric(scale) && length(scale) == nc) 
    x <- sweep(x, 2L, scale, "/", check.margin = FALSE)
  else stop("length of 'scale' must equal the number of columns of 'x'")
  x
}