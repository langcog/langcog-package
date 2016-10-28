#' Using ggplot2 to plot matrix of correlations
#' 
#' Lightly modified from
#' \href{https://groups.google.com/forum/#!searchin/ggplot2/ggcorplot/ggplot2/odaZVAyKvE4/lsFIE86pUVoJ}{a
#' post by Mike Lawrence}.
#' 
#' \code{ggcorplot} takes a data frame or matrix, strips numeric variables, and 
#' makes a "splom" style correlation plot, with significance values. Useful for 
#' exploratory data analysis.
#' 
#' @param data Data frame or matrix to be plotted.
#' @param var_text_size Variable text size.
#'   
#' @keywords plotting
#' @export
#' @examples
#' ## using iris data
#' ggcorplot(iris)
#' 
#' @seealso \code{\link[ggplot2]{qplot}}
ggcorplot <- function(data,
                      var_text_size = 5) {
  
  # munge data ----------------------------
  
  # drop non-numeric columns
  nums <- sapply(data, is.numeric)
  data <- data[,nums]
  
  # reshape
  data <- as.data.frame(scale(data)) # scale
  
  # obtain new data frame
  # this should be easy to make functional (but I haven't figured it out)
  z <- data.frame()
  i <- 1
  j <- i
  while (i <= length(data)) {
    if (j > length(data)) {
      i <- i + 1
      j <- i
    } else {
      x <- data[,i]
      y <- data[,j]
      temp <- as.data.frame(cbind(x, y))
      temp <- cbind(temp, names(data)[i], names(data)[j])
      z <- rbind(z, temp)
      j <- j + 1
    }
  }
  
  names(z) <- c("x", "y", "x_lab", "y_lab")
  z$x_lab <- ezLev(factor(z$x_lab), names(data))
  z$y_lab <- ezLev(factor(z$y_lab), names(data))
  z <- z[z$x_lab != z$y_lab,]
  
  # obtain correlation values
  z_cor <- data.frame()
  i <- 1
  j <- i
  while (i <= length(data)) {
    if (j > length(data)) {
      i <- i + 1
      j <- i
    } else {
      x <- data[,i]
      y <- data[,j]
      x_mid <- min(x) + diff(range(x)) / 2
      y_mid <- min(y) + diff(range(y)) / 2
      this_cor <- stats::cor(x,y)
      this_cor.test <- 0
      this_cor.test <- stats::cor.test(x, y)
      this_col <- ifelse(this_cor.test$p.value < .05, "<.05", ">.05")
      this_size <- (this_cor) ^ 2
      cor_text <- ifelse(this_cor > 0,
                         substr(format(c(this_cor, .123456789),
                                       digits = 2)[1], 2, 4),
                         paste("-", substr(format(c(this_cor, .123456789),
                                                  digits = 2)[1], 3, 5),
                               sep = "")
      )
      b <- as.data.frame(cor_text)
      b <- cbind(b, x_mid, y_mid, this_col, this_size, names(data)[j],
                 names(data)[i])
      z_cor <- rbind(z_cor, b)
      j <- j + 1
    }
  }
  names(z_cor) <- c("cor", "x_mid", "y_mid", "p", "rsq", "x_lab", "y_lab")
  z_cor$x_lab <- ezLev(factor(z_cor$x_lab), names(data))
  z_cor$y_lab <- ezLev(factor(z_cor$y_lab), names(data))
  diag <- z_cor[z_cor$x_lab == z_cor$y_lab,]
  z_cor <- z_cor[z_cor$x_lab != z_cor$y_lab,]
  
  # ggplot 2 layers ----------------------------
  # used to be jitter, but this obscures data structure if used naively
  points_layer <- ggplot2::geom_point(mapping = ggplot2::aes(x = x, y = y),
                                      data = z)
  
  lm_line_layer <- ggplot2::geom_smooth(mapping = ggplot2::aes(x = x, y = y),
                                        data = z,
                                        stat = "smooth",
                                        method = "lm",
                                        colour = "red")
  
  lm_ribbon_layer <- ggplot2::geom_ribbon(mapping = ggplot2::aes(x = x, y = y),
                                          data = z,
                                          stat = "smooth",
                                          method = "lm",
                                          fill = "green",
                                          alpha = .5)
  
  cor_text <- ggplot2::geom_text(mapping = ggplot2::aes_string(x = "y_mid",
                                                               y = "x_mid",
                                                               label = "cor",
                                                               size = "rsq",
                                                               colour = "p"),
                                 data = z_cor)
  
  var_text <- ggplot2::geom_text(mapping = ggplot2::aes_string(x = "y_mid",
                                                               y = "x_mid",
                                                               label = "x_lab"),
                                 data = diag,
                                 size = var_text_size)
  
  ggplot2::ggplot() +
    points_layer +
    lm_ribbon_layer + lm_line_layer +
    var_text + cor_text +
    ggplot2::facet_grid(y_lab ~ x_lab, scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   legend.position = "none") +
    ggplot2::scale_size(limits = c(0, 1))
}

#' Helper function for levels, from the ez package
#' 
#' @param x factor
#' @param new_order The order you want them in
#' 
#' @keywords factors
#' @export
#' @examples
#' x <- factor(c("A", "B", "C"))
#' ezLev(x, c(3,1,2))
ezLev <- function(x, new_order){
  for (i in rev(new_order)) {
    x <- stats::relevel(x, ref = i)
  }
  return(x)
}
