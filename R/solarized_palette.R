#' solarized colour palette
#'
#' Provides a colour scheme based on the
#' \href{http://ethanschoonover.com/solarized}{solarized} accent colours.
#'
#' @param num_values Number of values to colour. If more than 9, colours start
#'   to repeat.
#'
#' @examples
#' if (requireNamespace("scales", quietly = TRUE)) {
#'   library(scales)
#'   show_col(solarized_palette(9))
#' }
#' @export
solarized_palette <- function(num_values) {
  
  solarized_colors <- c(magenta = "#d33682",
                        red = "#dc322f",
                        orange = "#cb4b16",
                        yellow = "#b58900",
                        green = "#859900",
                        cyan = "#2aa198",
                        blue = "#268bd2",
                        violet = "#6c71c4",
                        purple = "#993399")
  
  color_order <- c("blue", "orange", "green", "purple", "magenta",
                   "yellow", "cyan", "violet", "red")
  
  num_colors <- length(color_order)
  num_color_values <- num_values %% num_colors
  palette <- c(rep(solarized_colors, num_values %/% num_colors),
               solarized_colors[Filter(
                 function(color) color %in% color_order[0:num_color_values],
                 names(solarized_colors)
               )])
  names(palette) <- NULL
  return(palette)
  
}


#' solarized colour palette for ggplot2
#'
#' Provides a colour scheme based on the
#' \href{http://ethanschoonover.com/solarized}{solarized} accent colours for use
#' with ggplot2.
#'
#' @param ... Arguments passed on to discrete_scale to control name, limits,
#'   breaks, labels and so forth.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = mpg, y = hp, colour = factor(cyl))) +
#'   geom_point() +
#'   scale_colour_solarized()
#' @export
scale_colour_solarized <- function(...) {
  ggplot2::discrete_scale("colour", "solarized", solarized_palette, ...)
}

#' @rdname scale_colour_solarized
scale_color_solarized <- function(...) {
  scale_colour_solarized(...)
}

#' @rdname scale_colour_solarized
scale_fill_solarized <- function(...) {
  ggplot2::discrete_scale("fill", "solarized", solarized_palette, ...)
}

#' ggplot2 theme
#' 
#' Has the same theme options as \code{\link[ggplot2]{theme_bw}}, except for different
#' defaults for \code{base_size} and \code{base_family}, and no minor gridlines.
#' 
#' @param base_size base font size
#' @param base_family base font family
#'   
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(x = mpg, y = hp, colour = factor(cyl))) +
#'   geom_point() +
#'   theme_mikabr()
#'   }
#' @export
theme_mikabr <- function(base_size = 14, base_family = "Open Sans") {
  ggplot2::`%+replace%`(
    ggplot2::theme_bw(base_size = base_size, base_family = base_family),
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  )
}
