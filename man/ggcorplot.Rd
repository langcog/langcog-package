% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggcorplot.R
\name{ggcorplot}
\alias{ggcorplot}
\title{Using ggplot2 to plot matrix of correlations}
\usage{
ggcorplot(data, var_text_size = 5)
}
\arguments{
\item{data}{Data frame or matrix to be plotted.}

\item{var_text_size}{Variable text size.}
}
\description{
Lightly modified from
\href{https://groups.google.com/forum/#!searchin/ggplot2/ggcorplot/ggplot2/odaZVAyKvE4/lsFIE86pUVoJ}{a
post by Mike Lawrence}.
}
\details{
\code{ggcorplot} takes a data frame or matrix, strips numeric variables, and 
makes a "splom" style correlation plot, with significance values. Useful for 
exploratory data analysis.
}
\examples{
## using iris data
ggcorplot(iris)

}
\seealso{
\code{\link[ggplot2]{qplot}}
}
\keyword{plotting}
