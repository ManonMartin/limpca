% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotMeans.R
\name{plotMeans}
\alias{plotMeans}
\title{Means plot}
\usage{
plotMeans(
  Y = NULL,
  design = NULL,
  lmpDataList = NULL,
  cols = NULL,
  x,
  z = NULL,
  w = NULL,
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  color = NULL,
  shape = NULL,
  linetype = NULL,
  size = 2,
  hline = NULL,
  theme = theme_bw()
)
}
\arguments{
\item{Y}{A numerical matrix containing the columns to be drawn. Can be \code{NULL} if \code{lmpDataList} is defined.}

\item{design}{A \eqn{n \times k} "freely encoded" experimental design data.frame. Can be \code{NULL} if \code{lmpDataList} is defined.}

\item{lmpDataList}{If not \code{NULL}, a list with outcomes, design and formula, as outputted by \code{\link{data2LmpDataList}}.}

\item{cols}{A vector with either the column name(s) of the \eqn{Y} matrix to plot (character) or the column index position(s) (integer).}

\item{x}{A character string giving the \code{design} factor whose levels will form the x-axis.}

\item{z}{A character string giving the \code{design} factor whose levels will form the traces.}

\item{w}{A character string giving the \code{design} factor whose levels will be used for the facet.}

\item{title}{Plot title.}

\item{xlab}{If not \code{NULL}, the label for the x-axis.}

\item{ylab}{If not \code{NULL}, the label for the y-axis.}

\item{color}{If not \code{NULL}, the color of the points and the line.}

\item{shape}{If not \code{NULL}, the points shape.}

\item{linetype}{If not \code{NULL}, the line type.}

\item{size}{Points size.}

\item{hline}{If not \code{NULL}, draws (a) horizontal line(s).}

\item{theme}{The \code{ggplot2} theme, see \code{?ggtheme} for more info.}
}
\value{
A list of \code{ggplot2} means plot(s).
}
\description{
For a given response variable, draws a plot of the response means by levels of up to three categorical factors from the design. When the design is balanced, it allows to visualize main effects or interactions for the response of interest. For unbalanced designs, this plot must be used with caution.
}
\details{
Either \code{Y} or \code{lmpDataList} need to be defined. If both are given, the priority goes to \code{Y}.
The same rule applies for \code{design} or \code{lmpDataList}.
}
\examples{

data("UCH")
# 1 factor
plotMeans(
  Y = UCH$outcomes, design = UCH$design, cols = "4.0628702",
  x = "Hippurate", color = "blue"
)

# equivalent to:
plotMeans(
  lmpDataList = UCH, cols = "4.0628702",
  x = "Hippurate", color = "blue"
)

# 2 factors
plotMeans(
  Y = UCH$outcomes, design = UCH$design, cols = c(364, 365),
  x = "Hippurate", z = "Time", shape = c(15, 1)
)

# 3 factors
plotMeans(
  Y = UCH$outcomes, design = UCH$design, cols = c(364, 365),
  x = "Hippurate", z = "Time", w = "Citrate", linetype = c(3, 3)
)

}
