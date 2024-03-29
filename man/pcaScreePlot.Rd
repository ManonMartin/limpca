% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcaScreePlot.R
\name{pcaScreePlot}
\alias{pcaScreePlot}
\title{Scree Plot}
\usage{
pcaScreePlot(
  resPcaBySvd,
  nPC = 5,
  title = "PCA scree plot",
  theme = theme_bw()
)
}
\arguments{
\item{resPcaBySvd}{A list corresponding to the output value of \code{\link{pcaBySvd}}.}

\item{nPC}{An integer with the number of Principal Components to plot.}

\item{title}{Plot title.}

\item{theme}{\code{ggplot2} theme, see \code{?ggtheme} for more info.}
}
\value{
A \code{ggplot2} PCA scree plot
}
\description{
Returns a bar plot of the percentage of variance explained by each Principal Component calculated by \code{\link{pcaBySvd}}.
}
\examples{

data("UCH")
resPCA <- pcaBySvd(UCH$outcomes)
pcaScreePlot(resPCA, nPC = 4)

}
