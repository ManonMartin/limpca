\name{LMWiRe}
\alias{LMWiRe}
\title{Linear Models for Wide Responses}

\description{
This package has for objectives to provide a method to make Linear Models for Wide Responses data. This method handles unbalanced design. At this development stage, the package can only perform one or two-way ANOVA of class I. More possibilities should be included in the future. The principal functions of the package are:

\describe{
  \item{\code{\link{lmwModelMatrix}}}{Creates a model matrix from the design and the model formula using sum coding}
  \item{\code{\link{lmwEffectMatrices}}}{Estimates the GLM and derives the effect matrix for each model effect}
  \item{\code{\link{lmwBootstrapTests}}}{Tests the significance of the effects using a parametric bootstrap}
  \item{\code{\link{lmwPcaEffects}}}{Performs a PCA on the effect matrices and adapts the results according to the method chosen between ASCA, APCA or ASCA-E}
}



The functions allowing the visualisation of the results are:
\describe{
  \item{\code{\link{lmwLoading1dPlot}} or \code{\link{lmwLoading2dPlot}}}{Plots the loadings as a line plot (1D) or in 2D as a scatterplot}
  \item{\code{\link{lmwScorePlot}}}{Plots scores of an effect for two components at a time}
  \item{\code{\link{lmwScoreScatterPlotM}}}{Plots the scores of several effects simultaneously through a scores plot matrix}
  \item{\code{\link{lmwEffectPlot}}}{Plots the ASCA scores of an effect for one Principal Component at a time and allows the scores to be decomposed by the levels of the factors found in the effect being studied.}
  \item{\code{\link{lmwContributions}}}{Produces plots and tables listing the contribution of the different effects to the total variance as well as the contribution of the Principal Components calculated on each model matrix}
  \item{\code{\link{lmwScreePlot}}}{Produces bar plots of the variance percentage explained by each Principal Component fo a given effect}
}
}

\details{
\tabular{ll}{
Package: \tab LMWiRe\cr
Type: \tab Package\cr
License: \tab GPL-2\cr
}
See the vignette for an example.
}

\author{
Sébastien Franceschini
}

\references{
Rousseau, R. (2011). \emph{Statistical contribution to the analysis of metabonomics data in 1H NMR spectroscopy}
  (Doctoral dissertation, PhD thesis. Institut de statistique, biostatistique et sciences actuarielles, Université catholique de Louvain, Belgium).


Thiel M.,Feraud B. and Govaerts B. (2017) \emph{ASCA+ and APCA+: Extensions of ASCA and APCA
in the analysis of unbalanced multifactorial designs}, Journal of Chemometrics


Guisset S.,Martin M. and Govaerts B. (2019) \emph{Comparison of PARAFASCA, AComDim, and AMOPLS approaches in the multivariate GLM modelling of multi-factorial designs}, Chemometrics and Intelligent Laboratory Systems
}


