\name{limpca}
\alias{limpca}
\title{Linear modeling of high-dimensional designed data based on ASCA/APCA family of methods}

\description{
This package has for objectives to provide a method to make Linear Models for high-dimensional designed data. This method handles unbalanced design. More features should be included in the future (e.g. generalized linear models, random effects, ...).


\strong{The core functions of the package are:}

\describe{
  \item{\code{\link{lmpModelMatrix}}}{Creates the model matrix \eqn{\mathbf{X}} from the design matrix and the model formula.}
  \item{\code{\link{lmpEffectMatrices}}}{Estimates the model by OLS based on the outcomes and model matrices provided in the outputs of the \code{\link{lmpModelMatrix}} function and calculates the estimated effect matrices \eqn{\hat{\mathbf{M}}_0, \hat{\mathbf{M}}_1, ...\hat{\mathbf{M}}_F} and residual matrix \eqn{\hat{\mathbf{E}}}. It calculates also the type III percentage of variance explained by each effect.}
  \item{\code{\link{lmpBootstrapTests}}}{Tests the significance of one or a combination of the model effects using bootstrap. This function is based on the outputs of the \code{\link{lmpEffectMatrices}} function.}
  \item{\code{\link{lmpPcaEffects}}}{Performs a PCA on each of the effect matrices from the outputs of \code{\link{lmpEffectMatrices}}. It has an option to choose the method applied: ASCA, APCA or ASCA-E. Combined effects (i.e. linear combinations of original effect matrices) can also be created and decomposed by PCA.}
}

\strong{The functions allowing the visualisation of the Linear Models results are:}

\describe{
  \item{\code{\link{lmpScreePlot}}}{Provides a barplot of the percentage of variance associated to the PCs of the effect matrices ordered by importance based on the outputs of \code{\link{lmpContributions}}.}
  \item{\code{\link{lmpContributions}}}{This reports the contribution of each effect to the total variance, but also the contribution of each PC to the total variance per effect. Moreover, these contributions are summarized in a barplot.}
  \item{\code{\link{lmpScorePlot}}}{Draws the score plots of each effect matrix provided in the \code{\link{lmpPcaEffects}} function output.}
  \item{\code{\link{lmpLoading1dPlot}} or \code{\link{lmpLoading2dPlot}}}{Plots the loadings as a line plot (1D) or in 2D as a scatterplot.}
  \item{\code{\link{lmpScoreScatterPlotM}}}{Plots the scores of all model effects simultaneously in a scatterplot matrix. By default, the first PC only is kept for each model effect.}
  \item{\code{\link{lmpEffectPlot}}}{Plots the ASCA scores by effect levels for a given model effect and for one PC at a time. This graph is especially appealing to interpret interactions or combined effects.}
}

\strong{Other useful functions to visualise and explore by PCA the multivariate data are:}

\describe{
  \item{\code{\link{plotDesign}}}{Provides a graphical representation of the experimental design. It allows to visualize factor levels and check the design balance.}
  \item{\code{\link{plotScatter}}}{Produces a plot describing the relationship between two columns of the outcomes matrix \eqn{\mathbf{Y}}. It allows to choose colors and symbols for the levels of the design factors. Ellipses, polygons or segments can be added to group different sets of points on the graph.}
  \item{\code{\link{plotScatterM}}}{Produces a scatter plot matrix between the selected columns of the outcomes matrix \eqn{\mathbf{Y}} choosing specific colors and symbols for up to four factors from the design on the upper and lower diagonals.}
  \item{\code{\link{plotMeans}}}{Draws, for a given response variable, a plot of the response means by levels of up to three categorical factors from the design. When the design is balanced, it allows to visualize main effects or interactions for the response of interest. For unbalanced designs, this plot must be used with caution.}
  \item{\code{\link{plotLine}}}{Generates the response profile of one or more observations i.e. plots of one or more rows of the outcomes matrix on the y-axis against the m response variables on the x-axis. Depending on the response type (spectra, gene expression...), point, line or segment plots can be used.}
  \item{\code{\link{pcaBySvd}}}{Operates a principal component analysis on the \eqn{\mathbf{Y}} outcome/response matrix by a singular value decomposition. Outputs are can be visulised with the functions \code{\link{pcaScorePlot}}, \code{\link{pcaLoadingPlot}} and \code{\link{pcaScreePlot}}.}
  \item{\code{\link{pcaScorePlot}}}{Produces score plots from the \code{\link{pcaBySvd}} output.}
  \item{\code{\link{pcaLoading1dPlot}} or \code{\link{pcaLoading2dPlot}}}{Plots the PCA loadings as a line plot (1D) or in 2D as a scatterplot.}
  \item{\code{\link{pcaScreePlot}}}{Returns a bar plot of the percentage of variance explained by each Principal Component (PC) calculated by \code{\link{pcaBySvd}}.}
 }

}

\details{

\tabular{ll}{
Package: \tab limpca\cr
Type: \tab Package\cr
License: \tab GPL-2\cr
}

See the package vignettes (\code{vignette(package = "limpca")}) for detailed case studies.
}

\author{
???
}

\references{
Thiel, M., Benaiche, N., Martin, M., Franceschini, S., Van Oirbeek, R., Govaerts, B. (2023). limpca: An R package for the linear modeling of high-dimensional designed data based on ASCA/APCA family of methods. \emph{Journal of Chemometrics}. e3482. \url{https://doi.org/10.1002/cem.3482}

Martin, M. (2020). \emph{Uncovering informative content in metabolomics data: from pre-processing of 1H NMR spectra to biomarkers discovery in multifactorial designs.}  Prom.: Govaerts, B. PhD thesis. Institut de statistique, biostatistique et sciences actuarielles, UCLouvain, Belgium. \url{http://hdl.handle.net/2078.1/227671}

Thiel M., Feraud B. and Govaerts B. (2017). ASCA+ and APCA+: Extensions of ASCA and APCA
in the analysis of unbalanced multifactorial designs. \emph{Journal of Chemometrics}. 31:e2895. \url{https://doi.org/10.1002/cem.2895}

}


