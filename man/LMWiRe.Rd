\name{LMWiRe}
\alias{LMWiRe}
\title{R Routines for Linear Model With Wide Response}

\description{
This package has for objectives to provide a method to make Linear Model for Wide Response data. This method handles unbalanced design. At this development stage, the package can only perform one or two-way ANOVA of class I. More possibilities should be included in the future. The principal functions of the package are:

\describe{
  \item{\code{\link{LMModelMatrix}}}{Create a model matrix from the design matrix}
  \item{\code{\link{LMEffectMatrices}}}{Compute an ANOVA-GLM model}
  \item{\code{\link{PCALMEffects}}}{Perform a PCA on the effect matrices}
  \item{\code{\link{LMModelMatrix}}}{Create a model matrix from the design matrix}
  %\item{\code{\link{LMPermutTest}}}{Perform a permutation test for each effect matrix}
}



Other functions to observe the results are:
\describe{
  \item{\code{\link{PlotScoresMatrix}}}{Plot multiple scores of different effects in one graph}
  \item{\code{\link{PlotScoresXY}}}{Plot scores of an effect on two components by graph }
  \item{\code{\link{PlotLoadings}}}{Plot the loadings}
  \item{\code{\link{PrintContributions}}}{Print a table with the contribution of each effect's component to the total variance}
  \item{\code{\link{PlotScree}}}{Plot a barplot of the variance percentage of each component}
  %\item{\code{\link{PlotLMSS}}}{Plot the variance of each effect}
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


