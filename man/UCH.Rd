\name{UCH}
\alias{UCH}
\docType{data}
\title{UCH: the Urine Citrate-Hippurate metabolomic dataset}

\description{This dataset comes from a 1H NMR analysis of urine of female rats with hippuric and citric acid were added to the samples in different known concentrations.}
\usage{data("UCH")}

\format{
A list of length 3: the experimental design (`design`), the outcomes for every observations (`outcomes`) and the formula considered to analyze the data (`formula`).
  \describe{
    \item{\code{design}}{A data.frame with the experimental design of 34 observations and 5 explanatory variables:
   \code{Hippurate}: concentration of hippuric acid;
   \code{Citrate}: concentration of citric acid;
   \code{Dilution}: dilution, here all the samples are diluted with a dilution rate of 50 \%;
   \code{Day}: for each medium, the preparation of the mixtures were performed in two series;
   \code{Time}: each mixture or experimental condition  was repeated twice.
  }
    \item{\code{outcomes}}{A numerical matrix with 34 observations and 600 response variables}
    \item{\code{formula}}{A character string with the suggested formula to analyze the data}
  }
}
\details{
The database has been experimentally created in order to control the spectral locations of the biomarkers to find (i.e. \code{Hippurate} and \code{Citrate}). This property allows us to evaluate the performances of the data analysis of various statistical methods.
This urine experimental database is also designed in order to explore the influence on spectra of intra-sample 1H NMR replications (\code{Time}), and inter-day 1H NMR measurements (\code{Day}).

The model formula is:

\code{outcomes = Hippurate + Citrate + Time +
            Hippurate:Citrate + Time:Hippurate + Time:Citrate + Hippurate:Citrate:Time}
}
\source{
Martin, M. (2020). \emph{Uncovering informative content in metabolomics data: from pre-processing of 1H NMR spectra to biomarkers discovery in multifactorial designs.}  Prom.: Govaerts, B. PhD thesis. Institut de statistique, biostatistique et sciences actuarielles, UCLouvain, Belgium. \url{http://hdl.handle.net/2078.1/227671}
}

\examples{
data("UCH")
str(UCH)
}

\keyword{datasets}
