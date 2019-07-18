\name{UCH}
\alias{UCH}
\docType{data}
\title{ UCH Data : Urine Citrate-Hippurate database}

\description{This dataset comes from a H-NMR analysis of urine.}
\usage{data("UCH")}
\format{
A list of 3 : the experimental design, the outcomes for every observations and the formula considered to analyze the data.
  \describe{
    \item{\code{outcomes}}{  A dataset with 34 observations and 600 response variables}
    \item{\code{formula}}{ The suggested formula to analyze the data}
    \item{\code{design}}{  The experimental design of 34 observations and 5 explanatory variables}
  }
}
\details{
\code{outcomes = Hippurate + Citrate + Time +
            H*C + T*H + T*C +
            H*C*T}
}
\source{
Rousseau, R. (2011),\emph{Statistical contribution to the analysis of metabonomic data in H-NMR spectroscopy}, PhD thesis, Louvain-la-Neuve: UCL.
}

\examples{
data(UCH)

}
\keyword{datasets}
