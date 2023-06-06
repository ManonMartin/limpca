\name{trout}
\alias{trout}
\docType{data}
\title{trout: the Rainbow trouts transcriptomic dataset}

\description{
This dataset comes from the study of the modulation of immunity in rainbow trout (Oncorhynchus mykiss) by exposure to cadmium (Cd) combined with polyunsaturated fatty acids (PUFAs) enriched diets [Cornet et al., 2018].

The responses were quantified by measuring the modification of the expression of 15 immune-related genes (m = 15) by RT-qPCR (reverse transcription quantita- tive polymerase chain reaction). The experiment was carried out on 72 trouts and 3 factors were considered in the experimental design:
\describe{
    \item{\code{Day}}{Measurements on trouts were collected on days 28, 70 and 72}
    \item{\code{Treatment}}{Four polyunsaturated fatty acid diets: alpha-linolenic acid (ALA), linoleic acid (LA), eicosapentaenoic acid (EPA) and docosahexaenoic acid (DHA)}
    \item{\code{Exposure}}{Trouts were exposed (level = 2) or not (level = 0) to high cadmium concentrations.}
}
This gives a 3 × 4 × 2 factorial design.  Each of the 24 trials corresponds to a different aquarium. Three fish were analysed (3 replicates) for each condition, giving a total of 72 observations.

In the limpca vignette, some outliers are first detected and removed from the dataset. The data of each aquarium are then transformed in log10 and mean aggregated in order to avoid the use of an aquarium random factor in the statictical model. Data are then centered and scaled by column.  The ASCA+/APCA+ analysis is then applyed on the transformed data.
}

\usage{data("trout")}
\format{
  A list of 3 : the experimental design, the outcomes for every observation and the formula considered to analyze the data.
  Caution ! Here, the data must first be aggregated before being analyzed with ASCA+ (see details in related vignette)
  \describe{
    \item{\code{outcomes}}{A dataset with 72 observations and 15 response variables}
    \item{\code{formula}}{The suggested formula to analyze the data}
    \item{\code{design}}{The experimental design of 72 observations and 4 explanatory variables}
  }
}
\details{

  The data must first be aggregated before being analyzed with limpca. This will remove the hierarchy in the design and allow to apply a classical fixed effect general linear model to the data. See more details in the trout vignette (\code{print(vignette(topic = "Trout", package = "limpca"))}).
}
\source{
  Cornet, V., Ouaach, A., Mandiki, S., Flamion, E., Ferain, A., Van Larebeke, M., Lemaire, B., Reyes Lopez F.,  Tort, L., Larondelle, Y. and Kestemont, P. (2018).
  Environmentally-realistic concentration of cadmium combined with polyunsaturated fatty acids enriched diets modulated non-specific immunity in rainbow trout.
  \emph{Aquatic Toxicology}, \bold{196}, 104--116. \url{https://doi.org/10.1016/j.aquatox.2018.01.012}

  Benaiche, N. (2022). \emph{Stabilisation of the R package LMWiRe – Linear Models for Wide Responses.}  Prom. : Govaerts, B. Master thesis. Institut de statistique, biostatistique et sciences actuarielles, UCLouvain, Belgium.
  \url{http://hdl.handle.net/2078.1/thesis:33996}
  }

\examples{
  data("trout")}

\keyword{datasets}
