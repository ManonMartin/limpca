limpca quickstart
================

<!-- badges: start -->

[![R-CMD-check](https://github.com/ManonMartin/limpca/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/ManonMartin/limpca/actions/workflows/check-standard.yaml)
<!-- badges: end -->

# Installation

``` r
remotes::install_github("ManonMartin/limpca", dependencies = TRUE)
library("limpca")
```

Note that if you would like to build the vignettes, you have to install
`BiocStyle` (from [Bioconductor](https://www.bioconductor.org/)) and
`rmarkdown` packages before installing `limpca` with the following
command:
`remotes::install_github("ManonMartin/limpca", dependencies = TRUE, build_vignettes = TRUE)`.

For any enquiry, you can send an email to the package authors:
<bernadette.govaerts@uclouvain.be> ; <michel.thiel@uclouvain.be> or
<manon.martin@uclouvain.be>

# Short application on the `UCH` dataset

## Data object

``` r
str(UCH)
```

## Data visualisation

``` r
# design
plotDesign(design = UCH$design, x = "Hippurate", 
           y = "Citrate", rows = "Time",
           title = "Design of the UCH dataset")

# row 3 of outcomes
plotLine(Y = UCH$outcomes,
         title = "H-NMR spectrum",
         rows = c(3),
         xlab = "ppm",
         ylab = "Intensity")
```

## PCA

``` r
ResPCA = pcaBySvd(UCH$outcomes)
pcaScreePlot(ResPCA, nPC = 6)
pcaScorePlot(resPcaBySvd = ResPCA, axes = c(1,2), 
             title = "PCA scores plot: PC1 and PC2", 
             design = UCH$design,
             color = "Hippurate", shape = "Citrate",
             points_labs_rn = FALSE)
```

## Model estimation and effect matrix decomposition

``` r
# Model matrix generation
resMM = lmpModelMatrix(UCH)

# Model estimation and effect matrices decomposition
resEM = lmpEffectMatrices(resMM)
```

## Effect matrix test of significance and importance measure

``` r
# Effects importance
resEM$varPercentagesPlot

# Bootstrap tests
resBT = lmpBootstrapTests(resLmpEffectMatrices = resEM, nboot=100)
resBT$resultsTable
```

## ASCA-E decomposition

``` r
# ASCA-E decomposition
resASCAE = lmpPcaEffects(resLmpEffectMatrices = resEM, method="ASCA-E")

# Scores Plot for the hippurate
lmpScorePlot(resASCAE, effectNames = "Hippurate", 
             color = "Hippurate", shape = "Hippurate")

# Loadings Plot for the hippurate
lmpLoading1dPlot(resASCAE, effectNames = c("Hippurate"), 
                              axes = 1, xlab = "ppm")

# Scores ScatterPlot matrix
lmpScoreScatterPlotM(resASCAE,PCdim=c(1,1,1,1,1,1,1,2),
                     modelAbbrev = TRUE,
                     varname.colorup = "Citrate",
                     varname.colordown  = "Time",
                     varname.pchup="Hippurate",
                     varname.pchdown="Time",
                     title = "ASCA scores scatterplot matrix")
```
