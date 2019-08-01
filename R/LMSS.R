#' @export LMSS
#' @title Linear Model Sum of Squares
#' @description Compute the type III sum of squares from a ResLMEffectMatrices list and return the variation percentages
#'
#' @param ResLMEffectMatrices a ResLMEffectMatrices list from \code{\link{LMEffectMatrices}}
#'
#' @return A list with the following elements :
#'  \describe{
#'   \item{\code{formula}}{The \emph{p} terms formula of the GLM used to predict the response variables.}
#'   \item{\code{Type3Residuals}}{A list of matrices with the type III residuals for each model terms}
#'   \item{\code{variationPercentages}}{A \emph{p} named vector with the variation percentages of each model terms}
#'  }
#'
#' @examples
#'  data('UCH')
#'  ResLMModelMatrix <- LMModelMatrix(formula=as.formula(UCH$formula),design=UCH$design)
#'  ResLMEffectMatrices <- LMEffectMatrices(ResLMModelMatrix,outcomes=UCH$outcomes)
#'  ResLMSS <- LMSS(ResLMEffectMatrices)
#'  PlotLMSS(ResLMSS,abbrev=TRUE)
#'
#' @import plyr




LMSS = function(ResLMEffectMatrices){

  #Check the ResLMEffectMatrices

  if(!is.list(ResLMEffectMatrices)){stop("ResLMEffectMatrices argument is not a list")}
  if(length(ResLMEffectMatrices)!=11)(stop("Length of ResLMEffectMatrices has not a length of 11."))
  if(!all(names(ResLMEffectMatrices)==c("formula","design","ModelMatrix","outcomes","effectMatrices","modelMatrixByEffect","predictedvalues","residuals","parameters","covariateEffectsNamesUnique","covariateEffectsNames"))){stop("Object is not a ResLMEffectMatrices list from LMEffectMatrices")}

  #Getting needed variables
  nEffect = length(ResLMEffectMatrices$effectMatrices)
  covariateEffectsNamesUnique = ResLMEffectMatrices$covariateEffectsNamesUnique
  covariateEffectsNames = ResLMEffectMatrices$covariateEffectsNames
  modelMatrix = ResLMEffectMatrices$ModelMatrix
  outcomes=ResLMEffectMatrices$outcomes
  residuals = ResLMEffectMatrices$residuals
  effectMatrices = ResLMEffectMatrices$effectMatrices

  #Creating empty list with type 3 residuals
  Type3Residuals <- list()
  length(Type3Residuals) <- nEffect
  names(Type3Residuals) <- covariateEffectsNamesUnique

  #Creating empty list with Frobenius norms
  matrixVolume <- list()
  length(matrixVolume) <- nEffect + 2
  names(matrixVolume) <- c(covariateEffectsNamesUnique, "outcomes", "residuals")

  #Creating empty list with variation percentages
  variationPercentages <- list()
  length(variationPercentages) <- nEffect
  names(variationPercentages) <- c(covariateEffectsNamesUnique[covariateEffectsNamesUnique != 'Intercept'], 'residuals')

  #Computing SS

  for(iEffect in 1:nEffect){
    selection <- which(covariateEffectsNames == covariateEffectsNamesUnique[iEffect])
    selectionComplement <- which(covariateEffectsNames != covariateEffectsNamesUnique[iEffect])

    # #Effect matrices
    # effectMatrices[[iEffect]] <- t(plyr::aaply(parameters, 2, function(xx) as.matrix(modelMatrix[, selection])%*%xx[selection]))
    # #Model matrices by effect
    # modelMatrixByEffect[[iEffect]] <- as.matrix(modelMatrix[, selection])

    matrixVolume[[iEffect]] <- (norm(effectMatrices[[iEffect]], "F"))^2
    if(covariateEffectsNamesUnique[iEffect] != 'Intercept'){
      resGLMComplement <- plyr::alply(outcomes, 2, function(xx) glm.fit(modelMatrix[, selectionComplement], xx))
      Type3Residuals[[iEffect]] <- t(plyr::laply(resGLMComplement, function(xx) xx$residuals))
    }
  }

  matrixVolume[[nEffect + 1]] <- (norm(outcomes, "F"))^2
  matrixVolume[[nEffect + 2]] <- (norm(residuals, "F"))^2

  denominatorSSType3 <- norm(outcomes - effectMatrices[['Intercept']], "F")^2
  numeratorFullModelSSType3 <- norm(residuals, "F")^2
  variationPercentages[1:nEffect-1] <- plyr::llply(Type3Residuals[-1], function(xx) 100*(norm(xx, "F")^2 - numeratorFullModelSSType3)/denominatorSSType3)
  #Variation percentages
  variationPercentages[[nEffect]] <- 100*numeratorFullModelSSType3/denominatorSSType3

  variationPercentages = unlist(variationPercentages)

  ResLMSS = list(formula=ResLMEffectMatrices$formula,Type3Residuals=Type3Residuals,variationPercentages=variationPercentages)
  return(ResLMSS)
}
