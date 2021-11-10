# @title Linear Model Sum of Squares
# @description Compute the type III sum of squares from a ResLMEffectMatrices list and return the variation percentages
#
# @param ResLMEffectMatrices a ResLMEffectMatrices list from \code{\link{LMEffectMatrices}}
#
# @return A list with the following elements :
#  \describe{
#   \item{\code{Type3Residuals}}{A list of matrices with the type III residuals for each model terms}
#   \item{\code{variationPercentages}}{A \emph{p} named vector with the variation percentages of each model terms}
# }
#
# @import plyr




LMSS = function(ResLMWEffectMatrices){

  #Check the ResLMWEffectMatrices

  if(!is.list(ResLMWEffectMatrices)){stop("ResLMWEffectMatrices argument is not a list")}
  if(length(ResLMWEffectMatrices)!=9)(stop("Length of ResLMWEffectMatrices has not a length of 9."))
  if(!all(names(ResLMWEffectMatrices)==c("LMWiRe_data_list","ModelMatrix","ModelMatrixByEffect","covariateEffectsNames","covariateEffectsNamesUnique","effectMatrices","predictedvalues","residuals","parameters"))){stop("Object is not a ResLMWEffectMatrices list from LMWEffectMatrices")}

  #Getting needed variables
  nEffect = length(ResLMWEffectMatrices$effectMatrices)
  covariateEffectsNamesUnique = ResLMWEffectMatrices$covariateEffectsNamesUnique
  covariateEffectsNames = ResLMWEffectMatrices$covariateEffectsNames
  modelMatrix = ResLMWEffectMatrices$ModelMatrix
  outcomes=ResLMWEffectMatrices$LMWiRe_data_list$outcomes
  residuals = ResLMWEffectMatrices$residuals
  effectMatrices = ResLMWEffectMatrices$effectMatrices

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

  ResLMSS = list(Type3Residuals=Type3Residuals,variationPercentages=variationPercentages)
  return(ResLMSS)
}
