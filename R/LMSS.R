# @title Linear Model Sum of Squares
# @description Compute the type III sum of squares from a resLmwEffectMatrices list and return the variation percentages
#
# @param resLmwEffectMatrices A resLmwEffectMatrices list from \code{\link{lmwEffectMatrices}}
#
# @return A list with the following elements :
#  \describe{
#   \item{\code{Type3Residuals}}{A list of matrices with the type III residuals for each model terms}
#   \item{\code{variationPercentages}}{A \emph{p} named vector with the variation percentages of each model terms}
# }
#
# @importfrom plyr alply laply llply

LMSS = function(resLmwEffectMatrices){

  # check the resLmwEffectMatrices

  if(!is.list(resLmwEffectMatrices)){stop("resLmwEffectMatrices argument is not a list")}
  if(length(resLmwEffectMatrices)!=9)(stop("Length of resLmwEffectMatrices has not a length of 9."))
  if(!all(names(resLmwEffectMatrices)==c("lmwDataList","ModelMatrix","ModelMatrixByEffect","covariateEffectsNames","covariateEffectsNamesUnique","effectMatrices","predictedvalues","residuals","parameters"))){stop("Object is not a resLmwEffectMatrices list from lmwEffectMatrices")}

  # getting needed variables
  nEffect = length(resLmwEffectMatrices$effectMatrices)
  covariateEffectsNamesUnique = resLmwEffectMatrices$covariateEffectsNamesUnique
  covariateEffectsNames = resLmwEffectMatrices$covariateEffectsNames
  modelMatrix = resLmwEffectMatrices$ModelMatrix
  outcomes=resLmwEffectMatrices$lmwDataList$outcomes
  residuals = resLmwEffectMatrices$residuals
  effectMatrices = resLmwEffectMatrices$effectMatrices

  # creating empty list with type 3 residuals
  Type3Residuals <- list()
  length(Type3Residuals) <- nEffect
  names(Type3Residuals) <- covariateEffectsNamesUnique

  # creating empty list with Frobenius norms
  matrixVolume <- list()
  length(matrixVolume) <- nEffect + 2
  names(matrixVolume) <- c(covariateEffectsNamesUnique, "outcomes", "residuals")

  # creating empty list with variation percentages
  variationPercentages <- list()
  length(variationPercentages) <- nEffect
  names(variationPercentages) <- c(covariateEffectsNamesUnique[covariateEffectsNamesUnique != 'Intercept'], 'residuals')

  # computing SS

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

  # variation percentages
  variationPercentages[[nEffect]] <- 100*numeratorFullModelSSType3/denominatorSSType3

  variationPercentages = unlist(variationPercentages)

  ResLMSS = list(Type3Residuals=Type3Residuals,variationPercentages=variationPercentages)
  return(ResLMSS)
}
