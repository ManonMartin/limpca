#' @export LMEffectMatrices
#' @title Computing the Effect Matrices
#' @description Runs a GLM model and decomposes the outcomes into effect matrices for each model terms
#'
#' @param ResLMModelMatrix A list of 3 from \code{\link{LMModelMatrix}}
#' @param outcomes A n x y matrix with n observations and y response variables
#'
#' @return A list with the following elements:
#'  \describe{
#'    \item{\code{formula}}{A formula object with the expression of the GLM used to predict the outcomes}
#'    \item{\code{design}}{A data frame with the "free encoded" experimental design}
#'    \item{\code{ModelMatrix}}{A model matrix specifically encoded for the ASCA-GLM}
#'    \item{\code{outcomes}}{A matrix with n observations and y response variables}
#'    \item{\code{effectMatrices}}{A list of effect matrices for each model terms}
#'    \item{\code{modelMatrixByEffect}}{A list of model matrix by models terms }
#'    \item{\code{predictedvalues}}{A matrix with the predicted values}
#'    \item{\code{residuals}}{A matrix with the residuals}
#'    \item{\code{parameters}}{A matrix with the coefficients of every parameters by response variables}
#'    \item{\code{covariateEffectsNamesUnique}}{A character vector with the name of the model terms}
#'    \item{\code{covariateEffectsNames}}{A character vector with every name of the coefficients}
#'  }
#'
#' @examples
#'  data('UCH')
#'  ResLMModelMatrix = LMModelMatrix(formula=as.formula(UCH$formula),design=UCH$design)
#'  LMEffectMatrices(ResLMModelMatrix,outcomes=UCH$outcomes)
#'
#' @import stringr
#' @import plyr



LMEffectMatrices = function(ResLMModelMatrix,outcomes){

  #Checking the object
  if(!is.list(ResLMModelMatrix)){stop("Argument is not a list")}
  if(length(ResLMModelMatrix)!=3){stop("List does not contain 3 arguments")}
  if(names(ResLMModelMatrix)[1]!="formula"|names(ResLMModelMatrix)[2]!="design"|names(ResLMModelMatrix)[3]!="ModelMatrix"){stop("Argument is not a ResLMModelMAtrix object")}

  #Attribute a name in the function environment

  formula=ResLMModelMatrix[[1]]
  design=ResLMModelMatrix[[2]]
  modelMatrix=ResLMModelMatrix[[3]]

  #Creating a list containing effect matrices and model matrices by effect
  dummyVarNames <- colnames(modelMatrix)
  presencePolynomialEffects <- stringr::str_detect(dummyVarNames, '\\^[0-9]') # Detect exponent
  covariateEffectsNames <- character(length = length(dummyVarNames))
  covariateEffectsNames[presencePolynomialEffects] <- dummyVarNames[presencePolynomialEffects]
  covariateEffectsNames[!presencePolynomialEffects] <- gsub('[0-9]', '', dummyVarNames[!presencePolynomialEffects])
  covariateEffectsNames[covariateEffectsNames == '(Intercept)'] <- 'Intercept'
  covariateEffectsNamesUnique <- unique(covariateEffectsNames)
  nEffect <- length(covariateEffectsNamesUnique)

  #Creating empty effects matrices
  effectMatrices <- list()
  length(effectMatrices) <- nEffect
  names(effectMatrices) <- covariateEffectsNamesUnique

  #Creating empty model matrices by effect
  modelMatrixByEffect <- list()
  length(modelMatrixByEffect) <- nEffect
  names(modelMatrixByEffect) <- covariateEffectsNamesUnique

  #GLM decomposition calculated by using glm.fit and alply on outcomes
  resGLM <- plyr::alply(outcomes, 2, function(xx) glm.fit(modelMatrix, xx))
  parameters <- t(plyr::laply(resGLM, function(xx) xx$coefficients))
  predictedValues <- t(plyr::laply(resGLM, function(xx) xx$fitted.values))
  residuals <- t(plyr::laply(resGLM, function(xx) xx$residuals))

  #Filling effectMatrices
  for(iEffect in 1:nEffect){
    selection <- which(covariateEffectsNames == covariateEffectsNamesUnique[iEffect])
    selectionComplement <- which(covariateEffectsNames != covariateEffectsNamesUnique[iEffect])
    #Effect matrices
    effectMatrices[[iEffect]] <- t(plyr::aaply(parameters, 2, function(xx) as.matrix(modelMatrix[, selection])%*%xx[selection]))
    #Model matrices by effect
    modelMatrixByEffect[[iEffect]] <- as.matrix(modelMatrix[, selection])

    # matrixVolume[[iEffect]] <- (norm(effectMatrices[[iEffect]], "F"))^2
    # if(covariateEffectsNamesUnique[iEffect] != 'Intercept'){
    #   resGLMComplement <- alply(outcomes, 2, function(xx) glm.fit(modelMatrix[, selectionComplement], xx))
    #   Type3Residuals[[iEffect]] <- t(laply(resGLMComplement, function(xx) xx$residuals))
    # }
  }

  ResLMEffectMatrices = list(formula=formula,
                             design=design,
                             ModelMatrix=modelMatrix,
                             outcomes=outcomes,
                             effectMatrices=effectMatrices,
                             modelMatrixByEffect=modelMatrixByEffect,
                             predictedvalues=predictedValues,
                             residuals=residuals,
                             parameters=parameters,
                             covariateEffectsNamesUnique=covariateEffectsNamesUnique,
                             covariateEffectsNames=covariateEffectsNames)
  return(ResLMEffectMatrices)
}
