#' @export LMEffectMatrices
#' @title Computing the Effect Matrices
#' @description Runs a GLM model and decomposes the outcomes into effect matrices for each model terms
#'
#' @param ResLMModelMatrix A list of 3 from \code{\link{LMModelMatrix}}
#' @param outcomes A \emph{nxm} matrix with n observations and m response variables
#' @param SS a logical whether to compute the effect percentage variations
#' @param newSSmethod a logical whether to use the new optimized method to compute SS
#' @param contrastList a list of contrast for each parameter. The function creates automatically the list by default
#'
#' @return A list with the following elements:
#'  \describe{
#'    \item{\code{formula}}{A formula object with the expression of the GLM used to predict the outcomes}
#'    \item{\code{design}}{A \emph{nxk} data frame with the "free encoded" experimental design.}
#'    \item{\code{ModelMatrix}}{A \emph{nxK} model matrix specifically encoded for the ASCA-GLM method.}
#'    \item{\code{outcomes}}{A \emph{nxm} matrix with n observations and m response variables}
#'    \item{\code{effectMatrices}}{A list of \emph{p} effect matrices for each model terms}
#'    \item{\code{modelMatrixByEffect}}{A list of \emph{p} model matrices by models terms }
#'    \item{\code{predictedvalues}}{A \emph{nxm} matrix with the predicted values}
#'    \item{\code{residuals}}{A \emph{nxm} matrix with the residuals}
#'    \item{\code{parameters}}{A \emph{pxm} matrix with the coefficients of every parameters by response variables}
#'    \item{\code{covariateEffectsNamesUnique}}{A character vector with the \emph{p} unique name of the model terms}
#'    \item{\code{covariateEffectsNames}}{A character vector with \emph{K} names of the coefficients}
#'  }
#'
#' @examples
#'  data('UCH')
#'  ResLMModelMatrix <- LMModelMatrix(formula=as.formula(UCH$formula),design=UCH$design)
#'  LMEffectMatrices(ResLMModelMatrix,outcomes=UCH$outcomes)
#'
#' @import stringr
#' @import plyr



LMEffectMatrices = function(ResLMModelMatrix,outcomes,SS=TRUE,newSSmethod=TRUE,contrastList=NA){

  #Checking the object
  if(!is.list(ResLMModelMatrix)){stop("Argument ResLMModelMatrix is not a list")}
  if(length(ResLMModelMatrix)!=6){stop("List does not contain 6 elements")}
  if(names(ResLMModelMatrix)[1]!="formula"|
     names(ResLMModelMatrix)[2]!="design"|
     names(ResLMModelMatrix)[3]!="ModelMatrix"|
     names(ResLMModelMatrix)[4]!="ModelMatrixByEffect"|
     names(ResLMModelMatrix)[5]!="covariateEffectsNames"|
     names(ResLMModelMatrix)[6]!="covariateEffectsNamesUnique"){stop("Argument is not a ResLMModelMAtrix object")}

  #Attribute a name in the function environment

  formula=ResLMModelMatrix$formula
  design=ResLMModelMatrix$design
  modelMatrix=ResLMModelMatrix$ModelMatrix
  ModelMatrixByEffect = ResLMModelMatrix$ModelMatrixByEffect
  covariateEffectsNames = ResLMModelMatrix$covariateEffectsNames
  covariateEffectsNamesUnique = ResLMModelMatrix$covariateEffectsNamesUnique
  nEffect <- length(covariateEffectsNamesUnique)

  #Creating empty effects matrices
  effectMatrices <- list()
  length(effectMatrices) <- nEffect
  names(effectMatrices) <- covariateEffectsNamesUnique

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
    }


  ResLMEffectMatrices = list(formula=formula,
                             design=design,
                             ModelMatrix=modelMatrix,
                             ModelMatrixByEffect=ModelMatrixByEffect,
                             covariateEffectsNames=covariateEffectsNames,
                             covariateEffectsNamesUnique=covariateEffectsNamesUnique,
                             outcomes=outcomes,
                             effectMatrices=effectMatrices,
                             predictedvalues=predictedValues,
                             residuals=residuals,
                             parameters=parameters)

  # Compute the Sum of Squares Type 3
  if(SS==TRUE){
    if(newSSmethod){
      if(is.na(contrastList)){L = contrastSS(ResLMModelMatrix)}else{L = contrastList}
      ResLMSS = LMSSv2(ResLMEffectMatrices,L)
    }else{
      ResLMSS = LMSS(ResLMEffectMatrices)
    }
    ResLMEffectMatrices = c(ResLMEffectMatrices,ResLMSS)
  }else{
    ResLMEffectMatrices = c(ResLMEffectMatrices,SS=NA,variationPercentages=NA)
  }


  return(ResLMEffectMatrices)
}
