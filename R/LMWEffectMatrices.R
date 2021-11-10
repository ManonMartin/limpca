#' @export LMWEffectMatrices
#' @title Computing the Effect Matrices
#' @description Runs a GLM model and decomposes the outcomes into effect matrices for each model terms
#'
#' @param ResLMWModelMatrix A list of 5 elements from \code{\link{LMWModelMatrix}}.
#' @param SS Logical. If `FALSE`, won't compute the effect percentage variations.
#' @param newSSmethod A logical whether to use the new optimized method to compute SS.
#' @param contrastList A list of contrast for each parameter. The function creates automatically the list by default.
#'
#' @return A list with the following elements:
#'  \describe{
#'    \item{\code{LMWiRe_data_list}}{A list containing the outcomes, the experimental design and the formula.}
#'    \item{\code{ModelMatrix}}{A \emph{nxK} model matrix specifically encoded for the ASCA-GLM method.}
#'    \item{\code{modelMatrixByEffect}}{A list of \emph{p} model matrices by models terms.}
#'    \item{\code{covariateEffectsNames}}{A character vector with \emph{K} names of the coefficients.}
#'    \item{\code{covariateEffectsNamesUnique}}{A character vector with the \emph{p} unique names of the model terms.}
#'    \item{\code{effectMatrices}}{A list of \emph{p} effect matrices for each model terms.}
#'    \item{\code{predictedvalues}}{A \emph{nxm} matrix with the predicted values.}
#'    \item{\code{residuals}}{A \emph{nxm} matrix with the residuals.}
#'    \item{\code{parameters}}{A \emph{pxm} matrix with the coefficients of every parameters by response variables.}
#'    \item{\code{SS}}{A vector with the type III residuals for each model terms.}
#'    \item{\code{variationPercentages}}{A \emph{p} named vector with the variation percentages of each model terms.}
#'  }
#'
#' @examples
#'  data('UCH')
#'  ResLMWModelMatrix <- LMWModelMatrix(UCH)
#'  LMWEffectMatrices(ResLMWModelMatrix)
#'
#' @import stringr
#' @import plyr



LMWEffectMatrices = function(ResLMWModelMatrix, SS=TRUE, newSSmethod=TRUE, contrastList=NA){

  #Checking the object
  if(!is.list(ResLMWModelMatrix)){stop("Argument ResLMWModelMatrix is not a list")}
  if(length(ResLMWModelMatrix)!=5){stop("List does not contain 5 elements")}
  if(names(ResLMWModelMatrix)[1]!="LMWiRe_data_list"|
     names(ResLMWModelMatrix)[2]!="ModelMatrix"|
     names(ResLMWModelMatrix)[3]!="ModelMatrixByEffect"|
     names(ResLMWModelMatrix)[4]!="covariateEffectsNames"|
     names(ResLMWModelMatrix)[5]!="covariateEffectsNamesUnique"){stop("Argument is not a ResLMWModelMatrix object")}

  #Attribute a name in the function environment

  formula = ResLMWModelMatrix$LMWiRe_data_list$formula
  design = ResLMWModelMatrix$LMWiRe_data_list$design
  outcomes = ResLMWModelMatrix$LMWiRe_data_list$outcomes
  LMWiRe_data_list = ResLMWModelMatrix$LMWiRe_data_list
  modelMatrix = ResLMWModelMatrix$ModelMatrix
  ModelMatrixByEffect = ResLMWModelMatrix$ModelMatrixByEffect
  covariateEffectsNames = ResLMWModelMatrix$covariateEffectsNames
  covariateEffectsNamesUnique = ResLMWModelMatrix$covariateEffectsNamesUnique
  nEffect <- length(covariateEffectsNamesUnique)

  #Creating empty effects matrices
  effectMatrices <- list()
  length(effectMatrices) <- nEffect
  names(effectMatrices) <- covariateEffectsNamesUnique

  #GLM decomposition calculated by using glm.fit and alply on outcomes

  #The following line gives an error of type: Error in glm.fit(modelMatrix, xx) : NAs in V(mu),
  #it is temporarily replaced by a loop
  #resGLM <- plyr::alply(outcomes, 2, function(xx) glm.fit(modelMatrix, xx))

  resGLM <- list()
  for(i in 1:ncol(outcomes)) resGLM[[i]] <- glm.fit(modelMatrix, outcomes[,i])
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


  ResLMWEffectMatrices = list(LMWiRe_data_list = LMWiRe_data_list,
                             ModelMatrix = modelMatrix,
                             ModelMatrixByEffect = ModelMatrixByEffect,
                             covariateEffectsNames = covariateEffectsNames,
                             covariateEffectsNamesUnique = covariateEffectsNamesUnique,
                             effectMatrices = effectMatrices,
                             predictedvalues = predictedValues,
                             residuals = residuals,
                             parameters = parameters)

  # Compute the Sum of Squares Type 3
  if(SS==TRUE){
    if(newSSmethod){
      if(is.na(contrastList)){L = contrastSS(ResLMWModelMatrix)}else{L = contrastList}
      ResLMSS = LMSSv2(ResLMWEffectMatrices,L)
    }else{
      ResLMSS = LMSS(ResLMWEffectMatrices)
    }
    ResLMWEffectMatrices = c(ResLMWEffectMatrices,ResLMSS)
  }else{
    ResLMWEffectMatrices = c(ResLMWEffectMatrices,SS=NA,variationPercentages=NA)
  }


  return(ResLMWEffectMatrices)
}
