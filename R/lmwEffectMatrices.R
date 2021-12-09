#' @export lmwEffectMatrices
#' @title Computes the effect matrices
#'
#' @description
#' Runs a GLM model and decomposes the outcomes into effect matrices for each model terms.
#'
#' @param resLmwModelMatrix A list of 5 elements from \code{\link{lmwModelMatrix}}.
#' @param SS Logical. If `FALSE`, won't compute the effect percentage variations.
#' @param newSSmethod Logical.If `FALSE`, the new optimized method to compute SS is not used.
#' @param contrastList A list of contrast for each parameter. If `NA`, the function creates automatically the list by default.
#'
#' @return A list with the following elements:
#'  \describe{
#'    \item{\code{lmwDataList}}{A list containing the outcomes, the experimental design and the formula.}
#'    \item{\code{ModelMatrix}}{A \emph{nxK} model matrix specifically encoded for the ASCA-GLM method.}
#'    \item{\code{ModelMatrixByEffect}}{A list of \emph{p} model matrices by models terms.}
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
#'  resLmwModelMatrix <- lmwModelMatrix(UCH)
#'  lmwEffectMatrices(resLmwModelMatrix)
#'
#' @import stringr
#' @importFrom plyr laply aaply



lmwEffectMatrices = function(resLmwModelMatrix, SS=TRUE, newSSmethod=TRUE, contrastList=NA){

  #Checking the object
  if(!is.list(resLmwModelMatrix)){stop("Argument resLmwModelMatrix is not a list")}
  if(length(resLmwModelMatrix)!=5){stop("List does not contain 5 elements")}
  if(names(resLmwModelMatrix)[1]!="lmwDataList"|
     names(resLmwModelMatrix)[2]!="ModelMatrix"|
     names(resLmwModelMatrix)[3]!="ModelMatrixByEffect"|
     names(resLmwModelMatrix)[4]!="covariateEffectsNames"|
     names(resLmwModelMatrix)[5]!="covariateEffectsNamesUnique"){stop("Argument is not a resLmwModelMatrix object")}

  #Attribute a name in the function environment
  formula = resLmwModelMatrix$lmwDataList$formula
  design = resLmwModelMatrix$lmwDataList$design
  outcomes = resLmwModelMatrix$lmwDataList$outcomes
  lmwDataList = resLmwModelMatrix$lmwDataList
  modelMatrix = resLmwModelMatrix$ModelMatrix
  ModelMatrixByEffect = resLmwModelMatrix$ModelMatrixByEffect
  covariateEffectsNames = resLmwModelMatrix$covariateEffectsNames
  covariateEffectsNamesUnique = resLmwModelMatrix$covariateEffectsNamesUnique
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


  resLmwEffectMatrices = list(lmwDataList = lmwDataList,
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
      if(is.na(contrastList)){L = contrastSS(resLmwModelMatrix)}else{L = contrastList}
      ResLMSS = LMSSv2(resLmwEffectMatrices,L)
    }else{
      ResLMSS = LMSS(resLmwEffectMatrices)
    }
    resLmwEffectMatrices = c(resLmwEffectMatrices,ResLMSS)
  }else{
    resLmwEffectMatrices = c(resLmwEffectMatrices,SS=NA,variationPercentages=NA)
  }


  return(resLmwEffectMatrices)
}
