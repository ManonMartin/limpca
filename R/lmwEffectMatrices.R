#' @export lmwEffectMatrices
#' @title Computes the effect matrices
#'
#' @description
#' Runs a GLM and decomposes the outcomes into effect matrices for each model effect.
#'
#' @param resLmwModelMatrix A list of 5 elements from \code{\link{lmwModelMatrix}}.
#' @param SS Logical. If `FALSE`, won't compute the percentage of variance for each effect.
#' @param contrastList A list of contrast for each parameter. If `NA`, the function creates automatically the list by default.
#'
#' @return A list with the following elements:
#'  \describe{
#'    \item{\code{lmwDataList}}{The initial object: a list with outcomes, design and formula.}
#'    \item{\code{modelMatrix}}{A \emph{nxK} model matrix specifically encoded for the ASCA-GLM method.}
#'    \item{\code{modelMatrixByEffect}}{A list of \emph{p} model matrices for each effect.}
#'    \item{\code{effectsNamesUnique}}{A character vector with the \emph{p} names of the model effects, each repeated once.}
#'    \item{\code{effectsNamesAll}}{A character vector with the \emph{K} names of the model effects ordered and repeated as the column names of the model matrix.}
#'    \item{\code{effectMatrices}}{A list of \emph{p} effect matrices for each model effect.}
#'    \item{\code{predictedvalues}}{The \emph{nxm} matrix of predicted outcome values.}
#'    \item{\code{residuals}}{The \emph{nxm} matrix of model residuals.}
#'    \item{\code{parameters}}{The \emph{pxm} matrix of the estimated parameters.}
#'    \item{\code{type3SS}}{A vector with the type III sum of squares for each model effect \emph{(If SS = TRUE)}.}
#'    \item{\code{variationPercentages}}{A vector with the percentage of variance for each model effect \emph{(If SS = TRUE)}.}
#'    \item{\code{varPercentagesPlot}}{A ggplot bar plot of the contributions of each model effect to the total variance \emph{(If SS = TRUE)}.}
#'  }
#'
#' @examples
#'  data('UCH')
#'  resLmwModelMatrix <- lmwModelMatrix(UCH)
#'  lmwEffectMatrices(resLmwModelMatrix)
#'
#' @import stringr
#' @importFrom plyr laply aaply



lmwEffectMatrices = function(resLmwModelMatrix, SS=TRUE, contrastList=NA){

  #Checking the object
  if(!is.list(resLmwModelMatrix)){stop("Argument resLmwModelMatrix is not a list")}
  if(length(resLmwModelMatrix)!=5){stop("List does not contain 5 elements")}
  if(names(resLmwModelMatrix)[1]!="lmwDataList"|
     names(resLmwModelMatrix)[2]!="modelMatrix"|
     names(resLmwModelMatrix)[3]!="modelMatrixByEffect"|
     names(resLmwModelMatrix)[4]!="effectsNamesUnique"|
     names(resLmwModelMatrix)[5]!="effectsNamesAll"){stop("Argument is not a resLmwModelMatrix object")}
  checkArg(SS,c("bool","length1"),can.be.null=FALSE)

  #Attribute a name in the function environment
  formula = resLmwModelMatrix$lmwDataList$formula
  design = resLmwModelMatrix$lmwDataList$design
  outcomes = resLmwModelMatrix$lmwDataList$outcomes
  lmwDataList = resLmwModelMatrix$lmwDataList
  modelMatrix = resLmwModelMatrix$modelMatrix
  modelMatrixByEffect = resLmwModelMatrix$modelMatrixByEffect
  effectsNamesAll = resLmwModelMatrix$effectsNamesAll
  effectsNamesUnique = resLmwModelMatrix$effectsNamesUnique
  nEffect <- length(effectsNamesUnique)

  #Creating empty effects matrices
  effectMatrices <- list()
  length(effectMatrices) <- nEffect
  names(effectMatrices) <- effectsNamesUnique

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
    selection <- which(effectsNamesAll == effectsNamesUnique[iEffect])
    selectionComplement <- which(effectsNamesAll != effectsNamesUnique[iEffect])
    #Effect matrices
    effectMatrices[[iEffect]] <- t(plyr::aaply(parameters, 2, function(xx) as.matrix(modelMatrix[, selection])%*%xx[selection]))
    colnames(effectMatrices[[iEffect]]) <- colnames(outcomes)
    }


  resLmwEffectMatrices = list(lmwDataList = lmwDataList,
                             modelMatrix = modelMatrix,
                             modelMatrixByEffect = modelMatrixByEffect,
                             effectsNamesUnique = effectsNamesUnique,
                             effectsNamesAll = effectsNamesAll,
                             effectMatrices = effectMatrices,
                             predictedvalues = predictedValues,
                             residuals = residuals,
                             parameters = parameters)

  # Compute the Sum of Squares Type 3
  if(SS==TRUE){
    if(is.na(contrastList)){L = contrastSS(resLmwModelMatrix)}else{L = contrastList}
    resLmwSS = lmwSS(resLmwEffectMatrices,L)

    # Plot of the total contribution
    contrib <- as.data.frame(resLmwSS$variationPercentages)
    rownames(contrib) = ModelAbbrev(rownames(contrib))

    plot <- ggplot2::ggplot(data=contrib,
                                          ggplot2::aes(x=reorder(rownames(contrib), -contrib[,1]),
                                                       y=contrib[,1]))+
      ggplot2::geom_bar(stat="identity")+
      ggplot2::xlab("Effects")+
      ggplot2::ylab("Percentage of Variance")+
      ggplot2::theme_bw()

    resLmwEffectMatrices = c(resLmwEffectMatrices,resLmwSS,
                             varPercentagesPlot = list(plot))
  }else{
    resLmwEffectMatrices = c(resLmwEffectMatrices,type3SS=NA,
                             variationPercentages=NA,varPercentagesPlot=NA)
  }

  return(resLmwEffectMatrices)
}
