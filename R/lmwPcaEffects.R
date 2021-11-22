#' @export lmwPcaEffects
#' @title PCA on the effect matrices
#'
#' @description
#' Runs a Principal Component Analysis on the effect matrices and adapts the result according to the method.
#'
#' @param resLmwEffectMatrices A resLmwEffectMatrices list from \code{\link{lmwEffectMatrices}}
#' @param method The method used to compute the PCA. One of \code{c("ASCA","APCA","ASCA-E")}
#' @param combineEffects If not \code{NULL}, a list of vectors containing the names of the effects to be combined.
#'
#' @return A list of PCA results from \code{\link{pcaBySvd}} for each effect matrix. Those results contain :
#'  \describe{
#'   \item{\code{scores}}{Scores from the PCA for each of the n components.}
#'   \item{\code{loadings}}{Loadings from the PCA for each of the n components.}
#'   \item{\code{eigval}}{Eigenvalues of each of the n components.}
#'   \item{\code{pcu}}{\emph{nxn} matrix of normalized scores.}
#'   \item{\code{pcd}}{Singular values of each of the n components.}
#'   \item{\code{var}}{Explained variance of each of the n components.}
#'   \item{\code{cumvar}}{Cumulated explained variance of each of the n components.}
#'   \item{\code{original.dataset}}{Original dataset}
#'  }
#'  There are also others outputs :
#'  \describe{
#'  \item{\code{lmwDataList}}{A list containing the outcomes, the experimental design and the formula.}
#'  \item{\code{covariateEffectsNamesUnique}}{A character vector with the \emph{p} unique names of the model terms.}
#'  \item{\code{method}}{The method used to compute the PCA. One of \code{c("ASCA","APCA","ASCA-E").}
#'  \item{\code{SS}}{A list of matrices with the type III residuals for each model terms.}
#'  \item{\code{variationPercentages}}{A \emph{p} named vector with the variation percentages of each model terms.}
#'  }
#'
#' @details
#'  The function allows 3 different methods :
#'
#'   \describe{
#'   \item{ASCA}{The PCA is applied directly on the pure effect matrix.}
#'   \item{ASCA-E}{The PCA is applied directly on the pure effect matrix but scores are updated.}
#'   \item{APCA}{The PCA is applied on the augmented effect matrix.}
#'  }
#' The ASCA-E method adds the residuals to the scores. The APCA method adds the residuals to the effect matrix before the PCA.
#'
#' @examples
#' data('UCH')
#' resLmwModelMatrix = lmwModelMatrix(UCH)
#' resLmwEffectMatrices = lmwEffectMatrices(resLmwModelMatrix)
#' resLmwPcaEffects = lmwPcaEffects(resLmwEffectMatrices, method="ASCA-E")
#'
#' PlotScoresXY(resLmwPcaEffects, UCH$design, EffectVector=c("Hippurate"),
#'              varname.color=c("Citrate"), varname.pch=c("Time"))


lmwPcaEffects = function(resLmwEffectMatrices, method=c("ASCA","APCA","ASCA-E"),
                         combineEffects=NULL){

  # Checking the resLmwEffectMatrices list

  checkname = c("lmwDataList","ModelMatrix","ModelMatrixByEffect","covariateEffectsNames",
                "covariateEffectsNamesUnique","effectMatrices",
                "predictedvalues","residuals","parameters",
                "SS","variationPercentages")


  if(!is.list(resLmwEffectMatrices)){stop("Argument resLmwEffectMatrices is not a list")}
  if(length(resLmwEffectMatrices)!=11){stop("List does not contain 11 arguments")}
  if(!all(names(resLmwEffectMatrices)==checkname)){stop("Argument is not a resLmwEffectMatrices object")}
  if(length(resLmwEffectMatrices$effectMatrices)!=length(resLmwEffectMatrices$covariateEffectsNamesUnique)){stop("Number of effect matrices different from the number of effects")}
  if(method %in% c("ASCA","APCA","ASCA-E")){}else{stop("Method must be one of the 3 : ASCA, ASCA-E, APCA")}

  # Attributing name
  lmwDataList = resLmwEffectMatrices$lmwDataList
  covariateEffectsNamesUnique = resLmwEffectMatrices$covariateEffectsNamesUnique

  # Combining effects

  if(!is.null(combineEffects)){

    combineMatrices <- function(combineVectors){
      matList <- list()
      combinedEffects <- list()
      matList <- lapply(combineVectors, function(x) resLmwEffectMatrices$effectMatrices[[x]])
      combinedEffects <- Reduce('+', matList)
    }

    combinedMatrices <- lapply(combineEffects, combineMatrices)
    names(combinedMatrices) <- lapply(combineEffects, FUN = function(x) paste0(x, collapse = "+"))

    resLmwEffectMatrices$effectMatrices <- c(resLmwEffectMatrices$effectMatrices,
                                           combinedMatrices)
  }


  # Construction of the list of pure effect matrix

  EffectMatGLM <- resLmwEffectMatrices$effectMatrices[-1]  # minus intercept
  res <- vector(mode = "list")
  res[[1]] <- resLmwEffectMatrices$residuals
  EffectMatGLM <- c(EffectMatGLM, Residuals = res)  # plus residuals

  if(!is.null(combineEffects)){
    p = length(covariateEffectsNamesUnique) + length(combineEffects)
  } else{
    p = length(covariateEffectsNamesUnique) # Number of parameters
  }


  # Defining the matrix for the PCA depending on the method

  if(method=="ASCA"){

    print("ASCA method used : PCA on the pure effect matrices")

  }else if(method=="APCA"){

    print("APCA method used : PCA on the augmented effect matrices")

    # Compute the augmented effect matrices

    EffectMatGLM = resLmwEffectMatrices$effectMatrices[-1]  # effectMatrices minus intercept
    EffectMatGLM = lapply(EffectMatGLM, function(x) x + resLmwEffectMatrices$residuals)
    EffectMatGLM = c(EffectMatGLM, Residuals = res)  # plus residuals

  }else if(method=="ASCA-E"){

    print("ASCA-E method used : PCA on the pure effect matrices but scores are updated")

  }else{

    stop("The method argument is not one of those : ASCA, APCA, ASCA-E")

  }


# Run the PCA on the different effects

  resLmwPcaEffects = vector(mode="list")

  for(i in 1:p){
    resLmwPcaEffects[[i]]=pcaBySvd(EffectMatGLM[[i]])
  }

  # Updating the score for ASCA-E method

  if(method=="ASCA-E"){
    for(i in 1:p){

      resLmwPcaEffects[[i]]$scores[,1:5] = (EffectMatGLM[[i]] + EffectMatGLM[[p]]) %*% resLmwPcaEffects[[i]]$loadings[,1:5]
    }
  }

  names(resLmwPcaEffects) = names(EffectMatGLM)

  resLmwPcaEffects2 = list(lmwDataList = lmwDataList,
                           covariateEffectsNamesUnique = covariateEffectsNamesUnique)

  resLmwPcaEffects = c(resLmwPcaEffects, resLmwPcaEffects2, method = method,
                       SS = list(resLmwEffectMatrices$SS),
                       variationPercentages = list(resLmwEffectMatrices$variationPercentages))

  return(resLmwPcaEffects)
}
