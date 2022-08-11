#' @export lmwPcaEffects
#' @title PCA on the effect matrices
#'
#' @description
#' Performs a PCA on each of the effect matrices from the outputs of \code{\link{lmwEffectMatrices}}. It has an option to determine the method applied: ASCA, APCA or ASCA-E. Combined effects (i.e. linear combinations of original effect matrices) can also be created and decomposed.
#'
#' @param resLmwEffectMatrices A resLmwEffectMatrices list from \code{\link{lmwEffectMatrices}}.
#' @param method The method used to compute the PCA. One of \code{c("ASCA","APCA","ASCA-E")}.
#' @param combineEffects If not \code{NULL}, a list of vectors containing the names of the effects to be combined.
#'
#' @return A list of PCA results from \code{\link{pcaBySvd}} for each effect matrix. Those results contain :
#'  \describe{
#'   \item{\code{scores}}{Scores from the PCA for each principal component.}
#'   \item{\code{loadings}}{Loadings from the PCA for each principal component.}
#'   \item{\code{eigval}}{Eigenvalues of each principal component.}
#'   \item{\code{singvar}}{Singular values of each principal component.}
#'   \item{\code{var}}{Explained variances of each principal component.}
#'   \item{\code{cumvar}}{Cumulated explained variances of each principal component.}
#'   \item{\code{original.dataset}}{Original dataset.}
#'  }
#'
#'  There are also others outputs :
#'  \describe{
#'  \item{\code{lmwDataList}}{The initial object: a list of outcomes, design and formula.}
#'  \item{\code{effectsNamesUnique}}{A character vector with the \emph{p} names of the model terms, each repeated once.}
#'  \item{\code{method}}{The dimension reduction method used: \code{c("ASCA","APCA","ASCA-E")}.}
#'  \item{\code{type3SS}}{A vector with the type III SS for each model term.}
#'  \item{\code{variationPercentages}}{A vector with the percentage of variance explained by each model term.}
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


lmwPcaEffects = function(resLmwEffectMatrices, method=c("ASCA","APCA","ASCA-E"),
                         combineEffects=NULL){

  # Checking the resLmwEffectMatrices list

  checkname = c("lmwDataList","modelMatrix","modelMatrixByEffect","effectsNamesUnique",
                "effectsNamesAll","effectMatrices",
                "predictedvalues","residuals","parameters",
                "type3SS","variationPercentages", "varPercentagesPlot")


  if(!is.list(resLmwEffectMatrices)){stop("Argument resLmwEffectMatrices is not a list")}
  if(length(resLmwEffectMatrices)!=12){stop("List does not contain 12 arguments")}
  if(!all(names(resLmwEffectMatrices)==checkname)){stop("Argument is not a resLmwEffectMatrices object")}
  if(length(resLmwEffectMatrices$effectMatrices)!=length(resLmwEffectMatrices$effectsNamesUnique)){stop("Number of effect matrices different from the number of effects")}
  # if(!method %in% c("ASCA","APCA","ASCA-E")){stop("Method must be one of: ASCA, ASCA-E, APCA")}
  method <- match.arg(method)
  checkArg(combineEffects,c("list"),can.be.null=TRUE)

  # Attributing name

  lmwDataList = resLmwEffectMatrices$lmwDataList
  effectsNamesUnique = resLmwEffectMatrices$effectsNamesUnique

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
    p = length(effectsNamesUnique) + length(combineEffects)
  } else{
    p = length(effectsNamesUnique) # Number of parameters
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
                           effectsNamesUnique = effectsNamesUnique)

  resLmwPcaEffects = c(resLmwPcaEffects, resLmwPcaEffects2, method = method,
                       type3SS = list(resLmwEffectMatrices$type3SS),
                       variationPercentages = list(resLmwEffectMatrices$variationPercentages))

  return(resLmwPcaEffects)
}
