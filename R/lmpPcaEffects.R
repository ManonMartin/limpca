#' @export lmpPcaEffects
#' @title PCA on the effect matrices
#'
#' @description
#' Performs a PCA on each of the effect matrices from the outputs of \code{\link{lmpEffectMatrices}}. It has an option to determine the method applied: ASCA, APCA or ASCA-E. Combined effects (i.e. linear combinations of original effect matrices) can also be created and decomposed.
#'
#' @param resLmpEffectMatrices A resLmpEffectMatrices list from \code{\link{lmpEffectMatrices}}.
#' @param method The method used to compute the PCA. One of \code{c("ASCA","APCA","ASCA-E")}.
#' @param combineEffects If not \code{NULL}, a list of vectors containing the names of the effects to be combined.
#' @param verbose If \code{TRUE}, will display a message with the duration of execution.
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
#'  \item{\code{lmpDataList}}{The initial object: a list of outcomes, design and formula.}
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
#' resLmpModelMatrix = lmpModelMatrix(UCH)
#' resLmpEffectMatrices = lmpEffectMatrices(resLmpModelMatrix)
#' resLmpPcaEffects = lmpPcaEffects(resLmpEffectMatrices, method="ASCA-E")


lmpPcaEffects = function(resLmpEffectMatrices, method=c("ASCA","APCA","ASCA-E"),
                         combineEffects=NULL,verbose = FALSE){

  # Checking the resLmpEffectMatrices list

  checkname = c("lmpDataList","modelMatrix","modelMatrixByEffect","effectsNamesUnique",
                "effectsNamesAll","effectMatrices",
                "predictedvalues","residuals","parameters",
                "type3SS","variationPercentages", "varPercentagesPlot")


  if(!is.list(resLmpEffectMatrices)){stop("Argument resLmpEffectMatrices is not a list")}
  if(length(resLmpEffectMatrices)!=12){stop("List does not contain 12 arguments")}
  if(!all(names(resLmpEffectMatrices)==checkname)){stop("Argument is not a resLmpEffectMatrices object")}
  if(length(resLmpEffectMatrices$effectMatrices)!=length(resLmpEffectMatrices$effectsNamesUnique)){stop("Number of effect matrices different from the number of effects")}
  # if(!method %in% c("ASCA","APCA","ASCA-E")){stop("Method must be one of: ASCA, ASCA-E, APCA")}
  method <- match.arg(method)
  checkArg(combineEffects,c("list"),can.be.null=TRUE)

  # Attributing name

  lmpDataList = resLmpEffectMatrices$lmpDataList
  effectsNamesUnique = resLmpEffectMatrices$effectsNamesUnique

  # Combining effects

  if(!is.null(combineEffects)){

    combineMatrices <- function(combineVectors){
      matList <- list()
      combinedEffects <- list()
      matList <- lapply(combineVectors, function(x) resLmpEffectMatrices$effectMatrices[[x]])
      combinedEffects <- Reduce('+', matList)
    }

    combinedMatrices <- lapply(combineEffects, combineMatrices)
    names(combinedMatrices) <- lapply(combineEffects, FUN = function(x) paste0(x, collapse = "+"))

    resLmpEffectMatrices$effectMatrices <- c(resLmpEffectMatrices$effectMatrices,
                                           combinedMatrices)
  }


  # Construction of the list of pure effect matrix

  EffectMatGLM <- resLmpEffectMatrices$effectMatrices[-1]  # minus intercept
  res <- vector(mode = "list")
  res[[1]] <- resLmpEffectMatrices$residuals
  EffectMatGLM <- c(EffectMatGLM, Residuals = res)  # plus residuals

  if(!is.null(combineEffects)){
    p = length(effectsNamesUnique) + length(combineEffects)
  } else{
    p = length(effectsNamesUnique) # Number of parameters
  }


  # Defining the matrix for the PCA depending on the method

  if(method=="ASCA"){
    if (verbose){
      print("ASCA method used : PCA on the pure effect matrices")
    }

  }else if(method=="APCA"){

    if (verbose){
      print("APCA method used : PCA on the augmented effect matrices")
    }

    # Compute the augmented effect matrices

    EffectMatGLM = resLmpEffectMatrices$effectMatrices[-1]  # effectMatrices minus intercept
    EffectMatGLM = lapply(EffectMatGLM, function(x) x + resLmpEffectMatrices$residuals)
    EffectMatGLM = c(EffectMatGLM, Residuals = res)  # plus residuals

  }else if(method=="ASCA-E"){

    if (verbose){
      print("ASCA-E method used : PCA on the pure effect matrices but scores are updated")
    }

  }else{

    stop("The method argument is not one of those : ASCA, APCA, ASCA-E")

  }


# Run the PCA on the different effects

  resLmpPcaEffects = vector(mode="list")

  for(i in 1:p){
    resLmpPcaEffects[[i]]=pcaBySvd(EffectMatGLM[[i]])
  }

  # Updating the score for ASCA-E method

  if(method=="ASCA-E"){
    for(i in 1:p){

      resLmpPcaEffects[[i]]$scores[,1:5] = (EffectMatGLM[[i]] + EffectMatGLM[[p]]) %*% resLmpPcaEffects[[i]]$loadings[,1:5]
    }
  }

  names(resLmpPcaEffects) = names(EffectMatGLM)

  resLmpPcaEffects2 = list(lmpDataList = lmpDataList,
                           effectsNamesUnique = effectsNamesUnique)

  resLmpPcaEffects = c(resLmpPcaEffects, resLmpPcaEffects2, method = method,
                       type3SS = list(resLmpEffectMatrices$type3SS),
                       variationPercentages = list(resLmpEffectMatrices$variationPercentages))

  return(resLmpPcaEffects)
}
