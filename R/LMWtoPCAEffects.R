#' @export LMWtoPCAEffects
#' @title PCA on the effect matrices
#' @description Runs a Principal Component Analysis on the effect matrices and adapts the result according to the method.
#'
#' @param ResLMWEffectMatrices A ResLMWEffectMatrices list from \code{\link{LMWEffectMatrices}}
#' @param method The method used to compute the PCA. One of \code{c("ASCA","APCA","ASCA-E")}
#' @param combineEffects A vector containing the names of the effects to be combined.
#'
#' @return A list of PCA results from \code{\link{SVDforPCA}} for each effect matrix. Those results contain :
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
#'  \item{\code{LMWiRe_data_list}}{A list containing the outcomes, the experimental design and the formula.}
#'  \item{\code{covariateEffectsNamesUnique}}{A character vector with the \emph{p} unique names of the model terms.}
#'  \item{\code{method}}{The method used to compute the PCA. One of \code{c("ASCA","APCA","ASCA-E").}
#'  \item{\code{Type3Residuals}}{A list of matrices with the type III residuals for each model terms.}
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
#' ResLMWModelMatrix = LMWModelMatrix(UCH)
#' ResLMWEffectMatrices = LMWEffectMatrices(ResLMWModelMatrix)
#' ResLMWtoPCAEffects = LMWtoPCAEffects(ResLMWEffectMatrices,method="ASCA-E")
#'  PlotScoresXY(ResLMWtoPCAEffects,UCH$design,EffectVector=c("Hippurate"),
#'                varname.color=c("Citrate"),varname.pch=c("Time"))
#'

LMWtoPCAEffects = function(ResLMWEffectMatrices,method=c("ASCA","APCA","ASCA-E")){

  # Checking the ResLMWEffectMatrices list

  checkname = c("LMWiRe_data_list","ModelMatrix","ModelMatrixByEffect","covariateEffectsNames",
                "covariateEffectsNamesUnique","effectMatrices",
                "predictedvalues","residuals","parameters",
                "SS","variationPercentages")


  if(!is.list(ResLMWEffectMatrices)){stop("Argument ResLMMEffectMatrices is not a list")}
  if(length(ResLMWEffectMatrices)!=11){stop("List does not contain 11 arguments")}
  if(!all(names(ResLMWEffectMatrices)==checkname)){stop("Argument is not a ResLMWEffectMatrices object")}
  if(length(ResLMWEffectMatrices$effectMatrices)!=length(ResLMWEffectMatrices$covariateEffectsNamesUnique)){stop("Number of effect matrices different from the number of effects")}
  if(method %in% c("ASCA","APCA","ASCA-E")){}else{stop("Method must be one of the 3 : ASCA, ASCA-E, APCA")}

  # Attributing name
  LMWiRe_data_list = ResLMWEffectMatrices$LMWiRe_data_list
  covariateEffectsNamesUnique = ResLMWEffectMatrices$covariateEffectsNamesUnique

  # Construction of the list of pure effect matrix

  EffectMatGLM <- ResLMWEffectMatrices$effectMatrices[-1]  # minus intercept
  res <- vector(mode = "list")
  res[[1]] <- ResLMWEffectMatrices$residuals
  EffectMatGLM <- c(EffectMatGLM, Residuals = res)  # plus residuals
  p = length(covariateEffectsNamesUnique) # Number of parameters

  # Defining the matrix for the PCA depending on the method

  if(method=="ASCA"){

    print("ASCA method used : PCA on the pure effect matrices")

  }else if(method=="APCA"){

    print("APCA method used : PCA on the augmented effect matrices")

    # Compute the augmented effect matrices

    EffectMatGLM = ResLMWEffectMatrices$effectMatrices[-1]  # effectMatrices minus intercept
    EffectMatGLM = lapply(EffectMatGLM, function(x) x + ResLMWEffectMatrices$residuals)
    EffectMatGLM = c(EffectMatGLM, Residuals = res)  # plus residuals

  }else if(method=="ASCA-E"){

    print("ASCA-E method used : PCA on the pure effect matrices but scores are updated")

  }else{

    stop("The method argument is not one of those : ASCA, APCA, ASCA-E")

  }


# Run the PCA on the different effects

  ResLMWtoPCAEffects = vector(mode="list")

  for(i in 1:p){
    ResLMWtoPCAEffects[[i]]=SVDforPCA(EffectMatGLM[[i]])
  }

  # Updating the score for ASCA-E method

  if(method=="ASCA-E"){
    for(i in 1:p){

      ResLMWtoPCAEffects[[i]]$scores[,1:5] = (EffectMatGLM[[i]] + EffectMatGLM[[p]]) %*% ResLMWtoPCAEffects[[i]]$loadings[,1:5]
    }
  }

  names(ResLMWtoPCAEffects) = names(EffectMatGLM)

  ResLMWtoPCAEffects2 = list(LMWiRe_data_list = LMWiRe_data_list,
              covariateEffectsNamesUnique = covariateEffectsNamesUnique)

  ResLMWtoPCAEffects = c(ResLMWtoPCAEffects, ResLMWtoPCAEffects2, method = method,
                       Type3Residuals = list(ResLMWEffectMatrices$Type3Residuals),
                       variationPercentages = list(ResLMWEffectMatrices$variationPercentages))

  return(ResLMWtoPCAEffects)
}
