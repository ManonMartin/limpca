#' @export PCALMEffects
#' @title PCA on the effect matrices
#' @description Run a Principal Component Analysis on the effect matrices
#'
#' @param ResLMEffectMatrices a ResLMEffectMatrices list from \code{\link{LMEffectMatrices}}
#' @param method The method use to compute the PCA. One of \code{c("ASCA","APCA","ASCA-E")}
#'
#' @return A list of PCA results from \code{\link{SVDforPCA}} for each effect matrix. Those results contain :
#'  \describe{
#'   \item{\code{scores}}{Scores from the PCA for each component}
#'   \item{\code{loadings}}{Loadings from the PCA for each component}
#'   \item{\code{eigval}}{Eigenvalues of each component}
#'   \item{\code{pcd}}{Singular values}
#'   \item{\code{pcu}}{Normalized scores}
#'   \item{\code{var}}{Explained variance}
#'   \item{\code{cumvar}}{Cumulated explained variance}
#'   \item{\code{original.dataset}}{Original dataset}
#'  }
#'
#' @examples
#'  data('UCH')
#'  ResLMModelMatrix = LMModelMatrix(formula=as.formula(UCH$formula),design=UCH$design)
#'  ResLMEffectMatrices = LMEffectMatrices(ResLMModelMatrix,outcomes=UCH$outcomes)
#'  ResPCALMEffects = PCALMEffects(ResLMEffectMatrices,method="ASCA-E")
#'  DrawScores(ResPCALMEffects$Hippurate,type.obj="PCA")
#'

PCALMEffects = function(ResLMEffectMatrices,method=c("ASCA","APCA","ASCA-E")){

  # Construction of the list of pure effect matrix

  EffectMatGLM <- ResLMEffectMatrices$effectMatrices[-1]  # minus intercept
  res <- vector(mode = "list")
  res[[1]] <- ResLMEffectMatrices$residuals
  EffectMatGLM <- c(EffectMatGLM, Residuals = res)  # plus residuals
  p = length(ResLMEffectMatrices$covariateEffectsNamesUnique) # Number of parameters

  # Defining the matrix for the PCA depending on the method

  if(method=="ASCA"){

    print("ASCA method used : PCA on the pure effect matrices")

  }else if(method=="APCA"){

    print("APCA method used : PCA on the augmented effect matrices")

    # Compute the augmented effect matrices

    EffectMatGLM = ResLMEffectMatrices$effectMatrices[-1]  # effectMatrices minus intercept
    EffectMatGLM = lapply(EffectMatGLM, function(x) x + ResLMEffectMatrices$residuals)
    EffectMatGLM = c(EffectMatGLM, residuals = res)  # plus residuals

  }else if(method=="ASCA-E"){

    print("ASCA-E method used : PCA on the pure effect matrices but scores are updated")

  }else{

    stop("The method argument is not one of those : ASCA, APCA, ASCA-E")

  }


# Run the PCA on the different effects

  ResPCALMEffects = vector(mode="list")

  for(i in 1:p){
    ResPCALMEffects[[i]]=SVDforPCA(EffectMatGLM[[i]])
  }

  # Updating the score for ASCA-E method

  if(method=="ASCA-E"){
    for(i in 1:p){

      ResPCALMEffects[[i]]$scores[,1:5] = (EffectMatGLM[[i]] + EffectMatGLM[[p]]) %*% ResPCALMEffects[[i]]$loadings[,1:5]
    }
  }

  names(ResPCALMEffects) = names(EffectMatGLM)

  ResPCALMEffects = utils::modifyList(ResPCALMEffects,list(method=method))

  return(ResPCALMEffects)
}
