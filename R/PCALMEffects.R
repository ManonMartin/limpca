#' @export PCALMEffects
#' @title PCA on the effect matrices
#' @description Run a Principal Component Analysis on the effect matrices and adapt the result according to the method.
#'
#' @param ResLMEffectMatrices a ResLMEffectMatrices list from \code{\link{LMEffectMatrices}}
#' @param method The method use to compute the PCA. One of \code{c("ASCA","APCA","ASCA-E")}
#'
#' @return A list of PCA results from \code{\link{SVDforPCA}} for each effect matrix. Those results contain :
#'  \describe{
#'   \item{\code{scores}}{Scores from the PCA for each of the n components}
#'   \item{\code{loadings}}{Loadings from the PCA for each of the n component}
#'   \item{\code{eigval}}{Eigenvalues of each of the n component}
#'   \item{\code{pcd}}{Singular values of each of the n component}
#'   \item{\code{pcu}}{\emph{nxn} matrix of normalized scores}
#'   \item{\code{var}}{Explained variance of each of the n component}
#'   \item{\code{cumvar}}{Cumulated explained variance of each of the n component}
#'   \item{\code{original.dataset}}{Original dataset}
#'  }
#'  There are also others outputs :
#'  \describe{
#'  \item{\code{method}}{}
#'  \item{\code{Type3Residuals}}{}
#'  \item{\code{variationPercentages}}{}
#'  }
#'
#' @details
#'  The function allows 3 different methods :
#'
#'   \describe{
#'   \item{ASCA}{The PCA is applied directly on the pure effect matrix}
#'   \item{ASCA-E}{The PCA is applied directly on the pure effect matrix but scores are updated}
#'   \item{APCA}{The PCA is applied on the augmented effect matrix}
#'  }
#' The ASCA-E method add the residual to the scores. APCA applied add the residuals to the effect matrix before the PCA.
#'
#' @examples
#' data('UCH')
#' ResLMModelMatrix = LMModelMatrix(formula=as.formula(UCH$formula),design=UCH$design)
#' ResLMEffectMatrices = LMEffectMatrices(ResLMModelMatrix,outcomes=UCH$outcomes)
#' ResPCALMEffects = PCALMEffects(ResLMEffectMatrices,method="ASCA-E")
#'  PlotScoresXY(ResPCALMEffects,UCH$design,EffectVector=c("Hippurate"),
#'                varname.color=c("Citrate"),varname.pch=c("Time"))
#'

PCALMEffects = function(ResLMEffectMatrices,method=c("ASCA","APCA","ASCA-E")){

  # Checking the ResLMEffectMatrices list

  checkname = c("formula","design","ModelMatrix","outcomes","effectMatrices","modelMatrixByEffect",
                "predictedvalues","residuals","parameters","covariateEffectsNamesUnique","covariateEffectsNames",
                "Type3Residuals","variationPercentages")


  if(!is.list(ResLMEffectMatrices)){stop("Argument ResLMMEffectMatrices is not a list")}
  if(length(ResLMEffectMatrices)!=13){stop("List does not contain 13 arguments")}
  if(!all(names(ResLMEffectMatrices)==checkname)){stop("Argument is not a ResLMEffectMatrices object")}
  if(length(ResLMEffectMatrices$effectMatrices)!=length(ResLMEffectMatrices$covariateEffectsNamesUnique)){stop("Number of effect matrices different from the number of effects")}
  if(method %in% c("ASCA","APCA","ASCA-E")){}else{stop("Method must be one of the 3 : ASCA, ASCA-E, APCA")}

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

  ResPCALMEffects = c(ResPCALMEffects,method=method,Type3Residuals=list(ResLMEffectMatrices$Type3Residuals),variationPercentages=list(ResLMEffectMatrices$variationPercentages))

  return(ResPCALMEffects)
}
