#' @export lmpOutcomesReduct
#' @title Reduce the number of columns in the outcomes matrix
#'
#' @description
#' Reduce the number of columns in the outcomes matrix with the PCA.
#' This function modify the \emph{lmpDataList} object by adding the loadings from PCA \emph{loadingsPCA} and a boolean \emph{isReduct} is TRUE to specify that the matrix has indeed been reduced.
#' \emph{outcomes} are replaced by the scores of the PCA, and a \emph{outcomesRaw} retains the initial outcomes.
#' It is possible to choose the number of principal components with the \emph{nPC} parameter. If this parameter is not specified, the function will take the first principal components to achieve a cumulative variance of 0.99.
#'
#' @param lmpDataList A list containing the outcomes, the experimental design, the formula and the type of model (model = 'lm' for model linear and model = 'lmm' for model linear mixt).
#' @param nPC The number of first principal components
#'
#' @return A list with the 3 following named elements and the result of PCA :
#' \describe{
#'    \item{\code{lmpDataList}}{a list with outcomes (scores of PCA), design, formula, type of model, old outcomes, loadings PCA and Boolean \emph{isReduct"} = TRUE }
#'    \item{\code{resPCA}}{a list with result of PCA}
#'    \item{\code{nPC}}{The number of first principal components}
#' }
#'
#'
#'
lmpOutcomesReduct <- function(lmpDataList, nPC = NULL){
  outcomes <- lmpDataList$outcomes
  
  # checks parameters
  checkArg(outcomes, "matrix", can.be.null = FALSE)
  checkArg(nPC, c("num", "pos", "length1"), can.be.null = TRUE)
  
  if(!is.null(nPC)){
    # Cheking nPC
    if(nPC > dim(outcomes)[[2]]){
      stop(paste("The number of princpal component must be smaller than colonne of outcomes."))
    }
    
    # Apply the PCA on the outcomes with the nPC first components principal
    resPCA <- pcaBySvd(outcomes, nPC)
    
  } else {
    # Take the first principal components to achieve a cumulative variance of 99%.
    # Apply the PCA on the outcomes
    resPCA <- pcaBySvd(outcomes)
    
    # Delete the PCs that have a cumulative explained variance that exceeds a cumulative variance of 99%
    nPC <- min(which(resPCA$cumvar > 99))
    resPCA$scores <- resPCA$scores[,1:nPC]
    resPCA$loadings <- resPCA$loadings[,1:nPC]
    resPCA$eigval <- resPCA$eigval[1:nPC]
    resPCA$singvar <- resPCA$singvar[1:nPC]
    resPCA$var <- resPCA$var[1:nPC]
    resPCA$cumvar <- resPCA$cumvar[1:nPC]
  }
    
  # Modify lmpDataList
  lmpDataList$outcomes <- resPCA$scores
  lmpDataList$isReduct <- TRUE
  lmpDataList$outcomesRaw <- resPCA$original.dataset
    
  # Add loadings to retrieve the real outcomes
  lmpDataList$loadingsPCA <- resPCA$loadings
    
  # Rename rows of the new outcomes
  rownames(lmpDataList$outcomes) <- rownames(lmpDataList$raw_outcomes)
  
  reslmpOutcomesReduct <- list(
    lmpDataList = lmpDataList,
    resPCA = resPCA,
    nPC = nPC
  )
  
  return(reslmpOutcomesReduct)
}
  