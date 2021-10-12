#' @export SVDforPCA
#' @title Singular value decomposition for PCA analysis
#'
#' @description
#' PCA by singular value decomposition, the preprocessing involves the mean-centering of X.
#'
#' @param outcomes The nxm matrix with n observations and m response variables.
#' @param ncomp  Number of Principal Components.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{scores}}{Scores}
#'   \item{\code{loadings}}{Loadings}
#'   \item{\code{eigval}}{Eigenvalues}
#'   \item{\code{pcd}}{Singular values}
#'   \item{\code{pcu}}{Normalized scores}
#'   \item{\code{var}}{Explained variance}
#'   \item{\code{cumvar}}{Cumulated explained variance}
#'  \item{\code{original.dataset}}{Original dataset}
#'
#' }
#'
#' @examples
#' data('UCH')
#' PCA.res = SVDforPCA(UCH$outcomes)


SVDforPCA <- function(outcomes, ncomp = min(dim(outcomes))) {


  original.dataset <- outcomes
  # column centering of outcomes
  outcomes <- outcomes - matrix(apply(outcomes, 2, mean), nrow = dim(outcomes)[1], ncol = dim(outcomes)[2], byrow = TRUE)  # centring of outcomes over the columns

  # SVD of outcomes
  outcomes.svd <- svd(outcomes)  # Compute the singular-value decomposition
  outcomes.scores <- outcomes.svd$u %*% diag(outcomes.svd$d)  # scores
  outcomes.normscores <- outcomes.svd$u  # normalised scores
  outcomes.loadings <- outcomes.svd$v  # loadings
  outcomes.singularval <- outcomes.svd$d  # singular values
  names(outcomes.singularval) <- paste0("PC", 1:length(outcomes.singularval))

  # X-Variance explained
  outcomes.vars <- outcomes.singularval^2/(nrow(outcomes) - 1)
  outcomes.eigval <- outcomes.singularval^2
  names(outcomes.eigval) <- paste0("PC", 1:length(outcomes.eigval))

  outcomes.totalvar <- sum(outcomes.vars)
  outcomes.relvars <- outcomes.vars/outcomes.totalvar

  outcomes.variances <- 100 * outcomes.relvars  # variance
  names(outcomes.variances) <- paste0("PC", 1:length(outcomes.variances))

  outcomes.cumvariances <- cumsum(outcomes.variances)  # cumulative variance
  names(outcomes.cumvariances) <- paste0("PC", 1:length(outcomes.cumvariances))

  # as matrix

  outcomes.scores <- as.matrix(outcomes.scores)
  dimnames(outcomes.scores) <- list(rownames(outcomes), paste0("PC", 1:ncol(outcomes.scores)))

  outcomes.normscores <- as.matrix(outcomes.normscores)
  dimnames(outcomes.normscores) <- list(rownames(outcomes), paste0("PC", 1:ncol(outcomes.normscores)))

  outcomes.loadings <- as.matrix(outcomes.loadings)

  dimnames(outcomes.loadings) <- list(colnames(outcomes), paste0("PC", 1:ncol(outcomes.loadings)))


  # selection of the first n components

  outcomes.scores <- outcomes.scores[, 1:ncomp]
  outcomes.normscores <- outcomes.normscores[, 1:ncomp]
  outcomes.loadings <- outcomes.loadings[, 1:ncomp]
  outcomes.singularval <- outcomes.singularval[1:ncomp]

  outcomes.variances <- outcomes.variances[1:ncomp]
  outcomes.cumvariances <- outcomes.cumvariances[1:ncomp]



  res <- list(scores = outcomes.scores, loadings = outcomes.loadings, 
              eigval = outcomes.eigval, pcu = outcomes.normscores,
              pcd = outcomes.singularval, var = outcomes.variances, 
              cumvar = outcomes.cumvariances,
              original.dataset = original.dataset)

  return(res)

}
