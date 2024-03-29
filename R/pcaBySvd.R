#' @export pcaBySvd
#' @title Singular Value Decomposition for PCA analysis
#'
#' @description
#' Operates a Principal Component Analysis on the `Y` outcome/response matrix by a singular
#' Value Decomposition (the pre-processing involves the mean-centering of `Y`).
#' Outputs are represented with the functions \code{\link{pcaScorePlot}},
#' \code{\link{pcaLoading1dPlot}}, \code{\link{pcaLoading2dPlot}} and \code{\link{pcaScreePlot}}.
#'
#' @param Y The \eqn{n \times m} matrix with \eqn{n} observations and \eqn{m} (response) variables.
#' @param lmpDataList A list with outcomes, design and formula, as outputted by \code{\link{data2LmpDataList}}.
#' @param nPC Number of Principal Components to extract.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{\code{scores}}{Scores}
#'   \item{\code{loadings}}{Loadings}
#'   \item{\code{eigval}}{Eigenvalues}
#'   \item{\code{singvar}}{Singular values}
#'   \item{\code{var}}{Explained variances}
#'   \item{\code{cumvar}}{Cumulated explained variances}
#'   \item{\code{original.dataset}}{Original dataset}
#'   \item{\code{design}}{Design of the study}
#'
#' }
#'
#' @examples
#'
#' data("UCH")
#'
#' PCA.res1 <- pcaBySvd(Y = UCH$outcomes)
#'
#' PCA.res2 <- pcaBySvd(lmpDataList = UCH)
#'
#' identical(PCA.res1, PCA.res2)
pcaBySvd <- function(Y = NULL, lmpDataList = NULL, nPC = min(dim(Y))) {

  design <- NULL

  if (is.null(Y) & is.null(lmpDataList)) {
    stop("both Y and lmpDataList are NULL, at least one should be non NULL.")
  } else if (!is.null(Y) & !is.null(lmpDataList)) {
    message("both Y and lmpDataList are non-NULL, Y will be used to perform PCA")
  } else if (is.null(Y) & !is.null(lmpDataList)) {
    lmpDataListCheck(lmpDataList, null_formula = TRUE,
                     null_design = TRUE)
    Y <- lmpDataList$outcomes
    design <- lmpDataList$design
  }


  checkArg(Y, "matrix", can.be.null = TRUE)
  checkArg(nPC, c("num", "pos", "length1"), can.be.null = FALSE)

  # PCA =========================
  original.dataset <- Y
  outcomes <- Y
  # column centering of outcomes
  outcomes <- outcomes - matrix(apply(outcomes, 2, mean),
    nrow = dim(outcomes)[1],
    ncol = dim(outcomes)[2],
    byrow = TRUE
  ) # centering of outcomes over the columns

  # SVD of outcomes
  outcomes.svd <- svd(outcomes) # Compute the singular-value decomposition
  outcomes.scores <- outcomes.svd$u %*% diag(outcomes.svd$d) # scores
  outcomes.normscores <- outcomes.svd$u # normalised scores
  outcomes.loadings <- outcomes.svd$v # loadings
  outcomes.singularval <- outcomes.svd$d # singular values
  names(outcomes.singularval) <- paste0("PC", seq_along(outcomes.singularval))

  # X-Variance explained
  outcomes.vars <- outcomes.singularval^2 / (nrow(outcomes) - 1)
  outcomes.eigval <- outcomes.singularval^2
  names(outcomes.eigval) <- paste0("PC", seq_along(outcomes.eigval))

  outcomes.totalvar <- sum(outcomes.vars)
  outcomes.relvars <- outcomes.vars / outcomes.totalvar

  outcomes.variances <- 100 * outcomes.relvars # variance
  names(outcomes.variances) <- paste0(
    "PC",
    seq_along(outcomes.variances)
  )

  outcomes.cumvariances <- cumsum(outcomes.variances) # cumulative variance
  names(outcomes.cumvariances) <- paste0(
    "PC",
    seq_along(outcomes.cumvariances)
  )

  # as matrix

  outcomes.scores <- as.matrix(outcomes.scores)
  dimnames(outcomes.scores) <- list(
    rownames(outcomes),
    paste0("PC", seq_len(ncol(outcomes.scores)))
  )

  outcomes.normscores <- as.matrix(outcomes.normscores)
  dimnames(outcomes.normscores) <- list(
    rownames(outcomes),
    paste0("PC", seq_len(ncol(outcomes.normscores)))
  )

  outcomes.loadings <- as.matrix(outcomes.loadings)
  dimnames(outcomes.loadings) <- list(
    colnames(outcomes),
    paste0("PC", seq_len(ncol(outcomes.loadings)))
  )


  # selection of the first n components

  outcomes.scores <- outcomes.scores[, seq_len(nPC)]
  outcomes.normscores <- outcomes.normscores[, seq_len(nPC)]
  outcomes.loadings <- outcomes.loadings[, seq_len(nPC)]
  outcomes.singularval <- outcomes.singularval[seq_len(nPC)]

  outcomes.variances <- outcomes.variances[seq_len(nPC)]
  outcomes.cumvariances <- outcomes.cumvariances[seq_len(nPC)]



  res <- list(
    scores = outcomes.scores, loadings = outcomes.loadings,
    eigval = outcomes.eigval,
    singvar = outcomes.singularval, var = outcomes.variances,
    cumvar = outcomes.cumvariances,
    original.dataset = original.dataset,
    design = design
  )

  return(res)
}
