#' @export PCALoadingPlot
#' @title Loadings plots
#'
#' @description
#' Draws loadings plots for the SVDforPCA function.
#'
#' @param ResSVDforPCA A list corresponding to the output value of \code{\link{SVDforPCA}}.
#' @param axes A numerical vector with the Principal Components axes to be drawn.
#' @param title Plot title.
#' @param ... Additional arguments to be passed to \code{\link{LinePlot}}.
#'
#' @return A PCA loadings plot.
#'
#' @details
#' `PCALoadingPlot` is a wrapper of \code{\link{LinePlot}}.
#'
#'
#' @examples
#'
#' data('UCH')
#' ResPCA = SVDforPCA(UCH$outcomes)
#'
#' PCALoadingPlot(ResSVDforPCA = ResPCA, axes = c(1,2),
#' title = "PCA loadings plot UCH", xlab = "ppm", ylab = "Values")
#'

PCALoadingPlot <- function(ResSVDforPCA, axes = c(1,2),
                         title = "PCA loadings plot", ...) {

  # checks   ===================

  checkArg(ResSVDforPCA,c("list"),can.be.null = FALSE)
  checkArg(axes,c("int","pos"),can.be.null = FALSE)
  checkArg(title,c("str", "length1"),can.be.null = FALSE)

  if (!identical(names(ResSVDforPCA),c("scores","loadings","eigval","pcu",
                                       "pcd", "var","cumvar","original.dataset"))){
    stop("ResSVDforPCA is not an output value of SVDforPCA")}

  # loadings
  loadings <- t(ResSVDforPCA$loadings)
  checkArg(loadings,c("matrix"),can.be.null = FALSE)

  if (max(axes) > nrow(loadings)){
    stop(paste0("axes (",paste0(axes, collapse = ",")
                ,") is beyond the ncol of loadings (",nrow(loadings),")"))
  }

  # percentage of explained variance   ===================

  pc_var <- ResSVDforPCA$var
  pc_var_x <- format(pc_var[pc_var>=0.1],digits = 2, trim=TRUE)
  pc_var_y <- format(pc_var[pc_var<0.1],digits = 2,
                     scientific = TRUE, trim=TRUE)
  pc_var_char <- as.character(pc_var)
  pc_var_char[pc_var>=0.1] <- pc_var_x
  pc_var_char[pc_var<0.1] <- pc_var_y

  pc_var_char <- paste0("PC", axes, " (",pc_var_char[axes], "%)")

  rownames(loadings)[axes] <- pc_var_char

  # Loadings plot  ===================

  fig <- LinePlot(X = loadings, title = title,
                       rows = axes,
                       ...)


  return(fig)

}
