#' @export PCAScorePlot
#' @title Scores plots
#'
#' @description
#' Draws scores plots for the SVDforPCA function.
#'
#' @param ResSVDforPCA A list corresponding to the output value of \code{\link{SVDforPCA}}.
#' @param axes A numerical vector with the 2 Principal Components axes to be drawn.
#' @param title Plot title.
#' @param points_labs_rn Boolean indicating if the rownames of the scores matrix should be plotted.
#' @param ... Additional arguments to be passed to \code{\link{ScatterPlot}}.
#'  
#' @return A PCA scores plot.
#' 
#' @details
#' `PCAScorePlot` is a wrapper of \code{\link{ScatterPlot}}.
#' 
#' 
#' @examples
#'
#' data('UCH')
#' ResPCA = SVDforPCA(UCH$outcomes)
#' 
#' PCAScorePlot(ResSVDforPCA = ResPCA, axes = c(1,2), 
#' title = "PCA scores plot UCH", design = UCH$design,
#' color = "Hippurate", shape = "Citrate")
#'

PCAScorePlot <- function(ResSVDforPCA, axes = c(1,2), 
                         title = "PCA scores plot", 
                         points_labs_rn = FALSE, ...) {

  # checks ===================
  checkArg(ResSVDforPCA,c("list"),can.be.null = FALSE)
  checkArg(axes,c("int","pos"),can.be.null = FALSE)
  checkArg(title,c("str", "length1"),can.be.null = FALSE)
  checkArg(points_labs_rn,c("bool"),can.be.null = FALSE)
  
  if (!identical(names(ResSVDforPCA),c("scores","loadings","eigval","pcu",
                      "pcd", "var","cumvar","original.dataset"))){
    stop("ResSVDforPCA is not an output value of SVDforPCA")}
   
  # scores 
  scores <- ResSVDforPCA$scores
  checkArg(scores,c("matrix"),can.be.null = FALSE)
  
  if (length(axes) !=2){
    stop("axes is not of length 2")
  }
  
  if (max(axes) > ncol(scores)){
    stop(paste0("axes (",paste0(axes, collapse = ",")
                ,") is beyond the ncol of scores (",ncol(scores),")"))
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
  
  # graphical parameters   ===================
  xlab <- pc_var_char[1]
  ylab <- pc_var_char[2]
  
  xlim1 <- max(abs(scores[,axes[1]]))
  xlim_val <- c(-xlim1,xlim1)
  
  ylim1 <- max(abs(scores[,axes[2]]))
  ylim_val <-  c(-ylim1,ylim1)
  
  if (points_labs_rn){
    fig <- ScatterPlot(X = scores, title = title,
                       xy = axes, xlab = xlab, ylab = ylab, 
                       points_labs = rownames(scores),
                       ...) 
  } else {
    fig <- ScatterPlot(X = scores, title = title,
                       xy = axes, xlab = xlab, ylab = ylab, ...) 
  }
  
  # Scores plot  ===================
  

  
  fig <- fig + xlim(xlim_val) + ylim(ylim_val)
  
  return(fig)
  
}
