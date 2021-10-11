#' @export PCAScorePlot
#' @title Scores plots
#'
#' @description
#' Draws scores plots for the SVDforPCA function.
#'
#' @param ResSVDforPCA A list corresponding to the output value of \code{\link{SVDforPCA}}.
#' @param axes A numerical vector with the 2 Principal Components axes to be drawn.
#' @param title Plot title.
#' @param ... additional arguments to be passed to \code{\link{ScatterPlot}}.
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
                         title = "PCA scores plot", ...) {

  # checks ++++++++++++++++++
  checkArg(ResSVDforPCA,c("list"),can.be.null = FALSE)
  checkArg(axes,c("num"),can.be.null = FALSE)
  checkArg(title,c("str", "length1"),can.be.null = FALSE)
  
  if (!identical(names(ResSVDforPCA),c("scores","loadings","eigval","pcu",
                      "pcd", "var","cumvar","original.dataset"))){
    stop("ResSVDforPCA is not an output value of SVDforPCA")}
   
  if (length(axes) !=2){
    stop("axes is not of length 2")
  }
  
  # scores   ++++++++++++++++++
  scores <- ResSVDforPCA$scores
  
  # percentage of explained variance  ++++++++++++++++++
  pc_var <- ResSVDforPCA$var
  pc_var_x <- format(pc_var[pc_var>=0.1],digits = 2, trim=TRUE)
  pc_var_y <- format(pc_var[pc_var<0.1],digits = 2, 
                     scientific = TRUE, trim=TRUE)
  pc_var_char <- as.character(pc_var)
  pc_var_char[pc_var>=0.1] <- pc_var_x
  pc_var_char[pc_var<0.1] <- pc_var_y
  
  pc_var_char <- paste0("PC", axes, " (",pc_var_char[axes], "%)")
  
  # graphical parameters  ++++++++++++++++++
  xlab <- pc_var_char[1]
  ylab <- pc_var_char[2]
  
  xlim1 <- max(abs(scores[,axes[1]]))
  xlim_val <- c(-xlim1,xlim1)
  
  ylim1 <- max(abs(scores[,axes[2]]))
  ylim_val <-  c(-ylim1,ylim1)
  
  # Scores plot ++++++++++++++++++
  
  ScatterPlot(outcomes = scores, title = title,
              xy = axes, xlab = xlab, ylab = ylab, ...) +
    xlim(xlim_val) + ylim(ylim_val)
  
}
