#' @export pcaScorePlot
#' @title Score plots
#'
#' @description
#' Produces rich score plots from \code{\link{pcaBySvd}} with the same graphical options as \code{\link{plotScatter}}.
#'
#' @param resPcaBySvd A list corresponding to the output value of \code{\link{pcaBySvd}}.
#' @param axes A numerical vector with the 2 Principal Components axes to be drawn.
#' @param title Plot title.
#' @param points_labs_rn Boolean indicating if the rownames of the scores matrix should be plotted.
#' @param ... Additional arguments to be passed to \code{\link{plotScatter}}.
#'
#' @return A PCA score plot (ggplot).
#'
#' @details
#' `pcaScorePlot` is a wrapper of \code{\link{plotScatter}}.
#'
#'
#' @examples
#'
#' data('UCH')
#' ResPCA = pcaBySvd(UCH$outcomes)
#'
#' pcaScorePlot(resPcaBySvd = ResPCA, axes = c(1,2),
#' title = "PCA score plot UCH", design = UCH$design,
#' color = "Hippurate", shape = "Citrate")
#'
#' @import ggplot2

pcaScorePlot <- function(resPcaBySvd, axes = c(1,2),
                         title = "PCA score plot",
                         points_labs_rn = FALSE, ...) {

  mcall = as.list(match.call())[-1L]

  # checks ===================
  checkArg(resPcaBySvd,c("list"),can.be.null = FALSE)
  checkArg(axes,c("int","pos"),can.be.null = FALSE)
  checkArg(title,c("str", "length1"),can.be.null = FALSE)
  checkArg(points_labs_rn,c("bool"),can.be.null = FALSE)

  if (!identical(names(resPcaBySvd),c("scores","loadings","eigval","singvar",
                                      "var","cumvar","original.dataset"))){
    stop("resPcaBySvd is not an output value of pcaBySvd")}

  # scores
  scores <- resPcaBySvd$scores
  checkArg(scores,c("matrix"),can.be.null = FALSE)

  if (length(axes) !=2){
    stop("axes is not of length 2")
  }

  if (max(axes) > ncol(scores)){
    stop(paste0("axes (",paste0(axes, collapse = ",")
                ,") is beyond the ncol of scores (",ncol(scores),")"))
  }


  # percentage of explained variance   ===================
  pc_var <- resPcaBySvd$var
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
    if (!"xlab" %in% names(mcall)){
      if (!"ylab" %in% names(mcall)){
        fig <- plotScatter(Y = scores, title = title,
                           xy = axes, xlab = xlab, ylab = ylab,
                           points_labs = rownames(scores),
                           ...)
      }else{
        fig <- plotScatter(Y = scores, title = title,
                           xy = axes, xlab = xlab,
                           points_labs = rownames(scores),
                           ...)
      }
      }else{
        if (!"ylab" %in% names(mcall)){
          fig <- plotScatter(Y = scores, title = title,
                             xy = axes, ylab = ylab,
                             points_labs = rownames(scores),
                             ...)
        }else{
          fig <- plotScatter(Y = scores, title = title,
                             xy = axes,
                             points_labs = rownames(scores),
                             ...)
        }
      }

  } else {
    if (!"xlab" %in% names(mcall)){
      if (!"ylab" %in% names(mcall)){
        fig <- plotScatter(Y = scores, title = title,
                           xy = axes, xlab = xlab, ylab = ylab,
                           ...)
      }else{
        fig <- plotScatter(Y = scores, title = title,
                           xy = axes, xlab = xlab,
                           ...)
      }
    }else{
      if (!"ylab" %in% names(mcall)){
        fig <- plotScatter(Y = scores, title = title,
                           xy = axes, ylab = ylab,
                           ...)
      }else{
        fig <- plotScatter(Y = scores, title = title,
                           xy = axes,
                           ...)
      }
    }

  }

  # Scores plot  ===================
  fig <- fig + ggplot2::xlim(xlim_val) + ggplot2::ylim(ylim_val)

  return(fig)

}
