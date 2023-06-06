#' @export lmpScorePlot
#' @title Score plots of effect matrices
#'
#' @description
#' Draws the score plots of each (augmented) effect matrix provided in \code{\link{lmpPcaEffects}}. As a wrapper of the \code{\link{plotScatter}} function, it allows to visualize the scores of the effect matrices for two components at a time with all the available options in  \code{\link{plotScatter}}.
#'
#' @param resLmpPcaEffects A list corresponding to the output value of \code{\link{lmpPcaEffects}}.
#' @param effectNames Names of the effects to be plotted. If `NULL`, all the effects are plotted.
#' @param axes A numerical vector with the 2 Principal Components axes to be drawn.
#' @param ... Additional arguments to be passed to \code{\link{plotScatter}}.
#'
#' @return A list of score plots (ggplot).
#'
#' @details
#' `lmpScorePlot` is a wrapper of \code{\link{plotScatter}}.
#'
#'
#' @examples
#'
#' data('UCH')
#' resLmpModelMatrix = lmpModelMatrix(UCH)
#' resLmpEffectMatrices = lmpEffectMatrices(resLmpModelMatrix)
#'
#' # PCA decomposition of effect matrices (ASCA)
#' resASCA = lmpPcaEffects(resLmpEffectMatrices)
#' # Score plot of Hippurate effect matrix
#' lmpScorePlot(resASCA, effectNames = "Hippurate",
#' color = "Hippurate", shape = "Hippurate")
#'
#' # PCA decomposition of augmented effect matrices (APCA)
#' resASCA = lmpPcaEffects(resLmpEffectMatrices,method="APCA")
#' # Score plot of Hippurate augmented effect matrix
#' lmpScorePlot(resASCA, effectNames = "Hippurate",
#' color = "Hippurate", shape = "Hippurate",drawShapes = "ellipse")

lmpScorePlot <- function(resLmpPcaEffects, effectNames = NULL,
                         axes = c(1,2), ...) {

  mcall = as.list(match.call())[-1L]

  # checks ===================
  checkArg(resLmpPcaEffects,c("list"),can.be.null = FALSE)
  checkArg(effectNames,c("str"),can.be.null = TRUE)
  checkArg(axes,c("int","pos"),can.be.null = FALSE)

  if(!all(effectNames%in%names(resLmpPcaEffects))){stop("One of the effects from effectNames is not in resLmpPcaEffects.")}

  if (!identical(names(resLmpPcaEffects[(length(resLmpPcaEffects)-5):length(resLmpPcaEffects)]),
                 c("Residuals","lmpDataList","effectsNamesUnique","method","type3SS","variationPercentages"))){
    stop("resLmpPcaEffects is not an output value of lmpPcaEffects")}

  if(is.null(effectNames)){
    effectNames <- resLmpPcaEffects$effectsNamesUnique
    effectNames <- effectNames[effectNames != "Intercept"]
    effectNames <- c(effectNames, "Residuals")
  }


  # scores
  scores <- lapply(effectNames, function(x) resLmpPcaEffects[[x]][["scores"]])
  names(scores) <- effectNames

  if (length(axes) !=2){
    stop("axes is not of length 2")
  }

  if (max(axes) > ncol(scores[[effectNames[1]]])){
    stop(paste0("axes (",paste0(axes, collapse = ",")
                ,") is beyond the ncol of scores (",ncol(scores),")"))
  }

  # percentage of explained variance   ===================

  pc_var_fun <- function(effect){

    pc_var <- resLmpPcaEffects[[effect]][["var"]]
    pc_var_x <- format(pc_var[pc_var>=0.1],digits = 2, trim=TRUE)
    pc_var_y <- format(pc_var[pc_var<0.1],digits = 2,
                       scientific = TRUE, trim=TRUE)
    pc_var_char <- as.character(pc_var)
    pc_var_char[pc_var>=0.1] <- pc_var_x
    pc_var_char[pc_var<0.1] <- pc_var_y

    pc_var_char <- paste0("PC", axes, " (",pc_var_char[axes], "%)")

    return(pc_var_char)

  }

  pc_axes <- lapply(effectNames, pc_var_fun)
  names(pc_axes) <- effectNames


  # graphical parameters   ===================

  buildFig <- function(effect){

    title = paste(effect, ":", resLmpPcaEffects$method, "score plot")

    xlab <- pc_axes[[effect]][1]
    ylab <- pc_axes[[effect]][2]

    xlim_val = c(1.4*min(resLmpPcaEffects[[effect]][["scores"]][,axes[1]]),
                 1.4*max(resLmpPcaEffects[[effect]][["scores"]][,axes[1]]))

    # Checking the second component
    if(resLmpPcaEffects$method != "APCA"){
      if(resLmpPcaEffects[[effect]][["var"]][axes[2]]<1){
        warning("The variance of PC2 is inferior to 1%. Graph scaled")
        ylim_val = c(100*min(resLmpPcaEffects[[effect]][["scores"]][,axes[2]]),
                     100*max(resLmpPcaEffects[[effect]][["scores"]][,axes[2]]))
      }else{
        ylim_val = c(1.4*min(resLmpPcaEffects[[effect]][["scores"]][,axes[2]]),
                     1.4*max(resLmpPcaEffects[[effect]][["scores"]][,axes[2]]))
      }
    }else{
      ylim_val = c(1.4*min(resLmpPcaEffects[[effect]][["scores"]][,axes[2]]),
                   1.4*max(resLmpPcaEffects[[effect]][["scores"]][,axes[2]]))
    }

    # Building plots

    if (!"xlab" %in% names(mcall)){
      if (!"ylab" %in% names(mcall)){
        fig <- plotScatter(Y = scores[[effect]], xy = axes,
                           xlab = xlab, ylab = ylab,
                           design = resLmpPcaEffects$lmpDataList$design, ...)
      }else {
        fig <- plotScatter(Y = scores[[effect]], xy = axes,
                           xlab = xlab,
                           design = resLmpPcaEffects$lmpDataList$design, ...)
      }
    }else{
      if (!"ylab" %in% names(mcall)){
        fig <- plotScatter(Y = scores[[effect]], xy = axes,
                           ylab = ylab,
                           design = resLmpPcaEffects$lmpDataList$design, ...)
      }else {
        fig <- plotScatter(Y = scores[[effect]], xy = axes,
                           design = resLmpPcaEffects$lmpDataList$design, ...)
      }
    }

    fig <- fig + ylim(ylim_val) + xlim(xlim_val) + ggtitle(title)
  }

  # Scores plot  ===================

  fig <- lapply(effectNames, buildFig)
  names(fig) <- effectNames

  if (length(fig)==1){
    fig <- fig[[1]]
  }


  return(fig)

}
