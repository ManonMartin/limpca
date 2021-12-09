#' @export lmwScorePlot
#' @title Scores plots
#'
#' @description
#' Draws scores plots for the lmwPcaEffects function.
#'
#' @param resLmwPcaEffects A list corresponding to the output value of \code{\link{lmwPcaEffects}}.
#' @param effectNames Names of the effects to be plotted. If `NULL`, all the effects are plotted.
#' @param axes A numerical vector with the 2 Principal Components axes to be drawn.
#' @param ... Additional arguments to be passed to \code{\link{plotScatter}}.
#'
#' @return A list of scores plots.
#'
#' @details
#' `lmwScorePlot` is a wrapper of \code{\link{plotScatter}}.
#'
#'
#' @examples
#'
#' data('UCH')
#' resLmwModelMatrix = lmwModelMatrix(UCH)
#' resLmwEffectMatrices = lmwEffectMatrices(resLmwModelMatrix)
#' resASCA = lmwPcaEffects(resLmwEffectMatrices)
#'
#' lmwScorePlot(resASCA, effectNames = "Hippurate",
#' color = "Hippurate", shape = "Hippurate")
#'
#' lmwScorePlot(resASCA, effectNames = "Hippurate:Time",
#' color = "Hippurate", shape = "Time")
#'

lmwScorePlot <- function(resLmwPcaEffects, effectNames = NULL,
                         axes = c(1,2), ...) {

  # checks ===================
  checkArg(resLmwPcaEffects,c("list"),can.be.null = FALSE)
  checkArg(effectNames,c("str"),can.be.null = TRUE)
  checkArg(axes,c("int","pos"),can.be.null = FALSE)

  if(!all(effectNames%in%names(resLmwPcaEffects))){stop("One of the effects from effectNames is not in resLmwPcaEffects.")}


  ### Add a check to verify that the first argument is a ResLMWtoPCA !

  if(is.null(effectNames)){
    effectNames <- c(resLmwPcaEffects$covariateEffectsNamesUnique)[-1] #Ajouter combinedEffects
  }

  # scores
  scores <- lapply(effectNames, function(x) resLmwPcaEffects[[x]][["scores"]])
  names(scores) <- effectNames

  # checkArg(scores[[effectNames]],c("matrix"),can.be.null = FALSE)

  if (length(axes) !=2){
    stop("axes is not of length 2")
  }

  if (max(axes) > ncol(scores[[effectNames[1]]])){
    stop(paste0("axes (",paste0(axes, collapse = ",")
                ,") is beyond the ncol of scores (",ncol(scores),")"))
  }

  # percentage of explained variance   ===================

  pc_var_fun <- function(effect){

    pc_var <- resLmwPcaEffects[[effect]][["var"]]
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

    title = paste(resLmwPcaEffects$method, "scores plot :", effect, "effect")

    xlab <- pc_axes[[effect]][1]
    ylab <- pc_axes[[effect]][2]

    xlim_val = c(1.4*min(resLmwPcaEffects[[effect]][["scores"]][,axes[1]]),
                 1.4*max(resLmwPcaEffects[[effect]][["scores"]][,axes[1]]))

    # Checking the second component
    if(resLmwPcaEffects[[effect]][["var"]][axes[2]]<1){
      warning("The variance of PC2 is inferior to 1%. Graph scaled")
      ylim_val = c(100*min(resLmwPcaEffects[[effect]][["scores"]][,axes[2]]),
                   100*max(resLmwPcaEffects[[effect]][["scores"]][,axes[2]]))
    }else{
      ylim_val = c(1.4*min(resLmwPcaEffects[[effect]][["scores"]][,axes[2]]),
                   1.4*max(resLmwPcaEffects[[effect]][["scores"]][,axes[2]]))
    }

    # Building plots
    fig <- plotScatter(X = scores[[effect]], xy = axes,
                       xlab = xlab, ylab = ylab,
                       design = resLmwPcaEffects$lmwDataList$design, ...)

    fig <- fig + ylim(ylim_val) + xlim(xlim_val) + ggtitle(title)
  }

  # Scores plot  ===================

  fig <- lapply(effectNames, buildFig)
  names(fig) <- effectNames

  return(fig)

}
