#' @export lmpLoading1dPlot
#' @title Loadings represented on a line plot.
#'
#' @description
#' Plots the loading vectors for each effect matrix from the \code{\link{lmpPcaEffects}} outputs with line plots. This is a wrapper of plotLine.
#'
#' @param resLmpPcaEffects A list corresponding to the output value of \code{\link{lmpPcaEffects}}.
#' @param effectNames Names of the effects to be plotted. if `NULL`, all the effects are plotted.
#' @param axes A numerical vector with the Principal Components axes to be drawn.
#' @param ... Additional arguments to be passed to \code{\link{plotLine}} such as `xaxis_type`, `type` or `ang_x_axis`.
#'
#' @return A list of `ggplot` objects representing the loading plots.
#'
#' @details
#' `lmpLoading1dPlot` is a wrapper of \code{\link{plotLine}}. See `?plotLine` for more information on the additional arguments.
#'
#'
#' @examples
#'
#' # Example of "spectral" type loadings (line and numerical x-axis)
#' data('UCH')
#' resLmpModelMatrix = lmpModelMatrix(UCH)
#' resLmpEffectMatrices = lmpEffectMatrices(resLmpModelMatrix)
#' resASCA = lmpPcaEffects(resLmpEffectMatrices)
#' lmpLoading1dPlot(resASCA, effectNames = c("Hippurate", "Citrate"))
#'
#' # Example of "segment" and discrete type loadings (segments and character x-axis)
#' data('trout')
#' resLmpModelMatrix = lmpModelMatrix(trout)
#' resLmpEffectMatrices = lmpEffectMatrices(resLmpModelMatrix)
#' resASCA = lmpPcaEffects(resLmpEffectMatrices)
#' lmpLoading1dPlot(resASCA,effectNames="Day",xaxis_type="character",type="s",ang_x_axis=90)


lmpLoading1dPlot <- function(resLmpPcaEffects, effectNames = NULL,
                           axes = c(1,2), ...) {

  # checks   ===================

  checkArg(resLmpPcaEffects,c("list"),can.be.null = FALSE)
  checkArg(effectNames,c("str"),can.be.null = TRUE)
  checkArg(axes,c("int","pos"),can.be.null = FALSE)

  if(!all(effectNames%in%names(resLmpPcaEffects))){stop("One of the effects from effectNames is not in resLmpPcaEffects.")}

  if (!identical(names(resLmpPcaEffects[(length(resLmpPcaEffects)-7):length(resLmpPcaEffects)]),
                 c("Residuals","lmpDataList","effectsNamesUnique","effectsNamesUniqueCombined",
                   "method","type3SS","variationPercentages", "combineEffects"))){
    stop("resLmpPcaEffects is not an output value of lmpPcaEffects")
    }

  if(is.null(effectNames)){
    effectNames <- resLmpPcaEffects$effectsNamesUniqueCombined
    effectNames <- effectNames[effectNames != "Intercept"]
    effectNames <- c(effectNames, "Residuals")
  }



  # loadings

  loadings <- lapply(effectNames, function(x) t(resLmpPcaEffects[[x]][["loadings"]]))
  names(loadings) <- effectNames

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

  lab <- lapply(effectNames, pc_var_fun)
  names(lab) <- effectNames


  # Loadings plot  ===================

  buildFig <- function(effect){
    title = paste(effect, ": loading plot")

    plotLine(Y = loadings[[effect]], title = title, rows = axes, facet_label = lab[[effect]], ...)
  }

  fig <- lapply(effectNames, buildFig)
  names(fig) <- effectNames


  if (length(fig)==1){
    fig <- fig[[1]]
  }

  return(fig)

}
