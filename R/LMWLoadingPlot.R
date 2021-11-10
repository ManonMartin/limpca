#' @export LMWLoadingPlot
#' @title Loadings plots
#'
#' @description
#' Draws loadings plots for the LMWtoPCAEffects function.
#'
#' @param ResLMWtoPCAEffects A list corresponding to the output value of \code{\link{LMWtoPCAEffects}}.
#' @param effectNames Names of the effects to be plotted. if `NULL`, all the effects are plotted.
#' @param axes A numerical vector with the Principal Components axes to be drawn.
#' @param ... Additional arguments to be passed to \code{\link{LinePlot}}.
#'
#' @return A list of loadings plots.
#'
#' @details
#' `LMWLoadingPlot` is a wrapper of \code{\link{LinePlot}}.
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

LMWLoadingPlot <- function(ResLMWtoPCAEffects, effectNames = NULL,
                           axes = c(1,2), ...) {

  # checks   ===================

  checkArg(ResLMWtoPCAEffects,c("list"),can.be.null = FALSE)
  checkArg(effectNames,c("str"),can.be.null = TRUE)
  checkArg(axes,c("int","pos"),can.be.null = FALSE)

  if(!all(effectNames%in%names(ResLMWtoPCAEffects))){stop("One of the effects from effectNames is not in ResLMWtoPCAEffects.")}

  ### Add a check to verify that the first argument is a ResLMWtoPCA !

  if(is.null(effectNames)){
    effectNames <- c(ResLMWtoPCAEffects$covariateEffectsNamesUnique)[-1]
  }

  # loadings

  loadings <- lapply(effectNames, function(x) t(ResLMWtoPCAEffects[[x]][["loadings"]]))
  names(loadings) <- effectNames

  # percentage of explained variance   ===================

  pc_var_fun <- function(effect){

    pc_var <- ResLMWtoPCAEffects[[effect]][["var"]]
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
    title = paste(effect, ": loadings plot")

    LinePlot(X = loadings[[effect]], title = title, rows = axes, facet_label = lab[[effect]], ...)
  }

  fig <- lapply(effectNames, buildFig)
  names(fig) <- effectNames

  return(fig)

}
