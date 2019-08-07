#' @export PlotScoresXY
#' @title Plotting the scores from the PCALMEffects function
#' @description Draw one or more graphs of the scores from a PCA. More specifically the result of the \code{\link{PCALMEffects}} function.
#'
#' @param ResPCALMEffects A list of p elements depending of the model terms from \code{\link{PCALMEffects}}
#' @param design A n x d "free encoded" experimental design matrix with n observations and d the categorical variables.
#' @param EffectVector A character vector of length l with the name of the model terms to plot
#' @param varname.color A character variable with the name of the model term to use as color
#' @param varname.pch A character variable with the name of the model term to use as pch
#' @param PCaxes A vector with the two PC to plot for every effects
#' @param ... Other arguments from \code{\link{DrawScores}}
#'
#' @return A list of l ggplot2 graphs
#'
#' @details Some messages from ggplot2 could be suppressed while we overwrite the graphs.
#'
#' @examples
#'  data('UCH')
#'  ResLMModelMatrix = LMModelMatrix(formula=as.formula(UCH$formula),design=UCH$design)
#'  ResLMEffectMatrices = LMEffectMatrices(ResLMModelMatrix,outcomes=UCH$outcomes)
#'  ResPCALMEffects = PCALMEffects(ResLMEffectMatrices,method="ASCA-E")
#'  PlotScoresXY(ResPCALMEffects = ResPCALMEffects, design = UCH$design,
#'   EffectVector = c("Time","Hippurate"),varname.color = "Citrate", varname.pch = "Hippurate")
#'
#' @import ggplot2


PlotScoresXY = function(ResPCALMEffects, design, EffectVector, PCaxes = c(1, 2),
                      varname.color, varname.pch, ...) {

  # Checking arguments :

  if(!is.character(EffectVector)){stop("The argument EffectVector is not a vector of characters")}
  if(!is.vector(PCaxes)){stop("The argument PCaxes is not a vector")}
  if(length(PCaxes)!=2){stop("The argument PCaxes must be of length 2")}
  checkArg(varname.color,"str")
  checkArg(varname.pch,"str")
  if(!is.list(ResPCALMEffects)){stop("The ResPCALMEffects argument is not a list")}

  # Corresponding Name
  if(!varname.color%in%names(ResPCALMEffects)){stop("The argument varname.color is not in ResPCALMEffects")}
  if(!varname.pch%in%names(ResPCALMEffects)){stop("The argument varname.pch is not in ResPCALMEffects")}
  if(!all(EffectVector%in%names(ResPCALMEffects))){stop("One of the effect from EffectVector is not in ResPCALMEffects")}
  # Check for matches between names from design and ResPCALMEffects


  # Defining the legend

  var.color = paste(deparse(substitute(design)), varname.color, sep = "$")
  var.pch = paste(deparse(substitute(design)), varname.pch, sep = "$")

  ListGraphs = vector(mode="list")

  for(i in 1:length(EffectVector)){

  # Finding the effect

  iEffect_temp=which(names(ResPCALMEffects)==EffectVector[i])
  iEffect = ResPCALMEffects[[iEffect_temp]]

  # Checking the second component
  if(iEffect$var[2]<1){
    warning("The variance of PC2 is inferior to 1%. Graph scaled")
    pc2lim = c(100*min(iEffect$scores[,2]),100*max(iEffect$scores[,2]))
  }else{
    pc2lim = c(1.4*min(iEffect$scores[,2]),1.4*max(iEffect$scores[,2]))
    }


  main = paste(ResPCALMEffects$method, "scores plot :", EffectVector[i], "effect")


  ListGraphs[[i]] = suppressMessages(DrawScores(obj = iEffect, type.obj = "PCA", size = 4, main = main, axes = PCaxes,
             color = eval(parse(text = var.color)), pch = eval(parse(text = var.pch)),
             drawNames = FALSE) + ggplot2::scale_shape_discrete(name = varname.pch) + ggplot2::scale_colour_discrete(name = varname.color) +
    ggplot2::geom_point(alpha = 0.5)+ggplot2::ylim(pc2lim))

  }

  return(ListGraphs)
}



