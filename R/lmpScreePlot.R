#' @export lmpScreePlot
#' @title Scree Plot
#'
#' @description
#' Plots barplots of the percentage of variance associated with each principal component and for each matrix effect on the basis of \code{\link{lmpContributions}} outputs.
#'
#' @param resLLmpContributions A resLLmpContributions list from the function \code{\link{lmpContributions}}.
#' @param effectNames Names of the effects to be plotted. if `NULL`, all the effects are plotted.
#' @param nPC An integer with the number of components to plot.
#' @param theme `ggplot` theme
#'
#' @return A scree plot (ggplot).
#'
#' @examples
#'
#'  data('UCH')
#'  resLLmpModelMatrix = lmpModelMatrix(UCH)
#'  resLLmpEffectMatrices = lmpEffectMatrices(resLLmpModelMatrix)
#'  resASCAE = lmpPcaEffects(resLLmpEffectMatrices, method="ASCA-E")
#'  resLLmpContributions = lmpContributions(resASCAE)
#'  lmpScreePlot(resLLmpContributions, effectNames ="Hippurate:Citrate", nPC=4)
#'
#' @import ggplot2

lmpScreePlot = function(resLLmpContributions, effectNames = NULL,
                        nPC=5, theme = theme_bw()){

  # checks ===================
  checkArg(resLLmpContributions,c("list"),can.be.null=FALSE)
  checkArg(effectNames,c("str"),can.be.null=TRUE)
  checkArg(nPC,c("num","pos"),can.be.null=FALSE)

  effectTable = resLLmpContributions$effectTable
  if(!nPC < ncol(effectTable)){
    stop("nPC must be inferior or equal to the nPC chosen in lmpContributions")
  }

  if(is.null(effectNames)){
    effectNames <- c(rownames(resLLmpContributions$effectTable))
  }

  if(!all(effectNames %in% rownames(resLLmpContributions$effectTable))){
    stop("One of the effects from effectNames is not in resLLmpContributions.")
  }

  # selecting the effect

  resdf = as.data.frame(t(effectTable[,c(1:nPC)]))

  buildFig <- function(x){
    iEffect=which(colnames(resdf) == effectNames[x])

    #Plotting the effect
    ggplot(data = resdf,
           aes(x = rownames(resdf), y = resdf[,iEffect])) +
      geom_bar(stat = "identity") +
      xlab("Principal Components") +
      ylab("Variance Percentage") +
      ggtitle(paste("Percentage of variance by PC for:", effectNames[x])) +
      theme
  }

  fig <- lapply(c(1:length(effectNames)), FUN = buildFig)
  names(fig) = effectNames

  return(fig)
}
