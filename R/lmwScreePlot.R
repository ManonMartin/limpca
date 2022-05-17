#' @export lmwScreePlot
#' @title Scree Plot
#'
#' @description
#' Plots barplots of the percentage of variance associated with each principal component and for each matrix effect on the basis of \code{\link{lmwContributions}} outputs.
#'
#' @param resLmwContributions A resLmwContributions list from the function \code{\link{lmwContributions}}.
#' @param effectNames Names of the effects to be plotted. if `NULL`, all the effects are plotted.
#' @param nPC An integer with the number of components to plot.
#'
#' @return A scree plot (ggplot).
#'
#' @examples
#'
#'  data('UCH')
#'  resLmwModelMatrix = lmwModelMatrix(UCH)
#'  resLmwEffectMatrices = lmwEffectMatrices(resLmwModelMatrix)
#'  resASCAE = lmwPcaEffects(resLmwEffectMatrices, method="ASCA-E")
#'  resLmwContributions = lmwContributions(resASCAE)
#'  lmwScreePlot(resLmwContributions, effectNames ="Hippurate:Citrate", nPC=4)
#'
#' @import ggplot2

lmwScreePlot = function(resLmwContributions, effectNames = NULL,
                        nPC=5, theme = theme_bw()){

  # checks ===================
  checkArg(resLmwContributions,c("list"),can.be.null=FALSE)
  checkArg(effectNames,c("str"),can.be.null=TRUE)
  checkArg(nPC,c("num","pos"),can.be.null=FALSE)

  effectTable = resLmwContributions$effectTable
  if(!nPC < ncol(effectTable)){
    stop("nPC must be inferior or equal to the nPC chosen in lmwContributions")
  }

  if(is.null(effectNames)){
    effectNames <- c(rownames(resLmwContributions$effectTable))
  }

  if(!all(effectNames %in% rownames(resLmwContributions$effectTable))){
    stop("One of the effects from effectNames is not in resLmwContributions.")
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
