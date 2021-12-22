#' @export lmwScreePlot
#' @title Scree Plot
#'
#' @description
#' Draws scree plots for the lmwPcaEffects function.
#'
#' @param resLmwPcaEffects A resLmwPcaEffects list from the function \code{\link{lmwPcaEffects}}.
#' @param effectName The name of the effect to be plotted.
#' @param title Plot title.
#' @param nPC An integer with the number of components to plot.
#'
#' @return A scree plot
#'
#' @examples
#'
#'  data('UCH')
#'  resLmwModelMatrix = lmwModelMatrix(UCH)
#'  resLmwEffectMatrices = lmwEffectMatrices(resLmwModelMatrix)
#'  resLmwPcaEffects = lmwPcaEffects(resLmwEffectMatrices,method="ASCA-E")
#'  lmwScreePlot(resLmwPcaEffects,"Hippurate:Citrate",nPC=4)
#'
#' @import ggplot2

lmwScreePlot = function(resLmwPcaEffects,effectName,
                        title=paste0("Scree plot of the ", effectName, " effect"),
                        nPC=5){

  # checks ===================
  checkArg(resLmwPcaEffects,c("list"),can.be.null = FALSE)
  checkArg(effectName,c("str"),can.be.null = FALSE)
  checkArg(nPC,c("num","pos"),can.be.null = FALSE)

  if(!(effectName%in%names(resLmwPcaEffects))){stop("effectName is not in resLmwPcaEffects.")}

  # if(names(resLmwPcaEffects[length(resLmwPcaEffects)-2])!= "method")

  # selecting the effect
  iEffect_temp=which(names(resLmwPcaEffects)==effectName)
  iEffect=resLmwPcaEffects[[iEffect_temp]]

  #Plotting the effect
  ggplot(data=as.data.frame(iEffect$var[1:nPC]),
         aes(x=names(iEffect$var[1:nPC]),y=iEffect$var[1:nPC]))+
    geom_bar(stat="identity")+
    xlab("Principal Components")+
    ylab("Variance Percentage")+
    ggtitle(title)

}
