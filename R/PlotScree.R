#' @export PlotScree
#' @title Plot a Screeplot from a ResPCALMEffects list
#' @description Plot a Screeplot of the Principal Component variances
#'
#' @param ResPCALMEffects a ResPCALMEffects list from the function \code{\link{PCALMEffects}}
#' @param EffectName a character with the name of the effect to plot
#' @param PCnumber Integer with the number of components to plot
#'
#' @return A ggplot2 object with the Screeplot
#'
#' @example
#'
#'  data('UCH')
#'  ResLMModelMatrix = LMModelMatrix(formula=as.formula(UCH$formula),design=UCH$design)
#'  ResLMEffectMatrices = LMEffectMatrices(ResLMModelMatrix,outcomes=UCH$outcomes)
#'  ResPCALMEffects = PCALMEffects(ResLMEffectMatrices,method="ASCA-E")
#'  PlotScree(ResPCALMEffects,"Hippurate:Citrate",PCnumber=8)
#'

PlotScree = function(ResPCALMEffects,EffectName,PCnumber=5){

  #Checking the arguments

  checkArg(EffectName,"str")
  checkArg(PCnumber,"num")
  if(!is.list(ResPCALMEffects)){stop("The ResPCALMEffects argument is not a list")}
  for(i in 1:(length(ResPCALMEffects)-1)){
    if(!isresultfromPCA(ResPCALMEffects[[i]])){stop("One of the element from the list beside method is not a PCA result from SVDforPCA")}
  }
  if(names(ResPCALMEffects[length(ResPCALMEffects)])!= "method")

  #Selecting the effect

  iEffect_temp=which(names(ResPCALMEffects)==EffectName)
  iEffect = ResPCALMEffects[[iEffect_temp]]

  #Plotting the effect
  ggplot2::ggplot(data=as.data.frame(iEffect$var[1:PCnumber]),ggplot2::aes(x=names(iEffect$var[1:PCnumber]),y=iEffect$var[1:PCnumber]))+
    ggplot2::geom_bar(stat="identity")+
    ggplot2::xlab("Principal Components")+
    ggplot2::ylab("Variance Percentage")


}

