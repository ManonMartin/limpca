#' @export PlotLMSS
#' @title Barplot of the variation percentages
#' @description Plotting the variation percentages of the model terms
#'
#' @param ResLMSS A list from the \code{\link{LMSS}} function
#' @param abbrev If TRUE, the interaction names are abbreviated
#'
#' @return A barplot from ggplot2
#'
#' @examples
#'  data('UCH')
#'  ResLMModelMatrix = LMModelMatrix(formula=as.formula(UCH$formula),design=UCH$design)
#'  ResLMEffectMatrices = LMEffectMatrices(ResLMModelMatrix,outcomes=UCH$outcomes)
#'  ResLMSS = LMSS(ResLMEffectMatrices)
#'  PlotLMSS(ResLMSS,abbrev=TRUE)
#'
#' @import ggplot2

PlotLMSS = function(ResLMSS,abbrev=FALSE){

  VariationPercentages = ResLMSS$variationPercentages

  if(abbrev==TRUE){names(VariationPercentages)=ModelAbbrev(ResLMSS)$ModelTermsAbbrev}

  #Making DataFrame for ggplot2

  SortedVariationPercentages = sort(x=VariationPercentages,decreasing=TRUE)
  SortedVarName = names(SortedVariationPercentages)
  Effects = factor(names(SortedVariationPercentages),levels= SortedVarName)

  df = as.data.frame(SortedVariationPercentages)
  df = cbind(df,Effects)

  #Plotting
  ggplot2::ggplot(data=df,aes(x=Effects,y=SortedVariationPercentages))+ ggplot2::geom_bar(stat="identity")

  # print(sum(VariationPercentages))

}
