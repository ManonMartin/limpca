#' @export LMWContributions
#' @title Summary of the contributions from each effect
#'
#' @description
#' Create a summary of the contribution of each effect and each of their Principal Component (PC) on the total variance. Additionally plots a graph with the ordered contributions.
#'
#' @param ResLMWtoPCAEffects A ResLMWtoPCAEffects list from \code{\link{LMWtoPCAEffects}}
#' @param nPC The number of Principal Components to print in the tables
#'
#' @return A list of 3 :
#' \describe{
#' \item{\code{EffectTable}}{A matrix with the variance percentage of each PC for each effect}
#' \item{\code{ContribTable}}{A matrix with the contribution of each PC to the total variance}
#' \item{\code{Barplot}}{A barplot with the PCs which have the biggest contributions to the total variance}
#' }
#'
#'
#' @examples
#' data('UCH')
#' ResLMWModelMatrix = LMWModelMatrix(UCH)
#' ResLMWEffectMatrices = LMWEffectMatrices(ResLMWModelMatrix)
#' ResLMWtoPCAEffects = LMWtoPCAEffects(ResLMWEffectMatrices, method="ASCA-E")
#' LMWContributions(ResLMWtoPCAEffects)

LMWContributions=function(ResLMWtoPCAEffects, nPC=5){

  neffect = (length(names(ResLMWtoPCAEffects))-5)

  # Effect table with the total contribution ===============

  total_contrib_table = matrix(data=NA,nrow=neffect,ncol=1)
  rownames(total_contrib_table) = names(ResLMWtoPCAEffects)[1:neffect]
  colnames(total_contrib_table) = "Variation Percentages"

  total_contrib_table[,1] = round(ResLMWtoPCAEffects$variationPercentages, 2)

  # Effect table with the variance of each component ===============

  effect_table = matrix(data=NA,nrow=neffect,ncol=(nPC+1))
  rownames(effect_table) = names(ResLMWtoPCAEffects)[1:neffect]

  # Colnames

  temp_colnames = vector()
  for(i in 1:nPC){
  temp_colnames[i]= paste0("PC",i)
  }
  temp_colnames = c(temp_colnames,"Sum")
  colnames(effect_table) = temp_colnames

  # Filling table

  for(i in 1:neffect){
    effect_table[i,1:nPC] = round(ResLMWtoPCAEffects[[i]]$var[1:nPC],2)
  }
  effect_table[,nPC+1] = c(rep(0,neffect))
  effect_table[,nPC+1] = apply(X=effect_table,MARGIN = 1,sum)


  # Effect table with the contribution of each component
  # to the variance of the effect ===============

  contrib_table = matrix(data=NA,nrow=neffect,(nPC+1))
  rownames(contrib_table) = names(ResLMWtoPCAEffects)[1:neffect]
  temp_colnames = c(temp_colnames[1:nPC],"Contrib")
  colnames(contrib_table) = temp_colnames

  # Filling table

  for(i in 1:neffect){

    contrib_table[i,1:nPC] = (effect_table[i,1:nPC]*ResLMWtoPCAEffects$variationPercentages[i])/100

  }

  contrib_table[,(nPC+1)] = ResLMWtoPCAEffects$variationPercentages
  contrib_table = round(contrib_table,2)


  # Plots ===============

  tabname = matrix(data=NA,nrow=neffect,nPC)
  effect_name = ModelAbbrev(names(ResLMWtoPCAEffects)[1:neffect])

  for(j in 1:nPC){
    for(i in 1:neffect){
      tabname[i,j] = paste(effect_name[i],colnames(contrib_table)[j],sep="\n")
    }
  }

  contrib_vector = as.vector(contrib_table[,1:nPC])
  names(contrib_vector) = as.vector(tabname)

  sorted_contrib_vector = sort(contrib_vector,decreasing=TRUE)

  data=as.data.frame(sorted_contrib_vector[1:10])

  plot = ggplot(data=data,aes(x=rownames(data),y=data[,1]))+
    geom_bar(stat="identity")+
    xlab("Contributions")+
    ylab("Variance Percentage")+
    scale_x_discrete(limits=rownames(data))

  ResLMWContributions=list(TotalContribTable=total_contrib_table,
                           EffectTable=effect_table, ContribTable=contrib_table,
                           Barplot=plot)
  return(ResLMWContributions)
}







