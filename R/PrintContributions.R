#' @export PrintContributions
#' @title Summary of the contributions from each effect
#'
#' @description
#' Create a summary of the contribution of each effect and each of their Principal Component (PC) on the total variance. Additionnally plot a graph with the ordered contribution.
#'
#' @param ResPCALMEffects A ResPCALMEffects list from \code{\link{PCALMEffects}}
#' @param nPC The number of PC to print in the tables
#'
#' @return A list of 3 :
#' \describe{
#' \item{\code{EffectTable}}{A matrix with variance percentage of each PC for each effect}
#' \item{\code{ContribTable}}{A matrix with the contribution of each PC to the total variance}
#' \item{\code{Barplot}}{A barplot with the PCs which have the biggest contributions to the total variance}
#' }
#'
#'
#' @examples
#' data('UCH')
#' ResLMModelMatrix = LMModelMatrix(formula=as.formula(UCH$formula),design=UCH$design)
#' ResLMEffectMatrices = LMEffectMatrices(ResLMModelMatrix,outcomes=UCH$outcomes)
#' ResPCALMEffects = PCALMEffects(ResLMEffectMatrices,method="ASCA-E")
#' PrintContributions(ResPCALMEffects)

PrintContributions=function(ResPCALMEffects,nPC=5){

  neffect = (length(names(ResPCALMEffects))-3)

  # Effect table with the variance of each component

  effect_table = matrix(data=NA,nrow=neffect,ncol=(nPC+1))
  rownames(effect_table) = names(ResPCALMEffects)[1:neffect]

  # Colnames

  temp_colnames = vector()
  for(i in 1:nPC){
  temp_colnames[i]= paste0("PC",i)
  }
  temp_colnames = c(temp_colnames,"Sum")
  colnames(effect_table) = temp_colnames

  #Filling table

  for(i in 1:neffect){
    effect_table[i,1:nPC] = round(ResPCALMEffects[[i]]$var[1:nPC],2)
  }
  effect_table[,nPC+1] = c(rep(0,neffect))
  effect_table[,nPC+1] = apply(X=effect_table,MARGIN = 1,sum)


  # Effect table with the contribution of each component to the variance of the effect

  contrib_table = matrix(data=NA,nrow=neffect,(nPC+1))
  rownames(contrib_table) = names(ResPCALMEffects)[1:neffect]
  temp_colnames = c(temp_colnames[1:nPC],"Contrib")
  colnames(contrib_table) = temp_colnames


  for(i in 1:neffect){

    contrib_table[i,1:nPC] = (effect_table[i,1:nPC]*ResPCALMEffects$variationPercentages[i])/100

  }

  contrib_table[,(nPC+1)] = ResPCALMEffects$variationPercentages
  contrib_table = round(contrib_table,2)


  # Graph


  tabname = matrix(data=NA,nrow=neffect,nPC)
  effect_name = ModelAbbrev(names(ResPCALMEffects)[1:neffect])

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

  ResPrintContributions=list(EffectTable=effect_table,ContribTable=contrib_table,Barplot=plot)
  return(ResPrintContributions)
}







