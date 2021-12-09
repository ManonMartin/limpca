#' @export lmwContributions
#' @title Summary of the contributions from each effect
#'
#' @description
#' Creates a summary of the contribution of each effect and each of their Principal Component (PC) on the total variance. Additionally plots a graph with the ordered contributions.
#'
#' @param resLmwPcaEffects A resLmwPcaEffects list from \code{\link{lmwPcaEffects}}
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
#' resLmwModelMatrix = lmwModelMatrix(UCH)
#' resLmwEffectMatrices = lmwEffectMatrices(resLmwModelMatrix)
#' resLmwPcaEffects = lmwPcaEffects(resLmwEffectMatrices, method="ASCA-E")
#'
#' lmwContributions(resLmwPcaEffects)
#'
#' @import ggplot2

lmwContributions=function(resLmwPcaEffects, nPC=5){

  neffect = length(resLmwPcaEffects$covariateEffectsNamesUnique)

  # Effect table with the total contribution ===============
  total_contrib_table = matrix(data=NA,nrow=neffect,ncol=1)
  rownames(total_contrib_table) = c(names(resLmwPcaEffects)[1:(neffect-1)], "Residuals")
  colnames(total_contrib_table) = "Variation Percentages"

  total_contrib_table[,1] = round(resLmwPcaEffects$variationPercentages, 2)

  # Effect table with the variance of each component ===============
  effect_table = matrix(data=NA,nrow=neffect,ncol=(nPC+1))
  rownames(effect_table) = c(names(resLmwPcaEffects)[1:(neffect-1)], "Residuals")

  # Colnames
  temp_colnames = vector()
  for(i in 1:nPC){
    temp_colnames[i]= paste0("PC",i)
  }
  temp_colnames = c(temp_colnames,"Sum")
  colnames(effect_table) = temp_colnames

  # Filling table
  for(i in 1:(neffect-1)){
    effect_table[i,1:nPC] = round(resLmwPcaEffects[[i]]$var[1:nPC],2)
  }
  effect_table[neffect,1:nPC] = round(resLmwPcaEffects[["Residuals"]]$var[1:nPC],2)
  effect_table[,nPC+1] = c(rep(0,neffect))
  effect_table[,nPC+1] = apply(X=effect_table,MARGIN = 1,sum)


  # Effect table with the contribution of each component
  # to the variance of the effect ===============

  contrib_table = matrix(data=NA,nrow=neffect,(nPC+1))
  rownames(contrib_table) = c(names(resLmwPcaEffects)[1:(neffect-1)], "Residuals")
  temp_colnames = c(temp_colnames[1:nPC],"Contrib")
  colnames(contrib_table) = temp_colnames

  # Filling table
  for(i in 1:neffect){
    contrib_table[i,1:nPC] = (effect_table[i,1:nPC]*resLmwPcaEffects$variationPercentages[i])/100
  }

  contrib_table[,(nPC+1)] = resLmwPcaEffects$variationPercentages
  contrib_table = round(contrib_table,2)

  # Effect table for combined effects ===============
  if(length(resLmwPcaEffects)-5 != length(resLmwPcaEffects$covariateEffectsNamesUnique)){
    neffectTot = length(resLmwPcaEffects)-5
    neffectComb = neffectTot - neffect

    combinedEffect_table = matrix(data=NA,nrow=neffectComb,ncol=(nPC+1))
    rownames(combinedEffect_table) = names(resLmwPcaEffects)[neffect:(neffectTot-1)]
    temp_colnames = c(temp_colnames[1:nPC],"Sum")
    colnames(combinedEffect_table) = temp_colnames

    # Filling table
    resCombined = resLmwPcaEffects[neffect:(neffectTot-1)]

    for(i in 1:length(resCombined)){
      combinedEffect_table[i,1:nPC] = round(resCombined[[i]]$var[1:nPC],2)
    }
    combinedEffect_table[,nPC+1] = c(rep(0,neffectComb))
    combinedEffect_table[,nPC+1] = apply(X=combinedEffect_table,MARGIN = 1,sum)
  }else{
    combinedEffect_table = NULL
  }

  # Plots ===============

  # Plot of the total contribution
  effect_name = ModelAbbrev(c(names(resLmwPcaEffects)[1:(neffect-1)], "Residuals"))
  dataTotal <- data.frame(effects = effect_name,
                          varPercentage = unname(resLmwEffectMatrices$variationPercentages))

  plotTotal <- ggplot2::ggplot(data=dataTotal, ggplot2::aes(x=reorder(effects, -varPercentage),y=varPercentage))+
    ggplot2::geom_bar(stat="identity")+
    ggplot2::xlab("Effects")+
    ggplot2::ylab("Variance Percentage")

  # Plot of the contribution of each component to the variance of the effect
  tabname = matrix(data=NA,nrow=neffect,ncol=nPC)

  for(j in 1:nPC){
    for(i in 1:neffect){
      tabname[i,j] = paste(effect_name[i],colnames(contrib_table)[j],sep="\n")
    }
  }

  effect_vector = as.vector(contrib_table[,1:nPC])
  names(effect_vector) = as.vector(tabname)

  dataContrib=as.data.frame(effect_vector[1:10])

  plotContrib = ggplot2::ggplot(data=dataContrib,
                                ggplot2::aes(x=reorder(rownames(dataContrib), -dataContrib[,1]),y=dataContrib[,1]))+
    ggplot2::geom_bar(stat="identity")+
    ggplot2::xlab("Contributions")+
    ggplot2::ylab("Variance Percentage")+
    ggplot2::scale_x_discrete(limits=rownames(data))

  # Output

  if(all(is.na(resLmwEffectMatrices$SS)) & all(is.na(resLmwEffectMatrices$variationPercentages))){
    if(is.null(combinedEffect_table)){
      resLmwContributions=list(EffectTable=effect_table)
    }else{
      resLmwContributions=list(EffectTable=effect_table,
                               CombinedEffectTable=combinedEffect_table)
    }
  }else{
    if(is.null(combinedEffect_table)){
      resLmwContributions=list(TotalContribTable=total_contrib_table,
                               EffectTable=effect_table,
                               ContribTable=contrib_table,
                               PlotTotal=plotTotal,
                               PlotContrib=plotContrib)
    }else{
        resLmwContributions=list(TotalContribTable=total_contrib_table,
                                 EffectTable=effect_table,
                                 ContribTable=contrib_table,
                                 CombinedEffectTable=combinedEffect_table,
                                 PlotTotal=plotTotal,
                                 PlotContrib=plotContrib)
    }
  }

  return(resLmwContributions)
}
