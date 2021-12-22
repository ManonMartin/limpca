#' @export lmwContributions
#' @title Summary of the contributions of each effect
#'
#' @description
#' Creates a summary of the contribution of the different effects to the total variance as well as the contribution of the Principal Components calculated on each model matrix. Additionally provides plots of the ordered contributions.
#'
#' @param resLmwPcaEffects A list corresponding to the output value of \code{\link{lmwPcaEffects}}.
#' @param nPC The number of Principal Components to display.
#'
#' @return A list of:
#' \describe{
#' \item{\code{totalContribTable}}{Table of the percentage of contribution of each effect to the total variance.}
#' \item{\code{effectTable}}{Table of the variance percentage explained by each Principal Component in each model effect decomposition.}
#' \item{\code{contribTable}}{Table of the variance percentage explained by each Principal Component of each effect reported to the percentage contribution of the given effect to the total variance.}
#' \item{\code{combinedEffectTable}}{Equivalent of the \emph{EffectTable} for combined effects.}
#' \item{\code{plotTotal}}{Plot of the ordered contributions of \emph{TotalContribTable}.}
#' \item{\code{plotContrib}}{Plot of the ordered contributions of \emph{ContribTable}.}
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

  neffect = length(resLmwPcaEffects$effectsNamesUnique)

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

  if(resLmwPcaEffects$method != "APCA"){

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
  }

  # Effect table for combined effects ===============
  if(length(resLmwPcaEffects)-5 != length(resLmwPcaEffects$effectsNamesUnique)){
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
    ggplot2::ylab("Variance Percentage")+
    ggplot2::theme_bw()

  # Plot of the contribution of each component to the variance of the effect
  if(resLmwPcaEffects$method != "APCA"){
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
    ggplot2::scale_x_discrete(limits=rownames(data))+
    ggplot2::theme_bw()
  }

  # Output

  if(resLmwPcaEffects$method == "APCA"){
    contrib_table = NULL
    plotContrib = NULL
  }

  if(all(is.na(resLmwEffectMatrices$type3SS)) & all(is.na(resLmwEffectMatrices$variationPercentages))){
    if(is.null(combinedEffect_table)){
      resLmwContributions=list(effectTable=effect_table)
    }else{
      resLmwContributions=list(effectTable=effect_table,
                               combinedEffectTable=combinedEffect_table)
    }
  }else{
    if(is.null(combinedEffect_table)){
      resLmwContributions=list(totalContribTable=total_contrib_table,
                               effectTable=effect_table,
                               contribTable=contrib_table,
                               plotTotal=plotTotal,
                               plotContrib=plotContrib)
    }else{
        resLmwContributions=list(totalContribTable=total_contrib_table,
                                 effectTable=effect_table,
                                 contribTable=contrib_table,
                                 combinedEffectTable=combinedEffect_table,
                                 plotTotal=plotTotal,
                                 plotContrib=plotContrib)
    }
  }

  return(resLmwContributions)
}
