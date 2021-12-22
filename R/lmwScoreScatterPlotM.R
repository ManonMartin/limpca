#' @export lmwScoreScatterPlotM
#' @title Plotting a Scores Matrix
#'
#' @description
#' Plots a matrix of scores graphs
#'
#' @param resLmwPcaEffects A list of p+3 elements depending of the model terms from \code{\link{PCALMEffects}}
#' @param effectNames A character vector with the name of the effects to plot
#' @param PCdim A numeric vector with the same length than effectNames and indicating the number of component to plot
#' @param modelAbbrev A logical whether to abbreviate the interaction terms
#' @param ... Additional arguments to be passed to \code{\link{plotScatterM}}.
#'
#' @return A matrix of graphs
#'
#' @details
#' `lmwScoreScatterPlotM` is a wrapper of \code{\link{plotScatterM}}.
#'
#' @examples
#'
#'  data('UCH')
#'  ResLMModelMatrix = LMModelMatrix(formula=as.formula(UCH$formula),design=UCH$design)
#'  ResLMEffectMatrices = LMEffectMatrices(ResLMModelMatrix,outcomes=UCH$outcomes)
#'  resLmwPcaEffects = PCALMEffects(ResLMEffectMatrices,method="ASCA-E")
#'  PlotScoresMatrix(resLmwPcaEffects,
#'                  modelAbbrev=FALSE,
#'                  design=UCH$design,
#'                  effectNames = c("Citrate","Hippurate","Hippurate:Citrate"),
#'                  PCdim=c(2,2,2),
#'                  varname.colorup = "Citrate",
#'                  vec.colorup = c("red","blue","green"),
#'                  varname.pchup="Hippurate",
#'                  vec.pchup=c(1,2,3),
#'                  varname.pchdown = "Day",
#'                  vec.pchdown = c(4,5),
#'                  varname.colordown="Time",
#'                  vec.colordown = c("brown","grey"))
#'
#' @import graphics grDevices

lmwScoreScatterPlotM = function(resLmwPcaEffects,
                                effectNames = NULL,
                                PCdim = NULL,
                                modelAbbrev=FALSE, ...){

  # Define allEffect in regard to effectNames
  if(is.null(effectNames)){
    allEffect = TRUE
  }else{
    allEffect = FALSE
  }

  # Checking arguments ===============
  checkArg(resLmwPcaEffects,"list",can.be.null = FALSE)
  checkArg(PCdim,"num",can.be.null = TRUE)
  checkArg(effectNames,"str",can.be.null = TRUE)

  #Checking resLmwPcaEffects object and match with effectNames
  if(names(resLmwPcaEffects[length(resLmwPcaEffects)-2])!= "method"){
    stop("Argument is not a resLmwPcaEffects object")}

    if(allEffect==FALSE){
      for(i in 1:length(effectNames)){
        if(!effectNames[i]%in%names(resLmwPcaEffects)){"One of the elements from effectNames is not in resLmwPcaEffects"}
      }
    }

  # Check the number of PC to use or create it with first PC only
  if(is.null(PCdim)){
    classicalPC = TRUE
    if(allEffect==TRUE){
      PCdim = rep(1,(length(resLmwPcaEffects)-5))
    }else{
      PCdim = rep(1,length(effectNames))
    }
  }else{
    classicalPC=FALSE
  }

  # Checking equivalent length
  if(allEffect==FALSE){
    if(length(PCdim)!=length(effectNames)){stop("length(PCdim) differs from length(effectNames)")}
  }


  # Create the matrix for the "pairs" function ===============
  k = sum(PCdim) # Length of the diagonal of the plot
  n = length(resLmwPcaEffects[[1]]$scores[,1]) # Find number of observations

  var = c(rep(1,k))

  coomatrix = matrix(data=NA,nrow=n,ncol=k)
  colnames(coomatrix) = seq(1:k)

  if(allEffect==TRUE){
    if(classicalPC){ # Only PC1 to all effects
      for(i in 1:(length(resLmwPcaEffects)-5)){
        coomatrix[,i] = resLmwPcaEffects[[i]]$scores[,PCdim[i]]
        colnames(coomatrix)[i] = paste(names(resLmwPcaEffects)[i],"PC1")
        var[i] = (resLmwPcaEffects$variationPercentages[i] * resLmwPcaEffects[[i]]$var[1])/100}
    }else{ # All effects but more than 1 PC
      l = 1
      for(j in 1:length(PCdim)){ # Some effects must be printed on PC2 or more

        if(PCdim[j] == 1){
          coomatrix[,l] = resLmwPcaEffects[[j]]$scores[,PCdim[j]]
          colnames(coomatrix)[l] = paste(names(resLmwPcaEffects)[j],"PC1")
          var[l] = (resLmwPcaEffects$variationPercentages[j] * resLmwPcaEffects[[j]]$var[1])/100
          l=l+1
        }else{
          for(m in 1:PCdim[j]){ # Get the others PC
            coomatrix[,l] = resLmwPcaEffects[[j]]$scores[,m]
            colnames(coomatrix)[l] = paste(names(resLmwPcaEffects)[j],colnames(resLmwPcaEffects[[j]]$scores)[m])
            var[l] = (resLmwPcaEffects$variationPercentages[j] * resLmwPcaEffects[[j]]$var[m])/100
            l=l+1
          }
        }
      }
    }

  }else{ # Not all effect and on one or more PC

    l = 1
    for(i in 1:length(effectNames)){

      iEffect_temp=which(names(resLmwPcaEffects)==effectNames[i])
      iEffect = resLmwPcaEffects[[iEffect_temp]]

      for(j in 1:PCdim[i]){

        coomatrix[,l] = iEffect$scores[,j]
        colnames(coomatrix)[l] = paste(names(resLmwPcaEffects)[iEffect_temp],colnames(iEffect$scores)[j])
        var[l] = (resLmwPcaEffects$variationPercentages[iEffect_temp] * resLmwPcaEffects[[iEffect_temp]]$var[j])/100
        l = l + 1
      }
    }
  }

  # Creation of the labels
  labelvector = vector()

  for(i in 1:k){
    div_name = strsplit(x=colnames(coomatrix)[i],split=" ")
    div_name = div_name[[1]]
    effect_name = div_name
    if(modelAbbrev==TRUE){effect_name=ModelAbbrev(div_name)}  # Abbrev interaction term
    labelvector[i] = paste(effect_name[1],"\n",div_name[2],"\n",round(var[i],2),"%")
  }



  # Plotting the graph

  plotScatterM(Y = coomatrix,
               cols = 1:ncol(coomatrix),
               design = resLmwPcaEffects$lmwDataList$design,
               labelVector = labelvector, ...)

}
