#' @export lmwScoreScatterPlotM
#' @title Score scatter plot matrix
#'
#' @description
#' Plots the scores of all model effects simultaneously in a scatterplot matrix. By default, the first principal component only is kept for each model effect and, as a wrapper of \code{\link{plotScatterM}}, the choice markers and colors for factor levels allow to enrich the visualization of the factor effects on the responses.
#'
#' @param resLmwPcaEffects A list corresponding to the output value of \code{\link{lmwPcaEffects}}.
#' @param effectNames A character vector with the name of the effects to plot.
#' @param PCdim A numeric vector with the same length than effectNames and indicating the number of component to plot.
#' @param modelAbbrev A logical whether to abbreviate the interaction terms or not.
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
#'  resLmwModelMatrix = lmwModelMatrix(UCH)
#'  ResLmwEffectMatrices = lmwEffectMatrices(resLmwModelMatrix)
#'  resLmwPcaEffects = lmwPcaEffects(ResLmwEffectMatrices,method="ASCA-E")
#'
#'  lmwScoreScatterPlotM(resLmwPcaEffects,
#'                  varname.colorup = "Citrate",
#'                  varname.pchup="Hippurate",
#'                  varname.pchdown = "Day",
#'                  varname.colordown="Time")
#'
#'  # advanced setting
#'  lmwScoreScatterPlotM(resLmwPcaEffects,
#'                  modelAbbrev=FALSE,
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
  checkArg(resLmwPcaEffects,"list",can.be.null=FALSE)
  checkArg(PCdim,"num",can.be.null=TRUE)
  checkArg(effectNames,"str",can.be.null=TRUE)
  checkArg(modelAbbrev,"bool",can.be.null=FALSE)

  #Checking resLmwPcaEffects object and match with effectNames
  if (!identical(names(resLmwPcaEffects[(length(resLmwPcaEffects)-5):length(resLmwPcaEffects)]),
                 c("Residuals","lmwDataList","effectsNamesUnique","method","type3SS","variationPercentages"))){
    stop("resLmwPcaEffects is not an output value of lmwPcaEffects")}

    if(allEffect==FALSE){
      for(i in 1:length(effectNames)){
        if(!effectNames[i]%in%names(resLmwPcaEffects)){"One of the elements from effectNames is not in resLmwPcaEffects"}
      }
    }

  # Check the number of PC to use or create it with first PC only
  if(is.null(PCdim)){
    classicalPC = TRUE
    if(allEffect==TRUE){
      effectsNamesUnique <- resLmwPcaEffects$effectsNamesUnique
      effectsNamesUnique <- effectsNamesUnique[effectsNamesUnique != "Intercept"]
      PCdim = rep(1,(length(effectsNamesUnique)+1)) # +1 for the residuals
      effectsNamesUniqueRes <- c(effectsNamesUnique, "Residuals")
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
      for(i in 1:length(effectsNamesUniqueRes)){
        coomatrix[,i] = resLmwPcaEffects[[effectsNamesUniqueRes[i]]]$scores[,PCdim[i]]
        colnames(coomatrix)[i] = paste(effectsNamesUniqueRes[i],"PC1")
        var[i] = (resLmwPcaEffects$variationPercentages[effectsNamesUniqueRes[i]] *
                    resLmwPcaEffects[[effectsNamesUniqueRes[i]]]$var[1])/100}
    }else{ # All effects but more than 1 PC
      l = 1
      for(j in 1:length(PCdim)){ # Some effects must be printed on PC2 or more

        if(PCdim[j] == 1){
          coomatrix[,l] = resLmwPcaEffects[[effectsNamesUniqueRes[j]]]$scores[,PCdim[j]]
          colnames(coomatrix)[l] = paste(effectsNamesUniqueRes[j],"PC1")
          var[l] = (resLmwPcaEffects$variationPercentages[effectsNamesUniqueRes[j]] *
                      resLmwPcaEffects[[effectsNamesUniqueRes[j]]]$var[1])/100
          l=l+1
        }else{
          for(m in 1:PCdim[j]){ # Get the others PC
            coomatrix[,l] = resLmwPcaEffects[[effectsNamesUniqueRes[j]]]$scores[,m]
            colnames(coomatrix)[l] = paste(effectsNamesUniqueRes[j],colnames(resLmwPcaEffects[[j]]$scores)[m])
            var[l] = (resLmwPcaEffects$variationPercentages[effectsNamesUniqueRes[j]] *
                        resLmwPcaEffects[[effectsNamesUniqueRes[j]]]$var[m])/100
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
