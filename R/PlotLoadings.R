#' @export PlotLoadings
#' @title Plot the loadings from PCALMEffects
#' @description Plot the loadings from the ASCA, ASCA-E or APCA methods from PCALMEffects
#'
#' @param ResPCALMEffects A list from the function \code{\link{PCALMEffects}}
#' @param EffectNames A character vector of the p effects to plot
#' @param PCdim A vector of length p with the number of dimensions to draw for each effects
#' @param ... More arguments to the function \code{\link{DrawLoadings}}
#'
#' @return A list of p graphs with the asked number of dimensions for each.
#'
#' @example
#'
#'  data('UCH')
#'  ResLMModelMatrix = LMModelMatrix(formula=as.formula(UCH$formula),design=UCH$design)
#'  ResLMEffectMatrices = LMEffectMatrices(ResLMModelMatrix,outcomes=UCH$outcomes)
#'  ResPCALMEffects = PCALMEffects(ResLMEffectMatrices,method="ASCA-E")
#'  PlotLoadings(ResPCALMEffects,c("Citrate","Hippurate"),PCdim=c(2,3))


PlotLoadings = function(ResPCALMEffects,EffectNames,PCdim,...){

  # Checking the argument

  if(!is.character(EffectNames)){stop("The EffectNames argument is not a vector of characters")}
  if(!is.numeric(PCdim)){stop("PCdim is not a numeric or a vector")}
  if(length(EffectNames)!=length(PCdim)){stop("Lengths differ between EffectNames and PCdim")}
  if(!is.list(ResPCALMEffects)){stop("The ResPCALMEffects argument is not a list")}
  for(i in 1:(length(ResPCALMEffects)-1)){
    if(!isresultfromPCA(ResPCALMEffects[[i]])){stop("One of the element from the list beside method is not a PCA result from SVDforPCA")}
  }


  if(!all(EffectNames %in% names(ResPCALMEffects))){stop("At least on of th effects is not found in ResPCALMEffects")}

  # Creating the list

  GraphList = vector(mode="list")

  # Creating the graph for each effect

  for(i in 1:length(EffectNames)){

        iEffect_temp=which(names(ResPCALMEffects)==EffectNames[i])
        iEffect = ResPCALMEffects[[iEffect_temp]]

        GraphList[i] = DrawLoadings(iEffect,type.obj = "PCA", axes=c(1:PCdim[i]),...)

  }
return(GraphList)
}

