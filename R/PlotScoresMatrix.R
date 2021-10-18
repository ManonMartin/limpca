#' @export PlotScoresMatrix
#' @title Plotting a Scores Matrix
#' @description Plot a matrix of scores graphs
#'
#' @param ResPCALMEffects A list of p+3 elements depending of the model terms from \code{\link{PCALMEffects}}
#' @param ModelAbbrev A logical whether to abbreviate the interaction terms
#' @param design The nxk "free encoded" experimental design data frame
#' @param EffectNames A character vector with the name of the effects to plot
#' @param alleffect A logical whether to plot every effect
#' @param PCdim A numeric vector with the same length than EffectNames and indicating the number of component to plot
#' @param varname.colorup A character with the name of variable used to color the upper triangle
#' @param varname.colordown A character with the name of variable used to color the lower triangle
#' @param varname.pchup A character with the name of variable used to mark points from the upper triangle
#' @param varname.pchdown A character with the name of variable used to mark points from the lower triangle
#' @param vec.colorup A color vector with a length equivalent to the number of levels from varname.colorup
#' @param vec.colordown A color vector with a length equivalent to the number of levels from varname.colordown
#' @param vec.pchup A pch vector with a length equivalent to the number of levels from varname.pchup
#' @param vec.pchdown A pch vector with a length equivalent to the number of levels from varname.pchdown
#'
#' @return A matrix of graphs
#'
#' @examples
#'
#'  data('UCH')
#'  ResLMModelMatrix = LMModelMatrix(formula=as.formula(UCH$formula),design=UCH$design)
#'  ResLMEffectMatrices = LMEffectMatrices(ResLMModelMatrix,outcomes=UCH$outcomes)
#'  ResPCALMEffects = PCALMEffects(ResLMEffectMatrices,method="ASCA-E")
#'  PlotScoresMatrix(ResPCALMEffects,
#'                  ModelAbbrev=FALSE,
#'                  design=UCH$design,
#'                  EffectNames = c("Citrate","Hippurate","Hippurate:Citrate"),
#'                  alleffect = FALSE,
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
#'
PlotScoresMatrix = function(
  ResPCALMEffects, #PCALMEffects Objects
  ModelAbbrev=FALSE,
  design,
  EffectNames = NULL, # Vector of character with the name of the effect to plot
  alleffect, #Logical whether to plot all effects
  PCdim = NULL, #Number of dimensions to use for each effect
  varname.colorup = NULL, #Effect to use as color to the upper triangle
  varname.colordown = NULL, #Effect to use as color to the lower triangle
  varname.pchup = NULL, #Effect to use as pch to the upper triangle
  varname.pchdown = NULL, #Effect to use as pch to the lower triangle
  vec.colorup = NULL, #Choice of the color upper triangle
  vec.colordown = NULL, #Choice of the color lower triangle
  vec.pchup = NULL, #Choice of the pch upper triangle
  vec.pchdown = NULL #Choice of the pch lower triangle
){

  # Checking arguments
# test
    #Checking the class

  if(alleffect==FALSE){

  checkArg(ResPCALMEffects,"list")
  checkArg(design,"data.frame")
  checkArg(PCdim,"num")
  checkArg(varname.colorup,"str",can.be.null = TRUE)
  checkArg(varname.colordown,"str",can.be.null = TRUE)
  checkArg(varname.pchup,"str",can.be.null = TRUE)
  checkArg(varname.pchdown,"str",can.be.null = TRUE)

  checkArg(EffectNames,"str")

    #Checking equivalent length

  if(length(PCdim)!=length(EffectNames)){stop("length(PCdim) differs from length(EffectNames)")}
  }

  # Checking for colors
  if(!is.null(varname.colorup)){if(is.null(vec.colorup)){
    message("No specified colors for the upper triangle, colors by default",immediate. = TRUE)}else{
      if(length(levels(design[,which(names(design)!=varname.colorup)])==length(vec.colorup))){
        stop("length(vec.colorup) differs from the number of levels of the varname.colorup variable")}
    }}
  if(!is.null(varname.colordown)){if(is.null(vec.colordown)){
    message("No specified colors for the lower triangle, colors by default")}else{
      if(!(length(levels(design[,which(names(design)==varname.colordown)]))==length(vec.colordown))){
        stop("length(vec.colordown) differs from the number of levels of the varname.colordown variable")}
    }}
  if(!is.null(varname.pchup)){if(is.null(vec.pchup)){
    message("No specified pch for the upper triangle, pch by default")}else{
      if(!(length(levels(design[,which(names(design)==varname.pchup)]))==length(vec.pchup))){
        stop("length(vec.pchup) differs from the number of levels of the varname.pchup variable")}
    }}
  if(!is.null(varname.pchdown)){if(is.null(vec.pchdown)){
    message("No specified pch for the lower triangle, pch by default")}else{
      if(!(length(levels(design[,which(names(design)==varname.pchdown)]))==length(vec.pchdown))){
        stop("length(vec.pchdown) differs from the number of levels of the varname.pchdown variable")}
    }}
    #Checking ResPCALMEffects object and match with EffectNames

  for(i in 1:(length(ResPCALMEffects)-3)){
    if(!isresultfromPCA(ResPCALMEffects[[i]])){stop("One of the element from the list beside method is not a PCA result from SVDforPCA")}
  }
  if(names(ResPCALMEffects[length(ResPCALMEffects)])!= "method")

  if(alleffect==FALSE){
    for(i in 1:length(EffectNames)){
      if(!EffectNames[i]%in%names(ResPCALMEffects)){"One of the elements from EffectNames is not in ResPCALMEffects"}
    }
  }

  # Check the number of PC to use or create it with first PC only

  if(is.null(PCdim)==TRUE){

    classicalPC = TRUE

    if(alleffect==TRUE){
      PCdim = rep(1,(length(ResPCALMEffects)-3))
    }else{
      PCdim = rep(1,length(EffectNames))
    }
  }else{

    classicalPC=FALSE

  }



  # Create the matrix for the "pairs" function

  k = sum(PCdim) # Length of the diagonal of the plot
  n = length(ResPCALMEffects[[1]]$scores[,1]) # Find number of observations

  var = c(rep(1,k))

  coomatrix = matrix(data=NA,nrow=n,ncol=k)
  colnames(coomatrix) = seq(1:k)

  if(alleffect==TRUE){
    if(classicalPC){ # Only PC1 to all effects
      for(i in 1:(length(ResPCALMEffects)-3)){
        coomatrix[,i] = ResPCALMEffects[[i]]$scores[,PCdim[i]]
        colnames(coomatrix)[i] = paste(names(ResPCALMEffects)[i],"PC1")
        var[i] = ResPCALMEffects$variationPercentages[i] * (ResPCALMEffects[[i]]$var[1]/100)}
    }else{ # All effects but more than 1 PC
      l = 1
      for(j in 1:length(PCdim)){ # Some effects must be print on PC2 or more

        if(PCdim[j] == 1){
            coomatrix[,l] = ResPCALMEffects[[j]]$scores[,PCdim[j]]
            colnames(coomatrix)[l] = paste(names(ResPCALMEffects)[j],"PC1")
            var[l] = ResPCALMEffects$variationPercentages[j] * (ResPCALMEffects[[j]]$var[1]/100)
            l=l+1
        }else{
            for(m in 1:PCdim[j]){ # Get the others PC
              coomatrix[,l] = ResPCALMEffects[[j]]$scores[,m]
              colnames(coomatrix)[l] = paste(names(ResPCALMEffects)[j],colnames(ResPCALMEffects[[j]]$scores)[m])
              var[l] = ResPCALMEffects$variationPercentages[j] * (ResPCALMEffects[[j]]$var[m]/100)
              l=l+1
            }
          }
      }
    }

  }else{ # Not all effect and on or more PC

  l = 1
  for(i in 1:length(EffectNames)){

    iEffect_temp=which(names(ResPCALMEffects)==EffectNames[i])
    iEffect = ResPCALMEffects[[iEffect_temp]]

    for(j in 1:PCdim[i]){

      coomatrix[,l] = iEffect$scores[,j]
      colnames(coomatrix)[l] = paste(names(ResPCALMEffects)[iEffect_temp],colnames(iEffect$scores)[j])
      var[l] = ResPCALMEffects$variationPercentages[iEffect_temp] * (ResPCALMEffects[[iEffect_temp]]$var[j]/100)
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
    if(ModelAbbrev==TRUE){effect_name=ModelAbbrev(div_name)}  # Abbrev interaction term
    labelvector[i] = paste(effect_name[1],"\n",div_name[2],"\n",round(var[i],2),"%")
  }

  # Determine the graphics parameter from the graph

  plot_type_colored = TRUE
  plot_type_pch = TRUE

  if(is.null(varname.colordown)&is.null(varname.colorup)){
    if(!is.null(varname.pchup)){
      varname.colordown=varname.pchup
      varname.colorup=varname.pchup
    }else if(!is.null(varname.pchdown)){
      varname.colorup=varname.pchdown
      varname.colordown=varname.pchdown
      }
  }

  if(is.null(varname.pchup) & is.null(varname.pchdown)){
    if(!is.null(varname.colorup)){
      varname.pchup = varname.colorup
      varname.pchdown = varname.colorup
    }else if(!is.null(varname.colordown)){
      varname.pchup = varname.colordown
      varname.pchdown = varname.colordown
    }
  }
  # Create default color and pch if not specified

  # # Define default colors
  # if(is.null(vec.colorup) & is.null(vec.colordown)){
  #   vec.colorup = grDevices::heat.colors(length(levels(design[,which(names(design)==varname.colorup)])),alpha=1)
  #   vec.colordown = grDevices::topo.colors(length(levels(design[,which(names(design)==varname.colordown)])),alpha=1)
  # }
  #
  # # Define default pch
  # if(is.null(vec.pchdown)&is.null(vec.pchup)){
  #   vec.pchdown = c(1:(length(levels(design[,which(names(design)==varname.pchdown)]))))
  #   vec.pchup = c(20:(20+(length(levels(design[,which(names(design)==varname.pchup)])))))
  # }

  if(is.null(varname.colorup)){
    if(is.null(varname.colordown)){
      plot_type_colored = FALSE
    }else{ # Copied
      varname.colorup = varname.colordown
      if(is.null(vec.colordown)){
        vec.colorup = grDevices::heat.colors(length(levels(design[,which(names(design)==varname.colorup)])),alpha=1)
      }else{
        vec.colorup = vec.colordown
      }
    }
  }else{
    if(is.null(varname.colordown)){varname.colordown=varname.colorup}
    if(is.null(vec.colorup)){ # Default
      if(is.null(vec.colordown)){
        vec.colorup = grDevices::heat.colors(length(levels(design[,which(names(design)==varname.colorup)])),alpha=1)
      }else if(varname.colordown == varname.colorup){
          vec.colorup = vec.colordown
      }

    }else{
      # OK
    }
  }


  if(is.null(varname.colordown)){
    if(is.null(varname.colorup)){
      plot_type_colored = FALSE
    }else{ # Copied
      varname.colordown = varname.colorup
      if(is.null(vec.colorup)){
        vec.colordown = grDevices::topo.colors(length(levels(design[,which(names(design)==varname.colordown)])),alpha=1)
      }else{
        vec.colordown = vec.colorup
      }

    }
  }else{
    if(is.null(varname.colorup)){varname.colorup=varname.colordown}
    if(is.null(vec.colordown)){ # Default
      # if(is.null(vec.colorup)){
      #   vec.colordown = grDevices::topo.colors(length(levels(design[,which(names(design)==varname.colordown)])),alpha=1)
      # }else
      if(varname.colorup == varname.colordown){
        vec.colordown=vec.colorup
      }else{
        vec.colordown = grDevices::topo.colors(length(levels(design[,which(names(design)==varname.colordown)])),alpha=1)
      }

    }else{
      # OK
    }
  }

  if(is.null(varname.pchdown)){
    if(is.null(varname.pchup)){
      plot_type_pch=FALSE
    }else{
      varname.pchdown = varname.pchup
      if(is.null(vec.pchup)){
        vec.pchdown = c(1:(length(levels(design[,which(names(design)==varname.pchdown)]))))
      }else{
        vec.pchdown = vec.pchup
      }
    }
  }else{
    if(is.null(vec.pchdown)){
      if(is.null(vec.pchup)){
        vec.pchdown = c(1:(length(levels(design[,which(names(design)==varname.pchdown)]))))
      }else if(varname.pchdown == varname.pchup){
        vec.pchdown = vec.pchup
      }else{
        vec.pchdown = c(1:(length(levels(design[,which(names(design)==varname.pchdown)]))))##############################
      }
    }else{
      #OK
    }
  }

  if(is.null(varname.pchup)){
    if(is.null(varname.pchdown)){
      plot_type_pch=FALSE
    }else{
      varname.pchup= varname.pchdown
      if(is.null(vec.pchdown)){
        vec.pchup = vec.pchdown
      }else{
        vec.pchup = c(20:(20+(length(levels(design[,which(names(design)==varname.pchup)])))))
      }
    }
  }else{
    if(is.null(vec.pchup)){
      # if(is.null(vec.pchdown)){
      #   vec.pchup = c(1:(length(levels(design[,which(names(design)==varname.pchup)]))))
      # }else
      if(varname.pchdown == varname.pchup){
        vec.pchup = vec.pchdown
      }else{
        vec.pchup = c(20:(20+(length(levels(design[,which(names(design)==varname.pchup)])))))
      }

    }else{
      #OK
    }
  }

  # print(varname.colordown)
  # print(varname.colorup)
  # print(varname.pchup)
  # print(varname.pchdown)
  # print(vec.colorup)
  # print(vec.colordown)
  # print(vec.pchup)
  # print(vec.pchdown)

  # Creation of the upper triangle

  panelup = function(x,y){

    iEffect.color=design[,which(names(design)==varname.colorup)]
    iEffect.pch=design[,which(names(design)==varname.pchup)]

    var.color.level = levels(iEffect.color)
    var.pch.level = levels(iEffect.pch)

    colorvector = vector()
    pchvector = vector()

    for(i in 1:length(var.color.level)){
      colorvector[iEffect.color==var.color.level[i]] = vec.colorup[i]
    }
    for(i in 1:length(var.pch.level)){
      pchvector[iEffect.pch==var.pch.level[i]] = vec.pchup[i]
    }
  graphics::points(x,y,col = colorvector, pch=pchvector)
  }

  # Creation of the lower triangle
  paneldown = function(x,y){

    iEffect.color=design[,which(names(design)==varname.colordown)]
    iEffect.pch=design[,which(names(design)==varname.pchdown)]

    var.color.level = levels(iEffect.color)
    var.pch.level = levels(iEffect.pch)

    colorvector = vector()
    pchvector = vector()

    for(i in 1:length(var.color.level)){
      colorvector[iEffect.color==var.color.level[i]] = vec.colordown[i]
    }
    for(i in 1:length(var.pch.level)){
      pchvector[iEffect.pch==var.pch.level[i]] = vec.pchdown[i]
    }
  graphics::points(x,y,col = colorvector, pch=pchvector)
  }

  # Creation Legend

  Legend = function(){

    #Dividing the space for each legend

    totallegend = length(c(vec.pchup,vec.pchdown,vec.colordown,vec.colorup))
    spacebyline = 0.85/totallegend

    spacepchup = c(0.05,0.05+length(vec.pchup)*spacebyline)
    spacepchdown = c(0.05+length(vec.pchup)*spacebyline,0.05+length(vec.pchup)*spacebyline + length(vec.pchdown)*spacebyline)
    spacecoldown = c(0.05+length(vec.pchup)*spacebyline + length(vec.pchdown)*spacebyline,0.05+length(vec.pchup)*spacebyline + length(vec.pchdown)*spacebyline +length(vec.colordown)*spacebyline)
    spacecolup = c(0.05+length(vec.pchup)*spacebyline + length(vec.pchdown)*spacebyline +length(vec.colordown)*spacebyline,0.9)


    #Plotting legend

    graphics::legend(x=c(0.93,1),y=spacecolup,
           title = varname.colorup,
           legend = levels(design[,which(names(design)==varname.colorup)]),
           bty = "n",
           col = vec.colorup,
           pch = 15,
           inset = c(0.03, 0.1),
           title.adj = 0,
           cex = 0.7)

    graphics::legend(x=c(0.93,1),y=spacecoldown,
           title = varname.colordown,
           legend = levels(design[,which(names(design)==varname.colordown)]),
           bty = "n",
           col = vec.colordown,
           pch = 15,
           inset = c(0.03,0.3),
           title.adj = 0,
           cex = 0.7)

    graphics::legend(x=c(0.93,1),y=spacepchup,
           title = varname.pchup,
           legend = levels(design[,which(names(design)==varname.pchup)]),
           bty = "n",
           pch = vec.pchup,
           inset = c(0.03, 0.6),
           cex = 0.7,
           title.adj = 0,
           xjust=0,
           adj=0)

    graphics::legend(x=c(0.93,1),y=spacepchdown,
                     title = varname.pchdown,
                     legend = levels(design[,which(names(design)==varname.pchdown)]),
                     bty = "n",
                     pch = vec.pchdown,
                     inset = c(0.03, 0.9),
                     cex = 0.7,
                     title.adj = 0,
                     xjust=0,
                     adj=0)

  }


  # Plotting the graph (2 cases)

  if(!plot_type_colored & !plot_type_pch){
    graphics::pairs(coomatrix,gap=0.3,labels=labelvector)
  }else if(plot_type_colored & plot_type_pch){
    graphics::pairs(coomatrix,upper.panel=panelup,lower.panel = paneldown,gap = 0.3,labels=labelvector,oma = c(3, 3, 5, 10))
    graphics::par(xpd=TRUE)
    Legend()
    graphics::par(xpd=FALSE)
  }

}

#
# PlotScoresMatrix(ResPCALMEffects,alleffect = TRUE,design=UCH$design)
#
# PlotScoresMatrix(ResPCALMEffects,alleffect = TRUE,PCdim=c(2,2,1,1,1,1,1,1),design=UCH$design)
#
# PlotScoresMatrix(ResPCALMEffects,design=UCH$design,alleffect = FALSE,EffectNames = c("Citrate","Hippurate","Time","Hippurate:Citrate"),PCdim=c(1,1,1,1))
#
# PlotScoresMatrix(ResPCALMEffects,design=UCH$design,alleffect = FALSE,EffectNames = c("Citrate","Hippurate","Time","Hippurate:Citrate"),PCdim=c(1,1,1,2))
#
# PlotScoresMatrix(ResPCALMEffects,
#                  alleffect = TRUE,
#                  design=UCH$design,
#                  varname.colorup = "Citrate",
#                  vec.colorup = c("red","blue","green"),
#                  varname.pchup="Hippurate",
#                  varname.pchdown = "Day",
#                  vec.pchdown = c(4,5),
#                  vec.pchup=c(1,2,3),
#                  varname.colordown="Time",
#                  vec.colordown = c("brown","grey"))
#
# PlotScoresMatrix(ResPCALMEffects,
#                  alleffect = TRUE,
#                  PCdim=c(2,2,1,1,1,1,1,1),
#                  design=UCH$design,
#                  varname.colorup = "Citrate",
#                  vec.colorup = c("red","blue","green"),
#                  varname.pchdown = "Day",
#                  vec.pchdown = c(4,5),
#                  varname.pchup="Hippurate",
#                  vec.pchup=c(1,2,3),
#                  varname.colordown="Time",
#                  vec.colordown = c("brown","grey"))
#
# PlotScoresMatrix(ResPCALMEffects,
#                  alleffect = TRUE,
#                  PCdim=c(2,2,1,1,1,1,1,1),
#                  design=UCH$design,
#                  varname.colorup = "Citrate",
#                  varname.pchdown = "Day",
#                  varname.pchup="Hippurate",
#                  varname.colordown="Time")
#
# PlotScoresMatrix(ResPCALMEffects,
#                  alleffect = FALSE,
#                  EffectNames = c("Citrate","Hippurate"),
#                  PCdim=c(2,2),
#                  design=UCH$design,
#                  varname.colorup = "Citrate",
#                  varname.pchdown = "Day",
#                  varname.pchup="Hippurate",
#                  varname.colordown="Time")
#
# PlotScoresMatrix(ResPCALMEffects,
#                  alleffect = FALSE,
#                  EffectNames = c("Citrate","Hippurate"),
#                  PCdim=c(2,2),
#                  design=UCH$design,
#                  varname.colorup = "Citrate",
#                  varname.pchdown = "Day",
#                  varname.pchup="Hippurate")
#
# PlotScoresMatrix(ResPCALMEffects,
#                  alleffect = FALSE,
#                  EffectNames = c("Citrate","Hippurate"),
#                  PCdim=c(2,2),
#                  design=UCH$design,
#                  varname.pchup  ="Hippurate")
#
# PlotScoresMatrix(ResPCALMEffects,
#                  alleffect = FALSE,
#                  EffectNames = c("Citrate","Hippurate"),
#                  PCdim=c(2,2),
#                  design=UCH$design,
#                  varname.colorup ="Hippurate",
#                  varname.colordown = "Citrate")

