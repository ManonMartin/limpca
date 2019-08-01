#' @export PlotScoresMatrix
#' @title Plotting a Scores Matrix
#' @description Plot a matrix of scores graphs
#'
#' @param ResPCALMEffects object
#' @param design design
#' @param EffectNames name
#' @param alleffect logical
#' @param PCdim pc
#' @param varname.colorup varname
#' @param varname.colordown varname
#' @param varname.pch varname
#' @param vec.colorup vec
#' @param vec.colordown vec
#' @param vec.pch vec
#'
#' @return A graph
#'
#' @example
#'
#'  data('UCH')
#'  ResLMModelMatrix = LMModelMatrix(formula=as.formula(UCH$formula),design=UCH$design)
#'  ResLMEffectMatrices = LMEffectMatrices(ResLMModelMatrix,outcomes=UCH$outcomes)
#'  ResPCALMEffects = PCALMEffects(ResLMEffectMatrices,method="ASCA-E")
#'  PlotScree(ResPCALMEffects,"Hippurate:Citrate",PCnumber=8)
#'
#' @import graphics
#'
PlotScoresMatrix = function(
  ResPCALMEffects, #PCALMEffects Objects
  design,
  EffectNames = NULL, # Vector of character with the name of the effect to plot
  alleffect, #Logical whether to plot all effects
  PCdim = NULL, #Number of dimensions to use for each effect
  varname.colorup = NULL, #Effect to use as color to the upper triangle
  varname.colordown = NULL, #Effect to use as color to the lower triangle
  varname.pch = NULL, #Effect to use as pch
  vec.colorup, #Choice of the color upper triangle
  vec.colordown, #Choice of the color lower triangle
  vec.pch #Choice of the pch
){

  # Checking arguments

  # Check the number of PC to use or create it with first PC only



  if(is.null(PCdim)==TRUE){

    classicalPC = TRUE

    if(alleffect==TRUE){
      PCdim = rep(1,(length(ResPCALMEffects)-1))
    }else{
      PCdim = rep(1,length(EffectNames))
    }
  }else{

    classicalPC=FALSE

  }

  # Create the matrix for the "pairs" function

  k = sum(PCdim) # Length of the diagonal of the plot
  n = length(ResPCALMEffects[[1]]$scores[,1]) # Find number of observations

  coomatrix = matrix(data=NA,nrow=n,ncol=k)
  colnames(coomatrix) = seq(1:k)

  if(alleffect==TRUE){
    if(classicalPC){ # Only PC1 to all effects
      for(i in 1:(length(ResPCALMEffects)-1)){
        coomatrix[,i] = ResPCALMEffects[[i]]$scores[,PCdim[i]]
        colnames(coomatrix)[i] = paste(names(ResPCALMEffects)[i],"PC1")
        }

    }else{ # All effects but more than 1 PC
      l = 1
      for(j in 1:length(PCdim)){ # Some effects must be print on PC2 or more

        if(PCdim[j] == 1){
            coomatrix[,l] = ResPCALMEffects[[j]]$scores[,PCdim[j]]
            colnames(coomatrix)[l] = paste(names(ResPCALMEffects)[j],"PC1")
            l=l+1
        }else{
            for(m in 1:PCdim[j]){ # Get the others PC
              coomatrix[,l] = ResPCALMEffects[[j]]$scores[,m]
              colnames(coomatrix)[l] = paste(names(ResPCALMEffects)[j],colnames(ResPCALMEffects[[j]]$scores)[m])
              l=l+1
            }
          }
      }
    }

  }else{ # Not all effect and on or more PC

  # coomatrix = matrix(data=NA,nrow=n,ncol=k)
  l = 1
  for(i in 1:length(EffectNames)){

    iEffect_temp=which(names(ResPCALMEffects)==EffectNames[i])
    iEffect = ResPCALMEffects[[iEffect_temp]]

    for(j in 1:PCdim[i]){

      coomatrix[,l] = iEffect$scores[,j]
      colnames(coomatrix)[l] = paste(names(ResPCALMEffects)[iEffect_temp],colnames(iEffect$scores)[j])
      l = l + 1


      }

    }
  }

  # Creation of the labels

  labelvector = vector()

  for(i in 1:k){
    div_name = strsplit(x=colnames(coomatrix)[i],split=" ")
    div_name = div_name[[1]]
    labelvector[i] = paste(div_name[1],"\n",div_name[2],"\n","Variance")
  }

  # Create default color if not specified

  # Creation of the upper triangle

  panelup = function(x,y){

    iEffect.color=design[,which(names(design)==varname.colorup)]
    iEffect.pch=design[,which(names(design)==varname.pch)]

    var.color.level = levels(iEffect.color)
    var.pch.level = levels(iEffect.pch)

    colorvector = vector()
    pchvector = vector()

    for(i in 1:length(var.color.level)){
      colorvector[iEffect.color==var.color.level[i]] = vec.colorup[i]
    }
    for(i in 1:length(var.pch.level)){
      pchvector[iEffect.pch==var.pch.level[i]] = vec.pch[i]
    }
  graphics::points(x,y,col = colorvector, pch=pchvector)
  }

  # Creation of the lower triangle
  paneldown = function(x,y){

    iEffect.color=design[,which(names(design)==varname.colordown)]
    iEffect.pch=design[,which(names(design)==varname.pch)]

    var.color.level = levels(iEffect.color)
    var.pch.level = levels(iEffect.pch)

    colorvector = vector()
    pchvector = vector()

    for(i in 1:length(var.color.level)){
      colorvector[iEffect.color==var.color.level[i]] = vec.colordown[i]
    }
    for(i in 1:length(var.pch.level)){
      pchvector[iEffect.pch==var.pch.level[i]] = vec.pch[i]
    }
  graphics::points(x,y,col = colorvector, pch=pchvector)
  }

  # Creation Legend

  Legend = function(){

    #Dividing the space for each legend

    totallegend = length(c(vec.pch,vec.colordown,vec.colorup))
    spacebyline = 0.85/totallegend

    spacepch = c(0.05,0.05+length(vec.pch)*spacebyline)
    spacecoldown = c(0.05+length(vec.pch)*spacebyline,0.05+length(vec.pch)*spacebyline + length(vec.colordown)*spacebyline)
    spacecolup = c(0.05+length(vec.pch)*spacebyline + length(vec.colordown)*spacebyline,0.9)


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
           inset = c(0.03,0.4),
           title.adj = 0,
           cex = 0.7)

    graphics::legend(x=c(0.93,1),y=spacepch,
           title = varname.pch,
           legend = levels(design[,which(names(design)==varname.pch)]),
           bty = "n",
           pch = vec.pch,
           inset = c(0.03, 0.7),
           cex = 0.7,
           title.adj = 0,
           xjust=0,
           adj=0)

  }


  # Plot the graph

  if(!is.null(varname.colorup) & !is.null(varname.pch)){

  graphics::pairs(coomatrix,upper.panel=panelup,lower.panel = paneldown,gap = 0.3,labels=labelvector,oma = c(3, 3, 5, 10))
    graphics::par(xpd=TRUE)
    Legend()
    graphics::par(xpd=FALSE)
  }else{
  graphics::pairs(coomatrix,gap=0.3,labels=labelvector)
    }

}
#
#
# PlotScoresMatrix(ResPCALMEffects,alleffect = TRUE)
#
# PlotScoresMatrix(ResPCALMEffects,alleffect = TRUE,PCdim=c(2,2,1,1,1,1,1,1))
#
# PlotScoresMatrix(ResPCALMEffects,alleffect = FALSE,EffectNames = c("Citrate","Hippurate","Time","Hippurate:Citrate"),PCdim=c(1,1,1,1))
#
# PlotScoresMatrix(ResPCALMEffects,alleffect = FALSE,EffectNames = c("Citrate","Hippurate","Time","Hippurate:Citrate"),PCdim=c(1,1,1,2))
#
# PlotScoresMatrix(ResPCALMEffects,
#                  alleffect = TRUE,
#                  design=UCH$design,
#                  varname.colorup = "Citrate",
#                  vec.colorup = c("red","blue","green"),
#                  varname.pch="Hippurate",
#                  vec.pch=c(1,2,3),
#                  varname.colordown="Time",
#                  vec.colordown = c("brown","grey"))
#
# PlotScoresMatrix(ResPCALMEffects,
#                  alleffect = TRUE,
#                  PCdim=c(2,2,1,1,1,1,1,1),
#                  design=UCH$design,
#                  varname.colorup = "Citrate",
#                  vec.colorup = c("red","blue","green"),
#                  varname.pch="Hippurate",
#                  vec.pch=c(1,2,3),
#                  varname.colordown="Time",
#                  vec.colordown = c("brown","grey"))
#
#
# PlotScoresMatrix(ResPCALMEffects,
#                  alleffect = FALSE,
#                  EffectNames = c("Citrate","Hippurate","Hippurate:Citrate"),
#                  PCdim=c(2,2,2),
#                  design=UCH$design,
#                  varname.colorup = "Citrate",
#                  vec.colorup = c("red","blue","green"),
#                  varname.pch="Hippurate",
#                  vec.pch=c(1,2,3),
#                  varname.colordown="Time",
#                  vec.colordown = c("brown","grey"))
#
