#' @export plotScatterM
#' @title Scatter plot matrix
#'
#' @description
#' Produces a scatter plot matrix between the selected columns of the outcomes matrix `Y` choosing specific colors and markers for the design factors on the upper and lower diagonal.
#'
#' @param Y nxm matrix with n observations and m variables.
#' @param cols A vector with either the column names of the Y matrix to plot (character) or the column index positions.
#' @param design A nxk "free encoded" experimental design data frame.
#' @param labelVector Labels to display on the diagonal. If `NULL`, the `cols` names are used.
#' @param varname.colorup A character with the name of the variable used to color the upper triangle.
#' @param varname.colordown A character with the name of the variable used to color the lower triangle.
#' @param varname.pchup A character with the name of the variable used to mark points for the upper triangle.
#' @param varname.pchdown A character with the name of the variable used to mark points for the lower triangle.
#' @param vec.colorup A color vector with a length equivalent to the number of levels of varname.colorup.
#' @param vec.colordown A color vector with a length equivalent to the number of levels of varname.colordown.
#' @param vec.pchup A pch vector with a length equivalent to the number of levels of varname.pchup.
#' @param vec.pchdown A pch vector with a length equivalent to the number of levels of varname.pchdown.
#'
#' @return A matrix of scatter plots.
#'
#' @examples
#'
#' data("UCH")
#'
#' plotScatterM(Y = UCH$outcomes, cols = c(1:4), design = UCH$design,
#'              varname.colorup = "Hippurate", varname.colordown = "Citrate",
#'              varname.pchup = "Time", varname.pchdown = "Day",
#'              vec.colorup = c("red","blue","black"),
#'              vec.colordown = c("orange","purple","green"),
#'              vec.pchup = c(1,2), vec.pchdown = c(15,16))
#'
#' @import graphics grDevices
#' @import ggsci


plotScatterM <- function(Y, cols, design, labelVector = NULL, title = "Scatterplot matrix",
                         varname.colorup = NULL, varname.colordown = NULL,
                         varname.pchup = NULL, varname.pchdown = NULL,
                         vec.colorup = NULL, vec.colordown = NULL,
                         vec.pchup = NULL, vec.pchdown = NULL){

  # Check arguments
  checkArg(Y,"matrix",can.be.null = FALSE)
  checkArg(design,"data.frame",can.be.null = TRUE)
  checkArg(labelVector,"str", can.be.null = TRUE)
  checkArg(title,c("str","length1"),can.be.null = TRUE)
  checkArg(varname.colorup,c("str","length1"),can.be.null = TRUE)
  checkArg(varname.colordown,c("str","length1"),can.be.null = TRUE)
  checkArg(varname.pchup,c("str","length1"),can.be.null = TRUE)
  checkArg(varname.pchdown,c("str","length1"),can.be.null = TRUE)

  if(is.numeric(cols)){
    checkArg(cols, c("pos","int"), can.be.null = FALSE)
  }

  if(!is.numeric(cols) & !is.character(cols)){
    stop("cols is neither numeric or character")
  }

  if (!is.null(varname.colorup)){
    if (!(varname.colorup %in% colnames(design))){
      stop(paste0("varname.colorup (", varname.colorup,
                  ") is not matching column names of design: ",paste0(colnames(design), collapse = ", ")))
    }
  }

  if (!is.null(varname.colordown)){
    if (!(varname.colordown %in% colnames(design))){
      stop(paste0("varname.colordown (", varname.colordown,
                  ") is not matching column names of design: ",paste0(colnames(design), collapse = ", ")))
    }
  }

  if (!is.null(varname.pchup)){
    if (!(varname.pchup %in% colnames(design))){
      stop(paste0("varname.pchup (", varname.pchup,
                  ") is not matching column names of design: ",paste0(colnames(design), collapse = ", ")))
    }
  }

  if (!is.null(varname.pchdown)){
    if (!(varname.pchdown %in% colnames(design))){
      stop(paste0("varname.pchdown (", varname.pchdown,
                  ") is not matching column names of design: ",paste0(colnames(design), collapse = ", ")))
    }
  }

  if(!is.null(varname.colorup) & !is.null(vec.colorup)){
    if(length(levels(design[[varname.colorup]])) != length(vec.colorup)){
      stop("vec.colorup must have the same length as varname.colorup")
    }
  }

  if(!is.null(varname.colordown) & !is.null(vec.colordown)){
    if(length(levels(design[[varname.colordown]])) != length(vec.colordown)){
      stop("vec.colordown must have the same length as varname.colordown")
    }
  }

  if(!is.null(varname.pchup) & !is.null(vec.pchup)){
    if(length(levels(design[[varname.pchup]])) != length(vec.pchup)){
      stop("vec.pchup must have the same length as varname.pchup")
    }
  }

  if(!is.null(varname.pchdown) & !is.null(vec.pchdown)){
    if(length(levels(design[[varname.pchdown]])) != length(vec.pchdown)){
      stop("vec.pchdown must have the same length as varname.pchdown")
    }
  }

  if(is.null(labelVector)){
    labelVector = colnames(Y[,cols])
  }

  if(length(labelVector) != length(cols)){
    stop("labelVector must have the same length as cols")
  }


  # Determine the graphic parameters ===============
  plot_type_colored = TRUE
  plot_type_pch = TRUE

  # varnames
  if(is.null(varname.colordown) & is.null(varname.colorup)){
    if(!is.null(varname.pchup) & !is.null(varname.pchdown)){
      varname.colordown=varname.pchdown
      varname.colorup=varname.pchup
    }else if(!is.null(varname.pchup)){
      varname.colordown=varname.pchup
      varname.colorup=varname.pchup
    }else if(!is.null(varname.pchdown)){
      varname.colordown=varname.pchdown
      varname.colorup=varname.pchdown
    }
  }

  if(is.null(varname.pchdown) & is.null(varname.pchup)){
    if(!is.null(varname.colorup) & !is.null(varname.colordown)){
      varname.pchdown=varname.colordown
      varname.pchup=varname.colorup
    }else if(!is.null(varname.colorup)){
      varname.pchdown=varname.colorup
      varname.pchup=varname.colorup
    }else if(!is.null(varname.colordown)){
      varname.pchdown=varname.colordown
      varname.pchup=varname.colordown
    }
  }


  if(is.null(varname.colorup)){
    if(is.null(varname.colordown)){
      plot_type_colored = FALSE
    }else{
      varname.colorup=varname.colordown
      if(is.null(vec.colordown)){
        vec.colorup=ggsci::pal_d3("category20")(length(levels(design[,which(names(design)==varname.colorup)])))
      }else{
        vec.colorup=vec.colordown
      }
    }
  }else{
    if(is.null(varname.colordown)){
      varname.colordown=varname.colorup}
    if(is.null(vec.colorup)){ # Default
      if(is.null(vec.colordown)){
        vec.colorup=ggsci::pal_d3("category20")(length(levels(design[,which(names(design)==varname.colorup)])))
      }else if(varname.colordown == varname.colorup){
        vec.colorup=vec.colordown
      }
    }
  }


  if(is.null(varname.colordown)){
    if(is.null(varname.colorup)){
      plot_type_colored = FALSE
    }else{ # Copied
      varname.colordown = varname.colorup
      if(is.null(vec.colorup)){
        vec.colordown=ggsci::pal_igv("default")(length(levels(design[,which(names(design)==varname.colordown)])))
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
        vec.colordown=ggsci::pal_igv("default")(length(levels(design[,which(names(design)==varname.colordown)])))
      }
    }
  }

  if(is.null(varname.pchdown)){
    if(is.null(varname.pchup)){
      plot_type_pch=FALSE
    }else{
      varname.pchdown = varname.pchup
      if(is.null(vec.pchup)){
        # vec.pchdown = c(1:(length(levels(design[,which(names(design)==varname.pchdown)]))))
        pchs <- c(4,16,2,1,3,0, 8, 6, 7, 5, 9:15,17:25)
        vec.pchdown = pchs[c(1:(length(levels(design[,which(names(design)==varname.pchdown)]))))]
      }else{
        vec.pchdown = vec.pchup
      }
    }
  }else{
    if(is.null(vec.pchdown)){
      if(is.null(vec.pchup)){
        # vec.pchdown = c(1:(length(levels(design[,which(names(design)==varname.pchdown)]))))
        pchs <- c(4,16,2,1,3,0, 8, 6, 7, 5, 9:15,17:25)
        vec.pchdown = pchs[c(1:(length(levels(design[,which(names(design)==varname.pchdown)]))))]
      }else if(varname.pchdown == varname.pchup){
        vec.pchdown = vec.pchup
      }else{
        vec.pchdown = c(1:(length(levels(design[,which(names(design)==varname.pchdown)]))))
      }
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
    }
  }

  # Creation of the upper panel
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

    graphics::points(x,y,col=colorvector,pch=pchvector)
  }

  # Creation of the lower panel
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

    graphics::points(x,y,col=colorvector,pch=pchvector)
  }

  # Creation Legend

  Legend = function(){

    #Dividing the space for each legend

    totallegend = length(c(vec.pchup,vec.pchdown,vec.colordown,vec.colorup))
    spacebyline = 0.85/totallegend

    spacecolup = c(0.05+length(vec.pchup)*spacebyline + length(vec.pchdown)*spacebyline +length(vec.colordown)*spacebyline,0.9)
    spacepchup = c(0.05+length(vec.pchup)*spacebyline + length(vec.pchdown)*spacebyline,0.05+length(vec.pchup)*spacebyline + length(vec.pchdown)*spacebyline +length(vec.colordown)*spacebyline)
    spacepchdown = c(0.05,0.05+length(vec.pchup)*spacebyline)
    spacecoldown = c(0.05+length(vec.pchup)*spacebyline,0.05+length(vec.pchup)*spacebyline + length(vec.pchdown)*spacebyline)

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

    graphics::legend(x=c(0.93,1),y=spacecoldown,
                     title = varname.colordown,
                     legend = levels(design[,which(names(design)==varname.colordown)]),
                     bty = "n",
                     col = vec.colordown,
                     pch = 15,
                     inset = c(0.03,0.3),
                     title.adj = 0,
                     cex = 0.7)

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


  # Building of the pairs plot

  if(!plot_type_colored & !plot_type_pch){
    graphics::pairs(Y[,cols],
                    gap = 0.3,
                    labels = labelVector)
    graphics::title(main = title, font.main = 1, adj = 0, line = 3)

  }else if(plot_type_colored & plot_type_pch){
    graphics::pairs(Y[,cols],
                    upper.panel = panelup,
                    lower.panel = paneldown,
                    gap = 0.3,
                    oma = c(3, 3, 5, 10),
                    labels = labelVector)
    graphics::title(main = title, font.main = 1, adj = 0, line = 3)
    graphics::par(xpd=TRUE)
    Legend()
    graphics::par(xpd=FALSE)
  }

}
