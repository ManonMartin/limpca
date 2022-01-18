#' @export plotMeans
#' @title Means plot
#'
#' @description
#' Draws means plot(s).
#'
#' @param Y A numerical matrix containing the columns to be drawn.
#' @param design A nxk "free encoded" experimental design data frame.
#' @param cols A vector with either the column name(s) of the Y matrix to plot (character) or the column index position(s).
#' @param x A character string giving the `design` factor whose levels will form the x axis.
#' @param z A character string giving the `design` factor whose levels will form the traces.
#' @param w A character string giving the `design` factor whose levels will be used for the facet.
#' @param title Plot title.
#' @param xlab If not \code{NULL}, label for the x-axis.
#' @param ylab If not \code{NULL}, label for the y-axis.
#' @param color If not \code{NULL}, argument giving the color of the points and the line.
#' @param shape If not \code{NULL}, argument giving the points shape.
#' @param linetype If not \code{NULL}, argument giving the line type.
#' @param size Argument of length 1 giving the points size.
#' @param hline If not \code{NULL}, draws (a) horizontal line(s).
#' @param theme ggplot theme, see `?ggtheme` for more info.
#'
#' @return A means plot (ggplot).
#'
#' @examples
#'
#' # 1 factor
#' plotMeans(Y = UCH$outcomes, design = UCH$design, cols = "4.0628702",
#'           x = "Hippurate", color = "blue")
#'
#' # 2 factors
#' plotMeans(Y = UCH$outcomes, design = UCH$design, cols = c(364,365),
#'           x = "Hippurate", z = "Time", shape = 15)
#'
#' # 3 factors
#' plotMeans(Y = UCH$outcomes, design = UCH$design, cols = c(364,365),
#'           x = "Hippurate", z = "Time", w = "Citrate", linetype = 3)
#'
#' @import ggplot2
#' @importFrom stats aggregate
#' @import reshape2

plotMeans <- function(Y, design, cols = NULL, x, z = NULL, w = NULL,
                      title = NULL, xlab = NULL, ylab = NULL,
                      color = NULL, shape = NULL, linetype = NULL, size = 2,
                      hline = NULL, theme = theme_bw()){

  # checks =========================

  checkArg(Y,"matrix",can.be.null = FALSE)
  checkArg(design,"data.frame",can.be.null = FALSE)
  if(is.numeric(cols)){
    checkArg(cols,c("pos","int"),can.be.null = TRUE)
  }
  if (!is.numeric(cols) & !is.character(cols) & !is.null(cols)){
    stop("cols is neither numeric or character")
  }
  checkArg(x,c("str","length1"),can.be.null = FALSE)
  checkArg(z,c("str","length1"),can.be.null = TRUE)
  checkArg(w,c("str","length1"),can.be.null = TRUE)
  checkArg(title,c("str"),can.be.null = TRUE)
  checkArg(xlab,c("str","length1"),can.be.null = TRUE)
  checkArg(ylab,c("str"),can.be.null = TRUE)
  checkArg(color,c("str"),can.be.null = TRUE)
  checkArg(shape,c("num","pos"),can.be.null = TRUE)
  checkArg(linetype,c("num","pos"),can.be.null = TRUE)
  checkArg(size,c("num","pos","length1"),can.be.null = FALSE)
  checkArg(hline,"num",can.be.null = TRUE)

  if(is.null(cols)){
    if(ncol(Y) < 10)
      cols = c(1:ncol(Y))
    else{
      cols = c(1:10)
    }
  }

  if (max(cols) > ncol(Y)){
    stop(paste0("Columns (",paste0(cols, collapse = ",")
                ,") is beyond the number of columns of Y (",ncol(Y),")"))
  }

  if(!x %in% colnames(design)){
    stop(paste0(x," is not a factor of the design"))
  }
  if(!is.null(z) && !z %in% colnames(design)){
    stop(paste0(z," is not a factor of the design"))
  }
  if(!is.null(w) && !w %in% colnames(design)){
    stop(paste0(w," is not a factor of the design"))
  }

  if(is.null(z) & !is.null(w)){
    stop("w must be NULL if z is NULL")
  }

  if(is.null(z)){
    if(!is.null(color) & length(color) != 1){
        stop("color must be of length 1")
    }
    if(!is.null(shape) & length(shape) != 1){
      stop("shape must be of length 1")
    }
    if(!is.null(linetype) & length(linetype) != 1){
      stop("linetype must be of length 1")
    }
  }else if(!is.null(z)){
    zlevels <- length(levels(factor(design[,z])))
    if(!is.null(color) & length(color) != zlevels){
      stop("color must be the same length as the number of levels of z")
    }
    if(!is.null(shape) & length(shape) != zlevels){
      stop("shape must be the same length as the number of levels of z")
    }
    if(!is.null(linetype) & length(linetype) != zlevels){
      stop("linetype must be the same length as the number of levels of z")
    }
  }

  # prepare the arguments  ==============================

  if (is.character(cols)){
    cols <- grep(cols, colnames(Y))
  }

  dataplot <- stats::aggregate(Y[,cols], by = design[c(x,z,w)], mean)
  names(dataplot) <- c(x, z, w, colnames(Y)[cols])

  if(is.null(title)){
    if(is.null(z)){
      title = paste0("Mean response as a function of ",x)
    }else if(!is.null(z) & is.null(w)){
      title = paste0("Mean response as a function of ",x, " and ", z)
    }else if(!is.null(w)){
      title = paste0("Mean response as a function of ",x, ", ", z, " and ", w)
    }
  }
  title = rep(title, times = length(cols))

  if(is.null(xlab)){
    xlab = x
  }

  if(is.null(ylab)){
    ylab = rep(colnames(Y)[cols], times = length(cols))
  }else{
    ylab = rep(ylab, times = length(cols))
  }

  # Means Plot  ==============================

  if(is.null(z)){
    buildFig <- function(columns){

      # color
      if(is.null(color)){
        color = "#E97D72"
      }

      if(is.null(shape)){
        shape = 16
      }

      if(is.null(linetype)){
        linetype = 1
      }

      # ggplot
      columnsNames <- colnames(Y)[columns]
      dataplot1 <- reshape2::melt(dataplot,
                                  id.vars = c(x),
                                  measure.vars = columnsNames,
                                  variable.name = "Variables")

      fig <- ggplot2::ggplot(data=dataplot1,
                             ggplot2::aes(x = dataplot1[,x],
                                          y = dataplot1[,"value"],
                                          group = 1)) +
        ggplot2::geom_point(col = color, shape = shape, size = size) +
        ggplot2::geom_line(col = color, linetype = linetype)
    }

    fig <- lapply(cols, FUN = buildFig)

  }else if(!is.null(z) && is.null(w)){ # z is not null
    buildFig <- function(columns){
      columnsNames <- colnames(Y)[columns]

      dataplot1 <- reshape2::melt(dataplot,
                                  id.vars = c(x,z),
                                  measure.vars = columnsNames,
                                  variable.name = "Variables")

      fig <- ggplot2::ggplot(data=dataplot1,
                             ggplot2::aes(x = dataplot1[,x],
                                          y = dataplot1[,"value"],
                                          group=dataplot1[,z],
                                          col = dataplot1[,z],
                                          linetype = dataplot1[,z],
                                          shape = dataplot1[,z])) +
        ggplot2::geom_point(size = size) +
        ggplot2::geom_line() +
        ggplot2::labs(col = names(dataplot1)[2],
                      linetype = names(dataplot1)[2],
                      shape = names(dataplot1)[2])
    }

    fig <- lapply(cols, FUN = buildFig)

  }else if(!is.null(z) && !is.null(w)){ # z and w are not null
    buildFig <- function(columns){
      columnsNames <- colnames(Y)[columns]

      dataplot1 <- reshape2::melt(dataplot,
                                  id.vars = c(x,z,w),
                                  measure.vars = columnsNames,
                                  variable.name = "Variables")

      fig <- ggplot2::ggplot(data=dataplot1,
                             ggplot2::aes(x = dataplot1[,x],
                                          y = dataplot1[,"value"],
                                          group=dataplot1[,z],
                                          col = dataplot1[,z],
                                          linetype = dataplot1[,z],
                                          shape = dataplot1[,z])) +
        ggplot2::geom_point(size = size) +
        ggplot2::geom_line() +
        ggplot2::labs(col = names(dataplot1)[2],
                      linetype = names(dataplot1)[2],
                      shape = names(dataplot1)[2]) +
        ggplot2::facet_wrap(w, labeller = label_both)
    }

    fig <- lapply(cols, FUN = buildFig)

  }

  # color
  if(!is.null(color)){
    fig <- lapply(1:length(cols), function(x) fig[[x]] +
                    ggplot2::scale_color_manual(values = color))
  }

  # shape
  if(!is.null(shape)){
    fig <- lapply(1:length(cols), function(x) fig[[x]] +
                    ggplot2::scale_shape_manual(values = shape))
  }

  # linetype
  if(!is.null(linetype)){
    fig <- lapply(1:length(cols), function(x) fig[[x]] +
                    ggplot2::scale_linetype_manual(values = linetype))
  }

  fig <- lapply(1:length(cols), function(x) fig[[x]] +
                  ggplot2::labs(title = title[x], x = xlab, y = ylab[x]) + theme)

  if (!is.null(hline)){
    fig <- lapply(fig, function(x) x + ggplot2::geom_hline(yintercept = hline,
                                     size = 0.5, linetype = "dashed",
                                     colour = "gray60"))
  }

  names(fig) = colnames(Y)[cols]

  return(fig)

}
