#' @export ScatterPlot
#' @title ScatterPlot
#'
#' @description
#' Draws a scatter plot.
#'
#' @param outcomes The nxm matrix with n observations and m response variables.
#' @param design The nxk "free encoded" experimental design data frame.
#' @param xy x- and y-axis values: A vector of length 2 with either column names of the outcomes matrix to plot (character) or the index positions.
#' @param color If not \code{NULL}, a character string giving the column name of `design` to be used as color
#' @param shape If not \code{NULL}, a character string giving the column name of `design` to be used as shape
#' @param points_labs If not \code{NULL}, a character vector with point labels.
#' @param title Plot title.
#' @param xlab If not \code{NULL}, label for the x-axis.
#' @param ylab If not \code{NULL}, label for the y-axis.
#' @param size The points size.
#' @param size_lab The size of points labels.
#' @param drawEllipses If \code{TRUE}, will draw ellipse(s).
#' @param typeEl The type of ellipse, either `norm` (multivariate normal distribution), `t` (multivariate t-distribution) or `euclid` (draws a circle with the radius equal to level, representing the euclidean distance from the center).
#' @param levelEl The confidence level at which to draw an ellipse.
#'
#' @return A scatterplot in the current device.
#' 
#' @examples
#'
#' data("UCH")
#' 
#' # With color and shape
#' ScatterPlot(outcomes = UCH$outcomes, design = UCH$design, 
#' xy = c(453, 369), color = "Hippurate",  shape = "Citrate")
#' 
#' # With color and ellipses
#' ScatterPlot(outcomes = UCH$outcomes, design = UCH$design, 
#' xy = c(453, 369), color = "Hippurate", 
#' drawEllipses = TRUE)
#'
#' # with labels
#' ScatterPlot(outcomes = UCH$outcomes, design = UCH$design, 
#' xy = c(453, 369), points_labs = rownames(UCH$design))
#'
#' @importFrom grDevices dev.new
#' @import ggplot2
#' @import reshape2
#' @import gridExtra
#' @import ggrepel

ScatterPlot <- function(outcomes, design, xy, color = NULL, 
                        shape = NULL, points_labs = NULL,
                        title = "scatterplot", xlab = NULL, 
                        ylab = NULL, size = 2,  size_lab = 3, 
                        drawEllipses = FALSE, 
                        typeEl = c("norm","t","euclid"), 
                        levelEl = 0.9) {
  
  # checks ==============================
  checkArg(design,"data.frame",can.be.null = FALSE)
  checkArg(outcomes,"matrix",can.be.null = FALSE)
  checkArg(color,c("str","length1"),can.be.null = TRUE)
  checkArg(shape,c("str","length1"),can.be.null = TRUE)
  checkArg(points_labs,"str", can.be.null = TRUE)
  checkArg(title,c("str","length1"),can.be.null = TRUE)
  checkArg(xlab,c("str","length1"),can.be.null = TRUE)
  checkArg(ylab,c("str","length1"),can.be.null = TRUE)
  checkArg(size,c("num", "pos","length1"),can.be.null = FALSE)  
  checkArg(size_lab,c("num", "pos","length1"),can.be.null = FALSE)
  checkArg(drawEllipses,c("bool","length1"),can.be.null = FALSE)
  checkArg(levelEl,c("num", "pos","length1"),can.be.null = FALSE)
  
  typeEl <- match.arg(typeEl)
  match.arg(color, choices = c(colnames(design), NULL))
  match.arg(shape, choices = c(colnames(design), NULL))
  
  
  if (length(xy) !=2){
    stop("xy is not of length 2")
  }
  
  if (!is.numeric(xy) & !is.character(xy)){
    stop("xy is neither numeric or character")
  }
  
  # if (!color %in% colnames(design)){
  #   stop("color is not a column name of design")
  # }
  # 
  # if (!shape %in% colnames(design)){
  #   stop("shape is not a column name of design")
  # }
  
  if(length(points_labs) != nrow(design) & !is.null(points_labs)){
    stop("length of points_labs is different than the number of observations")
  }
  
  
  # prepare the arguments  ==============================
  if (is.numeric(xy)){
    xy <- colnames(outcomes)[xy]
  }
  mn_xy <- make.names(xy) # correct the naming of variables
  
  if (is.null(xlab)) {
    xlab <- xy[1]
  }
  if (is.null(ylab)) {
    ylab <- xy[2]
  }
  
  out_df <- outcomes[,c(xy)]
  colnames(out_df) <- mn_xy
  
  design_df <- design[,c(color, shape), drop = FALSE]
  
  df_tot <- cbind(out_df, design_df)

  df_tot$points_labs <- points_labs
  
  # ScatterPlot  ==============================
  
  # ggplot ++++
  fig <- ggplot2::ggplot(data=df_tot, 
                         ggplot2::aes_string(x=mn_xy[1],y=mn_xy[2],
                                             label = "points_labs",
                                             color = color, 
                                             shape = shape)) + 
    ggplot2::geom_point(size=size) +
    labs(x = xlab, y = ylab, title = title)
  
  # points_labs ++++
  if (!is.null(points_labs)) {
    fig <- fig + 
      ggrepel::geom_text_repel(show.legend = F, 
                               size = size_lab)
  }
  
  # drawEllipses  ++++
  if (drawEllipses) {
    fig <- fig + ggplot2::stat_ellipse(type = typeEl, level = levelEl)
  }
  
  return(fig)
  
}
