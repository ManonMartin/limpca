#' @export PlotDesign
#' @title Plot the design matrix
#' @description Plot the experimental design of the data
#'
#' @param design Design matrix of the data
#' @param var1_name Variable for the X axis
#' @param var2_name Variable for the Y axis
#' @param var3_name Variable used for the color, the size and the shape
#'
#' @return A ggplot of the design
#'
#' @author Sebastien Franceschini
#' @examples
#' data("UCH.RData")
#' PlotDesign(design=design,var1_name="Hippurate",var2_name="Citrate",var3_name="Day")
#'
#' @import ggplot2
#' @import gridExtra
#' @import reshape2

PlotDesign = function(design,var1_name,var2_name,var3_name){

  var1 = unlist(design[which(colnames(design)==var1_name)])
  var2 = unlist(design[which(colnames(design)==var2_name)])
  var3 = unlist(design[which(colnames(design)==var3_name)])

  ggplot(design) +
    geom_point(aes(x=var1,y=var2,color=var3,size=var3),shape=var3) +
    labs(title = "Design") +
    xlab(var1_name) +
    ylab(var2_name) +
    scale_colour_discrete(name=var3_name) +
    scale_shape_discrete(name=var3_name) +
    scale_size_discrete(name=var3_name)
}
