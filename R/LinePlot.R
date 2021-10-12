#' @export LinePlot
#' @title Line plots
#'
#' @description
#' Draws Line plot(s).
#'
#' @param X A numerical matrix containing the rows to be drawn.
#' @param title Plot title.
#' @param rows A vector with either the row name(s) of the X matrix to plot (character) or the row index position(s).
#' @param type Type of graph to be drawn: "p" for point, "l" for line or "s" for segment. 
#' @param xlab If not \code{NULL}, label for the x-axis.
#' @param ylab If not \code{NULL}, label for the y-axis.
#' @param xaxis_type The data type of the x axis: either "numeric" or "character".
#' @param stacked Logical. If `TRUE`, will draw stacked plots, otherwise will draw separate plots.
#' @param ncol If stacked is `FALSE`, the number of columns to represent the separate plots.
#' @param nrow If stacked is `FALSE`, the number of rows to represent the separate plots.
#' @param hline If not \code{NULL}, draws (a) horizontal line(s).
#' @param size Argument of length 1 giving the points size (if `type` == "p") or the line size (if `type` == "l" or "s").
#' @param color If not `NULL`, argument of length 1 with possible values: "rows", a color name (character) or a numeric value representing a color.
#' @param shape The points shape if `type` == "p" (argument of length 1).
#'  
#'
#' @return A line plot.
#'
#' @examples
#'
#'LinePlot(X = UCH$outcomes)
#'
#' # separate plots
#' LinePlot(X = UCH$outcomes, rows = c(1:4), hline = NULL)
#' LinePlot(X = UCH$outcomes, rows = c(1:4), color = 2)
#' LinePlot(X = UCH$outcomes, rows = c(1:8), ncol=2)
#' LinePlot(X = UCH$outcomes, type = "p", 
#'          rows = c(1:8), ncol=2)
#'
#' # stacked plots
#' LinePlot(X = UCH$outcomes, rows = c(1:4), 
#'          stacked = TRUE, color = "rows") + 
#'          scale_color_brewer(palette="Set1")
#'
#'
#'
#' @import tidyr
#' @import ggplot2
#' @import dplyr
#' @import tibble


LinePlot <- function(X, title = "Line plot",  rows = 1,
                     type = c("l", "p", "s"), xlab = NULL, ylab = NULL,
                     xaxis_type = c("numeric", "character"), stacked = FALSE,
                     ncol = 1, nrow = NULL, hline = 0, size = 0.5,
                     color = NULL, shape = 1) {
  
  # checks =========================
  checkArg(X,"matrix",can.be.null = FALSE)
  checkArg(title, c("str", "length1"), can.be.null = TRUE)
  checkArg(xlab, c("str", "length1"), can.be.null = TRUE)
  checkArg(ylab, c("str", "length1"), can.be.null = TRUE)
  if (!is.null(ncol)){
    checkArg(ncol, c("int","length1"), can.be.null = FALSE)
  }
  if (!is.null(nrow)){
    checkArg(nrow, c("int","length1"), can.be.null = FALSE)
  }
  checkArg(hline, "num", can.be.null = TRUE)
  checkArg(size, "num", can.be.null = FALSE)
  checkArg(color, "length1", can.be.null = TRUE)
  checkArg(shape, "length1", can.be.null = FALSE)  
  checkArg(stacked, "bool", can.be.null = FALSE)  
  
  type <- match.arg(type)
  xaxis_type <- match.arg(xaxis_type)
  
  if (!is.numeric(rows) & !is.character(rows)){
    stop("rows is neither numeric or character")
  }
  
  # prepare the arguments  ==============================
  
  if (is.numeric(rows)){
    rows <- rownames(X)[rows]
  }
  
  X <- t(X[rows,, drop=FALSE])
  
  mn_xy <- make.names(rows) # correct the naming of variables
  
  X <- X %>% as.data.frame() %>%
    tibble::rownames_to_column(var = "x_axis")
  
  X_long <-  X %>% tidyr::pivot_longer(all_of(mn_xy), names_to = "rownames") 
  
  if (xaxis_type == "numeric") {
    rn <- X_long$x_axis
    rn <- as.numeric(rn)
    if(sum(is.na(rn))>0){
      stop("cannot convert column names to numerical, change xaxis_type from numeric to character")
    } else{
      X_long$x_axis <- rn
    }
  }
  
  if (!is.null(color)){
    if (color == "rows"){ 
      fig <- ggplot2::ggplot(data = X_long, ggplot2::aes(x = x_axis, y = value, 
                                                         group = rownames, color = rownames)) 
      color <- NULL
    }
    else {
      fig <- ggplot2::ggplot(data = X_long, ggplot2::aes(x = x_axis, y = value, 
                                                         group = rownames)) 
    }
  }
  
  
  fig <- fig + ggplot2::xlab(xlab) + ggplot2::ylab(ylab)
  
  # revert x axis if necessary 
  if (xaxis_type == "numeric")  {
    if ((X_long$x_axis[1] - X_long$x_axis[nrow(X_long)]) > 0) {
      fig <- fig + ggplot2::scale_x_reverse()
    }
  }
  
  if (!is.null(hline)){
    fig <- fig + ggplot2::geom_hline(yintercept = hline,
                                     size = 0.5, linetype = "dashed", 
                                     colour = "gray60")
  }
  
  if (type == "l"){
    if (!is.null(color)){
      fig <- fig + ggplot2::geom_line(color = color, size = size) 
    } else{ 
      fig <- fig + ggplot2::geom_line(size = size) 
    }
  }
  
  
  
  if (type == "p"){
    if (!is.null(color)){
      fig <- fig + ggplot2::geom_point(size = size, shape = shape, color = color)
    } else{ 
      fig <- fig + ggplot2::geom_point(size = size, shape = shape)
    }
  }
  
  if (type == "s"){
    if (!is.null(color)){
      fig <- fig + ggplot2::geom_segment(aes(xend = x_axis, yend = 0),
                                         size = size, lineend = "round", color = color)
    } else{ 
      fig <- fig + ggplot2::geom_segment(aes(xend = x_axis, yend = 0),
                                         size = size, lineend = "round")
    }
  }
  
  
  if(! stacked){
    # facet_wrap
    fig <- fig + 
      ggplot2::facet_wrap(~ rownames, ncol = ncol, nrow = nrow)  
  }
  
  return(fig)
  
}
