#' @export plotLine
#' @title Line plot
#'
#' @description
#' Generates the response profile of one or more observation i.e. plots of one or more rows of the outcomes matrix on the y-axis against the m response variables on the x-axis. Depending on the response type (spectra, gene expression...), point, line or segment plots are offered.
#'
#' @param Y A numerical matrix containing the rows to be drawn.
#' @param rows A vector with either the row name(s) of the Y matrix to plot (character) or the row index position(s). Default to 1.
#' @param type Type of graph to be drawn: "p" for point, "l" for line (default) or "s" for segment.
#' @param title Plot title.
#' @param xlab If not \code{NULL}, label for the x-axis.
#' @param ylab If not \code{NULL}, label for the y-axis.
#' @param xaxis_type The data type of the x axis: either "numeric" (default) or "character".
#' @param stacked Logical. If `TRUE`, will draw stacked plots, otherwise will draw separate plots.
#' @param ncol If stacked is `FALSE`, the number of columns to represent the separate plots. Default to 1.
#' @param nrow If stacked is `FALSE`, the number of rows to represent the separate plots.
#' @param facet_label If stacked is `FALSE`, the labels of the separate plots.
#' @param hline If not \code{NULL}, draws (a) horizontal line(s), by default at y intercept = 0.
#' @param size Argument of length 1 giving the points size (if `type` == "p") or the line size (if `type` == "l" or "s").
#' @param color If not `NULL`, argument of length 1 with possible values: "rows", a color name (character) or a numeric value representing a color.
#' @param shape Argument of length 1 giving the points shape (default = 1) if `type` == "p".
#' @param theme ggplot theme (default: `theme_bw()`), see `?ggtheme` for more info.
#' @param ang_x_axis If not `NULL`, rotation angle to rotate the x axis text (based on the argument `axis.text.x` from ggplot2::theme)
#'
#' @return A line plot (ggplot).
#'
#' @examples
#'
#' plotLine(Y = UCH$outcomes)
#'
#' # separate plots
#' plotLine(Y = UCH$outcomes, rows = c(1:4), hline = NULL)
#' plotLine(Y = UCH$outcomes, rows = c(1:4), color = 2)
#' plotLine(Y = UCH$outcomes, rows = c(1:8), ncol=2)
#' plotLine(Y = UCH$outcomes, type = "p",
#'          rows = c(1:8), ncol=2)
#'
#' # stacked plots
#' library(ggplot2)
#' plotLine(Y = UCH$outcomes, rows = c(1:4),
#'          stacked = TRUE, color = "rows") +
#'          scale_color_brewer(palette="Set1")
#'
#' @import tidyr
#' @import ggplot2
#' @import dplyr
#' @import tibble


plotLine <- function(Y,  rows = 1, type = c("l", "p", "s"),
                     title = "Line plot", xlab = NULL, ylab = NULL,
                     xaxis_type = c("numeric", "character"), stacked = FALSE,
                     ncol = 1, nrow = NULL, facet_label = NULL, hline = 0,
                     size = 0.5, color = NULL, shape = 1, theme = theme_bw(),
                     ang_x_axis = NULL) {

  # checks =========================
  checkArg(Y,"matrix",can.be.null = FALSE)
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
  checkArg(facet_label, c("str"), can.be.null = TRUE)
  checkArg(ang_x_axis, "int", can.be.null = TRUE)

  type <- match.arg(type)
  xaxis_type <- match.arg(xaxis_type)

  if(is.numeric(rows)){
    checkArg(rows, c("pos","int"), can.be.null = FALSE)
  }

  if (!is.numeric(rows) & !is.character(rows)){
    stop("rows is neither numeric or character")
  }

  if (!is.null(facet_label) && length(rows) != length(facet_label)){
    stop(paste0("the number of facet_label differs from the number of rows"))
  }

  # prepare the arguments  ==============================

  if(is.null(rownames(Y))){
    rownames(Y) <- rownames(Y, do.NULL = FALSE, prefix = "X.")
  }

  if (is.numeric(rows)){
    rows <- rownames(Y)[rows]
  }

  Y <- t(Y[rows,, drop=FALSE])

  mn_xy <- make.names(rows) # corrects the naming of variables

  Y <- Y %>% as.data.frame() %>%
    tibble::rownames_to_column(var = "x_axis")

  Y_long <-  Y %>% tidyr::pivot_longer(all_of(mn_xy), names_to = "rownames")

  if (xaxis_type == "numeric") {
    rn <- Y_long$x_axis
    rn <- as.numeric(rn)
    if(sum(is.na(rn))>0){
      stop("cannot convert column names to numerical, change xaxis_type from numeric to character")
    } else{
      Y_long$x_axis <- rn
    }
  }

  if (!is.null(color)){
    if (color == "rows"){
      fig <- ggplot2::ggplot(data = Y_long, ggplot2::aes(x = x_axis, y = value,
                                                         group = rownames, color = rownames))
      color <- NULL
    } else {
      fig <- ggplot2::ggplot(data = Y_long, ggplot2::aes(x = x_axis, y = value,
                                                         group = rownames))
    }
  } else {
      fig <- ggplot2::ggplot(data = Y_long, ggplot2::aes(x = x_axis, y = value,
                                                         group = rownames))
  }


  fig <- fig + ggplot2::ggtitle(title) + ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    theme

  # revert x axis if necessary
  if (xaxis_type == "numeric")  {
    if ((Y_long$x_axis[1] - Y_long$x_axis[nrow(Y_long)]) > 0) {
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
    if(is.null(facet_label)){
      fig <- fig +
        ggplot2::facet_wrap(~ rownames, ncol = ncol, nrow = nrow)
    } else{
      names(facet_label) <- rows
      fig <- fig +
        ggplot2::facet_wrap(~ rownames, ncol = ncol, nrow = nrow,
                            labeller = labeller(rownames = facet_label))
    }
  }

  if(!is.null(ang_x_axis)){
    fig <- fig +
      ggplot2::theme(axis.text.x = element_text(angle = ang_x_axis,
                                                vjust = 0.5, hjust=1))
  }

  return(fig)

}
