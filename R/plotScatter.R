#' @export plotScatter
#' @title Scatter plot
#'
#' @description
#' Produces a plot describing the relationship between two columns of the outcomes matrix \eqn{Y}. Colors and symbols can be chosen for the levels of the design factors. Ellipses, polygons or segments can be added to group different sets of points on the graph.
#'
#' @param Y A \eqn{n \times m} matrix with \eqn{n} observations and \eqn{m} variables.
#' @param xy x- and y-axis values: a vector of length 2 with either the column name(s) of the \eqn{Y} matrix to plot (character) or the index position(s) (integer).
#' @param design A \eqn{n \times k} "freely encoded" experimental design data.frame.
#' @param color If not \code{NULL}, a character string giving the column name of `design` to be used as color.
#' @param shape If not \code{NULL}, a character string giving the column name of `design` to be used as shape.
#' @param points_labs If not \code{NULL}, a character vector with point labels.
#' @param title Plot title.
#' @param xlab If not \code{NULL}, label for the x-axis.
#' @param ylab If not \code{NULL}, label for the y-axis.
#' @param size The points size, by default `2`.
#' @param size_lab The size of points labels, by default `3`.
#' @param drawShapes Multiple shapes can be drawn based on the `color`: `"none"` for no shape (default), `"ellipse"` (ellipses with `ggplot2::stat_ellipse()`), `"polygon"` (polygons with `ggplot2::geom_polygon()`) or `"segment"` (segment from the centroids with `ggplot2::geom_segment()`).
#' @param typeEl The type of ellipse, either `"norm"` (multivariate normal distribution, the default), `"t"` (multivariate t-distribution) or `"euclid"` (draws a circle with the radius equal to level, representing the euclidean distance from the center).
#' @param levelEl The confidence level at which to draw an ellipse, by default `0.9`.
#' @param alphaPoly The degree of transparency for polygons, by default `0.4`.
#' @param theme The `ggplot2` theme (default: `theme_bw()`), see `?ggtheme` for more info.
#' @param drawOrigin If \code{TRUE}, draws horizontal and vertical intercepts at (0,0).
#'
#' @return A `ggplot2` scatter plot.
#'
#' @examples
#'
#' data("UCH")
#'
#' # Without the design info
#' plotScatter(Y = UCH$outcomes, xy = c(453, 369))
#'
#' # With color and shape
#' plotScatter(Y = UCH$outcomes, design = UCH$design,
#'             xy = c(453, 369), color = "Hippurate",
#'             shape = "Citrate")
#'
#' # With color and shapes
#' plotScatter(Y = UCH$outcomes, design = UCH$design,
#'             xy = c(453, 369), color = "Hippurate",
#'             drawShapes = "ellipse")
#'
#' plotScatter(Y = UCH$outcomes, design = UCH$design,
#'             xy = c(453, 369), color = "Hippurate",
#'             drawShapes = "polygon")
#'
#' plotScatter(Y = UCH$outcomes, design = UCH$design,
#'             xy = c(453, 369), color = "Hippurate",
#'             drawShapes = "segment")
#'
#' # With customized shapes
#' library(ggplot2)
#' plotScatter(Y = UCH$outcomes, design = UCH$design,
#'             xy = c(453, 369), shape = "Hippurate", size = 3) +
#'   scale_discrete_identity(aesthetics = 'shape',
#'                           guide = 'legend')
#'
#' plotScatter(Y = UCH$outcomes, design = UCH$design,
#'             xy = c(453, 369), shape = "Hippurate") +
#'   scale_shape_discrete(solid=FALSE)
#'
#' plotScatter(Y = UCH$outcomes, design = UCH$design,
#'             xy = c(453, 369), shape = "Hippurate") +
#'   scale_shape_manual(values = c(15,16,17))
#'
#' # With labels
#' plotScatter(Y = UCH$outcomes, design = UCH$design,
#'             xy = c(453, 369), points_labs = rownames(UCH$design))
#'
#' @import ggplot2
#' @import ggrepel
#' @import dplyr
#' @importFrom plyr ddply

plotScatter <- function(Y, xy, design = NULL, color = NULL,
                        shape = NULL, points_labs = NULL,
                        title = "Scatter plot", xlab = NULL,
                        ylab = NULL, size = 2,  size_lab = 3,
                        drawShapes = c("none", "ellipse",
                                       "polygon", "segment"),
                        typeEl = c("norm","t","euclid"),
                        levelEl = 0.9, alphaPoly = 0.4,
                        theme = theme_bw(), drawOrigin = FALSE) {

  # checks ==============================
  checkArg(Y,"matrix",can.be.null = FALSE)
  checkArg(design,"data.frame",can.be.null = TRUE)
  checkArg(color,c("str","length1"),can.be.null = TRUE)
  checkArg(shape,c("str","length1"),can.be.null = TRUE)
  checkArg(points_labs,"str", can.be.null = TRUE)
  checkArg(title,c("str","length1"),can.be.null = TRUE)
  checkArg(xlab,c("str","length1"),can.be.null = TRUE)
  checkArg(ylab,c("str","length1"),can.be.null = TRUE)
  checkArg(size,c("num", "pos","length1"),can.be.null = FALSE)
  checkArg(size_lab,c("num", "pos","length1"),can.be.null = FALSE)
  checkArg(levelEl,c("num", "pos","length1"),can.be.null = FALSE)
  checkArg(alphaPoly,c("num", "pos","length1"),can.be.null = FALSE)

  typeEl <- match.arg(typeEl)
  drawShapes <- match.arg(drawShapes)

  if (drawShapes != "none"){
    if (is.null(color)){
      stop(paste0("drawShapes is set to ",drawShapes," but color is NULL"))
    }
  }


  if (!is.null(design)){
    match.arg(color, choices = c(colnames(design), NULL))
    match.arg(shape, choices = c(colnames(design), NULL))
  }

  if (length(color)==1 | length(shape)==1){
    if (is.null(design)){
      stop("color or shape is specified but design is NULL")
    }
  }


  if (length(xy) !=2){
    stop("xy is not of length 2")
  }

  if(is.numeric(xy)){
    checkArg(xy, c("pos","int"), can.be.null = FALSE)
  }


  if (!is.numeric(xy) & !is.character(xy)){
    stop("xy is neither numeric or character")
  }

  if (!is.null(design)){
    if(length(points_labs) != nrow(design) & !is.null(points_labs)){
      stop("length of points_labs is different than the number of observations")
    }
  }




  # prepare the arguments  ==============================
  if (is.numeric(xy)){
    if (is.null(colnames(Y)[xy])){
      colnames(Y)[xy] = xy = paste0("C",xy)
    }else{
      xy <- colnames(Y)[xy]
    }
  }else{
    if (!xy[1] %in% colnames(Y) | !xy[2] %in% colnames(Y)){
      stop("values in xy do not match the colnames of Y")
    }
  }

  mn_xy <- make.names(xy) # corrects the naming of variables

  if (is.null(xlab)) {
    xlab <- xy[1]
  }
  if (is.null(ylab)) {
    ylab <- xy[2]
  }

  out_df <- Y[,c(xy)]
  colnames(out_df) <- mn_xy

  if (!is.null(design)){
    design_df <- design[,c(color, shape), drop = FALSE]
    design_df <- design_df %>% mutate_all(as.factor)
    df_tot <- cbind(out_df, design_df)
  } else {df_tot = data.frame(out_df)}

  df_tot$points_labs <- points_labs

  # ScatterPlot  ==============================

  # ggplot ++++
  fig <- ggplot2::ggplot(data=df_tot,
                         ggplot2::aes_string(x=mn_xy[1],y=mn_xy[2],
                                             label = "points_labs",
                                             color = color,
                                             shape = shape))
  if (drawOrigin){
    fig <- fig +
      geom_hline(yintercept = 0, linetype = 2, color = "gray70") +
      geom_vline(xintercept = 0, linetype = 2, color = "gray80")
  }

  fig <- fig +
    ggplot2::geom_point(size=size) +
    labs(x = xlab, y = ylab, title = title) +
    theme

  # points_labs ++++
  if (!is.null(points_labs)) {
    fig <- fig +
      ggrepel::geom_text_repel(show.legend = F,
                               size = size_lab)
  }

  # drawShapes   ++++

  if (drawShapes !="none"){
    if (drawShapes == "ellipse"){
      fig <- fig + ggplot2::stat_ellipse(type = typeEl, level = levelEl)
    } else if (drawShapes == "polygon"){
        #getting the convex hull of each unique point set
        find_hull <- function(df) df[chull(df[,mn_xy[1]], df[,mn_xy[2]]), ]
        hulls <- plyr::ddply(df_tot, unique(c(color, shape)), find_hull)

        fig <- fig + ggplot2::geom_polygon(data = hulls,
                                    aes_string(fill=color, color = color),
                           alpha = alphaPoly)
    } else if (drawShapes == "segment"){

      centroids <- df_tot %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(unique(c(color, shape))))) %>%
        dplyr::summarise(across(dplyr::all_of(mn_xy),mean))

        df_tot_centr <- df_tot %>% dplyr::left_join(centroids, by = unique(c(color, shape)),
                                             suffix = c("", ".centroid"))

        centr_name <- paste0(mn_xy, ".centroid")
        fig <- fig +
          ggplot2::geom_segment(data = df_tot_centr,
                       aes_string(x = centr_name[1], y = centr_name[2],
                                  xend = mn_xy[1], yend = mn_xy[2],
                                  color = color))
    }
  }



  return(fig)

}

