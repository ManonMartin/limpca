#' @export plotDesign
#' @title Plot of the design matrix
#'
#' @description
#' Provides a graphical representation of the experimental design for up to 4 factors. It allows to visualize factors levels and check the balancing of the design.
#'
#' @param design A nxk "free encoded" experimental design data frame.
#' @param x By default: the first column of `design`. If not `NULL`, a character string giving the column name of `design` to be used for the x axis.
#' @param y By default: the second column of `design`. If not `NULL`, a character string giving the column name of `design` to be used for the y axis.
#' @param cols By default: the third column of `design` if present. If not `NULL`, a character vector with one or several column name(s) of `design` to be used for faceting along the columns.
#' @param rows By default: the fourth column of `design` if present. If not `NULL`, a character vector with one or several column name(s) of `design` to be used for faceting along the rows.
#' @param title Plot title.
#' @param theme ggplot theme, see `?ggtheme` for more info.
#' @param nudge_y Nudge points a fixed distance for text labels (used in `geom_text()`), see ggplot2::position_nudge.
#'
#' @return A plot of the design (ggplot).
#'
#' @examples
#'
#' ### mtcars
#' data(mtcars)
#' library(tidyverse)
#' df <-  mtcars %>%
#'   dplyr::select(cyl, vs, am, gear, carb) %>%
#'   as.data.frame() %>%
#'   dplyr::mutate(across(everything(), as.factor))
#'
#' # 2 factors
#' plotDesign(design = df, x = "cyl", y = "vs",
#'           cols = NULL, rows = NULL)
#' # 3 factors
#' plotDesign(design = df, x = "cyl", y = "vs",
#'           cols = NULL, rows = c("am"))
#' # 4 factors
#' plotDesign(design = df, x = "cyl", y = "vs",
#'           cols = c("gear"), rows = c("am"))
#' # 5 factors
#' plotDesign(design = df, x = "cyl", y = "vs",
#'           cols = c("gear"), rows = c("am","carb"))
#'
#' plotDesign(design = df, x = "cyl", y = "vs",
#'           cols = c("vs"), rows = c("am","carb"))
#'
#' ### UCH
#' data("UCH")
#' plotDesign(design = UCH$design, x = "Hippurate", y = "Citrate", rows = "Day")
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyverse

plotDesign <- function(design, x = NULL, y = NULL, rows = NULL, cols = NULL,
                       title = "Plot of the design", theme = theme_bw(),
                       nudge_y = 0.2){

  # checks ===================

  checkArg(design,"data.frame",can.be.null = FALSE)
  checkArg(x,c("str","length1"),can.be.null = TRUE)
  checkArg(y,c("str","length1"),can.be.null = TRUE)
  checkArg(rows,c("str"),can.be.null = TRUE)
  checkArg(cols,c("str"),can.be.null = TRUE)
  checkArg(title,c("str","length1"),can.be.null = TRUE)

  #
  # y <- match.arg(y, choices = colnames(design))
  if (!is.null(x)){
    x <- match.arg(x, choices = colnames(design))
  } else{
    x <- colnames(design)[1]
  }

  if (!is.null(y)){
    y <- match.arg(y, choices = colnames(design))
  } else{
    y <- colnames(design)[2]
  }


  if (!is.null(cols)){
    if (!all(cols %in% colnames(design))){
      stop(paste0("cols (",paste0(cols, collapse = ", "),
                  ") is not matching column names of design: ",
                  paste0(colnames(design), collapse = ", ")))
    }
  } else{
    cn <- colnames(design)[3]
    if(ncol(design)>2 & ! cn %in% c(rows,x,y)){cols = colnames(design)[3]}
  }

  if (!is.null(rows)){
    if (!all(rows %in% colnames(design))){
      stop(paste0("rows (",paste0(rows, collapse = ", "),
                  ") is not matching column names of design: ",
                  paste0(colnames(design), collapse = ", ")))
    }
  } else{
    cn <- colnames(design)[4]
    if(ncol(design)>3& ! cn %in% c(cols,x,y)){rows = colnames(design)[4]}
  }



  # plot design ===================

  df_count <- design %>%
    dplyr::count(dplyr::across(dplyr::all_of(c(x,y,rows,cols))))

  p <- ggplot2::ggplot(df_count, ggplot2::aes_string(x = x, y = y)) +
    ggplot2::geom_point()

  if (is.null(rows)){
    rows="."
  } else{rows = paste0(rows, collapse = "+")}

  if (is.null(cols)){
    cols="."
  } else{cols = paste0(cols, collapse = "+")}

  form <- paste0(rows,"~",cols)

  p <- p +  ggplot2::geom_text(aes_string(label = "n"),
                              nudge_y = nudge_y)


  if (!is.null(rows) | !is.null(cols)){
    p <- p + ggplot2::facet_grid(as.formula(form),
                        labeller = label_both)

  }
  p <- p + ggplot2::ggtitle(title) + theme


  return(p)

}
