#' @export PlotDesign
#' @title Plot the design matrix
#' @description Plot the experimental design of the data
#'
#' @param design A nxk "free encoded" experimental design data frame.
#' @param x A character string giving the column name of `design` to be used for the x axis.
#' @param y A character string giving the column name of `design` to be used for the y axis.
#' @param rows If not `NULL`,  a character vector with one or several column name(s) of `design` to be used for facetting along the rows.
#' @param cols If not `NULL`, a character vector with one or several column name(s) of `design` to be used for facetting along the columns
#' @param title Plot title.
#'
#' @return A plot of the design.
#'
#' @examples
#'
#' ### mtcars
#' data(mtcars)
#' df <-  mtcars %>%
#'   select(cyl, vs, am, gear, carb) %>%
#'   as.data.frame() %>%
#'   mutate(across(everything(), as.factor))
#'
#' # 2 factors
#' PlotDesign(design = df, x = "cyl", y = "vs",rows = NULL,
#'           cols = NULL)
#' # 3 factors
#' PlotDesign(design = df, x = "cyl", y = "vs",rows = c("am"),
#'           cols = NULL)
#' # 4 factors
#' PlotDesign(design = df, x = "cyl", y = "vs",rows = c("am"),
#'           cols = c("gear"))
#' # 5 factors
#' PlotDesign(design = df, x = "cyl", y = "vs",rows = c("am", "carb"),
#'           cols = c("gear"))
#'
#' PlotDesign(design = df, x = "cyl", y = "vs",rows = c("am", "carb"),
#'          cols = c("vs"))
#'
#' ### UCH
#' data("UCH")
#' PlotDesign(design = UCH$design, x = "Hippurate", y = "Citrate",rows = "Day")
#'
#' @import ggplot2
#' @import dplyr

PlotDesign <- function(design, x, y, rows = NULL,
                       cols = NULL, title = "Plot of the design"){

  # checks ===================
  checkArg(design,"data.frame",can.be.null = FALSE)
  checkArg(x,c("str","length1"),can.be.null = TRUE)
  checkArg(y,c("str","length1"),can.be.null = TRUE)
  checkArg(rows,c("str"),can.be.null = TRUE)
  checkArg(cols,c("str"),can.be.null = TRUE)
  checkArg(title,c("str","length1"),can.be.null = TRUE)

  match.arg(x, choices = colnames(design))
  match.arg(y, choices = colnames(design))

  if (!is.null(rows)){
    if (!all(rows %in% colnames(design))){
      stop(paste0("rows (",paste0(rows, collapse = ", "),
                  ") is not matching column names of design: ",paste0(colnames(design), collapse = ", ")))
    }
  }

  if (!is.null(cols)){
    if (!all(cols %in% colnames(design))){
      stop(paste0("cols (",paste0(cols, collapse = ", "),
                  ") is not matching column names of design: ",paste0(colnames(design), collapse = ", ")))
    }
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
                      vjust=-0.40)


  if (!is.null(rows) | !is.null(cols)){
    p <- p + ggplot2::facet_grid(as.formula(form),
                        labeller = label_both)

  }
  p <- p + ggplot2::ggtitle(title)
  return(p)

}
