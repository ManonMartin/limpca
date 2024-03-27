#' @export plotDesign
#' @title Plot of the design matrix
#'
#' @description
#' Provides a graphical representation of the experimental design. It allows to visualize factors levels and check the design balance.
#'
#' @param design A data.frame representing the \eqn{n \times k} "freely encoded" experimental design. Can be `NULL` if `lmpDataList` is defined.
#' @param lmpDataList If not `NULL`, a list with outcomes, design and formula, as outputted by \code{\link{data2LmpDataList}}.
#' @param x By default, the first column of `design` ; otherwise if not `NULL`, a character string giving the column name of `design` to be used for the x-axis. The column needs to be a factor.
#' @param y By default, the second column of `design` if present ; otherwise if not `NULL`, a character string giving the column name of `design` to be used for the y-axis.
#' @param cols By default, the third column of `design` if present ; otherwise if not `NULL`, a character vector with one or several column name(s) of `design` to be used for faceting along the columns. The column needs to be a factor.
#' @param rows By default, the fourth column of `design` if present ; otherwise if not `NULL`, a character vector with one or several column name(s) of `design` to be used for faceting along the rows. The column needs to be a factor.
#' @param title Plot title.
#' @param theme The `ggplot2` theme, see `?ggtheme` for more info.
#'
#' @return A `ggplot2` plot of the design matrix.
#'
#' @details
#'
#' Either \code{design} or \code{lmpDataList} need to be defined. If both are given, the priority goes to \code{design}.
#' The default behavior (parameters `x`, `y`, `cols` and `rows` are `NULL`) uses the first four columns of `df`. If at least one of these arguments is not `NULL`, the function will only use the non `NULL` parameters to be displayed.
#'
#' @examples
#' ### trout data
#' data(trout)
#'
#' plotDesign(design = trout$design, x = "Day", y = "Treatment")
#'
#' # equivalent to:
#' plotDesign(lmpDataList = trout, x = "Day", y = "Treatment")
#'
#' ### mtcars
#' data(mtcars)
#' library(tidyverse)
#' df <- mtcars %>%
#'   dplyr::select(cyl, vs, am, gear, carb) %>%
#'   as.data.frame() %>%
#'   dplyr::mutate(across(everything(), as.factor))
#'
#' # Default behavior: display the 4 first factors in the design
#' plotDesign(design = df)
#'
#' # 2 factors
#' plotDesign(
#'   design = df, x = "cyl", y = "vs",
#'   cols = NULL, rows = NULL
#' )
#' # 3 factors
#' plotDesign(
#'   design = df, x = "cyl", y = "vs",
#'   cols = NULL, rows = c("am")
#' )
#' # 4 factors
#' plotDesign(
#'   design = df, x = "cyl", y = "vs",
#'   cols = c("gear"), rows = c("am")
#' )
#' # 5 factors
#' plotDesign(
#'   design = df, x = "cyl", y = "vs",
#'   cols = c("gear"), rows = c("am", "carb")
#' )
#'
#' plotDesign(
#'   design = df, x = "cyl", y = "vs",
#'   cols = c("vs"), rows = c("am", "carb")
#' )
#'
#' ### UCH
#' data("UCH")
#' plotDesign(design = UCH$design, x = "Hippurate", y = "Citrate", rows = "Day")
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyverse

plotDesign <- function(design = NULL, lmpDataList = NULL, x = NULL,
                       y = NULL, rows = NULL, cols = NULL,
                       title = "Plot of the design", theme = theme_bw()) {
  # checks arguments ===================

  checkArg(design, "data.frame", can.be.null = TRUE)
  checkArg(x, c("str", "length1"), can.be.null = TRUE)
  checkArg(y, c("str", "length1"), can.be.null = TRUE)
  checkArg(rows, c("str"), can.be.null = TRUE)
  checkArg(cols, c("str"), can.be.null = TRUE)
  checkArg(title, c("str", "length1"), can.be.null = TRUE)


  # define design
  if (is.null(design) & is.null(lmpDataList)) {
    stop("both design and lmpDataList are NULL, at least one should be non NULL.")
  } else if (!is.null(design) & !is.null(lmpDataList)) {
    message("both design and lmpDataList are non-NULL, design will be used to perform plotDesign")
  } else if (is.null(design) & !is.null(lmpDataList)) {
    lmpDataListCheck(lmpDataList, null_formula = TRUE)
    design <- lmpDataList$design
  }

  # initialize x, y, rows, cols

  if (is.null(x) & is.null(y)) xynull <- TRUE else xynull <- FALSE

  if (length(colnames(design)) == 1) {
    design <- cbind(design,
      count = "n"
    )
  }

  if (!is.null(x)) {
    x <- match.arg(x, choices = colnames(design))
  } else {
    x <- colnames(design)[1]
  }

  if (!is.null(y)) {
    y <- match.arg(y, choices = colnames(design))
  } else {
    y <- colnames(design)[2]
  }

  if (!is.null(cols)) {
    if (!all(cols %in% colnames(design))) {
      stop(
        "cols (", paste(cols, collapse = ", "),
        ") is not matching column names of design: ",
        paste(colnames(design), collapse = ", ")
      )
    }
  } else {
    cn <- colnames(design)[3]
    if (xynull & ncol(design) > 2 & !cn %in% c(rows, x, y)) {
      cols <- colnames(design)[3]
    }
  }

  if (!is.null(rows)) {
    if (!all(rows %in% colnames(design))) {
      stop(
        "rows (", paste(rows, collapse = ", "),
        ") is not matching column names of design: ",
        paste(colnames(design), collapse = ", ")
      )
    }
  } else {
    cn <- colnames(design)[4]
    if (xynull & ncol(design) > 3 & !cn %in% c(cols, x, y)) {
      rows <- colnames(design)[4]
    }
  }


  vars <- c(x, rows, cols)
  if (!is.null(vars)) {
    testClass <- mapply(is.factor, design[, vars])
    if (!all(testClass)) {
      warning("at least one variable used as x, rows or cols is not a factor and will be converted as such")
    }
  }


  # Calculate replicates by design point ===================

  df_count <- design %>%
    dplyr::count(dplyr::across(dplyr::all_of(c(x, y, rows, cols))))

  # prepare plot ===================

  p <- ggplot2::ggplot(df_count, ggplot2::aes_string(x = x, y = y))

  if (is.null(rows)) {
    rows <- "."
  } else {
    rows <- paste0(rows, collapse = "+")
  }

  if (is.null(cols)) {
    cols <- "."
  } else {
    cols <- paste0(cols, collapse = "+")
  }

  form <- paste0(rows, "~", cols)

  p <- p + ggplot2::geom_text(aes_string(label = "n"))

  if (is.numeric(df_count[, y])) {
    # message("\n y numeric")
    maxy <- max(df_count[, y])
    miny <- min(df_count[, y])
    delta <- (maxy - miny) / 10
    p <- p + ylim(miny - delta, maxy + delta)
  }

  if (!is.null(rows) | !is.null(cols)) {
    p <- p + ggplot2::facet_grid(as.formula(form),
      labeller = label_both
    )
  }

  p <- p + ggplot2::ggtitle(title) + theme

  return(p)
}
