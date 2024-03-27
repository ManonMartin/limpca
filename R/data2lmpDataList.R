#' @export data2LmpDataList
#' @title Converts data to a lmpDataList.
#'
#' @description
#' Creates the lmpDataList from a SummarizedExperiment or by manually defining the design, the outcomes and the model formula.
#' lmpDataList serves as an input for the \code{\link{lmpModelMatrix}} function to start the limpca modeling.
#'
#' @param se A \code{\link{SummarizedExperiment}} object.
#' @param assay_name If not \code{NULL} (default), a character string naming the assay from the \code{\link{SummarizedExperiment}} object \code{se}. If \code{NULL}, the first assay is selected.
#' @param outcomes If not \code{NULL} (default), a numerical matrix with \emph{n} observations and \emph{m} response variables. The rownames needs to be non-NULL and match those of the design matrix.
#' @param design If not \code{NULL} (default), a data.frame with the experimental design of \emph{n} observations and \emph{q} explanatory variables. The rownames of design has to match the rownames of outcomes.
#' @param formula If not \code{NULL} (default), a character string with the formula that will be used to analyze the data. Only the right part of the formula is necessary, eg: \code{"~ A + B"}, The names of the formula should match the column names of the design
#' @param verbose If \code{TRUE}, prints useful information about the outputted list.
#'
#' @return A list with the 3 following named elements:
#' \describe{
#'    \item{\code{outcomes}}{A \emph{nxm} matrix with the \emph{m} response variables.}
#'    \item{\code{design}}{A \emph{nxq} data.frame with the experimental design.}
#'    \item{\code{formula}}{A character string with the model formula.}
#'    }
#'
#' @details
#' Data can be included as a \code{\link{SummarizedExperiment}} (SE) object or by manually defining one or multiple
#' elements of \code{outcomes}, \code{design} and \code{formula}. If a SE is provided,
#' the \code{outcomes} corresponds to a transposed assay of the SE (by default the first one),
#' the \code{design} corresponds to the \code{\link{colData}} of the SE and the \code{formula} can be provided as a
#' \code{formula} element in the \code{S4Vectors::metadata} of SE (\code{metadata(se)$formula}).
#'
#' In the outputted list, the outcomes are structured in a standard statistical fashion,
#' i.e. with observations in rows and the variables (features) in column.
#' If the \code{outcomes} argument is not \code{NULL}, it has to be formatted that way (see Arguments).
#'
#' Note that there is a priority to the \code{outcomes}, \code{design} and \code{formula}
#' arguments if they are not \code{NULL} (e.g. if both \code{se} and \code{outcomes} arguments are provided,
#' the resulting outcomes matrix will be from the \code{outcomes} argument). \code{outcomes} and \code{design} elements are mandatory.
#'
#' Multiple checks are performed to ensure that the data are correctly formatted:
#' - the rownames of \code{design} and \code{outcomes} should match
#' - the names of the model terms in the \code{formula} should match column names from the \code{design}
#'
#'
#' @seealso \code{\link{SummarizedExperiment}}
#'
#' @examples
#'
#' data(UCH)
#'
#' ### create manually the dataset
#'
#' res <- data2LmpDataList(
#'   outcomes = UCH$outcomes,
#'   design = UCH$design[, 1, drop = FALSE], formula = "~ Hippurate"
#' )
#'
#' ### create the dataset from a SummarizedExperiment
#'
#' library(SummarizedExperiment)
#'
#' se <- SummarizedExperiment(
#'   assays = list(
#'     counts = t(UCH$outcomes),
#'     counts2 = t(UCH$outcomes * 2)
#'   ), colData = UCH$design,
#'   metadata = list(formula = "~ Hippurate + Citrate")
#' )
#'
#' res <- data2LmpDataList(se, assay_name = "counts2")
#'
#' # changing the formula:
#' res <- data2LmpDataList(se,
#'   assay_name = "counts2",
#'   formula = "~ Hippurate + Citrate + Time"
#' )
#'
#' @import SummarizedExperiment
#' @importFrom stats as.formula
#' @importFrom S4Vectors metadata
#' @importFrom methods is

data2LmpDataList <- function(se = NULL, assay_name = NULL,
                             outcomes = NULL, design = NULL,
                             formula = NULL,
                             verbose = TRUE) {
  ## checks ===========

  # se
  if (!methods::is(se, "SummarizedExperiment") & !is.null(se)) {
    stop("se is not a SummarizedExperiment nor NULL")
  }

  checkArg(
    arg = assay_name, c("str", "length1"),
    can.be.null = TRUE
  )
  checkArg(verbose, "bool", can.be.null = FALSE)
  # checkArg(formula, c("str", "length1"), can.be.null = TRUE)
  # checkArg(design, "data.frame", can.be.null = TRUE)
  # checkArg(outcomes, "matrix", can.be.null = TRUE)

  ## if se is specified ===========
  coldat_se <- form_se <- assay_se <- NULL
  if (!is.null(se)) {
    coldat_se <- as.data.frame(SummarizedExperiment::colData(se))
    form_se <- S4Vectors::metadata(se)$formula

    if (!is.null(assay_name)) {
      if (!assay_name %in% assayNames(se)) {
        # checks that assay_name is in assayNames(se)
        stop("assay_name is not in assayNames(se)")
      }
      id <- which(assayNames(se) == assay_name)
      assay_se <- SummarizedExperiment::assay(se, i = id)
    } else if (is.null(assay_name)) {
      # if assay_name is null, take the first one
      assay_se <- SummarizedExperiment::assay(se, i = 1)
    }
  }

  # creating the output values
  out_design <- out_formula <- out_outcomes <- NULL

  if (!is.null(outcomes)) {
    out_outcomes <- outcomes
  } else if (!is.null(assay_se)) {
    out_outcomes <- t(assay_se)
  } else {
    stop("outcomes information is missing in the arguments")
  }

  if (!is.null(formula)) {
    out_formula <- formula
  } else if (!is.null(form_se)) {
    out_formula <- form_se
  } else {
    out_formula <- NULL
    warning("formula information is missing in the arguments")
  }

  # checkArg(out_formula, "formula", can.be.null = TRUE)

  if (!is.null(design)) {
    out_design <- design
  } else if (!is.null(coldat_se)) {
    out_design <- coldat_se
  } else {
    stop("design information is missing in the arguments")
  }


  ## format the data ====================
  lmpData <- list(
    outcomes = out_outcomes,
    design = out_design,
    formula = out_formula
  )

  lmpDataListCheck(
    lmpDataList = lmpData,
    null_formula = TRUE
  )


  # formatting formula

  if (!is.null(out_formula)) {
    formulaChar <- as.character(stats::as.formula(out_formula))

    if (length(formulaChar) == 3) {
      out_formula <- paste(
        formulaChar[1],
        formulaChar[3]
      )
    } else if (length(formulaChar) == 2) {
      out_formula <- paste(
        formulaChar[1],
        formulaChar[2]
      )
    } else {
      stop("Please put the formula argument in its right form: ~ model terms")
    }
  }


  # output ==============================

  l_out <- list(
    design = out_design, outcomes = out_outcomes,
    formula = out_formula
  )

  if (verbose) {
    # Function returning compact column classes
    col_classes <- function(df) {
      cc_l <- t(as.data.frame(lapply(df, function(x) {
        paste(class(x), collapse = ",")
      })))
      cc <- as.vector(cc_l)
      names(cc) <- rownames(cc_l)
      cc
    }
    cc <- col_classes(out_design)
    cn <- colnames(out_design)
    cc_p <- paste0("(", cc, ")")

    message("| dim outcomes: ", nrow(out_outcomes), "x", ncol(out_outcomes))
    message("| formula: ", as.character(out_formula))
    message(
      "| design variables (", ncol(out_design), "): \n",
      paste("*",paste(cn, cc_p), collapse = "\n")
    )
  }

  return(l_out)
}
