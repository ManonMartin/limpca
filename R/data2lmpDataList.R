#' @export data2lmpDataList
#' @title Converts data to a lmpDataList list.
#'
#' @description
#' Creates the lmpDataList list from a SummarizedExperiment or by manually defining the design, the outcomes and the model formula.
#' lmpDataList serves as an input for the \code{\link{lmpModelMatrix}} function to start the limpca modeling.
#'
#' @param se A \code{\link{SummarizedExperiment}} object.
#' @param assay_name If not \code{NULL} (default), a character string naming the assay from the \code{\link{SummarizedExperiment}} object \code{se}. If \code{NULL}, the first assay is selected.
#' @param outcomes If not \code{NULL}  (default), a numerical matrix with \emph{n} observations and \emph{m} response variables. The rownames needs to be non-NULL and match those of the design matrix.
#' @param design If not \code{NULL} (default), a data.frame with the experimental design of \emph{n} observations and \emph{q} explanatory variables. The rownames of design has to match the rownames of outcomes.
#' @param formula If not \code{NULL}  (default), a character string with the formula that will be used to analyze the data. Only the right part of the formula is necessary, eg: \code{"~ A + B"}, The names of the formula should marcht the column names of the design
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
#' the resulting outcomes matrix will be from the \code{outcomes} argument).
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
#' res <- data2lmpDataList(
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
#' res <- data2lmpDataList(se, assay_name = "counts2")
#'
#' # changing the formula:
#' res <- data2lmpDataList(se,
#'   assay_name = "counts2",
#'   formula = "~ Hippurate + Citrate + Time"
#' )
#'
#' @import SummarizedExperiment
#' @importFrom stats as.formula
#' @importFrom S4Vectors metadata

data2lmpDataList <- function(se = NULL, assay_name = NULL,
                             outcomes = NULL, design = NULL,
                             formula = NULL,
                             verbose = TRUE) {
  ## checks ===========

  # se
  if (!is(se, "SummarizedExperiment") & !is.null(se)) {
    stop("se is not a SummarizedExperiment nor NULL")
  }

  checkArg(
    arg = assay_name, c("str", "length1"),
    can.be.null = TRUE
  )
  checkArg(verbose, "bool", can.be.null = FALSE)
  checkArg(formula, c("str", "length1"), can.be.null = TRUE)
  checkArg(design, "data.frame", can.be.null = TRUE)
  checkArg(outcomes, "matrix", can.be.null = TRUE)

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
    out_formula <- stats::as.formula(formula)
  } else if (!is.null(form_se)) {
    out_formula <- stats::as.formula(form_se)
  } else {
    stop("formula information is missing in the arguments")
  }

  checkArg(out_formula, "formula", can.be.null = TRUE)

  if (!is.null(design)) {
    out_design <- design
  } else if (!is.null(coldat_se)) {
    out_design <- coldat_se
  } else {
    stop("design information is missing in the arguments")
  }


  ## format the data ====================

  # Checking formula

  formulaChar <- as.character(out_formula)

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
  } else{
    stop("Please put the formula argument in its right form: ~ model terms")
  }

  # Checking correspondence between formula names and design names ----------

  varNames <- all.vars(stats::as.formula(out_formula))
  matchesVarNames <- varNames %in% names(out_design)
  if (!all(matchesVarNames, na.rm = FALSE)) {
    stop(
      "Some of the variable names (", varNames[!matchesVarNames],
      "), present in the formula argument,
         do not correspond to one of the column names of the design argument.
         Please adapt either one of both arguments."
    )
  }

  # Checking correspondence between the rows of design and outcomes ----------

  # check if rownames are given for outcomes
  if (is.null(rownames(out_outcomes))) {
    stop("rownames for outcomes is not present and needs to be defined")
  }

  if (nrow(out_design) == nrow(out_outcomes)) {
    if (!identical(rownames(out_design), rownames(out_outcomes))) {
      # if same length but not well ordered/named
      if (all(rownames(out_design) %in% rownames(out_outcomes))) {
        warning("reordering the rownames of design to match those of outcomes")
        # reorder the rownames of design to match those of outcomes
        reorder_idx <- match(rownames(out_outcomes), rownames(out_design))
        out_design <- out_design[reorder_idx, ]
        if (!identical(rownames(out_design), rownames(out_outcomes))) {
          # if the reordering fails
          stop("mismatch between the rownames of design and outcomes")
        }
      } else {
        mismatch_names <- rownames(out_design)[!rownames(out_design) %in% rownames(out_outcomes)]
        stop(
          "some rownames of the design (", paste(mismatch_names, collapse = ", "),
          ") do not match the rownames of the outcomes"
        )
      }
    }
  } else {
    stop(
      "nrow of design (", nrow(out_design),
      ") is different from nrow outcomes (", nrow(out_outcomes), ")"
    )
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
      "| design variables (", ncol(out_design), "): ",
      paste(paste(cn, cc_p), collapse = ", ")
    )
  }

  return(l_out)
}
