# library(devtools)
# load_all()
# data(UCH)
# lmpData = UCH
#
# lmpDataListCheck(lmpData)
#
# lmpData = UCH
# lmpData$design = NULL
# lmpDataListCheck(lmpData)
# lmpDataListCheck(lmpData, null_design = TRUE)
#
# lmpData = UCH
# lmpData$formula = NULL
# lmpDataListCheck(lmpData)
# lmpDataListCheck(lmpData, null_formula = TRUE)
#
# lmpData = UCH
# lmpData$outcomes = NULL
# lmpDataListCheck(lmpData)
# lmpDataListCheck(lmpData, null_outcomes = TRUE)
#
#
# lmpData = UCH
# lmpData$outcomes = lmpData$outcomes[sample(1:nrow(lmpData$outcomes), replace=FALSE),]
# lmpDataListCheck(lmpData)
#
# lmpData = UCH
# lmpData$outcomes = lmpData$outcomes[-1,]
# lmpDataListCheck(lmpData)
#
# lmpData = UCH
# lmpData$design = lmpData$design[-1,]
# lmpDataListCheck(lmpData)
#
# lmpData = UCH
# colnames(lmpData$design)[1] = "aaa"
# lmpDataListCheck(lmpData)
# lmpData$formula=NULL
# lmpDataListCheck(lmpData, null_formula = TRUE)
#
# lmpData = UCH
# rownames(lmpData$design)[1] = "aaa"
# lmpDataListCheck(lmpData)


#' @import dplyr
#' @importFrom stats as.formula

lmpDataListCheck <- function(lmpData,
                             null_formula = FALSE,
                             null_design = FALSE,
                             null_outcomes = FALSE) {
  # checks =========================

  # check lmpdata


  checkArg(lmpData, "list", can.be.null = FALSE)

  if (length(lmpData) > 3) {
    stop("length of lmpData is superior to 3, there is unnecessary objects")
  }

  if (!all(names(lmpData) %in% c("design", "outcomes", "formula"))) {
    stop("the names of lmpData do not correspond to \"design\", \"outcomes\" and \"formula\"")
  }

  out_formula <- lmpData$formula
  out_design <- lmpData$design
  out_outcomes <- lmpData$outcomes

  checkArg(out_formula, c("str", "length1"), can.be.null = null_formula)
  checkArg(out_design, "data.frame", can.be.null = null_design)
  checkArg(out_outcomes, "matrix", can.be.null = null_outcomes)


  # Checking formula ==================================

  if (!is.null(out_formula)) {
    formulaChar <- as.character(as.formula(out_formula))

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


  # Checking outcomes ==================================

  if (!is.null(out_outcomes)) {
    # check if rownames are given for outcomes
    if (is.null(rownames(out_outcomes))) {
      stop("rownames for outcomes is not present and needs to be defined")
    }
  }

  # Checking design ==================================

  # Checking correspondence between formula names and design names ----------

  if (!is.null(out_formula) & !is.null(out_design)) {
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
  }

  # Checking correspondence between the rows of design and outcomes ----------

  if (!is.null(out_outcomes) & !is.null(out_design)) {
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
        ") is different from nrow of outcomes (", nrow(out_outcomes), ")"
      )
    }
  }
}
