#' @export LMModelMatrix
#' @title Creating the model matrix
#' @description Create the specific model matrix needed for the ASCA+ method from the design and model matrices
#'
#' @param



LMModelMatrix <- function(formula, outcomes, design) {

  # Checking no missing argument

  if (missing(formula))
    stop("Missing the formula")
  if (missing(outcomes))
    stop("Missing the outcomes")
  if (missing(design))
    stop("Missing the design")

  # Checking formula

  formulaChar = as.character(formula)
  if (length(formulaChar) == 3) {
    formulaDesignMatrix <- as.formula(paste(formulaChar[1], formulaChar[3]))
  } else if (length(formulaChar) == 2) {
    formulaDesignMatrix = formula
  } else {
    stop("Please put the formula argument in its right form")
  }

  # Checking correspondance between formula names and design names
  varNames <- all.vars(formulaDesignMatrix)
  matchesVarNames <- varNames %in% names(design)
  if (!all(matchesVarNames, na.rm = FALSE)) {
    stop("Some of the variable names, present in the formula argument, do not correspond to one of the column names of the design argument. Please   adapt either one of both arguments.")
  }

  # Checking which variables are factors
  factorsDesign <- names(Filter(is.factor, design))
  varNamesFactors <- intersect(factorsDesign, varNames)

  # Creating model matrix If factors are present, a list is created to specify
  # which variables are considered as factors in model.matrix
  if (length(varNamesFactors) != 0) {
    contrasts.arg.Values <- list()
    length(contrasts.arg.Values) <- length(varNamesFactors)
    names(contrasts.arg.Values) <- varNamesFactors
    for (iList in 1:length(contrasts.arg.Values)) contrasts.arg.Values[[iList]] <- "contr.sum"
    modelMatrix <- (model.matrix(formulaDesignMatrix, contrasts.arg = contrasts.arg.Values,
                                 data = design))
  }

  # If factors are not present
  if (length(varNamesFactors) == 0) {
    modelMatrix <- (model.matrix(formulaDesignMatrix, data = design))
  }

  ResLMModelMatrix = list(formula = formula, outcomes = outcomes, design = design,
                          modelMatrix = modelMatrix)

  return(ResLMModelMatrix)
}
