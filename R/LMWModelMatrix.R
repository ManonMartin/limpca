#' @export lmwModelMatrix
#' @title Creates the model matrix
#'
#' @description
#' Creates the specific model matrix from the design for the ASCA-GLM method.
#'
#' @param lmwDataList A list containing the outcomes, the experimental design and the formula.
#'
#' @return A list with the 5 following named elements :
#' \describe{
#'    \item{\code{lmwDataList}}{A list containing the outcomes, the experimental design and the formula.}
#'    \item{\code{ModelMatrix}}{A \emph{nxK} model matrix specifically encoded for the ASCA-GLM method.}
#'    \item{\code{ModelMatrixByEffect}}{A list of \emph{p} model matrices by models terms.}
#'    \item{\code{covariateEffectsNames}}{A character vector with \emph{K} names of the coefficients.}
#'    \item{\code{covariateEffectsNamesUnique}}{A character vector with the \emph{p} unique names of the model terms.}
#' }
#'
#' @details
#' In typical ASCA-GLM analysis, the GLM model needs the design to be re-encoded with the commonly used in industrial experimental design \emph{sum coding}. The result is the model matrix.
#' Suppose the design matrix is \emph{nxk} with n observations and \emph{k} factors. After the transformation the model matrix
#' will be \emph{nxK}. For a parameter with a levels the re-encoding is \emph{a-1} colums with 0 and 1 for \emph{a-1} first levels and -1 for the last.
#' \emph{K} is the sum of all the columns for every parameters.
#'
#' @seealso \code{\link{model.matrix}}
#'
#' More information about the specific encoding is available in the article from (\emph{Thiel et al}, 2017)
#'
#'
#' @examples
#'
#' data('UCH')
#' resLmwModelMatrix <- lmwModelMatrix(UCH)
#'
#' head(resLmwModelMatrix$ModelMatrix)
#'
#' @references Thiel M.,Feraud B. and Govaerts B. (2017) \emph{ASCA+ and APCA+: Extensions of ASCA and APCA
#' in the analysis of unbalanced multifactorial designs}, Journal of Chemometrics
#'
#'
#' @import grDevices
#' @import stats


lmwModelMatrix <- function(lmwDataList) {

  formula = as.formula(lmwDataList$formula)
  design = lmwDataList$design

  # Checking no missing argument and the class of the object

  checkArg(formula,"formula",can.be.null = FALSE)
  checkArg(design,"data.frame",can.be.null = FALSE)

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

  # Checking if all avariables are factors

  if(all(names(Filter(is.factor, design))!=colnames(design))){
    NoFactor = vector()
    for(i in 1:length(colnames(design))){
      NoFactor[i] = is.factor(design[,i])
    }
    stop(paste("Some of the variables from the design matrix are not factors :",colnames(design)[!NoFactor]))
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

  # If factors are not present (Currently not the case)
  if (length(varNamesFactors) == 0) {
    modelMatrix <- (model.matrix(formulaDesignMatrix, data = design))
  }

  #Creating a list containing model matrices by effect

    # Finding all unique variables
  dummyVarNames <- colnames(modelMatrix)
  presencePolynomialEffects <- stringr::str_detect(dummyVarNames, '\\^[0-9]') # Detect exponent
  covariateEffectsNames <- character(length = length(dummyVarNames))
  covariateEffectsNames[presencePolynomialEffects] <- dummyVarNames[presencePolynomialEffects]
  covariateEffectsNames[!presencePolynomialEffects] <- gsub('[0-9]', '', dummyVarNames[!presencePolynomialEffects])
  covariateEffectsNames[covariateEffectsNames == '(Intercept)'] <- 'Intercept'
  covariateEffectsNamesUnique <- unique(covariateEffectsNames)
  nEffect <- length(covariateEffectsNamesUnique)

    #Creating empty model matrices by effect
  modelMatrixByEffect <- list()
  length(modelMatrixByEffect) <- nEffect
  names(modelMatrixByEffect) <- covariateEffectsNamesUnique

    #Filling model matrices by effect
  for(iEffect in 1:nEffect){
    selection <- which(covariateEffectsNames == covariateEffectsNamesUnique[iEffect])
    selectionComplement <- which(covariateEffectsNames != covariateEffectsNamesUnique[iEffect])
    #Model matrices by effect
    modelMatrixByEffect[[iEffect]] <- as.matrix(modelMatrix[, selection])

  }

  resLmwModelMatrix = list(lmwDataList = lmwDataList,
                          ModelMatrix = modelMatrix,
                          ModelMatrixByEffect = modelMatrixByEffect,
                          covariateEffectsNames = covariateEffectsNames,
                          covariateEffectsNamesUnique = covariateEffectsNamesUnique)

  return(resLmwModelMatrix)
}
