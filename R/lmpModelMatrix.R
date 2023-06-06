#' @export lmpModelMatrix
#' @title Creates the model matrix `X`
#'
#' @description
#' Creates the model matrix `X` from the design matrix and the model formula.
#'
#' @param lmpDataList A list containing the outcomes, the experimental design and the formula.
#'
#' @return A list with the 5 following named elements :
#' \describe{
#'    \item{\code{lmpDataList}}{The initial object: a list with outcomes, design and formula.}
#'    \item{\code{modelMatrix}}{A \emph{nxK} model matrix specifically encoded for the ASCA-GLM method.}
#'    \item{\code{modelMatrixByEffect}}{A list of \emph{p} model matrices for each model effect.}
#'    \item{\code{effectsNamesUnique}}{A character vector with the \emph{p} names of the model effects, each repeated once.}
#'    \item{\code{effectsNamesAll}}{A character vector with the \emph{K} names of the model effects ordered and repeated as the column names of the model matrix.}
#' }
#'
#' @details
#' In typical ASCA-GLM (ASCA+) analysis, the effects of the GLM model must first be used to transform the design matrix to a model matrix where the design factors encoded usign \emph{sum coding} commonly used in industrial experimental design.
#' Suppose the design matrix is \emph{nxk} with n observations and \emph{k} factors. After the transformation, the model matrix
#' will be of size \emph{nxp}. For a fator with \emph{a} levels, the \emph{sum coding} creates \emph{a-1} columns in the model matrix with 0 and 1 for the \emph{a-1} first levels and -1 for the last one.
#' \emph{p} is the total number parameter for each response (outcome) in the ASCA model.
#' More information is available in the article (\emph{Thiel et al}, 2017)
#'
#' @seealso \code{\link{model.matrix}}
#'
#' @examples
#'
#' data('UCH')
#' resLmpModelMatrix <- lmpModelMatrix(UCH)
#'
#' head(resLmpModelMatrix$modelMatrix)
#'
#' @references Thiel M.,Feraud B. and Govaerts B. (2017) \emph{ASCA+ and APCA+: Extensions of ASCA and APCA
#' in the analysis of unbalanced multifactorial designs}, Journal of Chemometrics
#'
#'
#' @import grDevices
#' @importFrom stats as.formula


lmpModelMatrix <- function(lmpDataList) {

  formula = stats::as.formula(lmpDataList$formula)
  design = lmpDataList$design

  # Checking no missing argument and the class of the object

  checkArg(formula,"formula",can.be.null = FALSE)
  checkArg(design,"data.frame",can.be.null = FALSE)

  # Checking formula

  formulaChar = as.character(formula)
  if (length(formulaChar) == 3) {
    formulaDesignMatrix <- stats::as.formula(paste(formulaChar[1], formulaChar[3]))
  } else if (length(formulaChar) == 2) {
    formulaDesignMatrix = formula
  } else {
    stop("Please put the formula argument in its right form")
  }

  # Checking correspondence between formula names and design names

  varNames <- all.vars(formulaDesignMatrix)
  matchesVarNames <- varNames %in% names(design)
  if (!all(matchesVarNames, na.rm = FALSE)) {
    stop("Some of the variable names, present in the formula argument, do not correspond to one of the column names of the design argument. Please adapt either one of both arguments.")
  }

  # Checking if all variables are factors

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
  effectsNamesAll <- character(length = length(dummyVarNames))
  effectsNamesAll[presencePolynomialEffects] <- dummyVarNames[presencePolynomialEffects]
  effectsNamesAll[!presencePolynomialEffects] <- gsub('[0-9]', '', dummyVarNames[!presencePolynomialEffects])
  effectsNamesAll[effectsNamesAll == '(Intercept)'] <- 'Intercept'
  effectsNamesUnique <- unique(effectsNamesAll)
  nEffect <- length(effectsNamesUnique)

    #Creating empty model matrices by effect
  modelMatrixByEffect <- list()
  length(modelMatrixByEffect) <- nEffect
  names(modelMatrixByEffect) <- effectsNamesUnique

    #Filling model matrices by effect
  for(iEffect in 1:nEffect){
    selection <- which(effectsNamesAll == effectsNamesUnique[iEffect])
    selectionComplement <- which(effectsNamesAll != effectsNamesUnique[iEffect])
    #Model matrices by effect
    modelMatrixByEffect[[iEffect]] <- as.matrix(modelMatrix[, selection])

  }

  resLmpModelMatrix = list(lmpDataList = lmpDataList,
                          modelMatrix = modelMatrix,
                          modelMatrixByEffect = modelMatrixByEffect,
                          effectsNamesUnique = effectsNamesUnique,
                          effectsNamesAll = effectsNamesAll)

  return(resLmpModelMatrix)
}
