#' @export UPDATE_lmpModelMatrix
#' @title Creates the model matrix `X`
#'
#' @description
#' Creates the model matrix `X` and the model matrix `Z` (if mixed model) from the design matrix and the model formula.
#'
#' @param lmpDataList A list containing the outcomes, the experimental design, the formula and the type of model \emph{model} (\emph{"lmm"} for linear mixed model or \emph{"lm"} for linear model).
#'
#' @return A list with the 5 following named elements :
#' \describe{
#'    \item{\code{lmpDataList}}{The initial object: a list with outcomes, design, formula and type of model.}
#'    \item{\code{modelMatrix}}{A \emph{nxK} model matrix specifically encoded for the ASCA-GLM method.}
#'    \item{\code{modelMatrixByEffect}}{A list of \emph{p} model matrices for each model effect.}
#'    \item{\code{effectsNamesUnique}}{A character vector with the \emph{p} names of the model effects, each repeated once.}
#'    \item{\code{effectsNamesAll}}{A character vector with the \emph{K} names of the model effects ordered and repeated as the column names of the model matrix.}
#'    \item{\code{modelMatrixR}}{A \emph{nxK} model random matrix with all levels if linear mixed model.}
#'    \item{\code{modelMatrixByEffectR}}{A list of random model matrices for each random model effect if linear mixed model.}
#'    \item{\code{effectsNamesUniqueR}}{A character vector with the names of the random model effects, each repeated once if linear mixed model.}
#'    \item{\code{effectsNamesAllR}}{A character vector with the names of the random model effects ordered and repeated as the column names of the random model matrix if linear mixed model.}
#' }
#'
#' @details
#' In typical ASCA-GLM (ASCA+) analysis, the effects of the GLM model must first be used to transform the design matrix to a model matrix where the design factors encoded usign \emph{sum coding} commonly used in industrial experimental design.
#' Suppose the design matrix is \emph{nxk} with n observations and \emph{k} factors. After the transformation, the model matrix
#' will be of size \emph{nxp}. For a factor with \emph{a} levels, the \emph{sum coding} creates \emph{a-1} columns in the model matrix with 0 and 1 for the \emph{a-1} first levels and -1 for the last one.
#' \emph{p} is the total number parameter for each response (outcome) in the ASCA model.
#' More information is available in the article (\emph{Thiel et al}, 2017)
#' 
#' If the mixed-effects model is used, we obtain the same model matrix for fixed effects. 
#' However, for a random effect, the model matrix is created in such a way that for a factor with \emph{a} levels, \emph{sum coding} generates \emph{a} columns in the model matrix with only 0 and 1.
#' 
#' For now, \emph{limpca} only handles three types of mixed models: hierarchical models with 2 random variables, models with a single random variable, and models with 2 random variables, including an interaction effect between the random variable and a fixed variable.
#' All random effects must be of the form \emph{(1 | varR)} where \emph{varR} is the name of the random effect. 
#' However, the \emph{limpca} package handles all fixed models
#' 
#'
#' @seealso \code{\link{model.matrix}}
#'
#' @examples
#'
#' data("UCH")
#' resLmpModelMatrix <- lmpModelMatrix(UCH)
#'
#' head(resLmpModelMatrix$modelMatrix)
#'
#' @references Thiel M.,Feraud B. and Govaerts B. (2017) \emph{ASCA+ and APCA+: Extensions of ASCA and APCA
#' in the analysis of unbalanced multifactorial designs}, Journal of Chemometrics
#'
#' @import stringr
#' @import grDevices
#' @importFrom stats as.formula model.matrix


UPDATE_lmpModelMatrix <- function(lmpDataList) {
  formula <- stats::as.formula(lmpDataList$formula)
  design <- lmpDataList$design

  # Checking no missing argument and the class of the object

  checkArg(formula, "formula", can.be.null = FALSE)
  checkArg(design, "data.frame", can.be.null = FALSE)

  # Checking formula

  formulaChar <- as.character(formula)
  if (length(formulaChar) == 3) {
    formulaDesignMatrix <- stats::as.formula(paste(formulaChar[1],
                                                   formulaChar[3]))
  } else if (length(formulaChar) == 2) {
    formulaDesignMatrix <- formula
  } else {
    stop("Please put the formula argument in its right form")
  }

  # Checking correspondence between formula names and design names

  varNames <- all.vars(formulaDesignMatrix)
  matchesVarNames <- varNames %in% names(design)
  if (!all(matchesVarNames, na.rm = FALSE)) {
    stop("Some of the variable names, present in the formula argument,
         do not correspond to one of the column names of the design argument.
         Please adapt either one of both arguments.")
  }

  # Checking if all variables are factors

  if (all(names(Filter(is.factor, design)) != colnames(design))) {
    NoFactor <- vector()
    for (i in 1:length(colnames(design))) {
      NoFactor[i] <- is.factor(design[, i])
    }
    stop(paste("Some of the variables from the design
               matrix are not factors :",
               colnames(design)[!NoFactor]))
  }

  # Checking which variables are factors
  factorsDesign <- names(Filter(is.factor, design))
  varNamesFactors <- intersect(factorsDesign, varNames)
  
  ##########################update by Antoine############################
  modelMatrixR <- matrix(nrow = dim(design)[1], ncol = 0)
  modelMatrixByEffectR <- list()
  effectsNamesUniqueR <- NULL
  effectsNamesAllR <- NULL
  model <- lmpDataList$model
  UPDATE_checkArg(model,c("model"), can.be.null = TRUE)
  
  # not specified the model
  if(is.null(model)){
    # it's a mixed model if there is "|" in the formula 
    if(any(grepl("\\|",attr(terms(formula),"term.labels")))){
      model <- "lmm"
      lmpDataList$model <- "lmm"
    } else{
      model <- "lm"
      lmpDataList$model <- "lm"
    }
  }
  
  # If mixed model
  if(model == "lmm"){
    # Check isReduct is TRUE
    if(is.null(lmpDataList$isReduct)){
      stop("In the case of a mixed linear model, matrix reduction with the lmpOutcomesReduct function must be performed")
    } else if(!lmpDataList$isReduct){
      stop("In the case of a mixed linear model, matrix reduction with the lmpOutcomesReduct function must be performed")
    }
    # Finding random factors
    randFactors <- attr(terms(formula),"term.labels")[grepl("\\|",attr(terms(formula),"term.labels"))]
    # Filter the fix factors
    varFix <- attr(terms(formula(formula)),"term.labels")[!grepl("\\|",attr(terms(formula(formula)),"term.labels"))]
    varNamesFactors <- intersect(varNamesFactors, varFix)
    # Check the prefix (1 | ...) of random effect
    if(all(trimws(sapply(randFactors,function(x){strsplit(x,"\\|")[[1]][1]})) != 1)){
      stop("All random effects must be of the form (1 | varR) where varR is the name of the random effect")
    }
    
    if(length(randFactors) == 0){
      stop("The model to be estimated is not a linear mixed model")
    } else if(length(randFactors) == 2 & length(randFactors[grepl(":", randFactors)]) != 1){
      stop("If there are two random effects, there should be a marginal random effect and an interaction random effect, which is not the case")
    } else if (length(randFactors) > 2){
      stop("Limpca does not handle models with more than 2 random variables")
    } else if (length(randFactors) == 1 & length(randFactors[grepl(":", randFactors)]) == 1){
      stop("The model with only interaction random effect is not supported")
    }
    
    randFactors <- sapply(randFactors,function(x){strsplit(x,"\\|")[[1]][2]})
    randFactors <- trimws(randFactors)
    
    # Check the random interaction effect includes the marginal random effect
    interactionR <- randFactors[grepl(":", randFactors)]
    if(length(interactionR) == 1){
      # The random marginal effect
      marginalR <- randFactors[!grepl(":", randFactors)]
      # List of variable in the interaction
      listInterVar <- strsplit(interactionR,":")[[1]]
      
      if(length(intersect(listInterVar,marginalR)) != 1){
        stop("The random interaction effect must include the marginal random effect")
      }
      # Interaction first
      randFactors[1] <- interactionR
      randFactors[2] <- marginalR
    }
    
    # Create model matrix for random effect (with no Intercept)
    length(modelMatrixByEffectR) <- length(randFactors)
    names(modelMatrixByEffectR) <- randFactors
    for(i in 1:length(randFactors)){
      factor <- randFactors[i]
      formFactor <- formula(paste("~ 0 +",factor))
      
      modelMatrixByEffectR[[factor]] <- (stats::model.matrix(formFactor,
                                                   data = design
      ))
      modelMatrixR <- cbind(modelMatrixR, modelMatrixByEffectR[[factor]])
    }
    
    # Finding all unique random variables 
    dummyVarNames <- colnames(modelMatrixR)
    presencePolynomialEffects <- stringr::str_detect(dummyVarNames,
                                                     "\\^[0-9]") # Detect exponent
    effectsNamesAllR <- character(length = length(dummyVarNames))
    effectsNamesAllR[presencePolynomialEffects] <- dummyVarNames[presencePolynomialEffects]
    # Remove the levels in the names
    allLevels <- unlist(sapply(design,levels))
    effectsNamesAllR[!presencePolynomialEffects] <- stringr::str_replace_all(dummyVarNames[!presencePolynomialEffects], paste0("(", paste(allLevels, collapse = "|"), ")"), "")
    effectsNamesAllR[!presencePolynomialEffects] <- gsub("[0-9]", "",
                                                         effectsNamesAllR[!presencePolynomialEffects])
    effectsNamesUniqueR <- unique(effectsNamesAllR)
    
    
    # Formula with only fix effect
    tmpNamesVars <-  trimws(strsplit(lmpDataList$formula,"\\+")[[1]])
    tmpNamesVars <- tmpNamesVars[!grepl("\\|", tmpNamesVars)]
    formFix <- paste(tmpNamesVars, collapse = " + ")
    if(length(varNamesFactors) != 0){
      formulaDesignMatrix <- formula(formFix)
    } else {
      formulaDesignMatrix <- formula("~ 1")
    }
    
    warning("The random model matrix is provided for indicative purposes only.")
  }
  ###################################################################################
  # Check fixed model without random effect
  if(any(grepl("\\|",attr(terms(formulaDesignMatrix),"term.labels")))){
    stop("The model to be estimated is not a global linear model")
  }
  
  
  # Creating model matrix. If factors are present, a list is created to specify
  # which variables are considered as factors in model.matrix
  if (length(varNamesFactors) != 0) {
    contrasts.arg.Values <- list()
    length(contrasts.arg.Values) <- length(varNamesFactors)
    names(contrasts.arg.Values) <- varNamesFactors
    for (iList in 1:length(contrasts.arg.Values)){
      contrasts.arg.Values[[iList]] <- "contr.sum"
    }
    modelMatrix <- (stats::model.matrix(formulaDesignMatrix,
      contrasts.arg = contrasts.arg.Values,
      data = design
    ))
  }

  # If factors are not present (Currently not the case)
  if (length(varNamesFactors) == 0) {
    modelMatrix <- (stats::model.matrix(formulaDesignMatrix,
                                        data = design))
  }

  # Creating a list containing model matrices by effect

  # Finding all unique variables
  dummyVarNames <- colnames(modelMatrix)
  presencePolynomialEffects <- stringr::str_detect(dummyVarNames,
                                                   "\\^[0-9]") # Detect exponent
  effectsNamesAll <- character(length = length(dummyVarNames))
  effectsNamesAll[presencePolynomialEffects] <- dummyVarNames[presencePolynomialEffects]
  effectsNamesAll[!presencePolynomialEffects] <- gsub("[0-9]", "",
                                                      dummyVarNames[!presencePolynomialEffects])
  effectsNamesAll[effectsNamesAll == "(Intercept)"] <- "Intercept"
  effectsNamesUnique <- unique(effectsNamesAll)
  nEffect <- length(effectsNamesUnique)

  # Creating empty model matrices by effect
  modelMatrixByEffect <- list()
  length(modelMatrixByEffect) <- nEffect
  names(modelMatrixByEffect) <- effectsNamesUnique

  # Filling model matrices by effect
  for (iEffect in 1:nEffect) {
    selection <- which(effectsNamesAll == effectsNamesUnique[iEffect])
    selectionComplement <- which(effectsNamesAll != effectsNamesUnique[iEffect])
    # Model matrices by effect
    modelMatrixByEffect[[iEffect]] <- as.matrix(modelMatrix[, selection])
  }

  # the object is not the same for different models
  if(model == "lmm"){
    resLmpModelMatrix <- list(
      lmpDataList = lmpDataList,
      modelMatrix = modelMatrix,
      modelMatrixByEffect = modelMatrixByEffect,
      effectsNamesUnique = effectsNamesUnique,
      effectsNamesAll = effectsNamesAll,
      modelMatrixR = modelMatrixR,
      modelMatrixByEffectR = modelMatrixByEffectR,
      effectsNamesUniqueR = effectsNamesUniqueR,
      effectsNamesAllR = effectsNamesAllR)
  } else {
    resLmpModelMatrix <- list(
      lmpDataList = lmpDataList,
      modelMatrix = modelMatrix,
      modelMatrixByEffect = modelMatrixByEffect,
      effectsNamesUnique = effectsNamesUnique,
      effectsNamesAll = effectsNamesAll)
  }

  return(resLmpModelMatrix)
}
