#' @export UPDATE_lmpPcaEffects_3
#' @title PCA on the effect matrices
#'
#' @description
#' Performs a PCA on each of the effect matrices from the outputs of \code{\link{lmpEffectMatrices}}. It has an option to choose the method applied: ASCA, APCA or ASCA-E. Combined effects (i.e. linear combinations of original effect matrices) can also be created and decomposed by PCA.
#'
#' @param resLmpEffectMatrices A resLmpEffectMatrices list resulting of \code{\link{lmpEffectMatrices}}.
#' @param method The method used to compute the PCA. One of \code{c("ASCA","APCA","ASCA-E")}.
#' @param backtransform If \code{TRUE}, back-transforms the \code{outcomes} matrix if it has been reduced.
#' @param combineEffects If not \code{NULL}, a list of vectors containing the names of the effects to be combined.
#' @param verbose If \code{TRUE}, will display a message with the duration of execution.
#' @param correctedMatrixAdd If \code{TRUE} and for the method APCA or ASCA-E, allows effect matrices to be augmented with the effect matrix add multiplied by a factor corrected and not just by the effect matrix add.
#'
#' @return A list with the PCA results from \code{\link{pcaBySvd}} for each effect matrix. Those results contain :
#'  \describe{
#'   \item{\code{scores}}{Scores from the PCA for each principal component.}
#'   \item{\code{loadings}}{Loadings from the PCA for each principal component.}
#'   \item{\code{eigval}}{Eigenvalues of each principal component.}
#'   \item{\code{singvar}}{Singular values of each principal component.}
#'   \item{\code{var}}{Explained variances of each principal component.}
#'   \item{\code{cumvar}}{Cumulated explained variances of each principal component.}
#'   \item{\code{original.dataset}}{Original dataset.}
#'  }
#'
#'  There are also other outputs :
#'  \describe{
#'  \item{\code{lmpDataList}}{The initial object: a list of outcomes, designs and formulas.}
#'  \item{\code{effectsNamesUnique}}{A character vector with the \emph{F+1} names of the fixed model terms, each repeated once.}
#'  \item{\code{effectsNamesUniqueR}}{A character vector with the \emph{R} names of the random model terms, each repeated once.}
#'  \item{\code{effectsNamesUniqueCombined}}{A character vector with the names of the fixed model terms and the new combined fixed model terms, each repeated once.}
#'  \item{\code{effectsNamesUniqueCombinedR}}{A character vector with the names of the random model terms and the new combined random model terms, each repeated once.}
#'  \item{\code{method}}{The dimension reduction method used: \code{c("ASCA","APCA","ASCA-E")}.}
#'  \item{\code{type3SS}}{A vector with the type III SS for each model term.}
#'  \item{\code{varComponentsAbs}}{A vector with the absolute variance for each model effect If mixed model.}
#'  \item{\code{variationPercentages}}{A vector with the percentage of variance explained by each model term.}
#'  \item{\code{combineEffects}}{a list of vectors containing the names of the effects to be combined.}
#'  }
#'
#' @details
#'  The function allows 3 different methods :
#'
#'   \describe{
#'   \item{ASCA}{PCA is applied directly on each pure effect matrix \eqn{\hat{\mathbf{M}}_g, f=1...F+R}.}
#'   \item{ASCA-E}{PCA is applied on each pure effect matrix but then the augmented effect matrix is projected in the space of the ASCA components.}
#'   \item{APCA}{PCA is applied on each augmented effect matrix : \eqn{\hat{\mathbf{M}}_g+\hat{\mathbf{E}}}.}
#'  }
#'  
#'  
#'  If we are in the case of APCA or ASCA-E, all effect matrices are increased by the residuals matrix in general linear model. 
#'  
#'  For linear mixed model, if the model have structure of one of the three models : 
#'  \describe{
#'   \item{A mixed ANOVA2 with interaction. This type of model includes a fixed main effect, a random main effect, and a random interaction effect between the factor of the fixed effect and the factor of the random effect ; }
#'   \item{A hierarchical mixed ANOVA2. This type of model includes a random effect and another hierarchical random effect between the factor of the first higher-level random effect and another random factor at a lower level ; }
#'   \item{A two-factor mixed longitudinal model. This type of model includes two fixed effects, one being a Time effect, a fixed interaction effect between the two main fixed factors, and a random effect.}
#'  }
#'  one of the effect matrix is augmented with other random matrix than residuals matrix and all other effect matrices are augmented with the residuals matrix.
#'  
#'  The user have the choice to augmented all (random and fixed) effects matrices with corrected form or not with \code{correctedMatrixAdd}.
#'
#' @examples
#' data("UCH")
#' resLmpModelMatrix <- lmpModelMatrix(UCH)
#' resLmpEffectMatrices <- lmpEffectMatrices(resLmpModelMatrix)
#' resLmpPcaEffects <- lmpPcaEffects(resLmpEffectMatrices, method = "ASCA-E")
#'
#' @references Thiel M.,Feraud B. and Govaerts B. (2017) ASCA+ and APCA+: Extensions of ASCA and APCA
#' in the analysis of unbalanced multifactorial designs. \emph{Journal of Chemometrics}. 31:e2895.
#' \url{https://doi.org/10.1002/cem.2895}
#'


UPDATE_lmpPcaEffects_3 <- function(resLmpEffectMatrices, method = c("ASCA", "APCA", "ASCA-E"), 
                                 backtransform = FALSE, combineEffects = NULL, 
                                 verbose = FALSE, correctedMatrixAdd = FALSE) {
  ####################update Antoine##############
  if (!is.list(resLmpEffectMatrices)) {
    stop("Argument resLmpEffectMatrices is not a list")
  }
  
  if(names(resLmpEffectMatrices)[1] != "lmpDataList"){
    stop("Argument is not a resLmpEffectMatrices object")
  }
  
  model <- resLmpEffectMatrices$lmpDataList$model
  UPDATE_checkArg(model,c("model"), can.be.null = FALSE)
  
  if(model == "lmm"){
    # Checking the resLmpEffectMatrices list
    checkname <- c(
      "lmpDataList", "modelMatrix", "modelMatrixByEffect",
      "effectsNamesUnique",
      "effectsNamesAll", "effectMatrices",
      "predictedvalues", "residuals", "parameters", "modelMatrixR", 
      "modelMatrixByEffectR", "effectsNamesUniqueR",
      "effectsNamesAllR", "effectMatricesR", "parametersR", "resStdError",
      "variancesEstimatedR", "MM_full", "varComponentsAbs", "type3SS", "variationPercentages", 
      "varPercentagesPlot"
    )
    
    if (length(resLmpEffectMatrices) != 22) {
      stop("List does not contain 22 arguments")
    }
    if (!all(names(resLmpEffectMatrices) == checkname)) {
      stop("Argument is not a resLmpEffectMatrices object")
    }
    if (length(resLmpEffectMatrices$effectMatrices) !=
        length(resLmpEffectMatrices$effectsNamesUnique)) {
      stop("Number of effect fixed matrices differs from the number of fixed effects")
    }
    if (length(resLmpEffectMatrices$effectMatricesR) !=
        length(resLmpEffectMatrices$effectsNamesUniqueR)) {
      stop("Number of effect mixed matrices differs from the number of mixed effects")
    }
  } else{
    #############################
    # Checking the resLmpEffectMatrices list
  
    checkname <- c(
      "lmpDataList", "modelMatrix", "modelMatrixByEffect",
      "effectsNamesUnique",
      "effectsNamesAll", "effectMatrices",
      "predictedvalues", "residuals", "parameters",
      "type3SS", "variationPercentages", "varPercentagesPlot"
    )
  
  
    if (!is.list(resLmpEffectMatrices)) {
      stop("Argument resLmpEffectMatrices is not a list")
    }
    if (length(resLmpEffectMatrices) != 12) {
      stop("List does not contain 12 arguments")
    }
    if (!all(names(resLmpEffectMatrices) == checkname)) {
      stop("Argument is not a resLmpEffectMatrices object")
    }
    if (length(resLmpEffectMatrices$effectMatrices) !=
        length(resLmpEffectMatrices$effectsNamesUnique)) {
      stop("Number of effect matrices different from the number of effects")
    }
  }
  # if(!method %in% c("ASCA","APCA","ASCA-E")){stop("Method must be one of: ASCA, ASCA-E, APCA")}
  method <- match.arg(method)
  checkArg(combineEffects, c("list"), can.be.null = TRUE)

  # Attributing name

  lmpDataList <- resLmpEffectMatrices$lmpDataList
  effectsNamesUnique <- effectsNamesUniqueCombined <-
    resLmpEffectMatrices$effectsNamesUnique
  
  ### update Antoine
  if(model == "lmm"){
    effectsNamesUniqueR <- resLmpEffectMatrices$effectsNamesUniqueR
    effectsNamesUniqueCombinedR <- c(effectsNamesUniqueR)
  } else {
    effectsNamesUniqueR <- c()
  }
  ###

  # Combining effects
  combinedMatrices <- NULL
  if (!is.null(combineEffects)) {
    # if(correctedMatrixAdd){
    #   stop("The corrected augmented matrix is not supported in the current version with combinations of effects")
    # }
    combineMatrices <- function(combineVectors) {
      matList <- list()
      combinedEffects <- list()
      ### update Antoine
      matList <- lapply(combineVectors, function(x){
        # effect is fixed
        if(x %in% effectsNamesUnique){
          resLmpEffectMatrices$effectMatrices[[x]]
        } else if(x %in% effectsNamesUniqueR){ # effect is random
          resLmpEffectMatrices$effectMatricesR[[x]]
        } else{
          stop("effects in the combineEffects object is incorrect")
        }
      })
      ###
      combinedEffects <- Reduce("+", matList)
    }

    combinedMatrices <- lapply(combineEffects, combineMatrices)
    names(combinedMatrices) <- lapply(combineEffects,
                                      FUN = function(x) paste0(x,
                                                               collapse = "+"))
    
    #####update Antoine
    # if one effect is random in the combination, the combined effects is random
    isRandom <- lapply(combineEffects, function(vector_effects) {
      any(sapply(vector_effects, function(effect) effect %in% effectsNamesUniqueR))
    })
    
    for(i in 1:length(isRandom)){
      if(isRandom[[i]]){
        resLmpEffectMatrices$effectMatricesR <- c(
          resLmpEffectMatrices$effectMatricesR,
          combinedMatrices[i]
        )
        effectsNamesUniqueCombinedR <- c(effectsNamesUniqueCombinedR,
                                        names(combinedMatrices[i]))
      } else{
        resLmpEffectMatrices$effectMatrices <- c(
          resLmpEffectMatrices$effectMatrices,
          combinedMatrices[i]
        )
        effectsNamesUniqueCombined <- c(effectsNamesUniqueCombined,
                                        names(combinedMatrices[i]))
      }
    }
    
    #####
  }


  # Construction of the list of pure effect matrix
  
  ####update Antoine
  if(model == "lmm"){
    EffectMatGLMR <- resLmpEffectMatrices$effectMatricesR
    res <- vector(mode = "list")
    res[[1]] <- resLmpEffectMatrices$residuals
    EffectMatGLMR <- c(EffectMatGLMR, Residuals = res) # plus residuals
    
    if (!is.null(combineEffects)) {
      pR <- length(effectsNamesUniqueCombinedR) + 1
    } else {
      pR <- length(effectsNamesUniqueR) + 1 # Number of parameters random + residuals
    }
  }
  ####
  
  EffectMatGLM <- resLmpEffectMatrices$effectMatrices[-1] # minus intercept
  ### update Antoine
  # if lm -> the residuals in the random effects list
  if(model == "lm"){
    res <- vector(mode = "list")
    res[[1]] <- resLmpEffectMatrices$residuals
    EffectMatGLM <- c(EffectMatGLM, Residuals = res) # plus residuals
    if (!is.null(combineEffects)) {
      p <- length(effectsNamesUniqueCombined)
    } else {
      p <- length(effectsNamesUnique) # Number of parameters fixed minus intercept plus residuals
    }
  } else {
    if (!is.null(combineEffects)) {
      p <- length(effectsNamesUniqueCombined) - 1
    } else {
      p <- length(effectsNamesUnique) - 1 # Number of parameters fixed minus intercept
    }
  }
  ###
  
  # Defining the type of model :
  # Name of matrix to add 
  nameMadd <- ""
  # Name of matrix to augmented
  nameMaug <- ""
  if((method == "ASCA-E" | method == "APCA") & model == "lmm"){
    if(length(effectsNamesUniqueR) == 2 & length(effectsNamesUnique) == 2){
      # The interaction marginal effect
      interactionR <- effectsNamesUniqueR[grepl(":", effectsNamesUniqueR)]
      # List of variable in the interaction
      listInterVar <- strsplit(interactionR,":")[[1]]
      # One fixed effect in the interaction
      if(length(intersect(listInterVar,effectsNamesUnique)) == 1){
        print("The model is a mixed ANOVA 2 with interaction.")
        # name of matrix to add 
        nameMadd <- interactionR
        # name of matrix to augmented
        nameMaug <- effectsNamesUnique[2]
      }
    } else if(length(effectsNamesUniqueR) == 2 & length(effectsNamesUnique) == 1){
      # The interaction marginal effect
      interactionR <- effectsNamesUniqueR[grepl(":", effectsNamesUniqueR)]
      # List of variable in the interaction
      listInterVar <- strsplit(interactionR,":")[[1]]
      # No fixed effect in the interaction
      if(length(intersect(listInterVar,effectsNamesUnique)) == 0){
        print("The model is a nested mixed ANOVA 2.")
        # The random marginal effect
        marginalR <- effectsNamesUniqueR[!grepl(":", effectsNamesUniqueR)]
        # Name of matrix to add 
        nameMadd <- interactionR
        # Name of matrix to augmented
        nameMaug <- marginalR
      }
    } else if (length(effectsNamesUniqueR) == 1 & length(effectsNamesUnique) == 4){
      # Must one fixed effect names is time or temps and one fixed effect is interaction
      if(any(str_to_lower(effectsNamesUnique) %in% c("time", "temps")) & length(effectsNamesUnique[grepl(":", effectsNamesUnique)]) == 1){
        print("The model is a longitudinal 2 factor")
        # Name of matrix to add 
        nameMadd <- effectsNamesUniqueR[1]
        # Name of matrix to augmented
        nameMaug <- effectsNamesUnique[!grepl(":", effectsNamesUnique) & !(str_to_lower(effectsNamesUnique) %in% c("time", "temps"))][-1]
      }
    } else {
      warning("The model structure isn't a mixed ANOVA2 with interaction, a hierarchical mixed ANOVA2 model or a two-factor longitudinal mixed model (with the effect of 'Time'). All the effects matrices are augmented by the residuals matrix.")
      #stop("The type of model not supported")
    }
  } 

  # Defining the matrix for the PCA depending on the method

  if (method == "ASCA") {
    if (verbose) {
      print("ASCA method used : PCA on the pure effect matrices")
      start_time <- Sys.time()
    }
    
    # Run the PCA on the different effects
    resLmpPcaEffects <- vector(mode = "list")
    
    # update Antoine : if not fixed effect 
    if(p != 0){
      for (i in 1:p) {
        resLmpPcaEffects[[i]] <- pcaBySvd(EffectMatGLM[[i]])
      }
    }
    
    ### update Antoine
    resLmpPcaEffectsR <- vector(mode = "list")
    
    if(model == "lmm"){
      for (i in 1:pR) {
        resLmpPcaEffectsR[[i]] <- pcaBySvd(EffectMatGLMR[[i]])
      }
    }
    ###
    
  } else if (method == "APCA") {
    if (verbose) {
      print("APCA method used : PCA on the augmented effect matrices")
      start_time <- Sys.time()
    }

    # Compute the augmented effect matrices
    
    ###update Antoine
    
    if(correctedMatrixAdd){
      # Effective dimensions
      ED <- computeED(resLmpEffectMatrices)
    }else{
      # Not use effective dimensions
      if(model == "lm"){
        nbrEffect <- length(resLmpEffectMatrices$effectsNamesUnique)
        ED <- matrix(NA, ncol=nbrEffect, nrow=nbrEffect)
        rownames(ED) <- c(resLmpEffectMatrices$effectsNamesUnique[-1],"Residuals")
      } else {
        nbrEffect <- length(resLmpEffectMatrices$effectsNamesUniqueR) + length(resLmpEffectMatrices$effectsNamesUnique)
        ED <- matrix(NA, ncol=nbrEffect, nrow=nbrEffect)
        rownames(ED) <- c(resLmpEffectMatrices$effectsNamesUniqueR,resLmpEffectMatrices$effectsNamesUnique[-1],"Residuals")
      }
    }
    
    if(model == "lm"){
      resLmpPcaEffects <- lapply(names(EffectMatGLM[1:(length(EffectMatGLM) - 1)]), function(x){
        if(x %in% names(combinedMatrices)){
          computeAugmentedScoresAPCA(EffectMatGLM[[x]],resLmpEffectMatrices$residuals,0,ED["Residuals",],FALSE)
        } else{
          computeAugmentedScoresAPCA(EffectMatGLM[[x]],resLmpEffectMatrices$residuals,ED[x,],ED["Residuals",],correctedMatrixAdd)
        }
      })
      # Compute de residuals PCA
      resLmpPcaEffects[[length(EffectMatGLM)]] <- pcaBySvd(EffectMatGLM[[length(EffectMatGLM)]])
    } else{
      ######### Warning : the function add specific matrix for the models.
      ######### Otherwise by defaut we add the residuals to the matrix
      resLmpPcaEffects <- lapply(names(EffectMatGLM), function(x){
        # we do not calculate the ED for combination effect so not correction matrix for them
        if(x %in% names(combinedMatrices)){
          effectsComb <- strsplit(x, "\\+")
          if(nameMaug %in% effectsComb){
            computeAugmentedScoresAPCA(EffectMatGLM[[x]],resLmpEffectMatrices$effectMatricesR[[nameMadd]],0,ED[nameMadd,],FALSE)
          } else{
            computeAugmentedScoresAPCA(EffectMatGLM[[x]],resLmpEffectMatrices$residuals,0,ED["Residuals",],FALSE)
          }
        } else if(x == nameMaug){
          computeAugmentedScoresAPCA(EffectMatGLM[[x]],resLmpEffectMatrices$effectMatricesR[[nameMadd]],ED[x,],ED[nameMadd,],correctedMatrixAdd)
        } else{
          computeAugmentedScoresAPCA(EffectMatGLM[[x]],resLmpEffectMatrices$residuals,ED[x,],ED["Residuals",],correctedMatrixAdd)
        }
      })
      
      resLmpPcaEffectsR <- lapply(names(EffectMatGLMR[1:(length(EffectMatGLMR) - 1)]), function(x){
        # not calculate the ED for combination effect so not correction matrix
        if(x %in% names(combinedMatrices)){
          effectsComb <- strsplit(x, "\\+")
          if(nameMaug %in% effectsComb){
            computeAugmentedScoresAPCA(EffectMatGLMR[[x]],resLmpEffectMatrices$effectMatricesR[[nameMadd]],0,ED[nameMadd,],FALSE)
          } else{
            computeAugmentedScoresAPCA(EffectMatGLMR[[x]],resLmpEffectMatrices$residuals,0,ED["Residuals",],FALSE)
          }
        } else if(x == nameMaug){
          computeAugmentedScoresAPCA(EffectMatGLMR[[x]],resLmpEffectMatrices$effectMatricesR[[nameMadd]],ED[x,],ED[nameMadd,],correctedMatrixAdd)
        }else{
          computeAugmentedScoresAPCA(EffectMatGLMR[[x]],resLmpEffectMatrices$residuals,ED[x,],ED["Residuals",],correctedMatrixAdd)
        }
      })
      #########
      
      # Compute de residuals PCA
      resLmpPcaEffectsR[[length(EffectMatGLMR)]] <- pcaBySvd(EffectMatGLMR[[length(EffectMatGLMR)]])
    }
    ###
    
  } else if (method == "ASCA-E") { # Updating the score for ASCA-E method
    if (verbose) {
      print("ASCA-E method used : PCA on the pure effect matrices
            but scores are updated")
      start_time <- Sys.time()
    }
    
    if(correctedMatrixAdd){
      # Effective dimensions
      ED <- computeED(resLmpEffectMatrices)
    }else{
      # Not use effective dimensions
      if(model == "lm"){
        nbrEffect <- length(resLmpEffectMatrices$effectsNamesUnique)
        ED <- matrix(NA, ncol=nbrEffect, nrow=nbrEffect)
        rownames(ED) <- c(resLmpEffectMatrices$effectsNamesUnique[-1],"Residuals")
      } else {
        nbrEffect <- length(resLmpEffectMatrices$effectsNamesUniqueR) + length(resLmpEffectMatrices$effectsNamesUnique)
        ED <- matrix(NA, ncol=nbrEffect, nrow=nbrEffect)
        rownames(ED) <- c(resLmpEffectMatrices$effectsNamesUniqueR,resLmpEffectMatrices$effectsNamesUnique[-1],"Residuals")
      }
    }
    
    ### update Antoine :  
    if(model == "lm"){
      resLmpPcaEffects <- lapply(names(EffectMatGLM[1:(length(EffectMatGLM) - 1)]), function(x){
        # not calculate the ED for combination effect so not correction matrix
        if(x %in% names(combinedMatrices)){
          computeAugmentedScoresASCAE(EffectMatGLM[[x]],resLmpEffectMatrices$residuals,0,ED["Residuals",],FALSE)
        } else{
          computeAugmentedScoresASCAE(EffectMatGLM[[x]],resLmpEffectMatrices$residuals,ED[x,],ED["Residuals",],correctedMatrixAdd)
        }
      })
      
      # Compute de residuals PCA
      resLmpPcaEffects[[length(EffectMatGLM)]] <- pcaBySvd(EffectMatGLM[[length(EffectMatGLM)]])
      
    } else {
      
      ######### Warning : the function add specific matrix for the models Candies,CHOO and Serum.
      ######### Otherwise by defaut the matrix to add is the residuals matrix
      resLmpPcaEffects <- lapply(names(EffectMatGLM), function(x){
        # not calculate the ED for combination effect so not correction matrix
        if(x %in% names(combinedMatrices)){
          effectsComb <- strsplit(x, "\\+")
          if(nameMaug %in% effectsComb){
            computeAugmentedScoresASCAE(EffectMatGLM[[x]],resLmpEffectMatrices$effectMatricesR[[nameMadd]],0,ED[nameMadd,],FALSE)
          } else{
            computeAugmentedScoresASCAE(EffectMatGLM[[x]],resLmpEffectMatrices$residuals,0,ED["Residuals",],FALSE)
          }
        } else if(x == nameMaug){
          computeAugmentedScoresASCAE(EffectMatGLM[[x]],resLmpEffectMatrices$effectMatricesR[[nameMadd]],ED[x,],ED[nameMadd,],correctedMatrixAdd)
        }else{
          computeAugmentedScoresASCAE(EffectMatGLM[[x]],resLmpEffectMatrices$residuals,ED[x,],ED["Residuals",],correctedMatrixAdd)
        }
      })
      
      resLmpPcaEffectsR <- lapply(names(EffectMatGLMR[1:(length(EffectMatGLMR) - 1)]), function(x){
        # not calculate the ED for combination effect so not correction matrix
        if(x %in% names(combinedMatrices)){
          effectsComb <- strsplit(x, "\\+")
          if(nameMaug %in% effectsComb){
            computeAugmentedScoresASCAE(EffectMatGLMR[[x]],resLmpEffectMatrices$effectMatricesR[[nameMadd]],0,ED[nameMadd,],FALSE)
          } else{
            computeAugmentedScoresASCAE(EffectMatGLMR[[x]],resLmpEffectMatrices$residuals,0,ED["Residuals",],FALSE)
          }
        } else if(x == nameMaug){
          computeAugmentedScoresASCAE(EffectMatGLMR[[x]],resLmpEffectMatrices$effectMatricesR[[nameMadd]],ED[x,],ED[nameMadd,],correctedMatrixAdd)
        }else{
          computeAugmentedScoresASCAE(EffectMatGLMR[[x]],resLmpEffectMatrices$residuals,ED[x,],ED["Residuals",],correctedMatrixAdd)
        }
      })
      
      #########
      
      # Compute the residuals PCA
      resLmpPcaEffectsR[[length(EffectMatGLMR)]] <- pcaBySvd(EffectMatGLMR[[length(EffectMatGLMR)]])
    }
    ###
    
  } else {
    stop("The method argument is not one of those : ASCA, APCA, ASCA-E")
  }
  
  ### update Antoine
  # backtransformation so add the loadings
  isReduct <- lmpDataList$isReduct
  loadings <- lmpDataList$loadingsPCA
  if(backtransform & !is.null(isReduct)){
    if(isReduct){
      if(is.null(loadings)){
        stop("No backtransformation : The loadings from the PCA do not exist.")
      }
      if(model == "lmm"){
        for (i in 1:pR) {
          resLmpPcaEffectsR[[i]]$loadings <- t(t(resLmpPcaEffectsR[[i]]$loadings) %*% t(loadings))
        }
      }
      # update Antoine : if not fixed effect 
      if(p != 0){
        for (i in 1:p) {
          resLmpPcaEffects[[i]]$loadings <- t(t(resLmpPcaEffects[[i]]$loadings) %*% t(loadings))
        }
      }
      lmpDataList$outcomes <- lmpDataList$outcomesRaw
    }
  }
  
  if (verbose) {
    print(Sys.time() - start_time)
  }
  ###

  names(resLmpPcaEffects) <- names(EffectMatGLM)

  resLmpPcaEffects2 <- list(
    lmpDataList = lmpDataList,
    effectsNamesUnique = effectsNamesUnique,
    effectsNamesUniqueCombined = effectsNamesUniqueCombined
  )
  
  ###update Antoine
  if(model == "lm"){
    resLmpPcaEffects <- c(resLmpPcaEffects, resLmpPcaEffects2,
      method = method,
      type3SS = list(resLmpEffectMatrices$type3SS),
      variationPercentages = list(resLmpEffectMatrices$variationPercentages),
      combineEffects = list(combineEffects)
    )
  } else {
    names(resLmpPcaEffectsR) <- names(EffectMatGLMR)
    resLmpPcaEffects <- c(resLmpPcaEffects, resLmpPcaEffectsR, resLmpPcaEffects2,
      effectsNamesUniqueR = list(effectsNamesUniqueR),
      effectsNamesUniqueCombinedR = list(effectsNamesUniqueCombinedR),
      method = method,
      varComponentsAbs = list(resLmpEffectMatrices$varComponentsAbs),
      variationPercentages = list(resLmpEffectMatrices$variationPercentages),
      combineEffects = list(combineEffects)
    )
  }
  ###

  return(resLmpPcaEffects)
}
