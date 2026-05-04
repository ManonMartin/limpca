#' @export UPDATE_lmpEffectMatrices_2
#' @title Computes the effect matrices
#'
#' @description
#' For the linear model, estimates the model by OLS based on the outcomes and model matrices provided in the outputs of lmpModelMatrix function and calculates the estimated effect matrices
#' \eqn{\hat{\mathbf{M}}_0, \hat{\mathbf{M}}_1, ...\hat{\mathbf{M}}_F}, ...  and residual matrix \eqn{\hat{\mathbf{E}}}.
#' It calculates also the type III percentage of variance explained by each effect.
#' 
#' For the linear mixed model, the estimation of effects matrix is calculated using the lmer function from the lme4 package.The model is computed for each column of the outcomes matrix in parallel.
#' The function calculate the matrices of fixed effects \eqn{\hat{\mathbf{M}}_0, \hat{\mathbf{M}}_1, ...\hat{\mathbf{M}}_F}, the matrices of random effects \eqn{\hat{\mathbf{M}}_1, ...\hat{\mathbf{M}}_R}
#' and residual matrix \eqn{\hat{\mathbf{E}}} where F is the number of fixed effect and R is the the number of mixed effect. 
#' The Nakagawa and Schielzeth(2012) method is used to calculate the percentage of variance explained by each effect in mixed-effects models. 
#' But the variance estimation of fixed effect use the SS type III divides by numbers of observations.
#'
#' @param resLmpModelMatrix A list of 5 or 9 elements from \code{\link{lmpModelMatrix}}.
#' @param SS Logical. If `FALSE`, won't compute the percentage of variance for each effect.
#' @param contrastList A list of contrasts for each parameter. If `NA`, the function creates automatically the list by default.
#'
#' @return A list with the following elements:
#'  \describe{
#'    \item{\code{lmpDataList}}{The initial object: a list with outcomes, design and formula.}
#'    \item{\code{modelMatrix}}{A \emph{nxp} fixed model matrix specifically encoded for the ASCA-GLM method.}
#'    \item{\code{modelMatrixByEffect}}{A list of \emph{F+1} model matrices for each fixed effect.}
#'    \item{\code{effectsNamesUnique}}{A character vector with the \emph{F+1} names of the fixed model effects, each repeated once.}
#'    \item{\code{effectsNamesAll}}{A character vector with the \emph{p} names of the fixed model effects ordered and repeated as the column names of the fixed model matrix.}
#'    \item{\code{effectMatrices}}{A list of \emph{F+1} effect matrices for each fixed model effect.}
#'    \item{\code{predictedvalues}}{The \emph{nxm} matrix of predicted outcome values.}
#'    \item{\code{residuals}}{The \emph{nxm} matrix of model residuals.}
#'    \item{\code{parameters}}{The \emph{pxm} matrix of the fixed estimated parameters.}
#'    \item{\code{modelMatrixR}{The informative effect random matrix if mixed linear model.}
#'    \item{\code{modelMatrixByEffectR}}{A list of \emph{R} model matrices for each random effect if mixed linear model.}
#'    \item{\code{effectsNamesUniqueR}{A character vector with the \emph{R} names of the random model effects, each repeated once if mixed linear model.}
#'    \item{\code{effectsNamesAllR}{A character vector with the names of the random model effects ordered and repeated as the column names of the random model matrix if mixed linear model.}
#'    \item{\code{effectMatricesR}{A list of \emph{R} effect matrices for each random model effect if mixed linear model.}
#'    \item{\code{parametersR}{The matrix of the random estimated parameters if mixed linear model.}
#'    \item{\code{resStdError}{The estimated standard deviation (sigma) of residuals if mixed linear model.}
#'    \item{\code{variancesEstimatedR}{The estimated variances of the random effects if mixed linear model.}
#'    \item{\code{MM_full}{The list of lmerMod object for each response variable if mixed linear model.}
#'    \item{\code{type3SS}}{A vector with the type III sum of squares for each model effect fixed \emph{(If SS = TRUE)}.}
#'    \item{\code{varComponentsAbs}}{A vector with the absolute variance for each model effect if mixed linear model.}
#'    \item{\code{variationPercentages}}{A vector with the percentage of variance for each model effect \emph{(If SS = TRUE) or mixed linear model}.}
#'    \item{\code{varPercentagesPlot}}{A ggplot bar plot of the contributions of each model effect to the total variance \emph{(If SS = TRUE) or mixed linear model}.}
#'  }
#'
#' @examples
#' data("UCH")
#' resLmpModelMatrix <- lmpModelMatrix(UCH)
#' reslmpEffectMatrices <- lmpEffectMatrices(resLmpModelMatrix)
#' reslmpEffectMatrices$varPercentagesPlot
#'
#' @references Thiel M.,Feraud B. and Govaerts B. (2017) \emph{ASCA+ and APCA+: Extensions of ASCA and APCA
#' in the analysis of unbalanced multifactorial designs}, Journal of Chemometrics
#'
#' @import stringr
#' @importFrom plyr laply aaply
#' @importFrom lme4 lmer



UPDATE_lmpEffectMatrices_2 <- function(resLmpModelMatrix, SS = TRUE, contrastList = NA) {
  #######update Antoine###############
  # Create the variance percentages plots
  # I have create a separate function
  createVarPercentagesPlot <- function(variationPercentages){
    # Plot of the total contribution
    contrib <- as.data.frame(variationPercentages)
    rownames(contrib) <- ModelAbbrev(rownames(contrib))
    
    plot <- ggplot2::ggplot(
      data = contrib,
      ggplot2::aes(
        x = stats::reorder(
          rownames(contrib),
          -contrib[, 1]
        ),
        y = contrib[, 1]
      )
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::xlab("Effects") +
      ggplot2::ylab("Percentage of Variance") +
      ggplot2::theme_bw()
  
    return(plot)
  }
  #############################################################
  
  # Checking the object
  if (!is.list(resLmpModelMatrix)) {
    stop("Argument resLmpModelMatrix is not a list")
  }
  
  ###############update Antoine##############################
  if(names(resLmpModelMatrix)[1] != "lmpDataList"){
    stop("Argument is not a resLmpModelMatrix object")
  }
  
  model <- resLmpModelMatrix$lmpDataList$model
  UPDATE_checkArg(model,c("model"), can.be.null = FALSE)
  
  if(model == "lmm"){
    if (length(resLmpModelMatrix) != 9) {
      stop("List does not contain 9 elements")
    }
    if (names(resLmpModelMatrix)[1] != "lmpDataList" |
        names(resLmpModelMatrix)[2] != "modelMatrix" |
        names(resLmpModelMatrix)[3] != "modelMatrixByEffect" |
        names(resLmpModelMatrix)[4] != "effectsNamesUnique" |
        names(resLmpModelMatrix)[5] != "effectsNamesAll"|
        names(resLmpModelMatrix)[6] != "modelMatrixR" |
        names(resLmpModelMatrix)[7] != "modelMatrixByEffectR" |
        names(resLmpModelMatrix)[8] != "effectsNamesUniqueR" |
        names(resLmpModelMatrix)[9] != "effectsNamesAllR") {
      stop("Argument is not a resLmpModelMatrix object")
    }
    
    # Attribute a name in the function environment
    formula <- resLmpModelMatrix$lmpDataList$formula
    design <- resLmpModelMatrix$lmpDataList$design
    outcomes <- resLmpModelMatrix$lmpDataList$outcomes
    lmpDataList <- resLmpModelMatrix$lmpDataList
    modelMatrix <- resLmpModelMatrix$modelMatrix
    modelMatrixByEffect <- resLmpModelMatrix$modelMatrixByEffect
    effectsNamesAll <- resLmpModelMatrix$effectsNamesAll
    effectsNamesUnique <- resLmpModelMatrix$effectsNamesUnique
    nEffect <- length(effectsNamesUnique)

    modelMatrixR <- resLmpModelMatrix$modelMatrixR
    modelMatrixByEffectR <- resLmpModelMatrix$modelMatrixByEffectR
    effectsNamesAllR <- resLmpModelMatrix$effectsNamesAllR
    effectsNamesUniqueR <- resLmpModelMatrix$effectsNamesUniqueR
    nEffectR <- length(effectsNamesUniqueR)
    
    ### parallel LMM 
    #######################
    # delete outcomes in the formula
    formula <- gsub("outcomes", "",formula)
    
    fmla <- sapply(paste0(colnames(outcomes), formula), as.formula)
    data_full <- cbind(design, outcomes)
    
    # Create contrasts sum
    contrastsSumList <- createContrastsSum(formula)
    
    # Use lmer for parallel LMM
    res.lmer <- list()
    length(res.lmer) <- length(colnames(outcomes))
    names(res.lmer) <- colnames(outcomes)
    
    # The response variable with a message (name is the message and response name is the value).
    responseWithMsg <- list()
    # The response variable with a warning (name is the warning message and response name is the value).
    responseWithWarning <- list()
    for(i in 1:length(colnames(outcomes))){
    # Allows you to display a specific message error
      res.lmer[[i]] <- withCallingHandlers({
        if(length(contrastsSumList) != 0){
         res.lmer[[i]] <- lmer(fmla[[i]], data = data_full, contrasts = contrastsSumList, control = lmerControl(optimizer = "bobyqa"))
        } else {
         res.lmer[[i]] <- lmer(fmla[[i]], data = data_full, control = lmerControl(optimizer = "bobyqa"))
        }
      },
      message = function(msg) {
        # Concatenation the response with the same message
        if (is.null(responseWithMsg[[msg$message]])) {
          responseWithMsg[[msg$message]] <<- paste(responseWithMsg[[msg$message]], names(res.lmer)[i])
        } else{
          responseWithMsg[[msg$message]] <<- paste(responseWithMsg[[msg$message]], names(res.lmer)[i], sep = ", ")
        }
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        # Concatenation the response with the same warning
        if (is.null(responseWithMsg[[w$message]])) {
          responseWithWarning[[w$message]] <<- paste(responseWithMsg[[w$message]], names(res.lmer)[i])
        }else{
          responseWithWarning[[w$message]] <<- paste(responseWithMsg[[w$message]], names(res.lmer)[i], sep = ", ")
        }
        invokeRestart("muffleWarning")
      })
    }
    
    # Display messages
    if(length(responseWithMsg) != 0 ){
      for(msg in names(responseWithMsg)){
        responses <- sub(" ,","", responseWithMsg[[msg]])
        warning(paste(msg, "For the response(s) :", responses, "\n"))
      }
    }
    
    # Display warnings
    if(length(responseWithWarning) != 0 ){
      for(w in names(responseWithWarning)){
        responses <- sub("NULL ,","", responseWithWarning[[w]])
        warning(paste(w, "for the response(s) :", responses, "\n"))
      }
      warning("The model estimates for these response(s) can be biased.")
    }
    
    MM_full <- res.lmer
    
    # The random model matrix (same as input modelMatrixR)
    modelMatrixR <- as.matrix(t(MM_full[[1]]@pp[[".->Zt"]]))
    
    # The fixed model matrix (same as input modelMatrix)
    modelMatrix <- as.matrix(model.matrix(MM_full[[1]],type = c("fixed")))
    
    # Same as input object
    effectsNamesUniqueR <- names(MM_full[[1]]@flist)
    
    # Same as input object
    effectsNamesUnique  <- c("Intercept", attr(terms(formula(MM_full[[1]], fixed.only = TRUE)),"term.labels"))
    
    # Calculate modelMatrixByEffectR
    RanModMatlist <- list()
    
    # Columns belonging to the same random variable have the same number
    index_rand <- MM_full[[1]]@pp[[".->Lind"]]
    nbr_var <- length(unique(index_rand))
    
    for(i in 1:nbr_var){
      RanModMatlist[[effectsNamesUniqueR[i]]] <- modelMatrixR[,i == index_rand]
    }
    
    modelMatrixByEffectR <- RanModMatlist
    
    # Using and modifying a piece of code from parlmer_interacton by Manon Martin 
    # Create an object with the list of fixed variable matrices
    modelMatrixByEffect <- createModelMatrixFixByEffect(MM_full, design)
    
    # Predicted values
    predictedValues <- sapply(MM_full, predict)
    
    # Extract the coef of the random effects
    parametersR <- sapply(MM_full, function(x) unlist(ranef(x, condVar=FALSE)))
    if(class(parametersR)[1] == "matrix"){
      colnames(parametersR) <- colnames(outcomes)
      rownames(parametersR) <- colnames(modelMatrixR)
    }else{
      names(parametersR) <- colnames(outcomes)
    }
    
    # Extract fixed-effects estimates
    parameters <- sapply(MM_full, fixef)
    if(class(parameters)[1] == "matrix"){
      colnames(parameters) <- colnames(outcomes)
      rownames(parameters) <- colnames(modelMatrix)
    }else{
      names(parameters) <- colnames(outcomes)
    }
    
    # Residuals sd error
    resStdError <- sapply(MM_full, sigma)
    
    # Extract Variance Components
    variancesEstimatedR <- sapply(MM_full, VarCorr)
    names(variancesEstimatedR) <- rep(colnames(outcomes),length(effectsNamesUniqueR))
    variancesEstimatedR_df <- as.data.frame(variancesEstimatedR)
    
    # Fixed effects + intercept
    dim1FixedModMad <- sapply(modelMatrixByEffect, function(x) dim(x)[2])
    names_FixedEffects <- names(modelMatrixByEffect)
    shortFixNames <- gsub("[^A-z]&\\:", "", names_FixedEffects)
    
    Xmat <- modelMatrix
    if(dim(Xmat)[2] != 1){
      Xmat <- Xmat[,rownames(parameters)] # reorder colnames of Xmat
    }
    
    index <- cumsum(dim1FixedModMad)
    k <- 1
    # Compute the fixed effects
    effectMatrices  <- vector("list", length=length(shortFixNames))
    names(effectMatrices) <- shortFixNames
    
    for (i in 1:length(shortFixNames)){
      mat <- Xmat
      mat[,-c(k:index[i])] <- 0
      effectMatrices[[i]] <- mat %*% parameters
      k <-  index[i] + 1
    }
    
    # Random effects
    dim1RandModMad <- sapply(modelMatrixByEffectR, function(x) dim(x)[2])
    names_randomEffects <- names(modelMatrixByEffectR)
    shortRandNames <- gsub("[^A-z]&\\:", "", names_randomEffects)
    Zmat <- modelMatrixR
    colnames(Zmat) <- colnames(modelMatrixR)
    
    index <- cumsum(dim1RandModMad)
    k <- 1
    
    # Compute random effects
    effectMatricesR <- vector("list", length=length(shortRandNames))
    names(effectMatricesR) <- shortRandNames
    for (i in 1:length(shortRandNames)){
      mat <-  Zmat
      mat[,-c(k:index[i])] <- 0
      effectMatricesR[[i]] <- mat %*% parametersR
      k <-  index[i] + 1
    }
    
    # Residuals matrix
    residuals <- sapply(MM_full, residuals)
    
    resLmpEffectMatrices <- list(
      lmpDataList = lmpDataList,
      modelMatrix = modelMatrix,
      modelMatrixByEffect = modelMatrixByEffect,
      effectsNamesUnique = effectsNamesUnique,
      effectsNamesAll = effectsNamesAll,
      effectMatrices = effectMatrices,
      predictedvalues = predictedValues,
      residuals = residuals,
      parameters = parameters,
      modelMatrixR = modelMatrixR,
      modelMatrixByEffectR = modelMatrixByEffectR,
      effectsNamesUniqueR = effectsNamesUniqueR,
      effectsNamesAllR = effectsNamesAllR,
      effectMatricesR = effectMatricesR,
      parametersR = parametersR,
      resStdError = resStdError,
      variancesEstimatedR = variancesEstimatedR,
      MM_full = MM_full
    )
    
  }else if(model == "lm"){
  #################################################################
    
    if (length(resLmpModelMatrix) != 5) {
      stop("List does not contain 5 elements")
    }
    if (names(resLmpModelMatrix)[1] != "lmpDataList" |
      names(resLmpModelMatrix)[2] != "modelMatrix" |
      names(resLmpModelMatrix)[3] != "modelMatrixByEffect" |
      names(resLmpModelMatrix)[4] != "effectsNamesUnique" |
      names(resLmpModelMatrix)[5] != "effectsNamesAll") {
      stop("Argument is not a resLmpModelMatrix object")
    }
    checkArg(SS, c("bool", "length1"), can.be.null = FALSE)
  
    # Attribute a name in the function environment
    formula <- resLmpModelMatrix$lmpDataList$formula
    design <- resLmpModelMatrix$lmpDataList$design
    outcomes <- resLmpModelMatrix$lmpDataList$outcomes
    lmpDataList <- resLmpModelMatrix$lmpDataList
    modelMatrix <- resLmpModelMatrix$modelMatrix
    modelMatrixByEffect <- resLmpModelMatrix$modelMatrixByEffect
    effectsNamesAll <- resLmpModelMatrix$effectsNamesAll
    effectsNamesUnique <- resLmpModelMatrix$effectsNamesUnique
    nEffect <- length(effectsNamesUnique)
  
    # Creating empty effects matrices
    effectMatrices <- list()
    length(effectMatrices) <- nEffect
    names(effectMatrices) <- effectsNamesUnique
  
    # GLM decomposition calculated by using glm.fit and alply on outcomes
  
    # The following line gives an error of type: Error in glm.fit(modelMatrix, xx) : NAs in V(mu),
    # it is temporarily replaced by a loop
    # resGLM <- plyr::alply(outcomes, 2, function(xx) glm.fit(modelMatrix, xx))
  
    resGLM <- list()
    for (i in 1:ncol(outcomes)) resGLM[[i]] <- stats::glm.fit(modelMatrix, outcomes[, i])
    parameters <- t(plyr::laply(resGLM, function(xx) xx$coefficients))
    predictedValues <- t(plyr::laply(resGLM, function(xx) xx$fitted.values))
    residuals <- t(plyr::laply(resGLM, function(xx) xx$residuals))
    colnames(residuals) <- colnames(predictedValues) <- colnames(outcomes)
  
    # Filling effectMatrices
    for (iEffect in 1:nEffect) {
      selection <- which(effectsNamesAll == effectsNamesUnique[iEffect])
      selectionComplement <- which(effectsNamesAll != effectsNamesUnique[iEffect])
      # Effect matrices
      effectMatrices[[iEffect]] <- t(plyr::aaply(parameters, 2,
                                                 function(xx)
                                                   as.matrix(modelMatrix[, selection]) %*%
                                                   xx[selection]))
      colnames(effectMatrices[[iEffect]]) <- colnames(outcomes)
    }
  
  
    resLmpEffectMatrices <- list(
      lmpDataList = lmpDataList,
      modelMatrix = modelMatrix,
      modelMatrixByEffect = modelMatrixByEffect,
      effectsNamesUnique = effectsNamesUnique,
      effectsNamesAll = effectsNamesAll,
      effectMatrices = effectMatrices,
      predictedvalues = predictedValues,
      residuals = residuals,
      parameters = parameters
    )
  }
  
    # Compute the Sum of Squares Type 3
    # update Antoine
    if (SS == TRUE | model == "lmm") {
      
      if (is.na(contrastList)) {
        L <- contrastSS(resLmpModelMatrix)
      } else {
        L <- contrastList
      }
      # update Antoine
      resLmpSS <- UPDATE_lmpSS(resLmpEffectMatrices, L, model)
      
      #### update Antoine
      if(model == "lmm"){
        ####################################################
        # Percentage of explained variance
        ####################################################
        ## Method based from Nakagawa and Schielzeth (2012) 
        n <- dim(outcomes)[1]
        
        # Random effects
        sigma2Res <- resStdError^2 # Residual
        varMrand <- rbind(variancesEstimatedR_df, sigma2_res=sigma2Res) # only random effects
        varMrand <- data.matrix(varMrand)
        rownames(varMrand) <- c(effectsNamesUniqueR, "Residuals")
        
        # Fixed effect
        varFixed <- resLmpSS$type3SS/(dim(outcomes)[1])
        
        # all together
        varComp <- varMrand
        colnames(varComp) <- colnames(parameters)
        
        # Percent var explicate by each effect
        varComponentsAbs <- rowSums(varComp)
        varComponentsAbs <- c(varFixed, varComponentsAbs)
        variationPercentages <- (varComponentsAbs*100)/sum(varComponentsAbs[-1])
        names(variationPercentages) <- c(names(resLmpSS$type3SS),rownames(varComp))
        # Remove intercept
        variationPercentages <- variationPercentages[-1]
        
        resLmpSS$variationPercentages <- variationPercentages
        
        resLmpEffectMatrices <- c(resLmpEffectMatrices,varComponentsAbs = list(varComponentsAbs))
      }
      ####
  
      # Plot of the total contribution
      plot <- createVarPercentagesPlot(variationPercentages = resLmpSS$variationPercentages)
  
      resLmpEffectMatrices <- c(resLmpEffectMatrices, resLmpSS,
        varPercentagesPlot = list(plot)
      )
    } else {
      resLmpEffectMatrices <- c(resLmpEffectMatrices,
        type3SS = NA,
        variationPercentages = NA, varPercentagesPlot = NA
      )
    }
    
  return(resLmpEffectMatrices)
}
