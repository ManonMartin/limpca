#' @export UPDATE_lmpBootstrapTests
#' @title Tests the significance of model effects by bootstrap.
#'
#' @description
#' Tests the significance of the effects from the model using bootstrap. This function is based on the outputs of \code{\link{lmpEffectMatrices}}. Tests on combined effects are also provided.
#'
#' @param resLmpEffectMatrices A list of 12 for linear model and 22 for linear mixed model from \code{\link{lmpEffectMatrices}}.
#' @param nboot An integer with the number of bootstrap sample to be drawn.
#' @param nCores The number of cores to use for parallel execution.
#' @param verbose If \code{TRUE}, a message will be displayed with the duration of execution.
#'
#' @return A list with the following elements:
#'  \describe{
#'    \item{\code{f.obs}}{A vector of size F (number of effects in the model) with the F statistics for each model term calculated on the initial data.}
#'    \item{\code{f.boot}}{ b × F matrix with the F statistics calculated on the bootstrap samples.}
#'    \item{\code{p.values}}{A vector of size F with the p-value for each model effect.}
#'    \item{\code{resultsTable}}{A 2 × F matrix with the p-value and the percentage of variance for each model effect.}
#'  }
#'
#' @examples
#' data("UCH")
#' resLmpModelMatrix <- lmpModelMatrix(UCH)
#' resLmpEffectMatrices <- lmpEffectMatrices(resLmpModelMatrix = resLmpModelMatrix)
#'
#' res <- lmpBootstrapTests(
#'   resLmpEffectMatrices = resLmpEffectMatrices,
#'   nboot = 10, nCores = 2, verbose = TRUE
#' )
#'
#' @references
#' Thiel M.,Feraud B. and Govaerts B. (2017) \emph{ASCA+ and APCA+: Extensions of ASCA and APCA
#' in the analysis of unbalanced multifactorial designs}, Journal of Chemometrics
#'
#' Thiel, M., Benaiche, N., Martin, M., Franceschini, S., Van Oirbeek, R., & Govaerts, B. (2023)
#' \emph{limpca: an R package for the linear modeling of high dimensional designed data based on
#' ASCA/APCA family of methods}, Journal of Chemometrics
#' @import doParallel
#' @import parallel
#' @import doFuture
#' @import future
#' @importFrom plyr laply llply
#' @importFrom lmerTest ran
#' @importFrom lme4 lmer

UPDATE_lmpBootstrapTests <- function(resLmpEffectMatrices, nboot = 100, nCores = 2, verbose = FALSE) {
  ############update Antoine######################
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
    
    # Attributing names
    start_time <- Sys.time()
    form <- resLmpEffectMatrices$lmpDataList$formula
    formula <- formula(form)
    outcomes <- resLmpEffectMatrices$lmpDataList$outcomes
    design <- resLmpEffectMatrices$lmpDataList$design
    data_full <- cbind(design,outcomes)
    effectsNamesUnique <- resLmpEffectMatrices$effectsNamesUnique
    effectsNamesUniqueR <- resLmpEffectMatrices$effectsNamesUniqueR
    MM_full <- resLmpEffectMatrices$MM_full
    
    # Parallel computing
    # Add function bootstrapLT on the parallel process.
    # source("./R_new_update/bootstrapLT.R",local = TRUE)
    # source("./R_new_update/createContrastsSum.R",local = TRUE)
    options(doFuture.rng.onMisuse = "ignore")
    doFuture::registerDoFuture()
    future::plan("multisession", workers = nCores)
    
    # The message number with the message (name is the message and number is the value).
    responseWithMsg <- list()
    # The warning number with the warning message (name is the warning message and number is the value).
    responseWithWarning <- list()
    
    # All variable names
    namesAll <- effectsNamesUnique[-1]
    
    # Add "(1 | ...)" for all random variables
    namesAllTransformed <- effectsNamesUnique[-1]
    
    if(length(effectsNamesUniqueR) > 0){
      for(i in (length(effectsNamesUnique)):(length(effectsNamesUnique) - 1 +length(effectsNamesUniqueR))){
        namesAllTransformed[i] <- paste0("(1 | ", effectsNamesUniqueR[i - length(effectsNamesUnique) + 1], ")")
        namesAll[i] <- effectsNamesUniqueR[i - length(effectsNamesUnique) + 1]
      }
    } else {
      stop("It is not a linear mixed model")
    }
    
    # List of Formulas without the effect to test
    null_formulas <- list()
    # List of object for the formula
    null_form_data <- list()
    
    for(i in 1:length(namesAll)){
      effect <- namesAll[i]
      #not the effect
      namesWithoutEffect <- namesAllTransformed[effect != namesAll]
      
      # Only fixed effect
      namesWithoutEffectF <- namesWithoutEffect[namesWithoutEffect %in% effectsNamesUnique]
      
      if(length(namesWithoutEffectF) > 0){
        # fixed matrix
        dataFormF <- lapply(namesWithoutEffectF, function(x){resLmpEffectMatrices$modelMatrixByEffect[[x]]})
        names(dataFormF) <- namesWithoutEffectF
      } else {
        formulaWithoutEffectF = ""
        dataFormF = NULL
      }
      
      # only random effect
      namesWithoutEffectR <- namesWithoutEffect[!(namesWithoutEffect %in% effectsNamesUnique)]
      # randoms vectors 
      formulaWithoutEffectR <- paste0(namesWithoutEffectR ,collapse = " + ")
      
      if(!is.null(dataFormF)){
        # Rename the column of data dataFormF
        dataFormF <- lapply(names(dataFormF), function(matrice_nom) {
          matrice <- dataFormF[[matrice_nom]]
          colnames(matrice) <- gsub(":","",paste(matrice_nom, ".", colnames(matrice), sep = ""))
          return(matrice)
        })
        null_form_data[[effect]] <- cbind(do.call(cbind,dataFormF),design)
        formulaWithoutEffectF <- paste0(colnames(do.call(cbind,dataFormF)) ,collapse = " + ")
        if(formulaWithoutEffectR != ""){
          null_formulas[[effect]] <- paste("~", formulaWithoutEffectF , "+", formulaWithoutEffectR)
        } else{
          null_formulas[[effect]] <-  paste("~", formulaWithoutEffectF)
        }
      }else{
        null_form_data[[effect]] <- design
        null_formulas[[effect]] <- paste("~", formulaWithoutEffectR)
      }
    }
    
    # 1 random effect model is a particular case 
    # because the lmer function doesn't support models without random model and 
    # if the random effect is removed, the model will only have fixed effects.
    if(length(effectsNamesUniqueR) == 1){
      tmp_formula <- null_formulas[length(null_formulas)]
      tmp_form_data <-  null_form_data[length(null_form_data)]
      
      null_formulas <- null_formulas[-length(null_formulas)]
      null_form_data <- null_form_data[-length(null_form_data)]
      
      namesAll <- namesAll[-length(namesAll)]
      # vector with FALSE for fixed effect minus intercept 
      REML <- c(rep(FALSE, length(effectsNamesUnique) - 1))
    } else{
      # vector with FALSE for fixed effect minus intercept and TRUE for random effect
      REML <- c(rep(FALSE, length(effectsNamesUnique) - 1),rep(TRUE,length(effectsNamesUniqueR)))
    }
    names(REML) <- namesAll
    
    ##################################################
    # True log-likelihood Ratio statistics
    ##################################################
    
    # full model: MM_full
    ######################
    # REML
    loglik_full_REML <- sapply(MM_full, logLik, REML=T)
    
    # ML
    loglik_full_ML <- sapply(MM_full, logLik, REML=F)
    
    loglik_full <- matrix(NA, ncol = ncol(outcomes),
                             nrow=length(null_formulas), byrow = TRUE)
    for (i in 1:length(REML)){
      if (REML[i]==TRUE){
        loglik_full[i,] <- loglik_full_REML
      }else {loglik_full[i,] <- loglik_full_ML}
    }
    
    ### Restricted models  
    ######################
    
    # List with lmer without the effect index 
    res.lmer_NULL <- vector("list", length = length(null_formulas))
    names(res.lmer_NULL) <- names(null_formulas)
    
    for (i in 1:length(null_formulas)) {
      # Run lmer
      fmla_tmp <- sapply(paste0(colnames(outcomes), null_formulas[[i]]), as.formula)
      # Allows you to display a specific message error
      res.lmer_NULL[[i]] <- withCallingHandlers({
        res.lmer_NULL[[i]] <- lapply(fmla_tmp, lmer, data = cbind(null_form_data[[i]],outcomes), control = lmerControl(optimizer = "bobyqa"))
      },
      message = function(msg) {
        # Count the same message
        if (is.null(responseWithMsg[[msg$message]])) {
          responseWithMsg[[msg$message]] <<- 1
        } else{
          responseWithMsg[[msg$message]] <<- responseWithMsg[[msg$message]] + 1
        }
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        # Count the same message
        if (is.null(responseWithWarning[[w$message]])) {
          responseWithWarning[[w$message]] <<- 1
        }else{
          responseWithWarning[[w$message]] <<- responseWithWarning[[w$message]] + 1
        }
        invokeRestart("muffleWarning")
      })
    }
    
    # Save the results
    MM_null <- res.lmer_NULL
    
    ######################
    # compute the LRT 
    ######################
    
    # objects initialisation ------------
    Res_std_error_null <- vector("list", length=length(null_formulas))
    loglik_null <- vector("list", length=length(null_formulas))
    sumlog <- c()
    
    ### compute the LRT ----------------------------
    for (i in 1:length(null_formulas)){
      Res_std_error_null[[i]]  <- sapply(MM_null[[i]], sigma)
      
      #  sumlog
      loglik_null[[i]] <- sapply(MM_null[[i]], logLik, REML=REML[i])
      
      sumlog[i] <- 2*(sum(loglik_full[i,] - loglik_null[[i]]))
    }
    
    # LRT
    names(sumlog) <- names(null_formulas)
    sumlog_true <- sumlog
    
    # Integrate the function that fetchs the log-likelihood of each model 
    # for the random effect because the lmer function doesn't work 
    # if there is no random effect. 
    # We use the ran() function from the lmerTest package, 
    # which allows the testing of the random effect
    if(length(effectsNamesUniqueR) == 1){
      lmerand  <- MM_full
      
      for (i in 1:length(lmerand)){
        lmerand[[i]]@call[["data"]] <- data_full
        #specified the contrast sum
        lmerand[[i]]@call[["contrasts"]] <-  createContrastsSum(form)
      }
      
      loglik_null_R <- numeric()
      for (i in 1:length(lmerand)){
        loglik_null_R[i] <- lmerTest::rand(lmerand[[i]])[2,2]
      }
      
      loglik_full_R <- numeric()
      for (i in 1:length(lmerand)){
        loglik_full_R[i] <- lmerTest::rand(lmerand[[i]])[1,2]
      }
      
      sumlogR <- 2*(sum(loglik_full_R - loglik_null_R))
      
      # Add the random variable in the object
      nbr_effect <- length(null_formulas) + 1
      loglik_full <- rbind(loglik_full,loglik_full_R)
      loglik_null[[nbr_effect]] <- loglik_null_R
      null_formulas[[effectsNamesUniqueR[1]]] <- tmp_formula[[1]]
      null_form_data[[effectsNamesUniqueR[1]]] <- tmp_form_data[[1]]
      sumlog_true[effectsNamesUniqueR[1]] <- sumlogR
      namesAll <- names(null_formulas)
      MM_null[[effectsNamesUniqueR[1]]] <- MM_full
      REML[[effectsNamesUniqueR[1]]] <- TRUE
    }
    
    # The list of summed LLR 
    sumlog_boot <- vector("list", length=length(null_formulas))
    # The LLR per response
    ratio_boot <- vector("list", length=length(null_formulas))
    names(sumlog_boot) <- names(ratio_boot) <- namesAll
    
    for (i in 1:length(null_formulas)){
      # Allow to display a specific message error
      res <- withCallingHandlers({
        # Simulate nboot times for each effect, output list with sumlog and ratio
          null_effect <- namesAll[i]
          #print(null_formulas[[null_effect]])
          res <- plyr::laply(1:nboot, function(x){
            bootstrapLT(MM_null = MM_null[[null_effect]], 
                        useREML = REML[null_effect],
                        null_form_data = null_form_data[[null_effect]], 
                        null_formula = null_formulas[[null_effect]],
                        outcomes = outcomes, form_full = form)},
            .parallel = TRUE, .inform = TRUE)
      },
      message = function(msg) {
        # Count the same message
        if (is.null(responseWithMsg[[msg$message]])) {
          responseWithMsg[[msg$message]] <<- 1
        } else{
          responseWithMsg[[msg$message]] <<- responseWithMsg[[msg$message]] + 1
        }
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        # Count the same message
        if(grepl("Model failed to converge",w$message)){
          if (is.null(responseWithWarning[["Model failed to converge"]])){
            responseWithWarning[["Model failed to converge"]] <<- 1
          }else{
            responseWithWarning[["Model failed to converge"]] <<- responseWithWarning[["Model failed to converge"]] + 1
          }
        } else {
          if (is.null(responseWithWarning[[w$message]])) {
            responseWithWarning[[w$message]] <<- 1
          }else{
            responseWithWarning[[w$message]] <<- responseWithWarning[[w$message]] + 1
          }
        }
        invokeRestart("muffleWarning")
      })
      sumlog_boot[[i]] <- res[, "sumlog"]
      sumlog_boot[[i]] <- unlist(sumlog_boot[[i]])
      ratio_boot[[i]] <- res[, "ratio"]
      ratio_boot[[i]] <- do.call(rbind, ratio_boot[[i]])
    }
    
    
    # Calcule P-value
    pval <- c()
    for (i in 1:length(null_formulas)){
      pval[i] <- (sum(sumlog_true[i]<sumlog_boot[[i]])+1)/(nboot+1)
      # For add "<" in the pval at the end of code.
      if(pval[i] <= (1/nboot)){
        pval[i] = 0
      }
    }
    names(pval) <- names(null_formulas)
    
    # Results
    result <- pval
    Fobs <- sumlog_true
    Fboot <- sumlog_boot
    
    # Display messages
    if(length(responseWithMsg) != 0 ){
      for(msg in names(responseWithMsg)){
        warning(paste(msg, "call ", responseWithMsg[[msg]], "times.\n"))
      }
    }
    
    # Display warnings
    if(length(responseWithWarning) != 0 ){
      for(w in names(responseWithWarning)){
          warning("The model estimates for these response(s) may be biased.\n")
          warning(paste(w, "call ", responseWithWarning[[w]], "times.\n"))
      }
    }
    
  } else {
  #################################################
    # Checking the resLmpEffectMatrices list
    checkname <- c(
      "lmpDataList", "modelMatrix", "modelMatrixByEffect",
      "effectsNamesUnique",
      "effectsNamesAll", "effectMatrices",
      "predictedvalues", "residuals", "parameters",
      "type3SS", "variationPercentages", "varPercentagesPlot"
    )
  
    if (length(resLmpEffectMatrices) != 12) {
      stop("List does not contain 12 arguments")
    }
    if (!all(names(resLmpEffectMatrices) == checkname)) {
      stop("Argument is not a resLmpEffectMatrices object")
    }
    if (length(resLmpEffectMatrices$effectMatrices) !=
        length(resLmpEffectMatrices$effectsNamesUnique)) {
      stop("Number of effect matrices differs from the number of effects")
    }
  
    # check if SS = TRUE
  
    if (all(is.na(resLmpEffectMatrices$type3SS)) &
        all(is.na(resLmpEffectMatrices$variationPercentages))) {
      stop("lmpBootstrapTests can't be performed if
           resLmpEffectMatrices doesn't include the effect percentage variations (SS=FALSE)")
    }
    
    # Recreate resLmpModelMatrix
    resLmpModelMatrix <- resLmpEffectMatrices[1:6]
  
    # Attributing names
    start_time <- Sys.time()
    lmpDataList <- resLmpEffectMatrices$lmpDataList
    formula_complete <- resLmpEffectMatrices$lmpDataList$formula
    outcomes <- resLmpEffectMatrices$lmpDataList$outcomes
    modelMatrix <- resLmpEffectMatrices$modelMatrix
    modelMatrixByEffect <- resLmpEffectMatrices$modelMatrixByEffect
    effectsNamesAll <- resLmpEffectMatrices$effectsNamesAll
    effectsNamesUnique <- resLmpEffectMatrices$effectsNamesUnique
    nEffect <- length(effectsNamesUnique)
    SS_complete <- resLmpEffectMatrices$type3SS
    SSE_complete <- resLmpEffectMatrices$type3SS[which(names(SS_complete) == "Residuals")]
    nObs <- nrow(outcomes)
    nParam <- length(effectsNamesAll)
    
    # Parallel computing
    source("./R/lmpModelMatrix.R",local = TRUE)
    source("./R/lmpEffectMatrices.R",local = TRUE)
    source("./R/checkArg.R",local = TRUE)
    source("./R/contrastSS.R", local = TRUE)
    source("./R/lmpSS.R", local = TRUE)
    source("./R/ModelAbbrev.R", local = TRUE)
    doParallel::registerDoParallel(cores = nCores)
  
    #### Estimating the partial model for each effect ####
  
    listResultPartial <- list()
    Fobs <- list()
    Pobs <- list()
  
    partial_mod_fun <- function(iEffect) {
      selection_tmp <- which(effectsNamesAll == effectsNamesUnique[iEffect])
      selectionall <- which(effectsNamesAll == effectsNamesUnique[iEffect])
      selectionComplement_tmp <- which(effectsNamesUnique != effectsNamesUnique[iEffect])
      selectionComplementall <- which(effectsNamesAll != effectsNamesUnique[iEffect])
  
      # Model matrices Partial
  
      modelMatrixPartial <- modelMatrixByEffect[[selectionComplement_tmp[1]]]
      listModelMatrixByEffectPartial_temp <- list()
      listModelMatrixByEffectPartial_temp[[1]] <- modelMatrixByEffect[[selectionComplement_tmp[1]]]
  
      for (i in 2:length(selectionComplement_tmp)) {
        # Create Model Matrix for the partial model
        modelMatrixPartial <- cbind(
          modelMatrixPartial,
          modelMatrixByEffect[[selectionComplement_tmp[i]]]
        )
  
        # Create listModelMatrixByEffectPartial
        listModelMatrixByEffectPartial_temp[[i]] <- modelMatrixByEffect[[selectionComplement_tmp[i]]]
      }
  
      colnames(modelMatrixPartial) <- colnames(modelMatrix[, selectionComplementall])
  
      # Be careful: modelMatrixByEffect with 1 parameter has no colnames
  
      # Create effectsNamesAll
  
      effectsNamesUniquePartial <- effectsNamesUnique[selectionComplement_tmp]
      effectsNamesAllPartial <- effectsNamesAll[selectionComplementall]
      names(listModelMatrixByEffectPartial_temp) <- effectsNamesUniquePartial
  
      # Create the partial formula
  
      temp <- gsub(":", "*", effectsNamesUniquePartial)
      temp <- paste(temp, collapse = "+")
      temp <- paste0("outcomes~", temp)
      formula_temp <- as.formula(temp)
      lmpDataList$formula <- as.formula(temp)
  
      # Create pseudo ResLMModelMatrix
  
      Pseudo_resLmpModelMatrix <- list(
        lmpDataList = lmpDataList,
        modelMatrix = modelMatrixPartial,
        modelMatrixByEffect = listModelMatrixByEffectPartial_temp,
        effectsNamesUnique = effectsNamesUniquePartial,
        effectsNamesAll = effectsNamesAllPartial
      )
  
      # Compute the partial models
  
      listResultPartial <- lmpEffectMatrices(Pseudo_resLmpModelMatrix,
                                             SS = TRUE)
  
      # Compute Fobs
  
      Fobs <- (SS_complete[iEffect] / length(selection_tmp)) /
        (SSE_complete / (nObs - nParam))
  
      return(list(listResultPartial = listResultPartial, Fobs = Fobs))
    }
  
    res_partial_mod_fun <- plyr::llply(1:nEffect, partial_mod_fun,
                                       .parallel = TRUE)
  
  
    listResultPartial <- lapply(res_partial_mod_fun,
                                function(x) x[["listResultPartial"]])
    Fobs <- lapply(res_partial_mod_fun[2:nEffect],
                   function(x) x[["Fobs"]])
  
    # Formating the output
    names(listResultPartial) <- effectsNamesUnique
    Fobs <- unlist(Fobs)
    names(Fobs) <- effectsNamesUnique[2:nEffect]
  
    #### Bootstrap #####
  
    ###  useful functions for ComputeFboot()
  
    # Compute the Sum of Squares Type 3
    # function based on LMSSv2()
    LMSSv2_bis <- function(Res, listcontrast) {
      computeSS_bis <- function(Xmat, L, coef) {
        if (is.vector(L)) {
          L <- t(L)
        }
        LB <- L %*% coef
        BL <- t(LB)
        mat <- BL %*% solve(L %*% solve(t(Xmat) %*% Xmat) %*%
                              t(L)) %*% LB
        SS <- sum(diag(mat))
        return(SS)
      }
  
      L <- listcontrast
  
      Y_withoutIntercept <- Res$outcomes - Res$Intercept
      denom <- norm(x = data.matrix(Y_withoutIntercept), "F")^2
  
      result <- sapply(L, function(x) {
        computeSS_bis(
          Xmat = Res$modelMatrix, L = x,
          Res$parameters
        )
      })
  
      result <- c(result, ((norm(x = Res$residuals, "F")^2) / denom) * 100)
      # result = c(result,norm(x=Res$residuals,"F")^2)
  
      names(result) <- c(Res$effectsNamesUnique, "Residuals")
  
      LMSS <- list(SS = result)
      return(LMSS)
    }
  
    # Compute Fboot from Sum of Squares Type 3
    Fboot_fun <- function(i, result_boot, effect_names, npar, nObs) {
      nume <- result_boot[[i]]$SS[which(names(result_boot[[i]]$SS) == effect_names[i])] /
        npar[[effect_names[i]]]
      denom <- result_boot[[i]]$SS[which(names(result_boot[[i]]$SS) == "Residuals")] /
        (nObs - nParam)
      Fboot <- nume / denom
      return(Fboot)
    }
  
    ### ComputeFboot() function to compute the F statistic for every effect
  
    ComputeFboot <- function(E_sample, listResultPartial,
                             resLmpModelMatrix) {
      effectsNamesAll <- resLmpModelMatrix$effectsNamesAll
      effectsNamesUnique <- resLmpModelMatrix$effectsNamesUnique
      nEffect <- length(effectsNamesUnique)
  
      # prepare  Res_list input argument for LMSSv2_bis --------------
  
      # Y_boot_list (simulated outcomes with partial models) for all the effects
  
      E_boot_list <- plyr::llply(listResultPartial[2:nEffect],
        function(x) x$residuals[E_sample, ],
        .parallel = FALSE
      )
  
      Y_boot_list <- plyr::llply(1:length(E_boot_list),
        function(i) {
          listResultPartial[2:nEffect][[i]]$predictedvalues +
            E_boot_list[[i]]
        },
        .parallel = FALSE
      )
  
      names(Y_boot_list) <- names(E_boot_list)
  
      # # Find the number of parameters for each effect
      # npar <- plyr::llply(resLmpModelMatrix$modelMatrixByEffect, ncol,
      #                     .parallel = FALSE)
  
      # Estimate (X'X)-1X'
  
      X <- resLmpModelMatrix$modelMatrix
      XtX_1Xt <- solve(t(X) %*% X) %*% t(X)
  
      # Compute the parameters
  
      parameters_list <- plyr::llply(Y_boot_list, function(y) XtX_1Xt %*% y,
        .parallel = FALSE
      )
  
  
      # effectMatrices_list and intercept_list
  
      selection <- lapply(
        effectsNamesUnique,
        function(x) which(effectsNamesAll == x)
      )
      names(selection) <- effectsNamesUnique
  
      effectMatrices_list <- plyr::llply(parameters_list, function(y) {
        lapply(selection, function(x) {
          as.matrix(modelMatrix[, x]) %*% y[x, ]
        })
      },
      .parallel = FALSE
      )
  
      Intercept_list <- plyr::llply(effectMatrices_list,
                                    function(x) x[["Intercept"]],
        .parallel = FALSE
      )
  
  
      # residuals
  
      residuals_list <- plyr::llply(1:length(Y_boot_list),
        function(i) {
          Y_boot_list[[i]] -
            Reduce("+", effectMatrices_list[[i]])
        },
        .parallel = FALSE
      )
  
      names(residuals_list) <- names(Y_boot_list)
  
  
      # prepare  Res_list input arg for LMSSv2_bis
  
      Res <- list(
        outcomes = Y_boot_list,
        residuals = residuals_list,
        Intercept = Intercept_list,
        modelMatrix = resLmpModelMatrix$modelMatrix,
        parameters = parameters_list
      )
  
      Res_list <- plyr::llply(1:length(Y_boot_list),
        function(x) {
          list(
            outcomes = Res$outcomes[[x]],
            residuals = Res$residuals[[x]],
            Intercept = Res$Intercept[[x]],
            modelMatrix = Res$modelMatrix,
            parameters = Res$parameters[[x]],
            effectsNamesUnique =
              effectsNamesUnique
          )
        },
        .parallel = FALSE
      )
  
      names(Res_list) <- names(Y_boot_list)
  
      # List of contrasts
  
      listcontrast <- contrastSS(resLmpModelMatrix)
  
  
      ### Compute the Sum of Squares Type 3 -----------
  
      result_boot <- lapply(
        Res_list,
        function(x) {
          LMSSv2_bis(
            Res = x,
            listcontrast = listcontrast
          )
        }
      )
  
      effect_names <- names(result_boot)
  
      ### Compute Fboot from Sum of Squares Type 3 --------------
  
      # Find the number of parameters for each effect
      npar <- plyr::llply(resLmpModelMatrix$modelMatrixByEffect, ncol,
        .parallel = FALSE
      )
  
      Fboot <- plyr::laply(1:length(effect_names),
        function(x) {
          Fboot_fun(
            x, result_boot,
            effect_names, npar, nObs
          )
        },
        .parallel = FALSE
      )
  
      return(Fboot)
    }
  
  
    # sample the observations for bootstrap
  
    E_sample <- lapply(1:nboot, function(x) sample(c(1:nObs),
                                                   nObs, replace = TRUE))
  
    # compute Fboot for simulated data
    Fboot <- plyr::laply(E_sample,
      function(x) {
        ComputeFboot(
          E_sample = x,
          listResultPartial = listResultPartial,
          resLmpModelMatrix = resLmpModelMatrix
        )
      },
      .parallel = TRUE
    )
  
  
    ###### Compute the boostrapped pvalue ######
    result <- vector()
    matrix_temp <- rbind(Fobs, Fboot)
  
    ComputePval <- function(Effect, Fobs) {
      result <- 1 - sum(Effect[1] > Effect[2:(nboot + 1)]) / nboot
      return(result)
    }
  
    # Outputs generation
    result <- apply(X = matrix_temp, FUN = ComputePval, MARGIN = 2)
    colnames(Fboot) <- names(Fobs)
  }
  result <- signif(result, digits = log10(nboot))
  result <- replace(result, result == 0, paste0("< ",
                                                format(1 / nboot, digits = 1,
                                                        scientific = FALSE)))
  
  resultsTable_temp <- rbind(
    result,
    round(resLmpEffectMatrices$variationPercentages[1:length(Fobs)], 2)
  )
  resultsTable <- cbind(resultsTable_temp, c("-",
                                             round(resLmpEffectMatrices$variationPercentages[["Residuals"]], 2)))
  ### update Antoine
  if(model == "lm"){
    rownames(resultsTable) <- c("Bootstrap p-values", "% of variance (T III)")
  } else {
    rownames(resultsTable) <- c("Bootstrap p-values", "% of variance")
  }
  
  if(model == "lmm"){
    colnames(resultsTable) <- c(resLmpEffectMatrices$effectsNamesUnique[-1], 
                                resLmpEffectMatrices$effectsNamesUniqueR,
                                "Residuals")
  }else{
    colnames(resultsTable) <- c(resLmpEffectMatrices$effectsNamesUnique[-1], 
                              "Residuals")
  }
  
  resultsTable <- t(resultsTable)
  resultsTable <- as.data.frame(resultsTable[, c(2, 1)])
  resultsTable[, 1] <- as.numeric(resultsTable[, 1])
  
  resLmpBootstrapTests <- list(f.obs = Fobs, f.boot = Fboot,
                               p.values = result,
                                resultsTable = resultsTable)
  ### update Antoine  
  if(model == "lm"){
    doParallel::stopImplicitCluster
  } else{
    suppressWarnings({
      future::plan("multisession", stop = TRUE)
    })
  }
  ###
  
  if (verbose) {
    print(Sys.time() - start_time)
  }

  return(resLmpBootstrapTests)
}
