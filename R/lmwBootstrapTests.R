#' @export lmwBootstrapTests
#' @title Performs a test on the effects from the model
#'
#' @description
#' Computes a partial model without the tested effects then estimates the residuals. Next, computes new outcomes from the predicted values of the partial model and sampled residuals. Finally, computes the Sum of Squares and the test statistics.
#'
#' @param resLmwEffectMatrices A list of 11 from \code{\link{lmwEffectMatrices}}
#' @param nboot A integer with the number of iterations to perform.
#' @param nCores The number of cores to use for parallel execution.
#'
#' @return A list with the following elements:
#'  \describe{
#'    \item{\code{Fobs}}{A vector with the F statistics observed with the data}
#'    \item{\code{Fboot}}{A matrix with the F statistics observed with the bootstrap}
#'    \item{\code{Pvalues}}{A vector with the pvalue for every effect}
#'    \item{\code{ResultsTable}}{A results matrix with the pvalue and the variation percentage for every effect}
#'  }
#'
#' @details
#' The function works as follow:
#'
#' To be written again
#'
#' @examples
#'  data('UCH')
#'  resLmwModelMatrix <- lmwModelMatrix(UCH)
#'  resLmwEffectMatrices <- lmwEffectMatrices(resLmwModelMatrix = resLmwModelMatrix)
#'
#'  res <- lmwBootstrapTests(resLmwEffectMatrices = resLmwEffectMatrices, nboot=10, nCores=2)
#'
#' @references
#' Thiel M.,Feraud B. and Govaerts B. (2017) \emph{ASCA+ and APCA+: Extensions of ASCA and APCA
#' in the analysis of unbalanced multifactorial designs}, Journal of Chemometrics
#'
#' @import doParallel
#' @import parallel
#' @importFrom plyr laply llply

lmwBootstrapTests = function(resLmwEffectMatrices,nboot=100,nCores=2){

  # Checking the resLmwEffectMatrices list

  checkname = c("lmwDataList","ModelMatrix","ModelMatrixByEffect","covariateEffectsNames",
                "covariateEffectsNamesUnique","effectMatrices",
                "predictedvalues","residuals","parameters",
                "SS","variationPercentages")


  if(!is.list(resLmwEffectMatrices)){stop("Argument resLmwEffectMatrices is not a list")}
  if(length(resLmwEffectMatrices)!=11){stop("List does not contain 11 arguments")}
  if(!all(names(resLmwEffectMatrices)==checkname)){stop("Argument is not a resLmwEffectMatrices object")}
  if(length(resLmwEffectMatrices$effectMatrices)!=length(resLmwEffectMatrices$covariateEffectsNamesUnique)){stop("Number of effect matrices differs from the number of effects")}

  # check if SS = TRUE

  if(all(is.na(resLmwEffectMatrices$SS)) & all(is.na(resLmwEffectMatrices$variationPercentages))){
    stop("lmwBootstrapTests can't be performed if resLmwEffectMatrices doesn't include the effect percentage variations (SS=FALSE)")
  }

  # Attributing names
  start_time = Sys.time()
  lmwDataList <- resLmwEffectMatrices$lmwDataList
  formula_complete = resLmwEffectMatrices$lmwDataList$formula
  outcomes = resLmwEffectMatrices$lmwDataList$outcomes
  modelMatrix = resLmwEffectMatrices$ModelMatrix
  ModelMatrixByEffect = resLmwEffectMatrices$ModelMatrixByEffect
  covariateEffectsNames = resLmwEffectMatrices$covariateEffectsNames
  covariateEffectsNamesUnique = resLmwEffectMatrices$covariateEffectsNamesUnique
  nEffect <- length(covariateEffectsNamesUnique)
  SS_complete = resLmwEffectMatrices$SS
  SSE_complete = resLmwEffectMatrices$SS[which(names(SS_complete)=="Residuals")]
  nObs = nrow(outcomes)
  nParam = length(covariateEffectsNames)

  # Recreate resLmwModelMatrix

  resLmwModelMatrix = resLmwEffectMatrices[1:6]

  # Parallel computing

  doParallel::registerDoParallel(cores=nCores)

  #### Estimating the partial model for each effect ####

  listResultPartial = list()
  Fobs = list()
  Pobs = list()

  partial_mod_fun <- function(iEffect){

    selection_tmp <- which(covariateEffectsNames == covariateEffectsNamesUnique[iEffect])
    selectionall <- which(covariateEffectsNames == covariateEffectsNamesUnique[iEffect])
    selectionComplement_tmp <- which(covariateEffectsNamesUnique != covariateEffectsNamesUnique[iEffect])
    selectionComplementall <- which(covariateEffectsNames != covariateEffectsNamesUnique[iEffect])

    #Model matrices Partial

    ModelMatrixPartial = ModelMatrixByEffect[[selectionComplement_tmp[1]]]
    listModelMatrixByEffectPartial_temp = list()
    listModelMatrixByEffectPartial_temp[[1]] = ModelMatrixByEffect[[selectionComplement_tmp[1]]]

    for(i in 2:length(selectionComplement_tmp)){

      # Create Model Matrix for the partial model
      ModelMatrixPartial = cbind(ModelMatrixPartial,
                                 ModelMatrixByEffect[[selectionComplement_tmp[i]]])

      # Create listModelMatrixByEffectPartial
      listModelMatrixByEffectPartial_temp[[i]] = ModelMatrixByEffect[[selectionComplement_tmp[i]]]
    }

    colnames(ModelMatrixPartial) = colnames(modelMatrix[,selectionComplementall])

    # Be careful ModelMatrixByEffect with 1 parameters have no colnames

    # Create covariateEffectsNames

    covariateEffectsNamesUniquePartial = covariateEffectsNamesUnique[selectionComplement_tmp]
    covariateEffectsNamesPartial = covariateEffectsNames[selectionComplementall]
    names(listModelMatrixByEffectPartial_temp)=covariateEffectsNamesUniquePartial

    # Create the partial formula

    temp=gsub(":","*",covariateEffectsNamesUniquePartial)
    temp = paste(temp,collapse="+")
    temp = paste0("outcomes~",temp)
    formula_temp = as.formula(temp)
    lmwDataList$formula <- as.formula(temp)

    # Create pseudo ResLMModelMatrix

    Pseudo_resLmwModelMatrix = list(lmwDataList=lmwDataList,
                                   ModelMatrix=ModelMatrixPartial,
                                   ModelMatrixByEffect=listModelMatrixByEffectPartial_temp,
                                   covariateEffectsNames=covariateEffectsNamesPartial,
                                   covariateEffectsNamesUnique=covariateEffectsNamesUniquePartial)

    # Compute the partial models

    listResultPartial = lmwEffectMatrices(Pseudo_resLmwModelMatrix,SS=TRUE)

    # Compute Fobs

    Fobs = (SS_complete[iEffect]/length(selection_tmp))/(SSE_complete/(nObs - nParam ))

    return(list(listResultPartial=listResultPartial, Fobs=Fobs))
  }

  res_partial_mod_fun <- plyr::llply(1:nEffect, partial_mod_fun, .parallel = TRUE)


  listResultPartial <- lapply(res_partial_mod_fun, function(x) x[["listResultPartial"]])
  Fobs <- lapply(res_partial_mod_fun[2:nEffect], function(x) x[["Fobs"]])

  # Formating the output
  names(listResultPartial) = covariateEffectsNamesUnique
  Fobs = unlist(Fobs)
  names(Fobs) = covariateEffectsNamesUnique[2:nEffect]

  #### Bootstrap #####

  ###  useful functions for ComputeFboot()

  # Compute the Sum of Squares Type 3
  # function based on LMSSv2()
  LMSSv2_bis <- function(Res,listcontrast){
    computeSS_bis = function(Xmat,L,coef){
      if(is.vector(L)){L=t(L)}
      LB = L %*% coef
      BL = t(LB)
      mat = BL %*% solve(L%*%solve(t(Xmat)%*%Xmat)%*%t(L)) %*% LB
      SS = sum(diag(mat))
      return(SS)
    }

    L = listcontrast

    Y_withoutIntercept = Res$outcomes - Res$Intercept
    denom = norm(x= data.matrix(Y_withoutIntercept),"F")^2

    result <- sapply(L, function(x) computeSS_bis(Xmat=Res$ModelMatrix,L=x,
                                                  Res$parameters))

    result = c(result,((norm(x=Res$residuals,"F")^2)/denom)*100)

    names(result) = c(Res$covariateEffectsNamesUnique,"Residuals")

    LMSS = list(SS=result)
    return(LMSS)
  }

  # Compute Fboot from Sum of Squares Type 3
  Fboot_fun <- function(i,result_boot, effect_names, npar,nObs){
    nume <- result_boot[[i]]$SS[which(names(result_boot[[i]]$SS)==effect_names[i])]/npar[[effect_names[i]]]
    denom <- result_boot[[i]]$SS[which(names(result_boot[[i]]$SS)=="Residuals")]/(nObs - nParam)
    Fboot = nume/denom
    return(Fboot)
  }

  ### ComputeFboot() function to compute the F statistic for every effect

  ComputeFboot <- function(E_sample, listResultPartial,
                           resLmwModelMatrix){

    covariateEffectsNames = resLmwModelMatrix$covariateEffectsNames
    covariateEffectsNamesUnique = resLmwModelMatrix$covariateEffectsNamesUnique
    nEffect <- length(covariateEffectsNamesUnique)

    # prepare  Res_list input argument for LMSSv2_bis --------------

    # Y_boot_list (simulated outcomes with partial models) for all the effects

    E_boot_list <- plyr::llply(listResultPartial[2:nEffect],
                               function(x) x$residuals[E_sample,],
                               .parallel = FALSE)

    Y_boot_list <- plyr::llply(1:length(E_boot_list),
                               function(i) {
                                 listResultPartial[2:nEffect][[i]]$predictedvalues +
                                   E_boot_list[[i]]}, .parallel = FALSE)

    names(Y_boot_list) <- names(E_boot_list)

    # # Find the number of parameters for each effect
    # npar <- plyr::llply(resLmwModelMatrix$ModelMatrixByEffect, ncol,
    #                     .parallel = FALSE)

    # Estimate (X'X)-1X'

    X <- resLmwModelMatrix$ModelMatrix
    XtX_1Xt <- solve(t(X)%*%X)%*%t(X)

    # Compute the parameters

    parameters_list <- plyr::llply(Y_boot_list, function(y) XtX_1Xt %*% y,
                                   .parallel = FALSE)


    # effectMatrices_list and intercept_list

    selection <- lapply(covariateEffectsNamesUnique,
                        function(x) which(covariateEffectsNames == x))
    names(selection) <- covariateEffectsNamesUnique

    effectMatrices_list <- plyr::llply(parameters_list, function(y)
      lapply(selection, function(x)
        as.matrix(modelMatrix[, x]) %*% y[x,]),
      .parallel = FALSE)

    Intercept_list <- plyr::llply(effectMatrices_list, function(x) x[["Intercept"]],
                                  .parallel = FALSE)


    # residuals

    residuals_list <- plyr::llply(1:length(Y_boot_list),
                                  function(i) {
                                    Y_boot_list[[i]] -
                                      Reduce('+', effectMatrices_list[[i]])},
                                  .parallel = FALSE)

    names(residuals_list) <- names(Y_boot_list)


    # prepare  Res_list input arg for LMSSv2_bis

    Res <- list(outcomes = Y_boot_list,
                residuals = residuals_list ,
                Intercept = Intercept_list,
                ModelMatrix = resLmwModelMatrix$ModelMatrix,
                parameters = parameters_list)

    Res_list <- plyr::llply(1:length(Y_boot_list),
                            function(x) list(outcomes = Res$outcomes[[x]],
                                             residuals = Res$residuals[[x]],
                                             Intercept = Res$Intercept[[x]],
                                             ModelMatrix = Res$ModelMatrix,
                                             parameters = Res$parameters[[x]],
                                             covariateEffectsNamesUnique =
                                               covariateEffectsNamesUnique),
                            .parallel = FALSE)

    names(Res_list) <- names(Y_boot_list)

    # List of contrasts

    listcontrast <- contrastSS(resLmwModelMatrix)


    ### Compute the Sum of Squares Type 3 -----------

    result_boot <- lapply(Res_list,
                          function(x) LMSSv2_bis(Res = x,
                                                 listcontrast = listcontrast))

    effect_names <- names(result_boot)

    ### Compute Fboot from Sum of Squares Type 3 --------------

    # Find the number of parameters for each effect
    npar <- plyr::llply(resLmwModelMatrix$ModelMatrixByEffect, ncol,
                        .parallel = FALSE)

    Fboot <- plyr::laply(1:length(effect_names),
                         function(x) Fboot_fun(x, result_boot,
                                               effect_names, npar,nObs),
                         .parallel = FALSE)

    return(Fboot)
  }


  # sample the observations for bootstrap

  E_sample = lapply(1:nboot, function(x) sample(c(1:nObs),nObs,replace=TRUE))

  # compute Fboot for simulated data
  Fboot <- plyr::laply(E_sample,
                       function(x) ComputeFboot(E_sample=x,
                                                listResultPartial=listResultPartial,
                                                resLmwModelMatrix=resLmwModelMatrix),
                       .parallel = TRUE)


  ###### Compute the boostrapped pvalue ######
  result = vector()
  matrix_temp = rbind(Fobs,Fboot)

  ComputePval = function(Effect,Fobs){
    result=1-sum(Effect[1]>Effect[2:(nboot+1)])/nboot
    return(result)
  }

  # Outputs genration
  result = apply(X=matrix_temp,FUN = ComputePval,MARGIN = 2)
  result = signif(result, digits = 2)
  colnames(Fboot) = names(Fobs)

  result <- replace(result, result == 0, paste0("< ", format(1/nboot, digits = 1, scientific = FALSE)))

  resultsTable = rbind(result,
                       round(resLmwEffectMatrices$variationPercentages[1:length(Fobs)], log10(nboot)))
  rownames(resultsTable) = c("Observed p-values", "Variation percentages")

  resLmwBootstrapTests = list(Fobs=Fobs,Fboot=Fboot,Pvalues=result,ResultsTable=resultsTable)

  doParallel::stopImplicitCluster
  print(Sys.time() - start_time)

  return(resLmwBootstrapTests)
  }
