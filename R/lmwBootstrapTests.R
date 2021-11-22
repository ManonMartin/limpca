#' @export lmwBootstrapTests
#' @title Performs a test on the effects from the model
#'
#' @description
#' Computes a partial model without the tested effects then estimates the residuals. Next, computes new outcomes from the predicted values of the partial model and sampled residuals. Finally, computes the Sum of Squares and the test statistics.
#'
#' @param resLmwEffectMatrices A list of 11 from \code{\link{lmwEffectMatrices}}
#' @param nboot A integer with the number of iterations to perform (by default: 100)
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
#'  res <- lmwBootstrapTests(resLmwEffectMatrices = resLmwEffectMatrices, nboot=10)
#'
#' @references
#' Thiel M.,Feraud B. and Govaerts B. (2017) \emph{ASCA+ and APCA+: Extensions of ASCA and APCA
#' in the analysis of unbalanced multifactorial designs}, Journal of Chemometrics
#'
#' @import plyr
#' @import doParallel
#' @import parallel



lmwBootstrapTests = function(resLmwEffectMatrices,nboot=100){

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
  design = resLmwEffectMatrices$lmwDataList$design
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

  resLmwModelMatrix = resLmwEffectMatrices[1:5]

  #### Estimating the partial model for each effect ####

  listResultPartial = list()
  Fobs = list()
  Pobs = list()

  for(iEffect in 2:nEffect){
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
    ModelMatrixPartial = cbind(ModelMatrixPartial,ModelMatrixByEffect[[selectionComplement_tmp[i]]])

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

    dataListTemp = list(design=design,
                          outcomes=outcomes,
                          formule=formula_temp)

    # Create pseudo ResLMModelMatrix

    Pseudo_resLmwModelMatrix = list(lmwDataList=dataListTemp,
                                   ModelMatrix=ModelMatrixPartial,
                                   ModelMatrixByEffect=listModelMatrixByEffectPartial_temp,
                                   covariateEffectsNames=covariateEffectsNamesPartial,
                                   covariateEffectsNamesUnique=covariateEffectsNamesUniquePartial)

    # Compute the partial models

    listResultPartial[[iEffect]] = lmwEffectMatrices(Pseudo_resLmwModelMatrix,SS=TRUE)

    # Compute Fobs

    Fobs[[iEffect]] = (SS_complete[iEffect]/length(selection_tmp))/(SSE_complete/(nObs - nParam ))

  }

  # Formating the output
  names(listResultPartial) = covariateEffectsNamesUnique
  Fobs = unlist(Fobs)
  names(Fobs) = covariateEffectsNamesUnique[2:nEffect]

  #### Bootstrap #####

  # Parallel computing to go faster
  doParallel::registerDoParallel(cores=2)

  # Function to compute the F statistic for every effect
  ComputeFboot = function(resLmwObject,resLmwEffectMatrices,nObs,resLmwModelMatrix,nParam,E_sample){

    # Find the tested effect and its number of parameters
    effect = names(resLmwEffectMatrices$effectMatrices)[!(names(resLmwEffectMatrices$effectMatrices)%in%names(resLmwObject$ModelMatrixByEffect))]
    npar <- ncol(resLmwEffectMatrices$ModelMatrixByEffect[[which(names(resLmwEffectMatrices$effectMatrices) == effect)]])

    # Compute the Y_boot value
    E_boot = resLmwObject$residuals[E_sample,]
    Y_boot = resLmwObject$predictedvalues + E_boot

    resLmwModelMatrix$lmwDataList$outcomes = Y_boot

    # Estimate the model then
    result_boot = LMWiRe::lmwEffectMatrices(resLmwModelMatrix = resLmwModelMatrix)
    Fboot = (result_boot$SS[which(names(result_boot$SS)==effect)]/npar)/(result_boot$SS[which(names(result_boot$SS)=="Residuals")]/(nObs - nParam ))
    return(Fboot)
  }

  # Create the matrix of results
  Fboot=matrix(data=NA,nrow=nboot,ncol=(nEffect-1))

  # Loop for every iteration (nboot)
  for(j in 1:nboot){
  E_sample = sample(c(1:nObs),nObs,replace=TRUE)
  Fboot[j,]=  plyr::laply(listResultPartial[2:nEffect],ComputeFboot,E_sample=E_sample,resLmwEffectMatrices=resLmwEffectMatrices,nParam=nParam,nObs=nObs,resLmwModelMatrix=resLmwModelMatrix,.parallel = TRUE)
  }

  # Compute the pvalue
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
