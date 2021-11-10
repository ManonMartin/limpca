#' @export LMWBootstrapTest
#' @md
#' @title Perform a test on the effects from the model
#' @description Compute a partial model without the tested effects then estimate the residuals. Next, compute new outcomes from the predicted values of the partial model and sampled residuals. Finally, compute the Sum of Squares and the test statistic.
#'
#' @param ResLMWEffectMatrices A list of 11 from \code{\link{LMWEffectMatrices}}
#' @param nboot A integer with the number of iterations to perform (by default: 100)
#'
#' @return A list with the following elements:
#'  \describe{
#'    \item{\code{Fobs}}{A vector with the F statistics observed with the data}
#'    \item{\code{Fboot}}{A matrix with the F statistics observed with the bootstrap}
#'    \item{\code{Pvalues}}{A vector with the pvalue for every effect}
#'  }
#'
#' @details
#' The function works as follow:
#'
#' To be written again
#'
#' @examples
#'  data('UCH')
#'  ResLMWModelMatrix <- LMWModelMatrix(UCH)
#'  ResLMWEffectMatrices <- LMWEffectMatrices(ResLMWModelMatrix = ResLMWModelMatrix)
#'
#'  result <- LMWBootstrapTest(ResLMWEffectMatrices = ResLMWEffectMatrices, nboot=10)
#'
#' @references
#' Thiel M.,Feraud B. and Govaerts B. (2017) \emph{ASCA+ and APCA+: Extensions of ASCA and APCA
#' in the analysis of unbalanced multifactorial designs}, Journal of Chemometrics
#'
#' @import plyr
#' @import doParallel
#' @import parallel



LMWBootstrapTest = function(ResLMWEffectMatrices,nboot=100){

  # Checking the ResLMWEffectMatrices list

  checkname = c("LMWiRe_data_list","ModelMatrix","ModelMatrixByEffect","covariateEffectsNames",
                "covariateEffectsNamesUnique","effectMatrices",
                "predictedvalues","residuals","parameters",
                "SS","variationPercentages")


  if(!is.list(ResLMWEffectMatrices)){stop("Argument ResLMWEffectMatrices is not a list")}
  if(length(ResLMWEffectMatrices)!=11){stop("List does not contain 11 arguments")}
  if(!all(names(ResLMWEffectMatrices)==checkname)){stop("Argument is not a ResLMWEffectMatrices object")}
  if(length(ResLMWEffectMatrices$effectMatrices)!=length(ResLMWEffectMatrices$covariateEffectsNamesUnique)){stop("Number of effect matrices differs from the number of effects")}

  # Attributing names
  start_time = Sys.time()
  design = ResLMWEffectMatrices$LMWiRe_data_list$design
  formula_complete = ResLMWEffectMatrices$LMWiRe_data_list$formula
  outcomes = ResLMWEffectMatrices$LMWiRe_data_list$outcomes
  modelMatrix=ResLMWEffectMatrices$ModelMatrix
  ModelMatrixByEffect = ResLMWEffectMatrices$ModelMatrixByEffect
  covariateEffectsNames = ResLMWEffectMatrices$covariateEffectsNames
  covariateEffectsNamesUnique = ResLMWEffectMatrices$covariateEffectsNamesUnique
  nEffect <- length(covariateEffectsNamesUnique)
  SS_complete = ResLMWEffectMatrices$SS
  SSE_complete = ResLMWEffectMatrices$SS[which(names(SS_complete)=="Residuals")]
  nObs = nrow(outcomes)
  nParam = length(covariateEffectsNames)

  # Recreate ResLMWModelMatrix

  ResLMWModelMatrix = ResLMWEffectMatrices[1:5]

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

    data_list_temp = list(design=design,
                          outcomes=outcomes,
                          formule=formula_temp)

    # Create pseudo ResLMModelMatrix

    Pseudo_ResLMWModelMatrix = list(LMWiRe_data_list=data_list_temp,
                                   ModelMatrix=ModelMatrixPartial,
                                   ModelMatrixByEffect=listModelMatrixByEffectPartial_temp,
                                   covariateEffectsNames=covariateEffectsNamesPartial,
                                   covariateEffectsNamesUnique=covariateEffectsNamesUniquePartial)

    # Compute the partial models

    listResultPartial[[iEffect]] = LMWEffectMatrices(Pseudo_ResLMWModelMatrix,SS=TRUE)

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
  ComputeFboot = function(ResLMWObject,ResLMWEffectMatrices,nObs,ResLMWModelMatrix,nParam,E_sample){

    # Find the tested effect and its number of parameters
    effect = names(ResLMWEffectMatrices$effectMatrices)[!(names(ResLMWEffectMatrices$effectMatrices)%in%names(ResLMWObject$ModelMatrixByEffect))]
    npar <- ncol(ResLMWEffectMatrices$ModelMatrixByEffect[[which(names(ResLMWEffectMatrices$effectMatrices) == effect)]])

    # Compute the Y_boot value
    E_boot = ResLMWObject$residuals[E_sample,]
    Y_boot = ResLMWObject$predictedvalues + E_boot

    ResLMWModelMatrix$LMWiRe_data_list$outcomes = Y_boot

    # Estimate the model then
    result_boot = LMWiRe::LMWEffectMatrices(ResLMWModelMatrix = ResLMWModelMatrix)
    Fboot = (result_boot$SS[which(names(result_boot$SS)==effect)]/npar)/(result_boot$SS[which(names(result_boot$SS)=="Residuals")]/(nObs - nParam ))
    return(Fboot)
  }

  # Create the matrix of results
  Fboot=matrix(data=NA,nrow=nboot,ncol=(nEffect-1))

  # Loop for every iteration (nboot)
  for(j in 1:nboot){
  E_sample = sample(c(1:nObs),nObs,replace=TRUE)
  Fboot[j,]=  plyr::laply(listResultPartial[2:nEffect],ComputeFboot,E_sample=E_sample,ResLMWEffectMatrices=ResLMWEffectMatrices,nParam=nParam,nObs=nObs,ResLMWModelMatrix=ResLMWModelMatrix,.parallel = TRUE)
  }

  # Compute the pvalue
  result = vector()
  matrix_temp = rbind(Fobs,Fboot)

  ComputePval = function(Effect,Fobs){
    result=1-sum(Effect[1]>Effect[2:(nboot+1)])/nboot
    return(result)
  }

  result = apply(X=matrix_temp,FUN = ComputePval,MARGIN = 2)
  colnames(Fboot) = names(Fobs)

  resultsTable = rbind(result, ResLMWEffectMatrices$variationPercentages)
  rownames(resultsTable) = c("p-values", "Variation percentages")

  ResLMWBootstrapTest = list(Fobs=Fobs,Fboot=Fboot,Pvalues = result, ResultsTable = resultsTable)
  doParallel::stopImplicitCluster
  print(Sys.time() - start_time)
  return(ResLMWBootstrapTest)
  }
