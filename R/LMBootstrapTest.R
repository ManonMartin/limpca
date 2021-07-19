#' @export LMBootstrapTest
#' @md
#' @title Perform a test on the effects from the model
#' @description Compute a partial model without the tested effects then estimate the residuals. Next, compute new outcomes from the predicted values of the partial model and sampled residuals. Finally, compute the Sum of Squares and the test statistic.
#'
#' @param ResLMEffectMatrices A list of 13 from \code{\link{LMEffectMatrices}}
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
#'  ResLMModelMatrix <- LMModelMatrix(formula=as.formula(UCH$formula),design=UCH$design)
#'  ResLMEffectMatrices <- LMEffectMatrices(ResLMModelMatrix = ResLMModelMatrix,outcomes=UCH$outcomes)
#'
#'  result <- LMBootstrapTest(ResLMEffectMatrices = ResLMEffectMatrices,nboot=10)
#'
#' @references
#' Thiel M.,Feraud B. and Govaerts B. (2017) \emph{ASCA+ and APCA+: Extensions of ASCA and APCA
#' in the analysis of unbalanced multifactorial designs}, Journal of Chemometrics
#'
#' @import plyr
#' @import doParallel
#' @import parallel



LMBootstrapTest = function(ResLMEffectMatrices,nboot=100){

  # Checking the ResLMEffectMatrices list

  checkname = c("formula","design","ModelMatrix","ModelMatrixByEffect","covariateEffectsNames","covariateEffectsNamesUnique",
                "outcomes","effectMatrices","predictedvalues","residuals","parameters",
                "SS","variationPercentages")


  if(!is.list(ResLMEffectMatrices)){stop("Argument ResLMMEffectMatrices is not a list")}
  if(length(ResLMEffectMatrices)!=13){stop("List does not contain 13 arguments")}
  if(!all(names(ResLMEffectMatrices)==checkname)){stop("Argument is not a ResLMEffectMatrices object")}
  if(length(ResLMEffectMatrices$effectMatrices)!=length(ResLMEffectMatrices$covariateEffectsNamesUnique)){stop("Number of effect matrices different from the number of effects")}

  # Attributing names
  start_time = Sys.time()
  design = ResLMEffectMatrices$design
  formula_complete = ResLMEffectMatrices$formula
  outcomes = ResLMEffectMatrices$outcomes
  modelMatrix=ResLMEffectMatrices$ModelMatrix
  ModelMatrixByEffect = ResLMEffectMatrices$ModelMatrixByEffect
  covariateEffectsNames = ResLMEffectMatrices$covariateEffectsNames
  covariateEffectsNamesUnique = ResLMEffectMatrices$covariateEffectsNamesUnique
  nEffect <- length(covariateEffectsNamesUnique)
  SS_complete = ResLMEffectMatrices$SS
  SSE_complete = ResLMEffectMatrices$SS[which(names(SS_complete)=="Residuals")]
  nObs = nrow(outcomes)
  nParam = length(covariateEffectsNames)

  # Recreate ResLMModelMatrix

  ResLMModelMatrix = ResLMEffectMatrices[1:6]

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

    # Create pseudo ResLMModelMatrix

    Pseudo_ResLMModelMatrix = list(formula=formula_temp,
                                   design=design,
                                   ModelMatrix=ModelMatrixPartial,
                                   ModelMatrixByEffect=listModelMatrixByEffectPartial_temp,
                                   covariateEffectsNames=covariateEffectsNamesPartial,
                                   covariateEffectsNamesUnique=covariateEffectsNamesUniquePartial)

    # Compute the partial models

    listResultPartial[[iEffect]] = LMEffectMatrices(Pseudo_ResLMModelMatrix,outcomes,SS=TRUE)

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
  ComputeFboot = function(ResLMObject,ResLMEffectMatrices,nObs,ResLMModelMatrix,nParam,E_sample){

    # Find the tested effect and its number of parameters
    effect = names(ResLMEffectMatrices$effectMatrices)[!(names(ResLMEffectMatrices$effectMatrices)%in%names(ResLMObject$ModelMatrixByEffect))]
    npar <- ncol(ResLMEffectMatrices$ModelMatrixByEffect[[which(names(ResLMEffectMatrices$effectMatrices) == effect)]])

    # Compute the Y_boot value
    E_boot = ResLMObject$residuals[E_sample,]
    Y_boot = ResLMObject$predictedvalues + E_boot

    # Estimate the model then
    result_boot = LMWiRe::LMEffectMatrices(ResLMModelMatrix = ResLMModelMatrix,outcomes=Y_boot)
    Fboot = (result_boot$SS[which(names(result_boot$SS)==effect)]/npar)/(result_boot$SS[which(names(result_boot$SS)=="Residuals")]/(nObs - nParam ))
    return(Fboot)
  }

  # Create the matrix of results
  Fboot=matrix(data=NA,nrow=nboot,ncol=(nEffect-1))

  # Loop for every iteration (nboot)
  for(j in 1:nboot){
  E_sample = sample(c(1:nObs),nObs,replace=TRUE)
  Fboot[j,]=  plyr::laply(listResultPartial[2:nEffect],ComputeFboot,E_sample=E_sample,ResLMEffectMatrices=ResLMEffectMatrices,nParam=nParam,nObs=nObs,ResLMModelMatrix=ResLMModelMatrix,.parallel = TRUE)
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

  ResLMBootstrapTest = list(Fobs=Fobs,Fboot=Fboot,Pvalues = result)
  doParallel::stopImplicitCluster
  print(Sys.time() - start_time)
  return(ResLMBootstrapTest)
  }

# LMBootstrapTest(ResLMEffectMatrices,nboot=10)
