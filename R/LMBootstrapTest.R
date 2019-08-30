#' @export LMBootstrapTest
#' @md
#' @title Perform a test on the effects from the model
#' @description Compute a partial model without the tested effects then estimate the residuals. Next, compute new outcomes from the predicted values of the partial model and sampled residuals. Finally, compute the Sum of Squares and the test statistic.
#' @param ResLMEffectMatrices A list of 13 from \code{\link{LMEffectMatrices}}
#' @param TermToTest A character vector with the term to test and their interactions (in the right order)
#' @param nboot A integer with the number of iterations to perform (by default: 100)
#' @param ncomp A integer with the number of principal component used to compute the Sum of Squares
#'
#' @return A list with the following elements:
#'  \describe{
#'    \item{\code{pvalue}}{A numeric vector with the pvalue for each tested effect}
#'    \item{\code{SSEffect}}{A numeric vector with the SS for the complete model}
#'    \item{\code{SSboot}}{A matrix with the SS of each effect for each iterations }
#'  }
#'
#' @details
#' The function works as follow:
#'    - 1.1 Estimate the partial model H0
#'    - 2.1 Under H_0, estimate the variance of the residuals e*
#'    - 3.1 For nboot iterations,
#'    - 3.1 Simulate new outcomes from the predicted values of H0 and sampled residuals from N(0,e^*)
#'    - 3.2 Estimate the complete model on these new outcomes
#'    - 3.3 Perform a PCA on the Effect Matrices of the tested effects
#'    - 3.4 Compute the SS
#'    - 4.1 Compute the pval
#'
#' The pvalue is computed with the same formula than the permutation test in the paper of Thiel et al.
#'
#' @examples
#'  data('UCH')
#'  ResLMModelMatrix <- LMModelMatrix(formula=as.formula(UCH$formula),design=UCH$design)
#'  ResLMEffectMatrices=LMEffectMatrices(ResLMModelMatrix,outcomes=UCH$outcomes)
#'
#'  TermToTest = c("Hippurate","Hippurate * Citrate","Time * Hippurate","Hippurate * Citrate * Time")
#'  result=LMBootstrapTest(ResLMEffectMatrices = ResLMEffectMatrices,TermToTest = TermToTest,nboot=100)
#'
#'  # Plot Barplot
#'  for(i in 1:length(TermToTest)){
#'    hist(x=result$SSboot[i,],xlim=c(0,max(result$SSEffect[i],result$SSboot[i,])),
#'     breaks=20,main=rownames(result$SSboot)[i],xlab="SST III",freq=FALSE)
#'    points(result$SSEffect[i],0,col="red",pch=19)
#'  }
#'
#' @references
#' Thiel M.,Feraud B. and Govaerts B. (2017) \emph{ASCA+ and APCA+: Extensions of ASCA and APCA
#' in the analysis of unbalanced multifactorial designs}, Journal of Chemometrics
#'



LMBootstrapTest = function(ResLMEffectMatrices,TermToTest,nboot=100,ncomp=4){

  # Checking the ResLMEffectMatrices list

  checkname = c("formula","design","ModelMatrix","outcomes","effectMatrices","modelMatrixByEffect",
                "predictedvalues","residuals","parameters","covariateEffectsNamesUnique","covariateEffectsNames",
                "Type3Residuals","variationPercentages")


  if(!is.list(ResLMEffectMatrices)){stop("Argument ResLMMEffectMatrices is not a list")}
  if(length(ResLMEffectMatrices)!=13){stop("List does not contain 13 arguments")}
  if(!all(names(ResLMEffectMatrices)==checkname)){stop("Argument is not a ResLMEffectMatrices object")}
  if(length(ResLMEffectMatrices$effectMatrices)!=length(ResLMEffectMatrices$covariateEffectsNamesUnique)){stop("Number of effect matrices different from the number of effects")}

  # Checking TermToTest

  starting_time = Sys.time()
  design = ResLMEffectMatrices$design
  formula_complete = ResLMEffectMatrices$formula
  outcomes = ResLMEffectMatrices$outcomes

  temp_formula = as.character(formula_complete)
  split_formula_complete = strsplit(x=temp_formula[3],c("+"),fixed=TRUE)
  trim_formula_complete = stringr::str_trim(unlist(split_formula_complete),side="both")
  PartialFormula = trim_formula_complete
  if(!all(TermToTest %in% trim_formula_complete)){
    stop(paste("TermToTest must be one of the following",paste(trim_formula_complete,collapse=", ")))
  }

  # Remove the TermToTest from formula

  for(i in 1:length(TermToTest)){PartialFormula = PartialFormula[-which(PartialFormula==TermToTest[i])]}
  PartialFormula = paste(PartialFormula,collapse="+")
  PartialFormula = paste(temp_formula[2],PartialFormula,sep="~")
  formula_partial = PartialFormula

  # Create vector with term encoded with ":"

  TermToTest_encoded = attr(terms(formula_complete),"term.labels")
  TermfromFormula = attr(terms(as.formula(formula_partial)),"term.labels")
  for(i in 1:length(TermfromFormula)){TermToTest_encoded=TermToTest_encoded[-which(TermToTest_encoded==TermfromFormula[i])]}

  # Recreate ResLMModelMatrix form ResLMEffect

  ResLMModelMatrix_Complete = list(formula=formula_complete,design=design,ModelMatrix=ResLMEffectMatrices$ModelMatrix)

  # Compute SS for the complete model

  SS_vector = vector()
  for(i in 1:length(TermToTest_encoded)){
    iterm_temp=which(names(ResLMEffectMatrices$effectMatrices)==TermToTest_encoded[i])
    iterm = ResLMEffectMatrices$effectMatrices[[iterm_temp]]
    svdCenteredMatrix = svd(iterm) # Compute the singular-value decomposition
    ScoreMatrix = svdCenteredMatrix$u %*% diag(svdCenteredMatrix$d) # scores
    SS_vector[i] = norm(ScoreMatrix[,1:ncomp],"F")^2
  }

  # Estimate the partial model

  ModelMatrix_Partial = LMModelMatrix(formula_partial,design)
  EffectMatrices_Partial = LMEffectMatrices(ModelMatrix_Partial,outcomes,SS=FALSE)

  # Compute residual from the partial model

  PartialModelResidual = ResLMEffectMatrices$outcomes - EffectMatrices_Partial$predictedvalues
  numb_row = nrow(PartialModelResidual)
  numb_col = ncol(PartialModelResidual)

  # Compute residual variance

  ResidualVariance = apply(PartialModelResidual,2,var)

  # Function "loop" to bootstrap

  bootstrap_func = function(ResLMEffectMatrices,ResidualVariance){

    # Create bootstrapped outcomes

    boot_residuals_vector = rnorm((numb_col*numb_row),mean = 0,sd=ResidualVariance)
    boot_residuals = matrix(data=boot_residuals_vector,nrow=numb_row)
    boot_outcomes = boot_residuals + EffectMatrices_Partial$predictedvalues

    # Estimate complete model

    temp_complete_model = LMEffectMatrices(ResLMModelMatrix_Complete,boot_outcomes,SS=FALSE)

    # Compute SS = (Frobenius Norm)^2

    FrobNorm = function(TermToTest_encoded){
      tempSS = vector()
      iterm_temp=which(names(temp_complete_model$effectMatrices)==TermToTest_encoded)
      iterm = temp_complete_model$effectMatrices[[iterm_temp]]
      svdCenteredMatrix = svd(iterm) # Compute the singular-value decomposition
      scoreMatrix = svdCenteredMatrix$u %*% diag(svdCenteredMatrix$d) # scores
      tempSS = c(tempSS,norm(scoreMatrix[,1:ncomp],"F")^2)
      return(tempSS)
    }

    tempSS = sapply(TermToTest_encoded,FrobNorm)

    return(tempSS)
  }

  # Replicate the function nboot times

  SSboot = replicate(nboot,bootstrap_func(ResLMEffectMatrices=ResLMEffectMatrices,ResidualVariance=ResidualVariance))

  # Compute the statistics

  pvalue = vector()
  for(i in 1:length(SS_vector)){pvalue[i] = sum(SSboot[i,]>=SS_vector[i])/nboot}
  names(pvalue) = TermToTest

  ending_time = Sys.time()
  TimeDiff = ending_time - starting_time
  print(TimeDiff)

  return(list(pvalue=pvalue,SSEffect=SS_vector,SSboot=SSboot))

}








