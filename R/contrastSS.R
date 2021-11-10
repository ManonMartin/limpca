contrastSS = function(ResLMWModelMatrix){

  # Checking argument



  # Attributing name

  design = ResLMWModelMatrix$LMWiRe_data_list$design
  ModelMatrixByEffect = ResLMWModelMatrix$ModelMatrixByEffect
  covariateEffectsNamesUnique = ResLMWModelMatrix$covariateEffectsNamesUnique
  covariateEffectsNames = ResLMWModelMatrix$covariateEffectsNames
  neffect = length(covariateEffectsNamesUnique)
  nparam = length(covariateEffectsNames)

  # Creating empy list

  L= list()

  # Filling the list for every effect
  starting_col = 1
  Mat = diag(nparam)

  for(iEffect in 1:neffect){
    selection = which(names(ModelMatrixByEffect) == covariateEffectsNamesUnique[iEffect])
    MatEffect = ModelMatrixByEffect[[selection]]
    npi = ncol(MatEffect)

    L_iEffect = Mat[starting_col:(starting_col+npi-1),]
    L[[iEffect]] = L_iEffect

    starting_col = starting_col+npi
  }
  return(L)
}

# L = contrastSS(ResLMWModelMatrix)
