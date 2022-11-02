contrastSS = function(resLmwModelMatrix){

  # Attributing name
  design = resLmwModelMatrix$lmwDataList$design
  modelMatrixByEffect = resLmwModelMatrix$modelMatrixByEffect
  effectsNamesUnique = resLmwModelMatrix$effectsNamesUnique
  effectsNamesAll = resLmwModelMatrix$effectsNamesAll
  neffect = length(effectsNamesUnique)
  nparam = length(effectsNamesAll)

  # Creating empy list
  L= list()

  # Filling the list for every effect
  starting_col = 1
  Mat = diag(nparam)

  for(iEffect in 1:neffect){
    selection = which(names(modelMatrixByEffect) == effectsNamesUnique[iEffect])
    MatEffect = modelMatrixByEffect[[selection]]
    npi = ncol(MatEffect)

    L_iEffect = Mat[starting_col:(starting_col+npi-1),]
    L[[iEffect]] = L_iEffect

    starting_col = starting_col+npi
  }
  return(L)
}
