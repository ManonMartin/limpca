contrastSS <- function(resLmpModelMatrix) {
    # Attributing name
    design <- resLmpModelMatrix$LmpDataList$design
    modelMatrixByEffect <- resLmpModelMatrix$modelMatrixByEffect
    effectsNamesUnique <- resLmpModelMatrix$effectsNamesUnique
    effectsNamesAll <- resLmpModelMatrix$effectsNamesAll
    neffect <- length(effectsNamesUnique)
    nparam <- length(effectsNamesAll)

    # Creating empy list
    L <- list()

    # Filling the list for every effect
    starting_col <- 1
    Mat <- diag(nparam)

    for (iEffect in seq_len(neffect)) {
        selection <- which(names(modelMatrixByEffect) == effectsNamesUnique[iEffect])
        MatEffect <- modelMatrixByEffect[[selection]]
        npi <- ncol(MatEffect)

        L_iEffect <- Mat[starting_col:(starting_col + npi - 1), ]
        L[[iEffect]] <- L_iEffect

        starting_col <- starting_col + npi
    }
    names(L) <- effectsNamesUnique
    return(L)
}
