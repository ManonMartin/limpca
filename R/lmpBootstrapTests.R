#' @export lmpBootstrapTests
#' @title Tests the significance of model effects by bootstrap.
#'
#' @description
#' Tests the significance of the effects from the model using bootstrap. This function is based on the outputs of \code{\link{lmpEffectMatrices}}. Tests on combined effects are also provided.
#'
#' @param resLmpEffectMatrices A list of 12 from \code{\link{lmpEffectMatrices}}.
#' @param nboot An integer with the number of bootstrap sample to be drawn.
#' @param nCores The number of cores to use for parallel execution.
#' @param verbose If \code{TRUE}, will display a message with the duration of execution.
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
#'     resLmpEffectMatrices = resLmpEffectMatrices,
#'     nboot = 10, nCores = 2, verbose = TRUE
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
#' @importFrom plyr laply llply

lmpBootstrapTests <- function(resLmpEffectMatrices, nboot = 100, nCores = 2, verbose = FALSE) {
    # Checking the resLmpEffectMatrices list

    checkname <- c(
        "lmpDataList", "modelMatrix", "modelMatrixByEffect",
        "effectsNamesUnique",
        "effectsNamesAll", "effectMatrices",
        "predictedvalues", "residuals", "parameters",
        "type3SS", "variationPercentages", "varPercentagesPlot"
    )


    if (!is.list(resLmpEffectMatrices)) {
        stop("Argument resLmpEffectMatrices is not a list")
    }
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

    # Recreate resLmpModelMatrix

    resLmpModelMatrix <- resLmpEffectMatrices[seq_len(6)]

    # Parallel computing

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

        for (i in seq(2, length(selectionComplement_tmp))) {
            # Create Model Matrix for the partial model
            modelMatrixPartial <- cbind(
                modelMatrixPartial,
                modelMatrixByEffect[[selectionComplement_tmp[i]]]
            )

            # Create listModelMatrixByEffectPartial
            listModelMatrixByEffectPartial_temp[[i]] <- modelMatrixByEffect[[selectionComplement_tmp[i]]]
        }

        colnames(modelMatrixPartial) <- colnames(modelMatrix[, selectionComplementall])

        # Be careful modelMatrixByEffect with 1 parameters have no colnames

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
            SS = TRUE
        )

        # Compute Fobs

        Fobs <- (SS_complete[iEffect] / length(selection_tmp)) /
            (SSE_complete / (nObs - nParam))

        return(list(listResultPartial = listResultPartial, Fobs = Fobs))
    }

    res_partial_mod_fun <- plyr::llply(seq_len(nEffect), partial_mod_fun,
        .parallel = TRUE
    )


    listResultPartial <- lapply(
        res_partial_mod_fun,
        function(x) x[["listResultPartial"]]
    )
    Fobs <- lapply(
        res_partial_mod_fun[seq(2, nEffect)],
        function(x) x[["Fobs"]]
    )

    # Formating the output
    names(listResultPartial) <- effectsNamesUnique
    Fobs <- unlist(Fobs)
    names(Fobs) <- effectsNamesUnique[seq(2, nEffect)]

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

        result <- vapply(L, function(x) {
            computeSS_bis(
                Xmat = Res$modelMatrix, L = x,
                Res$parameters
            )
        }, FUN.VALUE = 0, USE.NAMES = TRUE)

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

        E_boot_list <- plyr::llply(listResultPartial[seq(2, nEffect)],
            function(x) x$residuals[E_sample, ],
            .parallel = FALSE
        )

        Y_boot_list <- plyr::llply(seq_along(E_boot_list),
            function(i) {
                listResultPartial[seq(2, nEffect)][[i]]$predictedvalues +
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

        residuals_list <- plyr::llply(seq_along(Y_boot_list),
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

        Res_list <- plyr::llply(seq_along(Y_boot_list),
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

        Fboot <- plyr::laply(seq_along(effect_names),
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

    E_sample <- lapply(seq_len(nboot), function(x) {
        sample(seq_len(nObs),
            nObs,
            replace = TRUE
        )
    })

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
        result <- 1 - sum(Effect[1] > Effect[seq(2, (nboot + 1))]) / nboot
        return(result)
    }

    # Outputs generation
    result <- apply(X = matrix_temp, FUN = ComputePval, MARGIN = 2)
    result <- signif(result, digits = log10(nboot))
    colnames(Fboot) <- names(Fobs)

    result <- replace(result, result == 0, paste0(
        "< ",
        format(1 / nboot,
            digits = 1,
            scientific = FALSE
        )
    ))

    resultsTable_temp <- rbind(
        result,
        round(resLmpEffectMatrices$variationPercentages[seq_along(Fobs)], 2)
    )
    resultsTable <- cbind(resultsTable_temp, c(
        "-",
        round(resLmpEffectMatrices$variationPercentages[["Residuals"]], 2)
    ))
    rownames(resultsTable) <- c("Bootstrap p-values", "% of variance (T III)")
    colnames(resultsTable) <- c(
        resLmpEffectMatrices$effectsNamesUnique[-1],
        "Residuals"
    )

    resultsTable <- t(resultsTable)
    resultsTable <- as.data.frame(resultsTable[, c(2, 1)])
    resultsTable[, 1] <- as.numeric(resultsTable[, 1])

    resLmpBootstrapTests <- list(
        f.obs = Fobs, f.boot = Fboot,
        p.values = result,
        resultsTable = resultsTable
    )

    doParallel::stopImplicitCluster
    if (verbose) {
        message(Sys.time() - start_time)
    }

    return(resLmpBootstrapTests)
}
