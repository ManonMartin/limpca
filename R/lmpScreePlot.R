#' @export lmpScreePlot
#' @title Scree Plot
#'
#' @description
#' Provides a barplot of the percentage of variance associated to the PCs of the effect matrices ordered by importance based on the outputs of \code{\link{lmpContributions}}.
#'
#' @param resLmpContributions A resLmpContributions list from the function \code{\link{lmpContributions}}.
#' @param effectNames Names of the effects to be plotted. if `NULL`, all the effects are plotted.
#' @param nPC An integer with the number of components to plot.
#' @param theme `ggplot` theme
#'
#' @return A scree plot (ggplot).
#'
#' @examples
#'
#' data("UCH")
#' resLmpModelMatrix <- lmpModelMatrix(UCH)
#' resLmpEffectMatrices <- lmpEffectMatrices(resLmpModelMatrix)
#' resASCAE <- lmpPcaEffects(resLmpEffectMatrices, method = "ASCA-E")
#' resLmpContributions <- lmpContributions(resASCAE)
#' lmpScreePlot(resLmpContributions, effectNames = "Hippurate:Citrate", nPC = 4)
#'
#' @import ggplot2

lmpScreePlot <- function(resLmpContributions, effectNames = NULL,
                         nPC = 5, theme = theme_bw()) {
    # checks ===================
    checkArg(resLmpContributions, c("list"), can.be.null = FALSE)
    checkArg(effectNames, c("str"), can.be.null = TRUE)
    checkArg(nPC, c("num", "pos"), can.be.null = FALSE)

    effectTable <- resLmpContributions$effectTable
    if (!nPC < ncol(effectTable)) {
        stop("nPC must be inferior or equal to the nPC chosen
         in lmpContributions")
    }

    if (is.null(effectNames)) {
        effectNames <- c(rownames(resLmpContributions$effectTable))
    }

    if (!all(effectNames %in% rownames(resLmpContributions$effectTable))) {
        stop("One of the effects from effectNames is not
         in resLmpContributions.")
    }

    # selecting the effect

    resdf <- as.data.frame(t(effectTable[, c(seq_len(nPC))]))

    buildFig <- function(x) {
        iEffect <- which(colnames(resdf) == effectNames[x])

        # Plotting the effect
        ggplot(
            data = resdf,
            aes(x = rownames(resdf), y = resdf[, iEffect])
        ) +
            geom_bar(stat = "identity") +
            xlab("Principal Components") +
            ylab("Variance Percentage") +
            ggtitle(paste(
                "Percentage of variance by PC for:",
                effectNames[x]
            )) +
            theme
    }

    fig <- lapply(c(seq_along(effectNames)), FUN = buildFig)
    names(fig) <- effectNames

    return(fig)
}
