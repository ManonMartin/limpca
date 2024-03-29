#' @export lmpContributions
#' @title Summary of the contributions of each effect
#'
#' @description
#' Reports the contribution of each effect to the total variance,
#' but also the contribution of each PC to the total variance per effect.
#' These contributions are also summarized in a barplot.
#'
#'
#' @param resLmpPcaEffects A list corresponding to the output value of \code{\link{lmpPcaEffects}}.
#' @param nPC The number of Principal Components to display.
#'
#' @return A list of:
#' \describe{
#' \item{\code{totalContribTable}}{Table of the percentage of contribution of each effect to the total variance.}
#' \item{\code{effectTable}}{Table of the percentage of variance explained by each principal component in each model effect decomposition.}
#' \item{\code{contribTable}}{Table of the percentage of variance explained by each principal component of each effect reported to the percentage contribution of the given effect to the total variance.}
#' \item{\code{combinedEffectTable}}{Equivalent of the \emph{EffectTable} for combined effects.}
#' \item{\code{plotTotal}}{Plot of the ordered contributions of \emph{TotalContribTable}.}
#' \item{\code{plotContrib}}{Plot of the ordered contributions of \emph{ContribTable}.}
#' }
#'
#' @examples
#' data("UCH")
#' resLmpModelMatrix <- lmpModelMatrix(UCH)
#' resLmpEffectMatrices <- lmpEffectMatrices(resLmpModelMatrix)
#' resLmpPcaEffects <- lmpPcaEffects(resLmpEffectMatrices, method = "ASCA-E")
#'
#' lmpContributions(resLmpPcaEffects)
#'
#' @import ggplot2

lmpContributions <- function(resLmpPcaEffects, nPC = 5) {
  if (resLmpPcaEffects$method == "APCA") {
    stop("Trying to compute the contributions based on the APCA method.
        The contribution of each principal component to the total variance
        per effect is only meaningful for ASCA and ASCA-E methods since
        the principal components are then derived from the
        pure effect matrices.")
  }
  neffect <- length(resLmpPcaEffects$effectsNamesUnique)

  # Effect table with the total contribution ===============
  total_contrib_table <- matrix(data = NA, nrow = neffect, ncol = 1)
  rownames(total_contrib_table) <- c(
    names(resLmpPcaEffects)[seq_len(neffect - 1)],
    "Residuals"
  )
  colnames(total_contrib_table) <- "Percentage of Variance"

  total_contrib_table[, 1] <- round(resLmpPcaEffects$variationPercentages, 2)

  # Effect table with the variance of each component ===============
  effect_table <- matrix(data = NA, nrow = neffect, ncol = (nPC + 1))
  rownames(effect_table) <- c(
    names(resLmpPcaEffects)[seq_len(neffect - 1)],
    "Residuals"
  )

  # Colnames
  temp_colnames <- vector()
  for (i in seq_len(nPC)) {
    temp_colnames[i] <- paste0("PC", i)
  }
  temp_colnames <- c(temp_colnames, "Sum")
  colnames(effect_table) <- temp_colnames

  # Filling table
  for (i in seq_len(neffect - 1)) {
    effect_table[i, seq_len(nPC)] <- round(resLmpPcaEffects[[i]]$var[seq_len(nPC)], 2)
  }
  effect_table[neffect, seq_len(nPC)] <- round(resLmpPcaEffects[["Residuals"]]$var[seq_len(nPC)], 2)
  effect_table[, nPC + 1] <- c(rep(0, neffect))
  effect_table[, nPC + 1] <- apply(X = effect_table, MARGIN = 1, sum)


  # Effect table with the contribution of each component
  # to the variance of the effect ===============

  contrib_table <- matrix(data = NA, nrow = neffect, (nPC + 1))
  rownames(contrib_table) <- c(
    names(resLmpPcaEffects)[seq_len(neffect - 1)],
    "Residuals"
  )
  temp_colnames <- c(temp_colnames[seq_len(nPC)], "Contrib")
  colnames(contrib_table) <- temp_colnames

  # Filling table
  for (i in seq_len(neffect)) {
    contrib_table[i, seq_len(nPC)] <- (effect_table[i, seq_len(nPC)] *
      resLmpPcaEffects$variationPercentages[i]) / 100
  }

  contrib_table[, (nPC + 1)] <- resLmpPcaEffects$variationPercentages
  contrib_table <- round(contrib_table, 2)


  # Effect table for combined effects ===============
  if (length(resLmpPcaEffects) - 6 != length(resLmpPcaEffects$effectsNamesUnique)) {
    neffectTot <- length(resLmpPcaEffects) - 6
    neffectComb <- neffectTot - neffect

    combinedEffect_table <- matrix(data = NA, nrow = neffectComb, ncol = (nPC + 1))
    rownames(combinedEffect_table) <- names(resLmpPcaEffects)[seq(neffect, (neffectTot - 1))]
    temp_colnames <- c(temp_colnames[seq_len(nPC)], "Sum")
    colnames(combinedEffect_table) <- temp_colnames

    # Filling table
    resCombined <- resLmpPcaEffects[seq(neffect, (neffectTot - 1))]

    for (i in seq_along(resCombined)) {
      combinedEffect_table[i, seq_len(nPC)] <- round(resCombined[[i]]$var[seq_len(nPC)], 2)
    }
    combinedEffect_table[, nPC + 1] <- c(rep(0, neffectComb))
    combinedEffect_table[, nPC + 1] <- apply(X = combinedEffect_table, MARGIN = 1, sum)
  } else {
    combinedEffect_table <- NULL
  }

  # Plots ===============

  # Plot of the total contribution
  effect_name <- ModelAbbrev(c(
    names(resLmpPcaEffects)[seq_len(neffect - 1)],
    "Residuals"
  ))
  dataTotal <- data.frame(
    effects = effect_name,
    varPercentage = unname(resLmpPcaEffects$variationPercentages)
  )

  plotTotal <- with(dataTotal, {
    ggplot2::ggplot(
      data = dataTotal,
      ggplot2::aes(
        x = stats::reorder(effects, -varPercentage),
        y = varPercentage
      )
    )
  }) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("Effects") +
    ggplot2::ylab("Percentage of Variance") +
    ggplot2::theme_bw()

  # Plot of the contribution of each component to the variance of the effect
  tabname <- matrix(data = NA, nrow = neffect, ncol = nPC)

  for (j in seq_len(nPC)) {
    for (i in seq_len(neffect)) {
      tabname[i, j] <- paste(effect_name[i], colnames(contrib_table)[j],
        sep = "\n"
      )
    }
  }

  effect_vector <- as.vector(contrib_table[, seq_len(nPC)])
  names(effect_vector) <- as.vector(tabname)

  dataContrib <- as.data.frame(effect_vector[seq_len(10)])

  plotContrib <- with(dataContrib, {
    ggplot2::ggplot(
      data = dataContrib,
      ggplot2::aes(
        x = stats::reorder(
          rownames(dataContrib),
          -dataContrib[, 1]
        ),
        y = dataContrib[, 1]
      )
    )
  }) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("Contributions") +
    ggplot2::ylab("Percentage of Variance") +
    ggplot2::scale_x_discrete(limits = rownames(dataContrib)) +
    ggplot2::theme_bw()


  # Output

  if (all(is.na(resLmpPcaEffects$type3SS)) &
    all(is.na(resLmpPcaEffects$variationPercentages))) {
    if (is.null(combinedEffect_table)) {
      resLmpContributions <- list(effectTable = effect_table)
    } else {
      resLmpContributions <- list(
        effectTable = effect_table,
        combinedEffectTable = combinedEffect_table
      )
    }
  } else {
    if (is.null(combinedEffect_table)) {
      resLmpContributions <- list(
        totalContribTable = total_contrib_table,
        effectTable = effect_table,
        contribTable = contrib_table,
        plotTotal = plotTotal,
        plotContrib = plotContrib
      )
    } else {
      resLmpContributions <- list(
        totalContribTable = total_contrib_table,
        effectTable = effect_table,
        contribTable = contrib_table,
        combinedEffectTable = combinedEffect_table,
        plotTotal = plotTotal,
        plotContrib = plotContrib
      )
    }
  }

  return(resLmpContributions)
}
