#' @export lmpEffectPlot
#' @title Effect plot
#'
#' @description
#' Plots the ASCA scores by effect levels for a given model effect and for one PC at a time.
#' This graph is especially appealing to interpret interactions or combined effects.
#' It is a wrapper of plotMeans.
#'
#' @param resASCA A list corresponding to the ASCA output value of \code{\link{lmpPcaEffects}}.
#' @param effectName Name of the effect to be used to plot the scores.
#' @param axes A numerical vector with the Principal Components axes to be drawn.
#' @param x A character string giving the `design` factor whose levels will form the x axis.
#' @param z A character string giving the `design` factor whose levels will form the traces.
#' @param w A character string giving the `design` factor whose levels will be used for the facet.
#' @param hline If not \code{NULL}, draws one or several horizontal line(s) at values given in `hline`.
#' @param ... Additional arguments to be passed to \code{\link{plotMeans}}.
#'
#' @return An effect plot (ggplot).
#'
#' @details
#' `lmpEffectPlot` is a wrapper of \code{\link{plotMeans}}.
#'
#' @examples
#'
#' data("UCH")
#' resLmpModelMatrix <- lmpModelMatrix(UCH)
#' resLmpEffectMatrices <- lmpEffectMatrices(resLmpModelMatrix)
#' resASCA <- lmpPcaEffects(
#'   resLmpEffectMatrices = resLmpEffectMatrices,
#'   method = "ASCA", combineEffects = list(c("Hippurate", "Time", "Hippurate:Time"))
#' )
#'
#' # Effect plot for an interaction effect
#' lmpEffectPlot(resASCA, effectName = "Hippurate:Time", x = "Hippurate", z = "Time")
#' # Effect plot for a combined effect
#' lmpEffectPlot(resASCA, effectName = "Hippurate+Time+Hippurate:Time", x = "Hippurate", z = "Time")

#'
#' @import ggplot2

lmpEffectPlot <- function(resASCA, effectName, axes = 1,
                          x, z = NULL, w = NULL, hline = 0, ...) {
  mcall <- as.list(match.call())[-1L]

  # checks =========================

  checkArg(resASCA, c("list"), can.be.null = FALSE)
  checkArg(effectName, c("str", "length1"), can.be.null = FALSE)
  checkArg(axes, c("int", "pos"), can.be.null = FALSE)
  checkArg(x, c("str", "length1"), can.be.null = FALSE)
  checkArg(z, c("str", "length1"), can.be.null = TRUE)
  checkArg(w, c("str", "length1"), can.be.null = TRUE)
  checkArg(hline, "num", can.be.null = TRUE)

  if (!identical(
    names(resASCA[(length(resASCA) - 7):length(resASCA)]),
    c(
      "Residuals", "lmpDataList", "effectsNamesUnique",
      "effectsNamesUniqueCombined", "method",
      "type3SS", "variationPercentages", "combineEffects"
    )
  )) {
    stop("resLmpPcaEffects is not an output value of lmpPcaEffects")
  }

  if (resASCA$method != "ASCA") {
    stop("resASCA does not correspond to an ASCA result
         of LmpEffectMatrices")
  }

  if (!effectName %in% names(resASCA)) {
    stop(effectName, " is not an effect of resASCA")
  }

  if (max(axes) > ncol(resASCA[[effectName]][["scores"]])) {
    stop(
      "PC (", paste(axes, collapse = ","),
      ") is beyond the number of PC of scores (",
      ncol(resASCA[[effectName]][["scores"]]), ")"
    )
  }

  if (str_detect(string = effectName, pattern = "[+]")) {
    numComb <- str_split(effectName, "[+]")
    numEffects <- unlist(str_split(numComb[[1]], ":"))
  } else {
    numEffects <- unlist(str_split(effectName, ":"))
  }

  if (!x %in% numEffects) {
    stop("x should appear in effectName")
  }
  if (!is.null(z) && !z %in% numEffects) {
    stop("z should appear in effectName")
  }
  if (!is.null(w) && !w %in% numEffects) {
    stop("w should appear in effectName")
  }

  # prepare the arguments  ==============================
  matEffect <- resASCA[[effectName]][["scores"]]

  title <- paste0(effectName, " scores as a function of ", x, ": PC", axes)
  ylab <- paste0(
    "Scores (", round(resASCA[[effectName]][["var"]][axes], 2),
    "% of variation explained)"
  )

  if (!"title" %in% names(mcall)) {
    if (!"ylab" %in% names(mcall)) {
      fig <- plotMeans(
        Y = matEffect,
        design = resASCA$lmpDataList$design,
        cols = axes,
        x = x,
        z = z,
        w = w,
        title = title,
        ylab = ylab,
        hline = hline,
        ...
      )
    } else {
      fig <- plotMeans(
        Y = matEffect,
        design = resASCA$lmpDataList$design,
        cols = axes,
        x = x,
        z = z,
        w = w,
        title = title,
        hline = hline,
        ...
      )
    }
  } else {
    if (!"ylab" %in% names(mcall)) {
      fig <- plotMeans(
        Y = matEffect,
        design = resASCA$lmpDataList$design,
        cols = axes,
        x = x,
        z = z,
        w = w,
        ylab = ylab,
        hline = hline,
        ...
      )
    } else {
      fig <- plotMeans(
        Y = matEffect,
        design = resASCA$lmpDataList$design,
        cols = axes,
        x = x,
        z = z,
        w = w,
        hline = hline,
        ...
      )
    }
  }

  if (length(fig) == 1) {
    fig <- fig[[1]]
  }


  return(fig)
}
