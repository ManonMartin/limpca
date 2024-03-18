#' @export lmpLoading2dPlot
#' @title Loading plots on a 2D scatter plot
#'
#' @description
#' Draws a 2D loading plot of each effect matrix provided in \code{\link{lmpPcaEffects}} outputs. As a wrapper of the \code{\link{plotScatter}} function, it allows the visualization of effect loading matrices for two components at a time with all options available in \code{\link{plotScatter}}.
#'
#' @param resLmpPcaEffects A list corresponding to the output value of \code{\link{lmpPcaEffects}}.
#' @param effectNames Names of the effects to be plotted. If `NULL`, all the effects are plotted.
#' @param axes A numerical vector with the 2 Principal Components axes to be drawn.
#' @param addRownames Boolean indicating if the labels should be plotted. By default, uses the column names of the outcome matrix but it can be manually specified with the `points_labs` argument from \code{\link{plotScatter}}.
#' @param pl_n The number of labels that should be plotted, based on the distance measure \eqn{d} (*see* Details).
#' @param metadata A nxk "free encoded" data.frame corresponding to `design` in \code{\link{plotScatter}}.
#' @param drawOrigin if \code{TRUE}, draws horizontal and vertical intercepts at (0,0) based on the \code{\link{plotScatter}} function.
#' @param ... Additional arguments to be passed to \code{\link{plotScatter}}.
#'
#' @return A list of loading plots (ggplot).
#'
#' @details
#' `lmpLoading2dPlot` is a wrapper of \code{\link{plotScatter}}. See `?plotScatter` for more information on the additional arguments.
#'
#'
#' The distance measure \eqn{d}{d} that is used to rank the variables is based on the following formula:
#' \deqn{d = \sqrt(P_{ab}^2*\lambda_{ab}^2)}{d = sqrt(P_ab^2 * lambda_ab^2)} where \eqn{a}{a}
#' and \eqn{b}{b} are two selected Principal Components, \eqn{P_{ab}}{P_ab} represents their
#' loadings and \eqn{\lambda_{ab}}{lambda_ab} their singular values.
#'
#' @examples
#'
#' data("UCH")
#' resLmpModelMatrix <- lmpModelMatrix(UCH)
#' resLmpEffectMatrices <- lmpEffectMatrices(resLmpModelMatrix)
#' resASCA <- lmpPcaEffects(resLmpEffectMatrices)
#'
#' lmpLoading2dPlot(resASCA, effectNames = "Hippurate")
#'
#' # adding color, shape and labels to points
#' id_hip <- c(seq(126, 156), seq(362, 375))
#' peaks <- rep("other", ncol(UCH$outcomes))
#' peaks[id_hip] <- "hip"
#' metadata <- data.frame(peaks)
#'
#' lmpLoading2dPlot(resASCA,
#'     effectNames = "Hippurate",
#'     metadata = metadata, addRownames = TRUE, color = "peaks",
#'     shape = "peaks"
#' )
#'
#' # changing max.overlaps of ggrepel
#' options(ggrepel.max.overlaps = 30)
#' lmpLoading2dPlot(resASCA,
#'     effectNames = "Hippurate",
#'     metadata = metadata, addRownames = TRUE, color = "peaks",
#'     shape = "peaks", pl_n = 20
#' )
#'
#' @importFrom methods hasArg


lmpLoading2dPlot <- function(resLmpPcaEffects,
                             effectNames = NULL,
                             axes = c(1, 2),
                             addRownames = FALSE,
                             pl_n = 10,
                             metadata = NULL,
                             drawOrigin = TRUE,
                             ...) {
    mcall <- as.list(match.call())[-1L]

    # checks ===================
    checkArg(resLmpPcaEffects, c("list"), can.be.null = FALSE)
    checkArg(effectNames, c("str"), can.be.null = TRUE)
    checkArg(axes, c("int", "pos"), can.be.null = FALSE)
    checkArg(metadata, "data.frame", can.be.null = TRUE)

    if (!all(effectNames %in% names(resLmpPcaEffects))) {
        stop("One of the effects from effectNames is not in resLmpPcaEffects.")
    }

    if (!identical(
        names(resLmpPcaEffects[seq((length(resLmpPcaEffects) - 7), length(resLmpPcaEffects))]),
        c(
            "Residuals", "lmpDataList", "effectsNamesUnique",
            "effectsNamesUniqueCombined",
            "method", "type3SS", "variationPercentages",
            "combineEffects"
        )
    )) {
        stop("resLmpPcaEffects is not an output value of lmpPcaEffects")
    }


    if (is.null(effectNames)) {
        effectNames <- resLmpPcaEffects$effectsNamesUniqueCombined
        effectNames <- effectNames[effectNames != "Intercept"]
        effectNames <- c(effectNames, "Residuals")
    }


    # loadings   ===================

    loadings <- lapply(effectNames, function(x) {
        resLmpPcaEffects[[x]][["loadings"]]
    })
    names(loadings) <- effectNames

    if (length(axes) != 2) {
        stop("axes is not of length 2")
    }

    if (max(axes) > ncol(loadings[[effectNames[1]]])) {
        stop(
            "axes (", paste(axes, collapse = ","),
            ") is beyond the ncol of loadings (", ncol(loadings), ")"
        )
    }



    # percentage of explained variance   ===================

    pc_var_fun <- function(effect) {
        pc_var <- resLmpPcaEffects[[effect]][["var"]]
        pc_var_x <- format(pc_var[pc_var >= 0.1],
            digits = 2, trim = TRUE
        )
        pc_var_y <- format(pc_var[pc_var < 0.1],
            digits = 2,
            scientific = TRUE, trim = TRUE
        )
        pc_var_char <- as.character(pc_var)
        pc_var_char[pc_var >= 0.1] <- pc_var_x
        pc_var_char[pc_var < 0.1] <- pc_var_y

        pc_var_char <- paste0(
            "PC", axes, " (",
            pc_var_char[axes], "%)"
        )

        return(pc_var_char)
    }

    pc_axes <- lapply(effectNames, pc_var_fun)
    names(pc_axes) <- effectNames


    # distance measure and labels  ===================

    if (methods::hasArg("points_labs")) {
        addRownames <- FALSE
    } else {
        labs <- colnames(resLmpPcaEffects$lmpDataList$outcomes)
        dist_labels_fun <- function(effect, axes, labs) {
            load <- resLmpPcaEffects[[effect]][["loadings"]][, axes]
            singvar <- resLmpPcaEffects[[effect]][["singvar"]][axes]
            dista <- load^2 %*% singvar^2
            ids <- order(dista, decreasing = TRUE)[seq_len(pl_n)]
            labs[-ids] <- ""
            labs
        }

        points_labels <- lapply(effectNames, dist_labels_fun,
            axes = axes, labs = labs
        )

        names(points_labels) <- effectNames
    }





    # graphical parameters   ===================

    buildFig <- function(effect) {
        title <- paste(
            effect, ":", resLmpPcaEffects$method,
            "loading plot"
        )

        xlab_pc <- pc_axes[[effect]][1]
        ylab_pc <- pc_axes[[effect]][2]

        xlim_val <- c(
            1.4 * min(resLmpPcaEffects[[effect]][["loadings"]][, axes[1]]),
            1.4 * max(resLmpPcaEffects[[effect]][["loadings"]][, axes[1]])
        )

        # Checking the second component
        if (resLmpPcaEffects$method != "APCA") {
            if (resLmpPcaEffects[[effect]][["var"]][axes[2]] < 1) {
                warning("The variance of PC2 is inferior to 1%. Graph scaled")
                ylim_val <- c(
                    100 * min(resLmpPcaEffects[[effect]][["loadings"]][, axes[2]]),
                    100 * max(resLmpPcaEffects[[effect]][["loadings"]][, axes[2]])
                )
            } else {
                ylim_val <- c(
                    1.4 * min(resLmpPcaEffects[[effect]][["loadings"]][, axes[2]]),
                    1.4 * max(resLmpPcaEffects[[effect]][["loadings"]][, axes[2]])
                )
            }
        } else {
            ylim_val <- c(
                1.4 * min(resLmpPcaEffects[[effect]][["loadings"]][, axes[2]]),
                1.4 * max(resLmpPcaEffects[[effect]][["loadings"]][, axes[2]])
            )
        }

        # Building plots

        if (addRownames) {
            if (!"xlab" %in% names(mcall)) {
                if (!"ylab" %in% names(mcall)) {
                    fig <- plotScatter(
                        Y = loadings[[effect]], xy = axes,
                        xlab = xlab_pc, ylab = ylab_pc,
                        design = metadata,
                        points_labs = points_labels[[effect]],
                        drawOrigin = drawOrigin,
                        ...
                    )
                } else {
                    fig <- plotScatter(
                        Y = loadings[[effect]], xy = axes,
                        xlab = xlab_pc,
                        design = metadata,
                        points_labs = points_labels[[effect]],
                        drawOrigin = drawOrigin,
                        ...
                    )
                }
            } else {
                if (!"ylab" %in% names(mcall)) {
                    fig <- plotScatter(
                        Y = loadings[[effect]], xy = axes,
                        ylab = ylab_pc,
                        design = metadata,
                        points_labs = points_labels[[effect]],
                        drawOrigin = drawOrigin,
                        ...
                    )
                } else {
                    fig <- plotScatter(
                        Y = loadings[[effect]], xy = axes,
                        design = metadata,
                        points_labs = points_labels[[effect]],
                        drawOrigin = drawOrigin,
                        ...
                    )
                }
            }
        } else {
            if (!"xlab" %in% names(mcall)) {
                if (!"ylab" %in% names(mcall)) {
                    fig <- plotScatter(
                        Y = loadings[[effect]], xy = axes,
                        xlab = xlab_pc, ylab = ylab_pc,
                        design = metadata,
                        drawOrigin = drawOrigin, ...
                    )
                } else {
                    fig <- plotScatter(
                        Y = loadings[[effect]], xy = axes,
                        xlab = xlab_pc,
                        design = metadata,
                        drawOrigin = drawOrigin, ...
                    )
                }
            } else {
                if (!"ylab" %in% names(mcall)) {
                    fig <- plotScatter(
                        Y = loadings[[effect]], xy = axes,
                        ylab = ylab_pc,
                        design = metadata,
                        drawOrigin = drawOrigin, ...
                    )
                } else {
                    fig <- plotScatter(
                        Y = loadings[[effect]], xy = axes,
                        design = metadata,
                        drawOrigin = drawOrigin, ...
                    )
                }
            }
        }


        fig <- fig + ylim(ylim_val) + xlim(xlim_val) +
            ggtitle(title)
    }

    # loadings plot  ===================

    fig <- lapply(effectNames, buildFig)
    names(fig) <- effectNames

    if (length(fig) == 1) {
        fig <- fig[[1]]
    }


    return(fig)
}
