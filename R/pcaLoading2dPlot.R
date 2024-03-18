#' @export pcaLoading2dPlot
#' @title Loading plots on a 2D scatter plot
#'
#' @description
#' Produces 2D loading plots from \code{\link{pcaBySvd}} with the same graphical options as \code{\link{plotScatter}} as this is a wrapper of this function.
#'
#' @param resPcaBySvd A list corresponding to the output value of \code{\link{pcaBySvd}}.
#' @param axes A numerical vector of length 2 with the Principal Components axes to be drawn.
#' @param title Plot title.
#' @param addRownames Boolean indicating if the labels should be plotted. By default, uses the row names of the loadings matrix but it can be manually specified with the `points_labs` argument from \code{\link{plotScatter}}.
#' @param pl_n The number of labels that should be plotted, based on a distance measure (see Details).
#' @param metadata A \eqn{n \times k} "freely encoded" data.frame corresponding to the `design` argument in \code{\link{plotScatter}}.
#' @param drawOrigin if \code{TRUE}, draws horizontal and vertical intercepts at (0,0) based on the \code{\link{plotScatter}} function.
#' @param ... Additional arguments to be passed to \code{\link{plotScatter}}.
#'
#' @return A `ggplot2` object with the PCA loading plot.
#'
#' @details
#' `pcaLoading2dPlot` is a wrapper of \code{\link{plotScatter}}. See `?plotScatter` for more information on the additional arguments.
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
#' ResPCA <- pcaBySvd(UCH$outcomes)
#'
#' pcaLoading2dPlot(
#'     resPcaBySvd = ResPCA, axes = c(1, 2),
#'     title = "PCA loading plot UCH"
#' )
#'
#' # adding color,  shape and labels to points
#' id_cit <- seq(446, 459)
#' id_hip <- c(seq(126, 156), seq(362, 375))
#' peaks <- rep("other", ncol(UCH$outcomes))
#' peaks[id_hip] <- "hip"
#' peaks[id_cit] <- "cit"
#' metadata <- data.frame(peaks)
#'
#' pcaLoading2dPlot(
#'     resPcaBySvd = ResPCA, axes = c(1, 2),
#'     title = "PCA loading plot UCH", metadata = metadata,
#'     color = "peaks", shape = "peaks", addRownames = TRUE
#' )
#'
#' # changing max.overlaps of ggrepel
#' options(ggrepel.max.overlaps = 30)
#' pcaLoading2dPlot(
#'     resPcaBySvd = ResPCA, axes = c(1, 2),
#'     title = "PCA loading plot UCH", metadata = metadata,
#'     color = "peaks", shape = "peaks", addRownames = TRUE,
#'     pl_n = 35
#' )
#'
#' @import ggplot2

pcaLoading2dPlot <- function(resPcaBySvd,
                             axes = c(1, 2),
                             title = "PCA loading plot",
                             addRownames = FALSE,
                             pl_n = 10,
                             metadata = NULL,
                             drawOrigin = TRUE,
                             ...) {
    mcall <- as.list(match.call())[-1L]

    # checks ===================
    checkArg(resPcaBySvd, c("list"), can.be.null = FALSE)
    checkArg(axes, c("int", "pos"), can.be.null = FALSE)
    checkArg(title, c("str", "length1"), can.be.null = FALSE)
    checkArg(addRownames, c("bool"), can.be.null = FALSE)
    checkArg(pl_n, c("int", "pos", "length1"), can.be.null = FALSE)
    checkArg(metadata, "data.frame", can.be.null = TRUE)

    if (!identical(names(resPcaBySvd), c(
        "scores", "loadings", "eigval", "singvar",
        "var", "cumvar", "original.dataset"
    ))) {
        stop("resPcaBySvd is not an output value of pcaBySvd")
    }

    # loadings   ===================
    loadings <- resPcaBySvd$loadings
    checkArg(loadings, c("matrix"), can.be.null = FALSE)

    if (length(axes) != 2) {
        stop("axes is not of length 2")
    }

    if (max(axes) > ncol(loadings)) {
        stop(
            "axes (", paste(axes, collapse = ","),
            ") is beyond the ncol of loadings (", ncol(loadings), ")"
        )
    }


    # percentage of explained variance   ===================
    pc_var <- resPcaBySvd$var
    pc_var_x <- format(pc_var[pc_var >= 0.1], digits = 2, trim = TRUE)
    pc_var_y <- format(pc_var[pc_var < 0.1],
        digits = 2,
        scientific = TRUE, trim = TRUE
    )
    pc_var_char <- as.character(pc_var)
    pc_var_char[pc_var >= 0.1] <- pc_var_x
    pc_var_char[pc_var < 0.1] <- pc_var_y

    pc_var_char <- paste0("PC", axes, " (", pc_var_char[axes], "%)")

    # distance measure and labels  ===================

    load <- resPcaBySvd$loadings[, axes]
    singvar <- resPcaBySvd$singvar[axes]

    dista <- load^2 %*% singvar^2

    if (methods::hasArg("points_labs")) {
        addRownames <- FALSE
    } else {
        pl_n <- min(ncol(loadings), pl_n)
        points_labels <- rownames(loadings)
        ids <- order(dista, decreasing = TRUE)[seq_len(pl_n)]
        points_labels[-ids] <- ""
    }

    # graphical parameters   ===================
    xlab <- pc_var_char[1]
    ylab <- pc_var_char[2]

    xlim1 <- max(abs(loadings[, axes[1]]))
    xlim_val <- c(-xlim1, xlim1)

    ylim1 <- max(abs(loadings[, axes[2]]))
    ylim_val <- c(-ylim1, ylim1)

    if (addRownames) {
        if (!"xlab" %in% names(mcall)) {
            if (!"ylab" %in% names(mcall)) {
                fig <- plotScatter(
                    Y = loadings, title = title,
                    xy = axes, xlab = xlab, ylab = ylab,
                    points_labs = points_labels,
                    design = metadata,
                    drawOrigin = drawOrigin,
                    ...
                )
            } else {
                fig <- plotScatter(
                    Y = loadings, title = title,
                    xy = axes, xlab = xlab,
                    points_labs = points_labels,
                    design = metadata,
                    drawOrigin = drawOrigin,
                    ...
                )
            }
        } else {
            if (!"ylab" %in% names(mcall)) {
                fig <- plotScatter(
                    Y = loadings, title = title,
                    xy = axes, ylab = ylab,
                    points_labs = points_labels,
                    design = metadata,
                    drawOrigin = drawOrigin,
                    ...
                )
            } else {
                fig <- plotScatter(
                    Y = loadings, title = title,
                    xy = axes,
                    points_labs = points_labels,
                    design = metadata,
                    drawOrigin = drawOrigin,
                    ...
                )
            }
        }
    } else {
        if (!"xlab" %in% names(mcall)) {
            if (!"ylab" %in% names(mcall)) {
                fig <- plotScatter(
                    Y = loadings, title = title,
                    xy = axes, xlab = xlab, ylab = ylab,
                    design = metadata,
                    drawOrigin = drawOrigin, ...
                )
            } else {
                fig <- plotScatter(
                    Y = loadings, title = title,
                    xy = axes, xlab = xlab,
                    design = metadata,
                    drawOrigin = drawOrigin, ...
                )
            }
        } else {
            if (!"ylab" %in% names(mcall)) {
                fig <- plotScatter(
                    Y = loadings, title = title,
                    xy = axes, ylab = ylab,
                    design = metadata,
                    drawOrigin = drawOrigin, ...
                )
            } else {
                fig <- plotScatter(
                    Y = loadings, title = title,
                    xy = axes,
                    design = metadata,
                    drawOrigin = drawOrigin, ...
                )
            }
        }
    }

    # loadings plot  ===================
    fig <- fig + ggplot2::xlim(xlim_val) + ggplot2::ylim(ylim_val)

    if (length(fig) == 1) {
        fig <- fig[[1]]
    }

    return(fig)
}
