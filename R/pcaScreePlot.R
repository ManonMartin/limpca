#' @export pcaScreePlot
#' @title Scree Plot
#'
#' @description
#' Returns a bar plot of the percentage of variance explained by each Principal Component calculated by \code{\link{pcaBySvd}}.
#'
#' @param resPcaBySvd A list corresponding to the output value of \code{\link{pcaBySvd}}.
#' @param nPC An integer with the number of Principal Components to plot.
#' @param title Plot title.
#' @param theme `ggplot2` theme, see `?ggtheme` for more info.
#'
#' @return A `ggplot2` PCA scree plot
#'
#' @examples
#'
#' data("UCH")
#' resPCA <- pcaBySvd(UCH$outcomes)
#' pcaScreePlot(resPCA, nPC = 4)
#'
#' @import ggplot2

pcaScreePlot <- function(resPcaBySvd, nPC = 5,
                         title = "PCA scree plot",
                         theme = theme_bw()) {
  # checks ===================
  checkArg(resPcaBySvd, c("list"), can.be.null = FALSE)
  checkArg(nPC, c("num", "pos", "length1"), can.be.null = FALSE)
  checkArg(title, c("str", "length1"), can.be.null = FALSE)

  if (!identical(names(resPcaBySvd), c(
    "scores", "loadings", "eigval", "singvar",
    "var", "cumvar", "original.dataset"
  ))) {
    stop("resPcaBySvd is not an output value of pcaBySvd")
  }

  # plot ===================
  res <- data.frame(var = resPcaBySvd$var[seq_len(nPC)])
  res <- res %>% rownames_to_column("PC")

  ggplot2::ggplot(
    data = res,
    ggplot2::aes(
      x = factor(PC, levels = PC),
      y = var
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("Principal Components") +
    ggplot2::ylab("Percentage of Variance") +
    ggplot2::ggtitle(title) +
    theme
}
