#' @export pcaScreePlot
#' @title Scree Plot
#'
#' @description
#' Returns a bar plot of the percentage of variance explained by each Principal Component calculated by \code{\link{pcaBySvd}}.
#'
#' @param resPcaBySvd A list corresponding to the output value of \code{\link{pcaBySvd}}.
#' @param nPC An integer with the number of Principal Components to plot.
#' @param title Plot title.
#' @param theme ggplot theme, see `?ggtheme` for more info.
#'
#' @return A PCA scree plot
#'
#' @examples
#'
#'  data('UCH')
#'  resPCA = pcaBySvd(UCH$outcomes)
#'  pcaScreePlot(resPCA, nPC=4)
#'
#' @import ggplot2

pcaScreePlot = function(resPcaBySvd, nPC = 5,
                        title = "PCA scree plot",
                        theme = theme_bw()){

  # checks ===================
  checkArg(resPcaBySvd,c("list"),can.be.null = FALSE)
  checkArg(nPC,c("num","pos","length1"),can.be.null = FALSE)
  checkArg(title,c("str", "length1"),can.be.null = FALSE)

  if (!identical(names(resPcaBySvd),c("scores","loadings","eigval","singvar",
                                      "var","cumvar","original.dataset"))){
    stop("resPcaBySvd is not an output value of pcaBySvd")}

  # plot ===================
  res = as.data.frame(resPcaBySvd$var[1:nPC])

  ggplot2::ggplot(data=res,
                  ggplot2::aes(x=rownames(res),
                               y=res[,1])) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::xlab("Principal Components") +
    ggplot2::ylab("Percentage of Variance") +
    ggplot2::ggtitle(title) +
    theme
}
