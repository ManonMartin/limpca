#' @export lmwEffectPlot
#' @title Effect plot
#'
#' @description
#' Draws effects plot for the lmwPcaEffects function.
#'
#' @param resASCA A list corresponding to the ASCA output value of \code{\link{lmwPcaEffects}}.
#' @param effectName Name of the effect matrix used for the scores.
#' @param nPC A numerical vector with the Principal Components axes to be drawn.
#' @param x A character string giving the `design` factor whose levels will form the x axis.
#' @param z A character string giving the `design` factor whose levels will form the traces.
#' @param w A character string giving the `design` factor whose levels will be used for the facet.
#' @param ...
#'
#' @return An effect plot.
#'
#' @details
#' `lmwEffectPlot` is a wrapper of \code{\link{plotMeans}}.
#'
#' @examples
#'
#' @import ggplot2

lmwEffectPlot <- function(resASCA,
                          effectName,
                          nPC = 1,
                          x,
                          z = NULL,
                          w = NULL,
                          hline = 0,
                          ...){

  # checks =========================

  checkArg(resASCA,c("list"),can.be.null = FALSE)
  checkArg(effectName, c("str", "length1"), can.be.null = FALSE)
  checkArg(nPC,c("int","pos"),can.be.null = FALSE)
  checkArg(x,c("str", "length1"), can.be.null = FALSE)
  checkArg(z,c("str", "length1"), can.be.null = TRUE)
  checkArg(w,c("str", "length1"), can.be.null = TRUE)
  checkArg(hline,"num", can.be.null = TRUE)

  if(resASCA$method != "ASCA"){
    stop("resASCA does not correspond to an ASCA result of LmwEffectMatrices")
  }

  if(!effectName %in% names(resASCA)){
    stop(paste0(effectName," is not an effect of resASCA"))
  }

  if(max(nPC) > ncol(resASCA[[effectName]][["scores"]])){
    stop(paste0("PC (",paste0(nPC, collapse = ",")
                ,") is beyond the number of PC of scores (",ncol(scores),")"))
  }

  if(str_detect(string = effectName, pattern = "[+]")){
    numComb <- str_split(effectName,"[+]")
    numEffects <- unlist(str_split(numComb[[1]],":"))
  }else{
    numEffects <- unlist(str_split(effectName,":"))
  }

  if(!x %in% numEffects){
    stop("x should appear in effectName")
  }
  if(!is.null(z) && !z %in% numEffects){
    stop("z should appear in effectName")
  }
  if(!is.null(w) && !w %in% numEffects){
    stop("w should appear in effectName")
  }

  # prepare the arguments  ==============================

  matEffect <- resASCA[[effectName]][["scores"]]

  title = paste0(effectName, " scores as a function of ",x, ": PC",nPC)
  ylab = paste0("Scores (",round(resASCA[[effectName]][["var"]][nPC], 2),"% of variation explained)")

  fig <- plotMeans(Y = matEffect,
            design = resASCA$lmwDataList$design,
            columns = nPC,
            x = x,
            z = z,
            w = w,
            title = title,
            ylab = ylab,
            hline = hline,
            ...)

  return(fig)

}
