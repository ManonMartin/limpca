## ----Install, results=FALSE, message=FALSE-------------------------------

# devtools::install_github("FranceschiniS/LMWiRe")

library("LMWiRe")


## ----Data Importation----------------------------------------------------

data("UCH")


## ----Spectrum visualization----------------------------------------------
firstobslab = names(UCH$outcomes[,1])[1]

ggplot2::ggplot(data = as.data.frame(t(UCH$outcomes))) + 
  ggplot2::geom_line(ggplot2::aes(x = as.numeric(colnames(UCH$outcomes)), y = UCH$outcomes[1, ])) +
  ggplot2::scale_x_reverse(lim = c(10, 0)) +
  ggplot2::xlab("ppm") +
  ggplot2::ylab("Intensity")+
  ggplot2::ggtitle(paste("H-NMR spectrum for the first observation : ",firstobslab))+
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))


## ----Design Visualization, warning=FALSE---------------------------------

PlotDesign(design = UCH$design, var1_name = "Citrate",var2_name = "Hippurate",var3_name = "Day")


## ----PCA-----------------------------------------------------------------
ResPCA = SVDforPCA(UCH$outcomes)

## ----VariancesPercentages------------------------------------------------

eig.res = rbind(ResPCA$var[1:6], ResPCA$cumvar[1:6])
rownames(eig.res) = c("Variances", "Cum Var Values")
pander::pander(eig.res)
pander::pander("A scree plot should be perform")


## ----Scores--------------------------------------------------------------

DrawScores(ResPCA, type.obj = "PCA", drawNames = TRUE, createWindow = F, 
    main = "Reponse matrix score plot", color = UCH$design$Citrate, pch = UCH$design$Hippurate, 
    axes = c(1, 2), size = 2.5) + ggplot2::scale_color_discrete(name = "Citrate") + ggplot2::scale_shape_discrete(name = "Hippurate")


## ----Loadings------------------------------------------------------------
DrawLoadings(ResPCA,type.obj = "PCA", main="Loadings from the two first component")

## ------------------------------------------------------------------------
pander::pander(UCH$formula)

## ----ModelMatrix---------------------------------------------------------

ResLMModelMatrix = LMModelMatrix(as.formula(UCH$formula),UCH$design)


## ----EffectMatrices------------------------------------------------------

ResLMEffectMatrices = LMEffectMatrices(ResLMModelMatrix,UCH$outcomes)


