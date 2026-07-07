library(tidyverse)
# setwd("~/Dropbox/bg__UCLPartage_Memoires_stages_thèses/Partage_2026_Lucie/Limpca-sigularity")
source("reshape_mod_mat.R")

# to do:
# [x] si mat singuliere et pas effet nesté (à tester), se plante -> mettre un message (--> Matrice singulière probablement à cause du design) et stop
# [x] tester si deux variables sont bien des facteurs, et stop sinon
# [x] si singulier, ressortir la matrice du modèle pour examination par l'utilisateur


##############################################################
# Exemple 1
##############################################################

# factors
treatment <- factor(c(rep(c("A", "B"), each = 15), "B", "B", "B"))         # 2 treatments
patient   <- factor(c(rep(paste0("P", c(1:5)), each = 3), rep(paste0("P", c(6:11)), each = 3)))
time      <- factor(c(rep(c("T1", "T2", "T3"), times = 10), "T1", "T2", "T3"))
clinic <- as.factor(sample(paste0("C", 1:4),replace = TRUE,length(time)) )
sex <- as.factor(sample(c("F","H"),replace = TRUE,length(time)) )

dat <- data.frame(
  treatment = treatment,
  time      = time,
  patient   = patient
)

dat$patient <- factor(dat$patient, levels = paste0("P", 1:11))

table(dat$treatment, dat$patient)

# response variable
dat$y <- rnorm(nrow(dat))

# formula
form <- as.formula("~ treatment/patient")

# nested parameter
nested = "treatment:patient"

#BG est ce que cette option doit se trouver dans la fonction ou en dehors  ?
#BG Quid sinon quand tu crées de nouvelles formules
options(contrasts = c("contr.sum", "contr.poly"))

### reshape model matrix columns linked to a nested effect

x1=reshape_mod_mat(dat = dat,
                form = form,
                nested = nested)
x1
##############################################################
# Exemple 2 : idem mais modèle défini un peu autrement
##############################################################

# factors
dat <- data.frame(
  treatment = treatment,
  time      = time,
  patient   = patient
)

dat$patient <- factor(dat$patient, levels = paste0("P", 1:11))

table(dat$treatment, dat$patient)

# response variable
dat$y <- rnorm(nrow(dat))

# formula
form <- as.formula("~ treatment+treatment:patient")

# nested parameter
nested = "treatment:patient"

#BG est ce que cette option doit se trouver dans la fonction ou en dehors  ?
#BG Quid sinon quand tu crées de nouvelles formules
options(contrasts = c("contr.sum", "contr.poly"))

### reshape model matrix columns linked to a nested effect

x2=reshape_mod_mat(dat = dat,
                   form = form,
                   nested = nested)

x2
sum(x1-x2)

##############################################################
# Exemple 3 : données longitudinale
##############################################################

dat <- data.frame(
  treatment = treatment,
  time      = time,
  patient   = patient
)

dat$patient <- factor(dat$patient, levels = paste0("P", 1:11))

table(dat$treatment, dat$patient,dat$time)

# On non balance un peu les données avec certains patients qui manquent pour certains temps

dat=dat[-c(2,6,1,16,17,20),]
table(dat$treatment, dat$patient,dat$time)
# response variable
dat$y <- rnorm(nrow(dat))

# formula
form <- as.formula("~ treatment+time+treatment:time+treatment:patient")

# nested parameter
nested = "treatment:patient"

#BG est ce que cette option doit se trouver dans la fonction ou en dehors  ?
#BG Quid sinon quand tu crées de nouvelles formules
options(contrasts = c("contr.sum", "contr.poly"))

### reshape model matrix columns linked to a nested effect

x3=reshape_mod_mat(dat = dat,
                   form = form,
                   nested = nested)
x3
##############################################################
# Exemple 4 : 3 facteurs -> ajout sexe, clinique + interaction entre les 2 par rapport à Exemple 1
##############################################################

table (sex,clinic)

dat <- data.frame(
  treatment = treatment,
  time      = time,
  patient   = patient,
  sex=sex,
  clinic=clinic
)

dat$patient <- factor(dat$patient, levels = paste0("P", 1:11))

table(dat$treatment, dat$patient)

# response variable
dat$y <- rnorm(nrow(dat))

# formula
form <- as.formula("~ sex+treatment/patient+clinic+sex:clinic")

# nested parameter
nested = "treatment:patient"

options(contrasts = c("contr.sum", "contr.poly"))

### reshape model matrix columns linked to a nested effect

x4=reshape_mod_mat(dat = dat,
                   form = form,
                   nested = nested)
x4
qr(x4)$rank
##############################################################
# Exemple 5 : idem exemple 4 avec matrice sigulière pour sexxclinique retrait un des croisements -> singularité
##############################################################

table (sex,clinic)
dat <- data.frame(
  treatment = treatment,
  time      = time,
  patient   = patient,
  sex=sex,
  clinic=clinic
)

tosupress=(dat$sex=="H")&(dat$clinic=="C2")
dat=dat[!tosupress,]
table (dat$sex,dat$clinic)
table(dat$treatment, dat$patient)
dat$sex <- as.character(dat$sex)
# response variable
dat$y <- rnorm(nrow(dat))

# formula
form <- as.formula("~ sex+treatment/patient+clinic+sex:clinic")

# nested parameter
nested = "treatment:patient"


options(contrasts = c("contr.sum", "contr.poly"))

### reshape model matrix columns linked to a nested effect

x5=reshape_mod_mat(dat = dat,
                   form = form,
                   nested = nested)
qr(x5)$rank

##############################################################
# Exemple 6 : cas simple de singulatité sans facteur nested
##############################################################

table (sex,clinic)
dat <- data.frame(
  treatment = treatment,
  time      = time,
  sex=sex,
  clinic=clinic
)

tosupress=(dat$sex=="H")&(dat$clinic=="C2")
dat=dat[!tosupress,]
table (dat$sex,dat$clinic)

# response variable
dat$y <- rnorm(nrow(dat))

# formula
form <- as.formula("~ sex+treatment+clinic+sex:clinic")

# nested parameter
nested = NULL

options(contrasts = c("contr.sum", "contr.poly"))

### reshape model matrix columns linked to a nested effect

x6=reshape_mod_mat(dat = dat,
                   form = form,
                   nested = nested)
qr(x6)$rank

##############################################################
# Exemple 7 : encore plus simple pas de singulatité et pas de facteur nested
##############################################################

table (sex,clinic)
dat <- data.frame(
  treatment = treatment,
  time      = time,
  sex=sex,
  clinic=clinic
)

table (dat$sex,dat$clinic)

# response variable
dat$y <- rnorm(nrow(dat))

# formula
form <- as.formula("~ sex+treatment+clinic+sex/clinic")
form <- as.formula("~ sex+treatment+clinic+treatment/patient")

# nested parameter
nested = NULL

options(contrasts = c("contr.sum", "contr.poly"))

### reshape model matrix columns linked to a nested effect

x7=reshape_mod_mat(dat = dat,
                   form = form,
                   nested = nested)

qr(x7)$rank

#######################################################
########### example metabolomics
#######################################################

## univariate ============================

library(SummarizedExperiment)
library(tidySummarizedExperiment)

mae <- readRDS("../FRESH_LACTIN_V_mae_for_Lucie_20260414.RDS")

se_metabo <- mae[["metabo"]]

assayNames(se_metabo)

se_metabo[["peak_batchNormImp_log"]]

# lm()

form <- as.formula("peak_batchNormImp_log ~ arm+visit_label+arm:visit_label+arm:pid")

dat <- se_metabo |>
  as_tibble() |>
  dplyr::select(.sample, peak_batchNormImp_log, visit_label, arm, pid) |>
  unique()

lm(formula = form, data = dat, singular.ok = TRUE)
lm(formula = form, data = dat, singular.ok = FALSE)

# reshape_mod_mat()

dat$pid <- as.factor(dat$pid)

# nested parameter
nested = "arm:pid"

form <- "~ arm+visit_label+arm:visit_label+arm:pid"

options(contrasts = c("contr.sum", "contr.poly"))

### reshape model matrix columns linked to a nested effect

x8 = reshape_mod_mat(dat = dat,
                   form = form,
                   nested = nested)

## multivariate (limpca) ============================

X <- t(assay(se_metabo, i = "peak_batchNormImp_log"))

form <- "~ arm+visit_label+arm:visit_label+arm:pid"

desi <- colData(se_metabo) |>
  as.data.frame() |>
  dplyr::select(visit_label, arm, pid)
desi$pid <- as.factor(desi$pid)

metabo_data <- list(design = desi, outcomes = X, formula = form)

# PCA reduction
resPCA_metabo <- lmpOutcomesReduct(metabo_data)
lmpDataList <- resPCA_metabo$lmpDataList

resPCA <- resPCA_metabo$resPCA

pcaScorePlot(resPcaBySvd = resPCA, axes = c(1,2),
             title = "Scores plot",
             design = metabo_data$design, color="arm", drawShapes = "segment",
             points_labs_rn = FALSE)

# Estimation du modèle et décomposition de la matrice d'effets

## Estimation des matrices de modèles

# # nested parameter
# nested = "arm:pid"
# x9 = reshape_mod_mat(dat = lmpDataList$design,
#                      form = lmpDataList$formula,
#                      nested = nested)

resLmpModelMatrix <- lmpModelMatrix(lmpDataList)

str(resLmpModelMatrix)

resLmpModelMatrix$effectsNamesUnique

## Estimations des modèles

#### Estimation avec la variance des effets fixes calculée avec la formule du type 3 SS de limpca (resLmpEffectMatrices_Candies).

resLmpEffectMatrices <-  lmpEffectMatrices(resLmpModelMatrix)

resLmpEffectMatrices$type3SS

# Pourcentage de variance expliqué par effet :

pander::pander(resLmpEffectMatrices$variationPercentages)
resLmpEffectMatrices$varPercentagesPlot


# Tests Bootstrap

resLmpBootstrapTests_Candies <- lmpBootstrapTests(resLmpEffectMatrices_Candies, nboot = 1000, verbose = TRUE)

# ASCA

resLmpPcaEffectsASCA_Candies <- lmpPcaEffects(resLmpEffectMatrices_Candies, method="ASCA",
                                              verbose = TRUE,backtransform = TRUE,
                                              correctedMatrixAdd = FALSE)

## APCA
resLmpPcaEffectsAPCA_Candies <- lmpPcaEffects(resLmpEffectMatrices_Candies,
                                              method="APCA",
                                              verbose = TRUE,
                                              backtransform = TRUE,
                                              correctedMatrixAdd = FALSE)

lmpScoreScatterPlotM(resLmpPcaEffectsAPCA_Candies,
                     varname.colorup = "Candies",
                     varname.colordown = "Candies",
                     varname.pchup = "Judges",
                     varname.pchdown = "Judges")
