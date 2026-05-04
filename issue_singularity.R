set.seed(123)

# # factors
treatment <- rep(c("A", "B"), each = 15)          # 2 treatments
patient   <- c(rep(paste0("P", 1:5), each = 3), rep(paste0("P", 1:5), each = 3))
time      <- rep(c("T1", "T2", "T3"), times = 10)

# factors
treatment <- c(rep(c("A", "B"), each = 15), "B", "B", "B")         # 2 treatments
patient   <- c(rep(paste0("P", c(1:4,6)), each = 3), rep(paste0("P", c(1:6)), each = 3))
time      <- c(rep(c("T1", "T2", "T3"), times = 10), "T1", "T2", "T3")

dat <- data.frame(
  treatment = factor(treatment),
  time      = factor(time),
  patient   = factor(patient)
)

interaction(dat$treatment, dat$patient, drop = TRUE)

levels((dat$patient))

table(dat$treatment, dat$patient)

# response variable
dat$y <- rnorm(nrow(dat))

str(dat)

modMat <- model.matrix(~ treatment + treatment:patient, data = dat,
contrasts = list(treatment = "contr.sum", time = "contr.sum", patient = "contr.sum"))

modMat

# check singularity
r_modMat <- qr(modMat)$rank # rank
p=ncol(modMat)
r_modMat-p

fit <- lm(y ~ treatment + treatment:patient, data = dat, singular.ok = TRUE,
contrasts = list(treatment = "contr.sum", time = "contr.sum", patient = "contr.sum"))
summary(fit)

model.matrix(fit)

fit <- lm(y ~ treatment + treatment:patient, data = dat, singular.ok = TRUE)
summary(fit)

# explicitly encode nesting (important conceptually)
dat2 <- dat
dat2$patient <- interaction(dat$treatment, dat$patient, drop = TRUE)
fit <- lm(y ~ treatment + patient, data = dat2, singular.ok = TRUE,
contrasts = list(treatment = "contr.sum", time = "contr.sum", patient = "contr.sum"))
summary(fit)

model.matrix(fit)

fit <- lm(y ~ treatment * time + patient, data = dat2, singular.ok = FALSE)
summary(fit)



# check singularity
modMat <- model.matrix(~ treatment * time + patient, data = dat)

# modMat <- model.matrix(~ treatment * time + patient, data = dat,
#   contrasts = list(treatment = "contr.sum", time = "contr.sum", patient = "contr.sum"))

r_modMat <- qr(modMat)$rank # rank
p=ncol(modMat)
r_modMat-p

##################################################################



# factors
treatment <- c(rep(c("A", "B"), each = 15), "B", "B", "B")         # 2 treatments
patient   <- c(rep(paste0("P", c(1:4,6)), each = 3), rep(paste0("P", c(1:6)), each = 3))
time      <- c(rep(c("T1", "T2", "T3"), times = 10), "T1", "T2", "T3")

dat <- data.frame(
  treatment = factor(treatment),
  time      = factor(time),
  patient   = factor(patient)
)

# response variable
dat$y <- rnorm(nrow(dat))

form <- as.formula("~ treatment/patient")

# reshape_mod_mat <- function(dat, form, nested){

# reshape model matrix columns linked to a nested effect

# extract model matrix without nested effect, then for each nested effect, do the following:
# 0. Tester la singularité et stopper si c’est le cas
# 1. Ajouter un argument pour spécifier la liste des facteurs hiérarchisés : help indiquer fact hiérarhcisé modèle x1:x2 mais en plus mettre x2 en argument et si X3 dans X2 dans X1, que mettre dans le modèle et dans l’argument.  On peut avoir x3 hiérarchisé dans (x1:x2) ce qui est différent.  
# 2; Que faire : recoder les niveaux du facteur inférieur avec des xx1,xx2,xx3 dans chaque niveau du facteur supérieur.  Créer des niveaux tels que le dernier apparaisse dans tous les blocs.  Le dernier xxi ou i est le nombre maximum de niveau dans le bloc où il y a le plus de niveaux.  
# 3. Créer la matrice du modèle avec limpca méthode actuelle
# 4. Enlever toutes les colonnes du bloc de ce terme de la matrice X ou il n’y a que des -1
# 5.  Et puis tester la singularité -> erreur mais renvoyer la matrice 


all_terms <- attr(terms(form), "term.labels")
library(tidyverse)

int <- grep(x = all_terms, pattern = ':', value = TRUE)

# possible nesting:
right_term <- str_remove(pattern = "^.*:", int)
left_term <- str_remove(pattern = ":.*$", int)

if (! right_term %in% all_terms){
  warning("A nested effect has been detected in the formula")
}



nested_common_levels <- function(data, parent, nested) {
  tab <- table(data[[nested]], data[[parent]])
  rownames(tab)[rowSums(tab > 0) == ncol(tab)]
}

ncl <- nested_common_levels(dat, left_term, right_term)

# last ncl == last levels(dat[,right_term]) ? if yes, just rename levels, if not, intervert and rename levels

# }

# vérifier si blocks données sont orthogonaux 2 à 2 et figure colinearite entre matrices du modele