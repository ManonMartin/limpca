# FIXME: 
# - rename column names as original
# - drop unused levels in factor
# - test lm sur modmat créé
# warning si effet nesté second ou plus ordre
# attention de ne pas selectionner d'effet aléatoire avec grep(:) !!

library(tidyverse)

# factors
treatment <- c(rep(c("A", "B"), each = 15), "B", "B", "B")         # 2 treatments
patient   <- c(rep(paste0("P", c(1:5)), each = 3), rep(paste0("P", c(6:11)), each = 3))
time      <- c(rep(c("T1", "T2", "T3"), times = 10), "T1", "T2", "T3")

dat <- data.frame(
  treatment = factor(treatment),
  time      = factor(time),
  patient   = factor(patient)
)

dat$patient <- factor(dat$patient, levels = paste0("P", 1:11))

table(dat$treatment, dat$patient)

# response variable
dat$y <- rnorm(nrow(dat))

# formula
form <- as.formula("~ treatment/patient")

# nested parameter
nested = "treatment:patient"

options(contrasts = c("contr.sum", "contr.poly"))

### reshape model matrix columns linked to a nested effect

reshape_mod_mat <- function(dat, form, nested = NULL){

  # extract model matrix without nested effect, then for each nested effect, do the following:
  # 0. Tester la singularité et stopper si c’est le cas
  # 1. Ajouter un argument pour spécifier la liste des facteurs hiérarchisés : help indiquer fact hiérarhcisé modèle x1:x2 mais en plus mettre x2 en argument et si X3 dans X2 dans X1, que mettre dans le modèle et dans l’argument.  On peut avoir x3 hiérarchisé dans (x1:x2) ce qui est différent.
  # 2; Que faire : recoder les niveaux du facteur inférieur avec des xx1,xx2,xx3 dans chaque niveau du facteur supérieur.  Créer des niveaux tels que le dernier apparaisse dans tous les blocs.  Le dernier xxi ou i est le nombre maximum de niveau dans le bloc où il y a le plus de niveaux.
  # 3. Créer la matrice du modèle avec limpca méthode actuelle
  # 4. Enlever toutes les colonnes du bloc de ce terme de la matrice X ou il n’y a que des -1
  # 5.  Et puis tester la singularité -> erreur mais renvoyer la matrice


  # Test singularity of model matrix

  modMat <- model.matrix(form, data = dat)

  r_modMat <- qr(modMat)$rank # rank
  p=ncol(modMat)
  if (r_modMat-p){
    warning("model matrix is singular",immediate. = TRUE)
  }

  # all model terms
  all_terms <- attr(terms(form), "term.labels")
  all_terms

  # other terms (no interaction)
  other_terms <- grep(x = all_terms, pattern = ':',
              value = TRUE, invert = TRUE)

  # all interaction(s) or nested effect(s)
  all_int <- grep(x = all_terms, pattern = ':', value = TRUE)

  test_nesting <- function(x){
    # possible nesting:
    right_term <- str_remove(pattern = "^.*:", x)
    left_term <- str_remove(pattern = ":.*$", x)
    int <- NA
    if (! right_term %in% all_terms){
      int <- x
    }
    return(int)
  }

  # all_int <- c("treatment:patient", "treatment:time", "treatment:treatment")

  possibly_nested <- map_chr(all_int, test_nesting)
  possibly_nested <- possibly_nested[!is.na(possibly_nested)]

  test_nested <- map_lgl(possibly_nested, \(x) ! x %in% nested)
  names(test_nested) <- possibly_nested

  test_nestedTRUE <- test_nested[test_nested]
  if (sum(test_nested) > 0){
    warning(paste0("One or several nested effect(s) has/have been detected in the formula but are not present in the 'nested' argument: ", names(test_nestedTRUE)))
  }

  # build the model matrix for all the terms but the nested effects
  interactionTerms <- all_int[!all_int %in% possibly_nested]

  otherTermsInteraction_formula <- as.formula(paste0("~ ", paste(c(other_terms, interactionTerms), collapse=" + ")))

  modMat_otherTerms <- model.matrix(otherTermsInteraction_formula, data = dat)

  # build the corrected model matrix the nested effects

  modMat_nested <- vector(mode = "list", length = seq_along(possibly_nested))

  for (i in seq_along(possibly_nested)){

    int <- possibly_nested[i]

    right_term <- str_remove(pattern = "^.*:", int)
    left_term <- str_remove(pattern = ":.*$", int)

    # rename levels per level of the nesting variable
    old_var <- dat[,right_term]

    new_var <- ave(as.character(dat$patient), dat$treatment, FUN = function(x) {
      paste0("patient", as.integer(factor(x, levels = unique(x))))
    })

    lookup_var_levels <- data.frame(old_var, new_var)
    dat[,right_term] <- as.factor(new_var)

    # check if last level is common to all levels of nesting variable
    nested_common_levels <- function(data, parent, nested) {
      tab <- table(data[[nested]], data[[parent]])
      rownames(tab)[rowSums(tab > 0) == ncol(tab)]
    }

    ncl <- nested_common_levels(dat, left_term, right_term)

    levs <- levels(dat[,right_term])

    if (tail(ncl, 1) != tail(levs, 1)){
      # intervert and rename levels
      # to have the last level as common
      last_common <- tail(ncl, 1)
      # last_common <- "P5"
      id <- which(levs == last_common)
      new_levs <- c(levs[-id], levs[id])
      new_names <- paste0(right_term, seq_along(new_levs))
      lookup_newNames <- data.frame(original_names = levs, new_levels = new_levs)

      lookup_var_levels <- lookup_var_levels |>
        left_join(lookup_newNames, by = c("new_var" = "original_names")) |>
        dplyr::select(-new_var) |>
        dplyr::rename("new_var" = "new_levels")

      dat[,right_term] <- as.factor(lookup_var_levels[,"new_var"])

    }

    modMat_intermediate <- model.matrix(as.formula(paste0("~ ", left_term," + ",int)), data = dat)
    modMat_nest <- modMat_intermediate[, grepl(":", colnames(modMat_intermediate)), drop = FALSE]

    unique_levels <- apply(modMat_nest, 2, unique)

    contains1 <- map_lgl(unique_levels, \(x) 1 %in% x)
    modMat_nest <- modMat_nest[, names(contains1)[contains1]]

    modMat_nested[[i]] <- as.data.frame(modMat_nest)
    
  }

  modMat_nested_all <- purrr::list_cbind(modMat_nested)


  modMat_allTerms <- cbind(modMat_otherTerms, modMat_nested_all)

  # Test again singularity of model matrix

  modMat <- modMat_allTerms

  r_modMat <- qr(modMat)$rank # rank
  p=ncol(modMat)
  if (r_modMat-p){
    stop("model matrix is singular, even after the removal of some model matrix columns. The design is mispecified")
  }

  return(modMat)
}

reshape_mod_mat(dat = dat,
                form = as.formula("~ treatment/patient"),
                nested = "treatment:patient")


