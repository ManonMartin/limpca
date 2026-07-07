## parameters:
# - nested: Character vector containing the nested effects of two categorial variables, formatted as: `nesting_effect:nested_effect`. Only the one level nested effects are allowed in the formula.
# - form: character string with the formula
# - dat: design matrix

#' @import stringr
#' @import purrr
#' @import dplyr

reshape_mod_mat <- function(dat, form, nested = NULL, contrasts.arg){

  form <- stats::as.formula(form)
  # Check that all terms in form are present in dat
  terms <- attr(terms(form), "term.labels")
  unique_terms <- unique(unlist(base::strsplit(terms, ":")))
  if (sum(! unique_terms %in% colnames(dat) > 0)){
    id <- ! unique_terms %in% colnames(dat) > 0
    stop(paste0("Some elements from the formula are not present in the design: ",
                paste(unique_terms[id], collapse=", ")))
  }

  # Check the nested argument
  if (!is.null(nested)){
      # test if character string FIXME

      # test if only one level nested effects
      if (sum(!base::grepl("^[^:]+:[^:]+$", as.character(nested))) > 0){
        stop("Some elements in the nested argument are not formatted as: nesting_effect:nested_effect")
      }

      # test if all elements from nested argument are present in the formula

      if (sum(!nested %in% attr(terms(form), "term.labels")) > 0){
        id <- !nested %in% attr(terms(form), "term.labels")
        stop(paste0("Some elements in the nested argument are not present in the formula: ",
                    paste(nested[id], collapse = ", ")))
      }

    }

  ##############################################################
  # Construction de la matrice du modèle et gestion des effets
  # hiérarchisés (nested effects)
  ##############################################################

  # Construction de la matrice du modèle complète sans codage spécial
  modMat <- stats::model.matrix(form, data = dat, contrasts.arg = contrasts.arg)

  # # Vérification du rang pour détecter une singularité
  # r_modMat <- qr(modMat)$rank
  # p <- ncol(modMat)
  #
  # if ((r_modMat - p) < 0){
  #   warning("Model matrix is singular, it can be due to hierarchical effects ...", immediate. = TRUE)
  # }


  ##############################################################
  # Extraction des termes du modèle
  ##############################################################

  # Tous les termes du modèle (sans l'intercept)
  all_terms <- attr(stats::terms(form), "term.labels")

  # Termes simples (sans interaction)
  other_terms <- base::grep(
    x = all_terms,
    pattern = ":",
    value = TRUE,
    invert = TRUE
  )

  # Tous les termes d'interaction
  all_int <- base::grep(
    x = all_terms,
    pattern = ":",
    value = TRUE
  )

  ##############################################################
  # Détection automatique des effets potentiellement hiérarchisés
  ##############################################################

  test_nesting <- function(x){

    # Partie droite de l'interaction (facteur potentiellement imbriqué)
    right_term <- stringr::str_remove(pattern = "^.*:", x)

    # Partie gauche (facteur parent)
    left_term <- stringr::str_remove(pattern = ":.*$", x)

    int <- NA

    # Si le facteur imbriqué n'apparaît pas seul dans le modèle,
    # on suppose qu'il s'agit d'un effet hiérarchisé
    if (! right_term %in% all_terms){
      int <- x
    }

    return(int)
  }

  possibly_nested <- NULL

  if(length(all_int) > 0){
    # Recherche des effets potentiellement hiérarchisés
    possibly_nested <- purrr::map_chr(all_int, test_nesting)
    possibly_nested <- possibly_nested[!is.na(possibly_nested)]
  }

  if (length(possibly_nested) > 0){ # if nested effects

    # test if possibly_nested only contains categorical variables from dat data.frame
    unique_terms <- unique(unlist(base::strsplit(possibly_nested, ":")))
    unique_terms

    if (!all(base::sapply(dat[, unique_terms], is.factor))){
      id <- (! base::sapply(dat[, unique_terms], class)  %in% "factor")
      stop(paste0("Some variables in the nested argument are not of class factor: ",
                  paste(unique_terms[id], collapse = ", ")))
    }

    ##############################################################
    # Vérification de la cohérence avec l'argument "nested"
    ##############################################################

    test_nested <- map_lgl(
        possibly_nested,
        \(x) ! x %in% nested
      )

    names(test_nested) <- possibly_nested

    test_nestedTRUE <- test_nested[test_nested]

    if (sum(test_nested) > 0){
      # warning(
      #   paste0(
      #     "One or several nested effect(s) has/have been detected in the formula but are not present or misspelled in the 'nested' argument: ",
      #     names(test_nestedTRUE),". All the detected terms will be taken into account to remove the singularity issues."
      #   )
      # )
      message(
        paste0(
          "One or several nested effect(s) has/have been detected in the formula: ", possibly_nested,". All the detected terms will be taken into account to remove the singularity issues."
        )
      )

    }

    ##############################################################
    # Construction de la matrice sans les effets hiérarchisés
    ##############################################################

    # Interactions classiques (non imbriquées)
    interactionTerms <- all_int[!all_int %in% possibly_nested]

    # Formule contenant les termes simples + interactions classiques
    otherTermsInteraction_formula <- stats::as.formula(
      paste0(
        "~ ",
        paste(c(other_terms, interactionTerms), collapse = " + ")
      )
    )

    # Matrice du modèle correspondante
    modMat_otherTerms <- stats::model.matrix(
      otherTermsInteraction_formula,
      contrasts.arg = contrasts.arg,
      data = dat
    )

    ##############################################################
    # Traitement des effets hiérarchisés
    ##############################################################

    # Pour l'instant on traite uniquement le premier effet imbriqué
    # int <- possibly_nested[1]

    modMat_nested_fun <- function(nested_effect){

      # Facteur parent
      left_term <- stringr::str_remove(pattern = ":.*$", nested_effect)

      # Facteur imbriqué
      right_term <- stringr::str_remove(pattern = "^.*:", nested_effect)

      ##############################################################
      # Renumérotation des niveaux du facteur imbriqué
      # à l'intérieur de chaque niveau du facteur parent
      ##############################################################

      old_var <- dat[, right_term]

      #BG ici ta fonnction n'est pas encore générale tu dois mettre dat[, right_term]
      # BG dat[, left_term] et les niveaux donne leur peut-être un nom comme "levelw"
      # j'ai fait la correction dans les 10 lignes suivantes !!!
      new_var <- ave(
        as.character(dat[[right_term]]),
        dat[[left_term]],
        FUN = function(x){
          paste0(
            right_term,
            as.integer(
              factor(x, levels = unique(x))
            )
          )
        }
      )

      lookup_var_levels <- data.frame(old_var, new_var)
      lookup_var_levels <- unique(lookup_var_levels)

      # Remplacement des niveaux dans les données
      dat[, right_term] <- as.factor(new_var)

      ##############################################################
      # Recherche des niveaux communs à tous les groupes
      ##############################################################

      nested_common_levels <- function(data, parent, nested){

        tab <- table(data[[nested]], data[[parent]])

        # Niveaux présents dans tous les groupes du facteur parent
        rownames(tab)[rowSums(tab > 0) == ncol(tab)]
      }

      ncl <- nested_common_levels(
        dat,
        left_term,
        right_term
      )

      levs <- levels(dat[[right_term]])

      ##############################################################
      # Réorganisation des niveaux pour placer un niveau commun
      # en dernière position
      ##############################################################

      if (tail(ncl, 1) != tail(levs, 1)){

        last_common <- tail(ncl, 1)

        id <- which(levs == last_common)

        new_levs <- c(
          levs[-id],
          levs[id]
        )

        new_names <- paste0(
          right_term,
          seq_along(new_levs)
        )

        lookup_newNames <- data.frame(
          original_names = levs,
          new_levels = new_levs
        )

        lookup_var_levels <- lookup_var_levels |>
          dplyr::left_join(
            lookup_newNames,
            by = c("new_var" = "original_names")
          ) |>
          dplyr::select(-new_var) |>
          dplyr::rename("new_var" = "new_levels")

        dat[, right_term] <- as.factor(
          lookup_var_levels[, "new_var"]
        )
      }

      ##############################################################
      # Construction de la matrice correspondant à l'effet imbriqué
      ##############################################################

      modMat_intermediate <- stats::model.matrix(
        as.formula(
          paste0(
            "~ ",
            left_term,
            " + ",
            nested_effect
          )
        ),
        data = dat, contrasts.arg = contrasts.arg
      )

      # Conservation des colonnes correspondant à l'interaction
      modMat_nested <- modMat_intermediate[
        ,
        grepl(":", colnames(modMat_intermediate)),
        drop = FALSE
      ]

      ##############################################################
      # Suppression des colonnes ne contenant jamais la valeur +1
      # (colonnes inutiles pour le codage)
      ##############################################################

      unique_levels <- base::apply(
        modMat_nested,
        2,
        unique
      )

      contains1 <- purrr::map_lgl(
        unique_levels,
        \(x) 1 %in% x
      )

      modMat_nested <- modMat_nested[,names(contains1)[contains1]]
      return((modMat_nested))
    }

    res_map <- purrr::map(possibly_nested, modMat_nested_fun)

    modMat_nested <- base::do.call(cbind, res_map)

    ##############################################################
    # Assemblage de la matrice finale
    ##############################################################

    modMat_allTerms <- cbind(
      modMat_otherTerms,
      modMat_nested
    )

    modMat <- modMat_allTerms

    # Test if modmat is singular

    r_modMat <- qr(modMat)$rank
    p <- ncol(modMat)

    isSingular <- (r_modMat - p) < 0

    if (isSingular){
      warning(
        "The model matrix is singular due to design mispecification, even after the removal of some columns due to herarchical effects."
      )
    }


  }else{

    # Interactions classiques (non imbriquées)
    interactionTerms <- all_int[!all_int %in% possibly_nested]

    # Formule contenant les termes simples + interactions classiques
    otherTermsInteraction_formula <- as.formula(
      paste0(
        "~ ",
        paste(c(other_terms, interactionTerms), collapse = " + ")
      )
    )

    # Matrice du modèle correspondante
    modMat_otherTerms <- stats::model.matrix(
      otherTermsInteraction_formula,
      contrasts.arg = contrasts.arg,
      data = dat
    )

    modMat_allTerms <- modMat_otherTerms

    # Test if modmat is singular
    modMat <- modMat_allTerms

    r_modMat <- qr(modMat)$rank
    p <- ncol(modMat)

    isSingular <- (r_modMat - p) < 0

    if (isSingular){
      warning(
        "The model matrix is singular due to design mispecification."
      )
    }

    }

  rownames(modMat) <- rownames(dat)

  ##############################################################
  # Retour de la matrice du modèle corrigée
  ##############################################################

  return(list(modMat = modMat, isSingular = isSingular, possibly_nested = possibly_nested))
}

