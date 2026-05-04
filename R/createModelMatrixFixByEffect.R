# create an object with the list of fixed variable matrices
createModelMatrixFixByEffect <- function(res.lmer,design){
  fixed_form_term.labels <- attr(terms(formula(res.lmer[[1]], fixed.only = TRUE)),"term.labels")
  
  # If there is a fixed effect
  if (length(fixed_form_term.labels)>0){
    fix_lev_Names <- colnames(model.matrix(res.lmer[[1]],type = c("fixed")))[!colnames(model.matrix(res.lmer[[1]],type = c("fixed")))%in%"(Intercept)"]
    #Creation of a signs list to determine which levels belong to which variable 
    # => Same sign for levels of the same variable.
    sign <- c()
    for (i in 1:length(fixed_form_term.labels[!grepl(":", fix_lev_Names)])){
      sign <- rbind(sign, as.integer(grepl(fixed_form_term.labels[i], fix_lev_Names)))
    }
    sign <- apply(sign,2,paste, collapse = "")
    names(sign) <- fix_lev_Names
    uniqSign <- unique(sign)
    
    
    FixedModMatlist <- vector(mode = "list", length = length(fixed_form_term.labels))
    names(FixedModMatlist) <- fixed_form_term.labels
    
    mat <- as.matrix(model.matrix(res.lmer[[1]],type = c("fixed")))
    # Splitting the complete fixed matrix into several fixed matrices for each variable with their levels.
    for (i in 1:length(fixed_form_term.labels)){
      id <- names(sign)[sign==uniqSign[i]]
      FixedModMatlist[[i]] <- matrix(mat[,id], dimnames = list(rownames(design), id), ncol = length(id))
    }
    if ("(Intercept)"%in%colnames(model.matrix(res.lmer[[1]],type = c("fixed")))){
      FixedModMatlist <- append(list(matrix(model.matrix(res.lmer[[1]],type = c("fixed"))[,"(Intercept)"], ncol = 1,
                                            dimnames = list(rownames(design), "(Intercept)"))), FixedModMatlist)
      names(FixedModMatlist)[1] <- "(Intercept)"
    }
    # No fixed effect (except perhaps the intercept).
  } else{
    fixNames <- NULL
    if ("(Intercept)"%in%colnames(model.matrix(res.lmer[[1]],type = c("fixed")))){
      FixedModMatlist <- vector(mode = "list", length = 1)
      FixedModMatlist[[1]] <- matrix(model.matrix(res.lmer[[1]],type = c("fixed"))[,"(Intercept)"], ncol = 1,
                                     dimnames = list(rownames(design), "(Intercept)"))
      names(FixedModMatlist)[1] <- "(Intercept)"
    } else{
      FixedModMatlist <- NULL
    }
    
  }
  return(FixedModMatlist)
}