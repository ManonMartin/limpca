#Create a list of contrasts sum with the formula
createContrastsSum <- function(form){
  contrasts.arg.Values <- list()
  fixNames <- attr(terms(formula(form)),"term.labels")[!grepl("\\|",attr(terms(formula(form)),"term.labels"))]
  length(contrasts.arg.Values) <- length(fixNames[!grepl(":", fixNames)])
  names(contrasts.arg.Values) <- fixNames[!grepl(":", fixNames)]
  if (length(contrasts.arg.Values) != 0){
    for(iList in 1:length(contrasts.arg.Values)) contrasts.arg.Values[[iList]] <- "contr.sum"
  }
  return(contrasts.arg.Values)
} 

# V2 use also the effect in the interaction
createContrastsSum_V2 <- function(form){
  contrasts.arg.Values <- list()
  fixNames <- attr(terms(formula(form)),"term.labels")[!grepl("\\|",attr(terms(formula(form)),"term.labels"))]
  length(contrasts.arg.Values) <- length(fixNames[!grepl(":", fixNames)])
  names(contrasts.arg.Values) <- fixNames[!grepl(":", fixNames)]
  # add effect on the interaction
  if(any(grepl(":", fixNames))){
    varInteraction <- strsplit(fixNames[grepl(":", fixNames)],":")
    for(i in length(varInteraction)){
      tmp_names <- names(contrasts.arg.Values)
      length(contrasts.arg.Values) <- length(contrasts.arg.Values) + 2
      names(contrasts.arg.Values) <- c(tmp_names,varInteraction[[i]])
    }
    # Create a unique list using non-duplicated indices
    duplicated_names <- duplicated(names(contrasts.arg.Values))
    contrasts.arg.Values <- contrasts.arg.Values[!duplicated_names]
  }
  if (length(contrasts.arg.Values) != 0){
    for(iList in 1:length(contrasts.arg.Values)) contrasts.arg.Values[[iList]] <- "contr.sum"
  }
  return(contrasts.arg.Values)
} 