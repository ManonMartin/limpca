ModelAbbrev <- function(vectorname) {
  # formula = ResLMobject[[1]]
  #
  # ModelTerms_sansE <- attr(terms(formula),"term.labels") # without residuals
  # ModelTerms <- c(ModelTerms_sansE, "residuals") # with residuals
  ModelTerms <- vectorname

  ModelTerms_abbrev <- ModelTerms # Abbreviated model terms
  index <- gregexpr(":", ModelTerms)
  for (i in 1:length(ModelTerms)) {
    if (index[[i]][1] != -1) {
      ModelTerms_abbrev[i] <- substr(ModelTerms[i], 1, 1)
      index2 <- index[[i]]
      for (k in 1:length(index2)) {
        ModelTerms_abbrev[i] <- paste(ModelTerms_abbrev[i],
                                      substr(ModelTerms[i],
                                             index2[k] + 1,
                                             index2[k] + 1),
                                      sep = "x")
      }
    }
  }

  ModelTerms_abbrev[length(ModelTerms_abbrev)] <- "Residuals"

  # ResLMobject = utils::modifyList(ResLMobject,list(ModelTermsAbbrev=ModelTerms_abbrev))
  # return(ResLMobject)
  return(ModelTerms_abbrev)
}
