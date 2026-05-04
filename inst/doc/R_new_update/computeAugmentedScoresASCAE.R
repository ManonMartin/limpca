# Compute the augmented scores, corrected or not, of Meffect by Madd with the method ASCA-E
# min_df2 use to correct if the df2 is too small but not ideal solution
computeAugmentedScoresASCAE <- function(Meffect,Madd,df1,df2,correctedMatrixAdd, min_df2 = 4){
  
  ascaSVD <- pcaBySvd(Meffect)
  
  if(correctedMatrixAdd){
    # Check if df2 is large enough
    if (any(df2 < min_df2)) {
      warning("The degree of freedom df2 is too small. Using df2 = ", min_df2,"\n")
      df2[df2 < min_df2] <- min_df2
    }
    
    Fstat <- qf(.95, df1=df1 ,df2= df2)
    coef <- sqrt((Fstat*df1)/df2)
    
    mat <- matrix(NA, ncol=ncol(Meffect), nrow=nrow(Meffect))
    for (i in 1:ncol(Meffect)){
      mat[,i] <- Madd[,i]*coef[i]
    }
  }else{
    mat <- Madd
  }
  ascaSVD$scores[,1:ncol(ascaSVD$scores)] <- (Meffect + mat) %*%
    ascaSVD$loadings[,1:ncol(ascaSVD$scores)]
  return(ascaSVD)
}