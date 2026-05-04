### Compute the estimation of  effective dimensions (ED) for each response model
# Method "Effective Simple Size" of Eilers (2018)
# Warnings : if the model is singular, the estimations of ED is biased
# LM model: ED = number of parameters
# LMM model: ED = trace of influence matrix blocks

computeED <- function(resLmpEffectMatrices){
  outcomes <- resLmpEffectMatrices$lmpDataList$outcomes
  nbr_y <- dim(outcomes)[2]
  
  if(resLmpEffectMatrices$lmpDataList$model == "lmm"){
    nbr_effect <- length(resLmpEffectMatrices$effectsNamesUnique[-1]) + length(resLmpEffectMatrices$effectsNamesUniqueR) + 1
    
    ED <- matrix(numeric(nbr_effect * nbr_y), nrow = nbr_effect, ncol = nbr_y)
    MM_full <- resLmpEffectMatrices$MM_full
    
    for (i in 1:length(MM_full)){
      
      lmer_res <- MM_full[[i]]
      y <- outcomes[,i]
      
      # Retrieve the variances of the PC i
      sigma2r <- (resLmpEffectMatrices$resStdError[i])^2
      if(class(resLmpEffectMatrices$variancesEstimatedR)[1] == "matrix"){
        sigma2rand <- sapply(resLmpEffectMatrices$variancesEstimatedR[,i],function(x) x[[1]])
      } else{
        sigma2rand <- sapply(resLmpEffectMatrices$variancesEstimatedR[i],function(x) x[[1]])
      }
      
      # Check that the variances are not zero otherwise, set a very small default value
      for(v in 1:length(sigma2rand)){
        if(sigma2rand[[v]] < 1e-15){
          warning("The variance of the random ", resLmpEffectMatrices$effectsNamesUniqueR[v], " variable for response ",  colnames(outcomes)[i]," is 0.\n")
          var_effect <- as.numeric(resLmpEffectMatrices$variancesEstimatedR)
          max_var <- max(var_effect[seq(v, length(var_effect), by = length(sigma2rand))])
          sigma2rand[[v]] <- 10e-7*max_var
        }
      }
      
      q <- ncol(resLmpEffectMatrices$modelMatrixR)
      m <- ncol(resLmpEffectMatrices$modelMatrix)
      n <- nrow(outcomes)
      
      Zmat <- resLmpEffectMatrices$modelMatrixR
      Xmat <- resLmpEffectMatrices$modelMatrix
      
      #lmer_res@Gpallows retrieving the list of indices of random effects
      G <- diag(q)*0
      for(l in 1:(length(lmer_res@Gp)-1)){
        G[(lmer_res@Gp[l] + 1):lmer_res@Gp[l + 1],(lmer_res@Gp[l] + 1):lmer_res@Gp[l + 1]] <- diag(lmer_res@Gp[l + 1] - lmer_res@Gp[l])*sigma2rand[l]
      }
      
      R <- diag(sigma2r,ncol= n,nrow = n)
      Rinv <- diag((1/sigma2r),ncol= n,nrow = n)
      
      # Equ (2) Eilers 2018
      XRX <- t(Xmat)%*%Rinv%*%Xmat
      ZRX <- t(Zmat)%*%Rinv%*%Xmat
      XRZ <- t(Xmat)%*%Rinv%*%Zmat
      ZRZ <- t(Zmat)%*%Rinv%*%Zmat
      ZRZG <- ZRZ + solve(G)
      XRY <- t(Xmat)%*%Rinv%*%y
      ZRY <- t(Zmat)%*%Rinv%*%y
      Q <- solve(rbind(cbind(XRX, XRZ), cbind(ZRX, ZRZG)))
      vecbc <- Q%*%rbind(XRY,ZRY)
      
      # Equ (11) Eilers 2018: dimensions effective based on K
      K <- Q%*%cbind(rbind(XRX,ZRX),rbind(XRZ,ZRZ))
      
      list_K <- list()
      list_K[[1]] <- K[1:m,1:m] # fixeds variables
      cpt <- m
      for(l in 2:length(lmer_res@Gp)){
        c <- cpt + (lmer_res@Gp[l] - lmer_res@Gp[l-1])
        list_K[[l]] <- K[(cpt+1):c,(cpt+1):c]
        cpt <- cpt + lmer_res@Gp[l]
      }
      
      for(v in 1:length(list_K)){
        # Remove fixed effect
          if(v != 1){
            ED[v - 1,i] <- sum(diag(list_K[[v]]))
        }
      }
    }
  
    colnames(ED) <- colnames(outcomes)
    if(length(resLmpEffectMatrices$effectsNamesUnique[-1]) != 0){
      # Add ED of fixed effect
      for(i in 1:length(resLmpEffectMatrices$effectsNamesUnique[-1])){
        ED[length(resLmpEffectMatrices$effectsNamesUniqueR) + i,] <- rep(ncol(resLmpEffectMatrices$modelMatrixByEffect[[i + 1]]),ncol(ED))
      }
      # Add ED of residuals
      ED[length(resLmpEffectMatrices$effectsNamesUniqueR) + length(resLmpEffectMatrices$effectsNamesUnique[-1]) + 1,] <- 0
      ED[length(resLmpEffectMatrices$effectsNamesUniqueR) + length(resLmpEffectMatrices$effectsNamesUnique[-1]) + 1,] <- n - colSums(ED)
      rownames(ED) <- c(resLmpEffectMatrices$effectsNamesUniqueR,resLmpEffectMatrices$effectsNamesUnique[-1],"Residuals")
    }else{
      # Add ED of residuals
      ED[length(resLmpEffectMatrices$effectsNamesUniqueR) + 1,] <- 0
      ED[length(resLmpEffectMatrices$effectsNamesUniqueR) + 1,] <- nrow(outcomes) - colSums(ED)
      rownames(ED) <- c(resLmpEffectMatrices$effectsNamesUniqueR,"Residuals")
    }
    
    # the effective dimension of random effect must not exceeding
    # the number of levels for the variable.
    for(randName in resLmpEffectMatrices$effectsNamesUniqueR){
      if(!all(ED[randName,]<ncol(resLmpEffectMatrices$modelMatrixByEffectR[[randName]]))) stop("one effective dimension too high")
    }
  } else {# For lm : Add ED of fixed effect and ED Residuals
    nbr_effect <- length(resLmpEffectMatrices$effectsNamesUnique)
    ED <- matrix(numeric(nbr_effect * nbr_y), nrow = nbr_effect, ncol = nbr_y)
    
    # Add ED of fixed effect
    for(i in 1:length(resLmpEffectMatrices$effectsNamesUnique[-1])){
      ED[i,] <- rep(ncol(resLmpEffectMatrices$modelMatrixByEffect[[i + 1]]),ncol(ED))
    }
    
    # Add ED of residuals
    ED[length(resLmpEffectMatrices$effectsNamesUnique),] <- 0
    ED[length(resLmpEffectMatrices$effectsNamesUnique),] <- nrow(outcomes) - colSums(ED)
    rownames(ED) <- c(resLmpEffectMatrices$effectsNamesUnique[-1],"Residuals")
  }
  
  return(ED)
}