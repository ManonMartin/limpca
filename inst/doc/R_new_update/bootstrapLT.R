# Calculate the summed LLR (sumlog) and the LLR per response (ratio) 
# for one effect and with simulated outcomes
bootstrapLT <- function(useREML, MM_null, null_formula, null_form_data, form_full, outcomes) {
  nPC <- length(MM_null)
  simulatedY <- c()
  
  # If the model is a global linear model, use the lm function for build MM_null
  if(!grepl("\\|", null_formula)){
    # Estimate null models
    data <- cbind(null_form_data,outcomes)
    fmla_tmp <- sapply(paste0(colnames(outcomes), null_formula), as.formula)
    MM_null <- lapply(fmla_tmp, lm, data = data)
  }
  
  # Simulate y from null models
  for (i in 1:nPC){
    ysim       <- unlist(simulate(MM_null[[i]], re.form=NA))
    simulatedY <- cbind(simulatedY, ysim)
  }
  y <- simulatedY
  dimnames(y) <- dimnames(outcomes)
  data <- cbind(null_form_data, y)
  
  # If the model is a linear mixed model
  if(grepl("\\|", null_formula)){
    
    # Build restricted model
    fmla_tmp <- sapply(paste0(colnames(y), null_formula), as.formula)
    MM_f_null <- lapply(fmla_tmp, lme4::lmer, data = data, REML=useREML, control = lmerControl(optimizer = "bobyqa"))
    
    # Build full model
    contrasts.arg.Values <- createContrastsSum(form_full)
    fmla_tmp <- sapply(paste0(colnames(y), form_full), as.formula)
    MM_f_full <- lapply(fmla_tmp, lme4::lmer, data = data, REML=useREML,contrasts = contrasts.arg.Values, control = lmerControl(optimizer = "bobyqa"))
    
    # LR
    loglikelihood_null <- sapply(MM_f_null, logLik, REML=useREML)
    
    loglikelihood_full <- sapply(MM_f_full, logLik, REML=useREML)
    
    ratio <- 2*(loglikelihood_full-loglikelihood_null)
    sumlog <- 2*(sum(loglikelihood_full - loglikelihood_null))
    
    # If the model is a linear model
  }else{
    # Estimate full models with simulate y
    contrasts.arg.Values <- createContrastsSum(form_full)
    fmla_tmp <- sapply(paste0(colnames(y), form_full), as.formula)
    lmerand <- lapply(fmla_tmp, lme4::lmer, data = data, REML=useREML, contrasts = contrasts.arg.Values, control = lmerControl(optimizer = "bobyqa"))
    
    for (i in 1:length(lmerand)){
      lmerand[[i]]@call[["data"]] <- data
      lmerand[[i]]@call[["contrasts"]] <- contrasts.arg.Values
    }
    
    # Fetch the log-likelihood of the null model 
    loglikelihood_null <- numeric()
    for (i in 1:length(lmerand)){
      loglikelihood_null[i]<-lmerTest::rand(lmerand[[i]])[2,2]
    }
    
    # Fetch the log-likelihood of the full model
    loglikelihood_full <- numeric()
    for (i in 1:length(lmerand)){
      loglikelihood_full[i]<-lmerTest::rand(lmerand[[i]])[1,2]
    }
    
    # LR
    ratio <- 2*(loglikelihood_full-loglikelihood_null)
    sumlog<- 2*(sum(loglikelihood_full - loglikelihood_null))
  }
  return(list(sumlog=sumlog, ratio=ratio))
}