lmpSS = function(resLmpEffectMatrices,listcontrast){
computeSS = function(Xmat,L,coef){
  if(is.vector(L)){L=t(L)}
  LB = L %*% coef
  BL = t(LB)
  mat = BL %*% solve(L%*%solve(t(Xmat)%*%Xmat)%*%t(L)) %*% LB
  SS = sum(diag(mat))
  return(SS)
}

L = listcontrast

result = vector()
var_percentage = vector()

Y_withoutIntercept = resLmpEffectMatrices$lmpDataList$outcomes - resLmpEffectMatrices$effectMatrices$Intercept
denom = norm(x= data.matrix(Y_withoutIntercept),"F")^2

for(i in 1:length(L)){
  result[i]=computeSS(Xmat=resLmpEffectMatrices$modelMatrix,L=L[[i]],resLmpEffectMatrices$parameters)
  var_percentage[i] = (result[i]/denom)*100
}
result = c(result,((norm(x=resLmpEffectMatrices$residuals,"F")^2)/denom)*100)
var_percentage = var_percentage[2:length(L)]
var_percentage = c(var_percentage,((norm(x=resLmpEffectMatrices$residuals,"F")^2)/denom)*100)

names(result) = c(resLmpEffectMatrices$effectsNamesUnique,"Residuals")
names(var_percentage) = c(resLmpEffectMatrices$effectsNamesUnique[2:length(L)],"Residuals")

LMSS = list(type3SS=result,variationPercentages=var_percentage)
return(LMSS)
}
