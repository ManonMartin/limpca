isresultfromPCA = function(resultPCA){

  cond1 = is.list(resultPCA)
  cond2 = length(resultPCA)==8
  cond3 = names(resultPCA)==c("scores","loadings","eigval","pcu","pcd","var","cumvar","original.dataset")

  return(all(cond1,cond2,cond3))
}
