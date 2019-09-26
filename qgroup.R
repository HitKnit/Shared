qgroup = function(numvec, n = 4){
  
  qtile = quantile(numvec, probs = seq(0, 1, 1/n))
  out = sapply(numvec, function(x) sum(x >= qtile[-(n+1)]))
  
  return(out)
}
