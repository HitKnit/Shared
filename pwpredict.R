pwpredict = function(model, test, type, split, match.levels = FALSE, replacement = NULL) {
  
  require(data.table)
  
  if(match.levels == TRUE){
    
    model_factors = names(model$xlevels)
    factor_col_ids = sapply(model_factors, function(x) which(colnames(test) == x))
  
    test_levels = sapply(factor_col_ids, function(x) c(unique(test[, x, with = FALSE])))
    model_levels = sapply(model$xlevels, function(x) c(x))

    remove = list()

    
    for(i in 1:length(factor_col_ids)) {
        remove[[i]] = as.character(test_levels[[i]][!test_levels[[i]] %in% model_levels[[i]]])
    }
    
    names(remove) = names(model_levels)
    
    rows = c()
    signal = NULL
    
    for(i in 1:length(remove)){
      
      if(length(remove[[i]]) > 0){
        signal = 1
        col_index = which(colnames(test) == names(remove)[i])
        rows_new = sapply(remove[[i]], function(x) which(test[, col_index, with = F] == x))
        rows = rbindlist(list(data.table(rows), data.table(rows_new)))
      }
      
    }
    
    if(is.null(signal)){print('No missing factors, please set match.levels = F'); return()}
    
    
    rows_replace = unique(as.numeric(unlist(rows)))
    rows = NULL
    rows_new = NULL
    remove = NULL
    test_levels = NULL
    model_levels = NULL

    gc()
    
    test_orig = nrow(test)
    test = test[-rows_replace, ]

  }
  
  total_len = nrow(test)
  chunk_size = floor(nrow(test) / split)
  vals = c(0, sapply(1:split, function(x) x*chunk_size))
  
  pred = data.table()
  
  for (i in 1:split){
    
    rows = c(1+vals[i], vals[i+1])
    new_preds = data.table(predict(model, type = type, test[rows[1]:rows[2],]))
    pred = rbindlist(list(pred, new_preds))
    
    str = paste(round(100*i/split, 2), '%', sep = "")
    print(str)
  }
  
  if (max(vals) < total_len) {
    
    diff = total_len - max(vals)
    index = tail(1:total_len, diff)
    pred = rbindlist(list(pred, data.table(predict(model, type = type, test[index,]))))
    
  }
  
  gc()
  
  if(match.levels == TRUE){
    out = rep(NA, test_orig)
    out[-rows_replace] = as.numeric(unlist(pred))
    out[rows_replace] = replacement
    
    return(data.table(out))
  }
  
  else{return(pred)}
  
}
