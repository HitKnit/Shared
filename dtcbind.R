# Say you have a range of data.tables overlapping on a specific variable. This function cbinds the data.tables on a specified variable, 
# allowing for assignment of rows which are not recorded across the dtlist (NA_val).

# dt1 = data.table(
#   Date = seq(as.Date('2000-01-01'), as.Date('2000-01-10'), by = 1),
#   Return1 = rnorm(10)
# )
# 
# dt2 = data.table(
#   Date = seq(as.Date('2000-01-05'), as.Date('2000-01-10'), by = 1),
#   Return2 = rnorm(6)
# )
# 
# dt3 = data.table(
#   Date = seq(as.Date('2000-01-02'), as.Date('2000-01-8'), by = 1),
#   Return3 = rnorm(7)
# )
#
# Usage: dtcbind(list(dt1, dt2, dt3), on = 'Date')

dtcbind = function(dtlist, by, NA_val = NA){
  
  master = Reduce(function(...) merge(..., all = TRUE, by = by), dtlist)
  
  if(!is.na(NA_val)){
      cols = colnames(master)[-1]
      master[, (cols) := lapply(.SD, function(x) {x[which(is.na(x))] = NA_val; return(x)}), .SDcols = cols]
  }
  
  return(master)
}
