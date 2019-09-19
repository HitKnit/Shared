# This function performs an operation (f) across multiple subsets (filters) of a data.table, 
# whereby a joined data.table on a particular variable (on) is returned.

# In essence, it performs dt[n, v = f(), by = z] across n restrictions and returns a data.table containing v(x1), v(x2) ... v(xn) by = z.

# Example: You have a dt of (i) stock returns, (ii) capitalisation identifier and (iii) date. 
# Say you want to calculate mean daily returns across several particular subsets of the stock returns. 
# Usage: dt_filterfunc(dt, filter = list(quote(cap == midcap), quote(cap == largecap & ret > 0.02)), f = quote(mean(ret)), by = date)

dt_filterfunc = function(dt, restr, fun, by, restr_labs = NULL) {
  
  require(data.table)

  dtlist = lapply(restr, function(x) dt[eval(x), eval(f), keyby = by])
  master = Reduce(function(...) merge(..., all = TRUE, by = by), dtlist)
  
  if (is.null(restr_labs)) {
    colnames(master) = c(by, paste(restr))
  } else {
    colnames(master) = c(by, restr_labs)
  }

  return(master)
}
  
