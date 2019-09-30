wageqtile = function(q1, q2, q3, offer, n = 500000){
  
  require(data.table)
  
  # Log wage quartiles
  qs = c(q1, q2, q3)
  logq = sapply(qs, log)
  
  # Estimate mean and SD from Wan et al. (2014): https://stats.stackexchange.com/a/256496/237398
  meanlog = sum(logq)/3 
  sdlog = diff(range(logq))/1.35
  
  # Generate (log) wage distribution
  wages = data.table(LogWage = rnorm(n = n, mean = meanlog, sd = sdlog))
  wages[, Wage := exp(LogWage)]
  
  # Obtain offer percentile:
  perc = round(wages[offer >= Wage, .N]/wages[, .N], 3)*100
  
  # Plot
  wages[, lapply(.SD, function(x){
    
    d = density(x)
    qtile = quantile(x)

    plot(d$x, d$y, lwd = 3, col = 'cornflowerblue', t = 'l',
         xlab = 'Monthly Wage Distribution', ylab = NA, main = 'Distributional Properties of Estimated Wage')
    abline(v = offer, col = 'firebrick', lwd = 2, lty = 2)
    mtext(paste('Wage Quartiles'), line = - 2, adj = .95, cex = 0.7)
    mtext(paste(names(qtile)[2:4], collapse = " - "), line= - 3, adj = .95, cex = 0.7)
    mtext(paste('Real: ', paste(qs, collapse = ' - ')), line= - 4, adj = .95, cex = 0.7, col = 'darkgreen')
    mtext(paste('Sim: ', paste(round(unname(qtile[2:4])), collapse = ' - ')), line= - 5, adj = .95, cex = 0.7, col = 'darkblue')
    mtext(paste('Monthly Offer: ', offer), line = -7, adj = .95, col = 'firebrick', cex = 0.7)
    mtext(paste('Percentile:', perc, '%'), line = -8, adj = .95, col = 'firebrick', cex = 0.7)
 
     }), .SDcols = 'Wage']
}
