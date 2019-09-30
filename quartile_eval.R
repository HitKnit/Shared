quartile_eval = function(svec, AR, yield, principal, title = NULL){
  
  dt = data.table(svec, AR, yield, principal)
  sharpe_qtile = dt[, quantile(svec)]
  
  dt[, qgroup := sapply(svec, function(x) sum(x >= sharpe_qtile[-5]))]

  texts = c('Fourth', 'Third', 'Second', 'First')
  xr = dt[, range(AR)]
  
  par(mfrow = c(4,1), mar = c(2,1,1,1), oma = c(2,0,2,0))
  
  for(i in (4:1)){
    
    dtt = dt[qgroup == i]
    
    d = dtt[, density(AR)]
    dtt[, plot(d$x, d$y, xlim = xr, t = 'l', col = 'coral3', lwd = 2.5, yaxt = 'n')]
    dtt[, abline(v = mean(yield), col = 'cornflowerblue', lty = 2, lwd = 1.5)]
    dtt[, abline(v = mean(AR), col = 'darkgreen', lty = 2, lwd = 1.5)]
    dtt[, mtext(paste(texts[i], 'Sharpe Quartile'), padj = 2, adj = 0.01, cex = 0.8)]
    dtt[, mtext(paste('Wtd. Yield: ', round(weighted.mean(yield, principal), 2), '%', sep = ''), padj = 4.5, adj = 0.01, cex = 0.6, col = 'cornflowerblue')]
    dtt[, mtext(paste('Wtd. AR: ', round(weighted.mean(AR, principal), 2), '%', sep = ''), padj = 6, adj = 0.01, cex = 0.6, col = 'darkgreen')]
    dtt[, mtext(paste('Realised Sharpe: ', round(weighted.mean(AR, principal)/sqrt(var(AR)), 2)), padj = 7.5, adj = 0.01, cex = 0.6, col = 'darkred')]
    
  }
  title(outer = TRUE, main = list(title, font = 3))
  
}
