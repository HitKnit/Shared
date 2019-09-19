plot.multi = function(mat, title = NULL, ylim = NULL, leg = 'topleft', type = 'l', cex = 1, cex.leg = 1){
  
  mat = as.xts.data.table(mat)
  
  rb = rainbow(n = (ncol(mat)), alpha = 0.6)
  plot.xts(mat, ylim = ylim, main = title, minor.ticks = T, col = rb, type = type, cex = cex, grid.col = 'lightgrey')
  
  if(!is.null(leg)) {addLegend(leg, lty = 1, lwd = 2, cex = cex.leg)}
  
}