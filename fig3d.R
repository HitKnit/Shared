# Turns three vectors (x, y, z) into a 3D Plotly surface.

fig3d = function(x, y, z, labs = list(x = 'x', y = 'y', z = 'z'), title = NULL, type = 'surface'){
  
  require(akima)
  require(plotly)
  
  mat = interp(x, y, z, duplicate = 'mean')
  
  x = mat$x
  y = mat$y
  z = matrix(mat$z, nrow = length(mat$y), byrow = TRUE)
  
  text = paste0('<b>', labs$x, '</b>: ', round(rep(x, times = 40), 2),
               '<br><b>', labs$y, '</b>: ', round(rep(y, each = 40), 2),
               '<br><b>', labs$z, '</b>: ', round(t(z), 2)
  
               ) %>% matrix(40, 40, byrow = TRUE)
  
  p = plot_ly(x = x, y = y, z = z, type = type, hoverinfo = 'text', text = text) %>%
    
    config(displaylogo = F) %>%
    
    layout(title = title, 
           
           scene = list(xaxis = list(title = labs$x),
                        yaxis = list(title = labs$y),
                        zaxis = list(title = labs$z)
           )
    )
  
  return(p)
}


