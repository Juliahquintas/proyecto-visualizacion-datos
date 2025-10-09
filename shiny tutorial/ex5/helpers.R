# helpers.R

# Este script contiene funciones auxiliares para calcular rangos y colores.

library(maps)
library(mapproj)

percent_map <- function(var, color, legend, min = 0, max = 100) {
  # Verificar límites de variable
  nclr <- 8
  bins <- seq(min, max, length.out = nclr + 1)
  cutvar <- cut(var, bins, include.lowest = TRUE)
  
  # Generar paleta
  palette <- colorRampPalette(c("white", color))(nclr)
  
  # Graficar mapa
  map("county", col = palette[cutvar], fill = TRUE, resolution = 0,
      lty = 0, projection = "polyconic")
  
  # Agregar leyenda
  legend("bottomleft",
         legend = paste(round(bins[-length(bins)]), "%–", round(bins[-1]), "%"),
         fill = palette, bty = "n", title = legend)
}
