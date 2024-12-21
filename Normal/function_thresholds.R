ft_thresholds <- function(num_categories) {
  thresholds <- switch(num_categories,
                       "2" = c(-Inf, 0, Inf),
                       "3" = c(-Inf, -1.645, 0, 1.645, Inf), 
                       "4" = c(-Inf, -1.645, -0.5, 0.5, 1.645, Inf),
                       "5" = c(-Inf, -1.645, -.643, .643, 1.645, Inf),
                       "6" = c(-Inf, -1.645, -1.0, -0.25, 0.25, 1.0, 1.645, Inf),
                       "7" = c(-Inf, -1.645, -1.28, -0.67, 0.67, 1.28, 1.645, Inf),
                       stop("Número de categorías no soportado")  #Un debug por si acaso
  )
  return(thresholds)
}
