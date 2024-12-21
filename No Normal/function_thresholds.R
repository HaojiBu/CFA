
# Se modifican los puntos de corte en esta función para datos con asimetría +2
ft_thresholds <- function(num_categories) {
  thresholds <- switch(num_categories,
                       "2" = c(-Inf, 0.9485798 , Inf),           
                       "3" = c(-Inf, 0.4166745,  1.1402983, Inf),  
                       "4" = c(-Inf, 0.3066974, 0.6636925, 1.1402983 , Inf),
                       "5" = c(-Inf, 0.1655961, 0.4456566, 0.8280311, 1.2796361 , Inf), 
                       "6" = c(-Inf, 0.08679925, 0.37625604, 0.66369249, 1.02417713, 1.43928788 , Inf),
                       "7" = c(-Inf, -0.02698556,  0.23658597,  0.52021228,  0.82803115,  1.14029831,  1.64290825, Inf),
                       stop("Número de categorías no soportado")  # Debug
  )
  return(thresholds)
}
