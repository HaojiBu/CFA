ft_cfa <- function(data_files, model, estimator = "MLR", load_simulated, ordinal = FALSE) {
  
  # Inicializar matrices para índices de ajuste y cargas factoriales
  fit_indices <- matrix(NA, nrow = length(data_files), ncol = 4)  # RMSEA, SRMR, CFI, TLI
  load_estimate <- matrix(NA, nrow = length(data_files), ncol = length(load_simulated))  # Cargas factoriales
  arb_values <- numeric(length(data_files))  
  
  # Determinar índices de ajuste según el tipo de datos
  index_names <- if (!ordinal) c("rmsea", "srmr", "cfi.scaled", "tli.scaled") else c("rmsea", "srmr", "cfi", "tli")
  
  # Bucle para ajustar el modelo a cada archivo de datos
  for (i in seq_along(data_files)) {
    # Leer cada archivo
    data_sim <- tryCatch(
      read.table(data_files[i]),
      error = function(e) {
        warning(paste("Error al leer el archivo:", data_files[i]))
        return(NULL)
      }
    )
    
    # Debug
    if (is.null(data_sim)) next
    
    data_sim <- data_sim[, -1]  # Eliminar la primera columna porque solo contiene enumeracion de vbs
    colnames(data_sim) <- paste0("x", 1:ncol(data_sim)) 
    
    # Ajustar el modelo según el tipo de datos
    fit <- tryCatch({
      if (!ordinal) {
        lavaan::cfa(model = model, data = data_sim, estimator = estimator)
      } else {
        lavaan::cfa(model = model, data = data_sim, ordered = colnames(data_sim), estimator = estimator)
      }
    }, error = function(e) {
      warning(paste("Error al ajustar el modelo para el archivo:", data_files[i]))
      return(NULL)
    })
    
    # Otro debug
    if (is.null(fit)) next
    
    # Extraer índices de ajuste
    indices <- lavaan::fitMeasures(fit, index_names)
    fit_indices[i, ] <- indices
    
    # Extraer pesos factoriales 
    loadings <- lavaan::inspect(fit, what = "std")$lambda
    loadings_matrix <- as.matrix(loadings)
    load_estimate[i, ] <- loadings_matrix
    arb_value <- arb(load_estimate[i, , drop = FALSE], load_simulated)  
    arb_values[i] <- arb_value
  }
  
  # Calcular promedios
  mean_arb <- mean(arb_values, na.rm = TRUE)
  index_means <- colMeans(fit_indices, na.rm = TRUE)
  
  # Crear un data.frame con los resultados finales
  results <- data.frame(
    RMSEA = index_means[1],
    SRMR = index_means[2],
    CFI = index_means[3],
    TLI = index_means[4],
    ARB = mean_arb
  )
  
  return(results)
}
