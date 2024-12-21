#install.packages("sgr")
#install.packages("MonteCarloSEM")


library("ggplot2")
library("lavaan")
library("sgr")
library("MonteCarloSEM")
library("gridExtra")
source("function_cfa.R")
source("function_thresholds.R")
source("function_results.R")


#Tamaños muestrales y réplicas
n_sujetos_list <- c(250, 500, 750, 1000)
nreplicas <- 300

#lista para almacenar los resultados
all_results <- list()

# Definir la ubicación para guardar los datos
FileLoc <- "Ubicacion/para/guardar/datos"
data_loc <- FileLoc

# Modelo CFA
model <- '
  F1 =~ x1 + x2 + x3 + x4 + x5 + x6 + x7
  F2 =~ x8 + x9 + x10 + x11 + x12 + x13 + x14
  F3 =~ x15 + x16 + x17 + x18 + x19 + x20 + x21
  
  # Correlaciones entre factores
  F1~~F2
  F1~~F3
  F2~~F3
  
  # Varianza de los factores
  F1 ~~ 1*F1
  F2 ~~ 1*F2
  F3 ~~ 1*F3
'

#Correlaciones entre los factores
fc <- fcors.value(nf = 3, cors = c(
  1, 0.3, 0.3,  
  0.3, 1, 0.3,  
  0.3, 0.3, 1   
))

#Pesos factoriales
fl <- loading.value(nf = 3, fl.loads = c(
  # Factor 1
  0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  # Factor 2
  0, 0, 0, 0, 0, 0, 0,                
  0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,  
  0, 0, 0, 0, 0, 0, 0,   
  # Factor 3
  0, 0, 0, 0, 0, 0, 0,   
  0, 0, 0, 0, 0, 0, 0,   
  0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7  
))

for (nsujetos in n_sujetos_list) {          # Bucle que itera segun numero de sujetos
  for (i in 2:7) {                          # Bucle que itera según numero de categorias
    thres <- ft_thresholds(as.character(i))               # puntos de corte para las varaibles ordinales
    threshold_label <- paste0("thres_", i)   # Crear una etiqueta para cada condicion de categorias
    
    # Realizar la simulación
    sim.categoric(
      nd = nreplicas,
      ss = nsujetos,
      fcors = fc,
      loading = fl,
      f.loc = data_loc,
      threshold = thres,
      cont = "FALSE"
    )
    
    # Lista de todos los datos
    data_files <- paste0(data_loc, "/C_Data_", 1:nreplicas, ".dat")
    
    # Llamada a la funcion ft_cfa donde tiene incorporado la funcion cfa de lavaan
    results_ml <- ft_cfa(data_files, model, estimator = "MLR", fl, ordinal = F)
    results_dwls <- ft_cfa(data_files, model, estimator = "DWLS", fl, ordinal = T)
    results_uls <- ft_cfa(data_files, model, estimator = "ULS", fl, ordinal = T)
    
    # Almacenar los resultados
    all_results[[paste0("results_", nsujetos, "_sujetos_", threshold_label)]] <- list(
      ML = results_ml,
      DWLS = results_dwls,
      ULS = results_uls
    )
  }
}

all_results
