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


# Condiciones
n_sujetos_list <- c(250, 500, 750, 1000)
nreplicas <- 300
# Lista para almacenar los resultados
all_results <- list()

# Definir la ubicación para guardar los datos
FileLoc <- "C:Ubicacion/para/guardar/datos"
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

# Definir las correlaciones entre los factores
fc <- fcors.value(nf = 3, cors = c(
  1, 0.3, 0.3,  
  0.3, 1, 0.3,  
  0.3, 0.3, 1   
))

# Definir las cargas factoriales
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

ifN <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
fleis<-c(1.26094346,  0.25347641, -0.04950496)  #Esto es para que tenga asimetria +2

for (nsujetos in n_sujetos_list) {          
  for (i in 2:7) {                          
    thres <- ft_thresholds(as.character(i))              
    threshold_label <- paste0("thres_", i)   
    
    # Realizar la simulación, esta vez, con la funcion sim.skewed
    sim.skewed(
      nd = nreplicas,
      ss = nsujetos,
      fcors = fc,
      loading = fl,
      nonnormal = ifN,
      Fleishman = fleis,
      f.loc = FileLoc
    )
    
    #En cada iteracion debemos categorizar las variables
    categorize(f.loc = FileLoc, 
               threshold = thres)
    
    data_files <- paste0(data_loc, "/C", i, "_Data_", 1:nreplicas, ".dat")
    
    results_ml <- ft_cfa(data_files, model, estimator = "MLR", fl, ordinal = F)
    results_dwls <- ft_cfa(data_files, model, estimator = "DWLS", fl, ordinal = T)
    results_uls <- ft_cfa(data_files, model, estimator = "ULS", fl, ordinal = T)
    
    # Almacenar los resultados bajo el nombre del threshold y número de sujetos
    all_results[[paste0("results_", nsujetos, "_sujetos_", threshold_label)]] <- list(
      ML = results_ml,
      DWLS = results_dwls,
      ULS = results_uls
    )
  }
}

all_results
