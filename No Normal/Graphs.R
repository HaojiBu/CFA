#### Representación Gráfica #####
library(ggplot2)
library("gridExtra")
source("function_results.R")
#load("ubicacion/de/los/datos/.Rdata")


all_results

results_250_sujetos <- ft_results(all_results, 250)
results_500_sujetos <- ft_results(all_results, 500)
results_750_sujetos <- ft_results(all_results, 750)
results_1000_sujetos <- ft_results(all_results, 1000)



# arb_ml <- results_250_sujetos$ML[ , 5]
# arb_dwls <- results_250_sujetos$DWLS[ , 5]
# arb_uls <- results_250_sujetos$ULS[ , 5]

#Para visualizar el resto de conficiones
# arb_ml <- results_500_sujetos$ML[ , 5]
# arb_dwls <- results_500_sujetos$DWLS[ , 5]
# arb_uls <- results_500_sujetos$ULS[ , 5]


# arb_ml <- results_750_sujetos$ML[ , 5]
# arb_dwls <- results_750_sujetos$DWLS[ , 5]
# arb_uls <- results_750_sujetos$ULS[ , 5]


arb_ml <- results_1000_sujetos$ML[ , 5]
arb_dwls <- results_1000_sujetos$DWLS[ , 5]
arb_uls <- results_1000_sujetos$ULS[ , 5]


categorias <- 2:7
df <- data.frame(
  Categories = rep(categorias, 3),
  ARB = c(arb_ml, arb_dwls, arb_uls),
  Method = rep(c("ML", "DWLS", "ULS"), each = length(categorias))
)

y_limits <- c(-7,4) # Se cambian los limites porque habia mas sesgo esta vez

# Primera gráfica: ML
plot_ml <- ggplot(df[df$Method == "ML", ], aes(x = Categories, y = ARB)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "ARB estimado por ML", x = "Número de Categorías", y = "ARB%") +
  coord_cartesian(ylim = y_limits) +
  theme_minimal()

# Segunda gráfica: DWLS
plot_dwls <- ggplot(df[df$Method == "DWLS", ], aes(x = Categories, y = ARB)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "ARB estimado por DWLS", x = "Número de Categorías", y = "ARB%") +
  coord_cartesian(ylim = y_limits) +
  theme_minimal()

# Tercera gráfica: ULS
plot_uls <- ggplot(df[df$Method == "ULS", ], aes(x = Categories, y = ARB)) +
  geom_line(color = "black") +
  geom_point(color = "black") +
  labs(title = "ARB estimado por ULS", x = "Número de Categorías", y = "ARB%") +
  coord_cartesian(ylim = y_limits) +
  theme_minimal()


grid.arrange(plot_ml, plot_dwls, plot_uls, nrow = 1)

