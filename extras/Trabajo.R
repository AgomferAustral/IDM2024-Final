# TP Final IDM

# Importar bibliotecas
library(knitr)        # Documentos dinámicos y reproducibles
library(kableExtra)   # Mejora de tablas con knitr
library(tidyverse)    # Manipulación y visualización de datos
library(gtExtras)     # Mejoras para tablas con gt
library(readr)        # Lectura de archivos CSV y otros formatos
library(dplyr)        # Manipulación de datos
library(ggplot2)      # Generacion de graficos
library(gridExtra)    # Combina graficos en una visualizacion
library(corrplot)     # Visualización de correlaciones

# Abrir el archivo que se encuentra en la carpeta datos_entrada
archivo_datos <- readRDS("./datos_entrada/df_bcra_individuals.rds")

# Describir el dataset
summary(archivo_datos)
str(archivo_datos)
head(archivo_datos)

# Ver las columnas con datos nulos o faltantes
columnas_con_nulos <- archivo_datos %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "columna", values_to = "num_nulos") %>%
  filter(num_nulos > 0)

print(columnas_con_nulos)

# Asegurar que las columnas numéricas solo contienen datos numéricos
archivo_datos <- archivo_datos %>%
  mutate(across(where(is.numeric), ~ as.numeric(as.character(.))))

# Asegurar que las columnas especificadas sean de tipo categórico
archivo_datos <- archivo_datos %>%
  mutate(across(c(tipo_persona, n_deudas_actual, situacion_mes_actual, tiene_garantia_actual, 
                  max_situacion_mes, max_sit_mes_con_garantia, max_sit_mes_sin_garantia, 
                  peor_situacion_respuesta, mora_mayor_30_dias, default), as.factor))

# Verificar la estructura del dataset después de las conversiones
str(archivo_datos)

# Filtrar filas con deuda_total_actual mayor a 100
filas_con_deuda_mayor_100 <- archivo_datos %>%
  filter(deuda_total_actual > 100)

# Excluir registros con deuda_total_actual mayor a 100
df_seleccionados <- archivo_datos %>%
  filter(deuda_total_actual <= 100)

# Ver las filas con deuda_total_actual mayor a 100
print(filas_con_deuda_mayor_100)

# Eliminar la columna id_individuo
df_seleccionados <- df_seleccionados %>%
  select(-id_individuo)

# Hacer un analisis exploratorio inicial de las variables
# gt_plt_summary(df_seleccionados, title="Figura 1. Análisis exploratorio inicial de las variables en conjunto excluyendo deudas mayores a 100000")

# Ver las columnas con datos nulos o faltantes
columnas_con_nulos <- df_seleccionados %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "columna", values_to = "num_nulos") %>%
  filter(num_nulos > 0)

print(columnas_con_nulos)

# Asignar 0 a max_sit_mes_con_garantia si deuda_con_garantia_actual es 0 y max_sit_mes_con_garantia es NA
df_seleccionados <- df_seleccionados %>%
  mutate(max_sit_mes_con_garantia = ifelse(is.na(max_sit_mes_con_garantia) & deuda_con_garantia_actual == 0, 0, max_sit_mes_con_garantia))

# Volver a ver las columnas con datos nulos o faltantes
columnas_con_nulos <- df_seleccionados %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "columna", values_to = "num_nulos") %>%
  filter(num_nulos > 0)

print(columnas_con_nulos)

# Asignar 0 a max_sit_mes_sin_garantia si prop_con_garantia_actual es 1 y max_sit_mes_sin_garantia es NA
df_seleccionados <- df_seleccionados %>%
  mutate(max_sit_mes_sin_garantia = ifelse(is.na(max_sit_mes_sin_garantia) & prop_con_garantia_actual == 1, 0, max_sit_mes_sin_garantia))

# Volver a ver las columnas con datos nulos o faltantes
columnas_con_nulos <- df_seleccionados %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "columna", values_to = "num_nulos") %>%
  filter(num_nulos > 0)

print(columnas_con_nulos)

# Función para calcular el rango de edad
calcular_rango_edad_2019 <- function(proxy_edad_actual) {
  if (is.na(proxy_edad_actual)) {
    return(NA)
  }
  
  # Calcular año de nacimiento
  if (proxy_edad_actual >= 500) {
    anio_nacimiento <- 2010 + floor((proxy_edad_actual - 500)/10)
  } else if (proxy_edad_actual >= 424) {
    anio_nacimiento <- 2000 + floor((proxy_edad_actual - 424)/7.6)
  } else if (proxy_edad_actual >= 284) {
    anio_nacimiento <- 1980 + floor((proxy_edad_actual - 284)/7)
  } else if (proxy_edad_actual >= 217) {
    anio_nacimiento <- 1970 + floor((proxy_edad_actual - 217)/6.7)
  } else if (proxy_edad_actual >= 142) {
    anio_nacimiento <- 1960 + floor((proxy_edad_actual - 142)/7.5)
  } else if (proxy_edad_actual >= 45) {
    anio_nacimiento <- 1940 + floor((proxy_edad_actual - 45)/4.85)
  } else {
    anio_nacimiento <- 1920 + floor((proxy_edad_actual - 20) / 1.25)
  }
  
  # Calcular edad en 2019
  edad_2019 <- 2019 - anio_nacimiento
  
  # Asignar rango de edad
  if (edad_2019 < 20) {
    rango_edad <- "<20"
  } else if (edad_2019 <= 30) {
    rango_edad <- "20-29"
  } else if (edad_2019 <= 40) {
    rango_edad <- "30-39"
  } else if (edad_2019 <= 50) {
    rango_edad <- "40-49"
  } else if (edad_2019 <= 60) {
    rango_edad <- "50-60"
  } else {
    rango_edad <- ">60"
  }
  
  return(rango_edad)
}

# Crear nueva columna con rango de edad calculado para cada individuo
df_seleccionados <- df_seleccionados %>%
  mutate(rango_edad_2019 = sapply(proxy_edad_actual, calcular_rango_edad_2019))

# Mostrar un resumen de los resultados
cat("Resumen de rangos de edad al 2019:\n")
print(table(df_seleccionados$rango_edad_2019))

# Mostrar algunas filas de ejemplo
cat("Ejemplos de registros con el rango de edad al 2019:\n")
print(head(df_seleccionados[, c("proxy_edad_actual", "rango_edad_2019")]))

# Asegurar que la columna rango_edad_2019 sea de tipo categórico
df_seleccionados <- df_seleccionados %>%
  mutate(rango_edad_2019 = as.factor(rango_edad_2019))

# Hacer un análisis exploratorio inicial de las variables
# gt_plt_summary(df_seleccionados, title="Figura 2. Análisis exploratorio inicial de las variables en conjunto excluyendo deudas mayores a 100000, sin valores nulos")

# Análisis univariado
summary(df_seleccionados)

# Función para crear gráficos de distribución
plot_distribution <- function(data, column) {
  if (is.numeric(data[[column]])) {
    # Calcular el número óptimo de bins usando la regla de Freedman-Diaconis
    num_bins <- nclass.FD(data[[column]])
    ggplot(data, aes_string(x = column)) +
      geom_histogram(bins = num_bins, fill = "blue", color = "black") +
      labs(title = paste("Distribución de", column), x = column, y = "Frecuencia") +
      theme_minimal()
  } else if (is.factor(data[[column]])) {
    ggplot(data, aes_string(x = column)) +
      geom_bar(fill = "blue", color = "black") +
      labs(title = paste("Distribución de", column), x = column, y = "Frecuencia") +
      theme_minimal()
  }
}

# Crear una lista de gráficos de distribución para cada columna
plots <- lapply(colnames(df_seleccionados), function(column) {
  plot_distribution(df_seleccionados, column)
})

# Presentar los gráficos en una matriz
do.call(grid.arrange, c(plots, ncol = 6))

# Función para crear boxplots
plot_boxplot <- function(data, column) {
  ggplot(data, aes_string(y = column)) +
    geom_boxplot(fill = "blue", color = "black") +
    labs(title = paste("Boxplot de", column), y = column) +
    theme_minimal()
}

# Crear una lista de gráficos de boxplot para cada columna numérica
boxplots <- lapply(names(df_seleccionados), function(column) {
  if (is.numeric(df_seleccionados[[column]])) {
    plot_boxplot(df_seleccionados, column)
  }
})

# Filtrar los elementos NULL de la lista
boxplots <- Filter(Negate(is.null), boxplots)

# Presentar los gráficos en una matriz
do.call(grid.arrange, c(boxplots, ncol = 6))

# Función para identificar outliers usando el IQR
identify_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- data %>%
    filter(data[[column]] < lower_bound | data[[column]] > upper_bound)
  return(outliers)
}

# Identificar outliers para cada columna numérica
outliers_list <- lapply(names(df_seleccionados), function(column) {
  if (is.numeric(df_seleccionados[[column]])) {
    outliers <- identify_outliers(df_seleccionados, column)
    if (nrow(outliers) > 0) {
      return(list(column = column, outliers = outliers))
    }
  }
  return(NULL)
})

# Filtrar los elementos NULL de la lista
outliers_list <- Filter(Negate(is.null), outliers_list)

# Mostrar los outliers identificados
for (outlier_info in outliers_list) {
  cat("Outliers en la columna:", outlier_info$column, "\n")
  print(outlier_info$outliers)
  cat("\n")
}

# Calcular e interpretar la matriz de correlaciones de variables disponibles a Jun-2019
# (las posibles predictores de default en el período Jul-2019 a Jun-2020)

# Filtrar las columnas numéricas del DataFrame
col_numericas <- df_seleccionados %>%
  select(where(is.numeric))

# Calcular la matriz de correlaciones
matriz_corr <- cor(col_numericas, use = "complete.obs")

# Mostrar la matriz de correlaciones
print(matriz_corr)

# Visualizar la matriz de correlaciones
corrplot(matriz_corr, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")



#
# CPA
#

# Realizar el análisis de componentes principales (PCA)
pca_result <- prcomp(col_numericas, scale. = TRUE)

# Resumen del PCA
summary(pca_result)

# Porcentaje de la variabilidad total explicada por las dos primeras componentes
var_explicada <- summary(pca_result)$importance[2, 1:2]
cat("Porcentaje de la variabilidad total explicada por las dos primeras componentes:\n")
print(var_explicada)

# Crear un DataFrame con las componentes principales
pca_df <- as.data.frame(pca_result$x)

# Añadir la columna tipo_persona al DataFrame de PCA
pca_df <- pca_df %>%
  mutate(tipo_persona = df_seleccionados$tipo_persona)

# Visualizar las dos primeras componentes principales
ggplot(pca_df, aes(x = PC1, y = PC2, color = tipo_persona)) +
  geom_point(alpha = 0.7) +
  labs(title = "PCA: Primeras dos componentes principales",
       x = "Componente Principal 1",
       y = "Componente Principal 2") +
  theme_minimal()

# Interpretación de los componentes
cat("Cargas de las variables en las dos primeras componentes:\n")
print(pca_result$rotation[, 1:2])