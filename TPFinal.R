# TP Final IDM - trabajo


#############################################################
# Pendientes al código: 
# 1) Frecuencia de variables categóricas para cada valor
# 2) Ver codificación: listo -> números enteros del 1 al 5 - 0 para NA
# 3) Dejarlas como variables numéricas (para correlaciones)
# 4) tipo_persona 23 y 24 -> desconocido (codificación 0, 1 y 2)
# 5) Reemplazar rango_edad_2019 por codificación de naturales y 0
# 6) Dejar el dataframe inicial intacto y todas las modificaciones 
#    hacerlas en el segundo dataframe, el de trabajo
#############################################################



# Importar bibliotecas

knitr::opts_chunk$set(echo = TRUE)

listofpackages <- c("corrplot", "dplyr", "FactoMineR", "factoextra", 
                    "ggfortify", "ggplot2", "grid", "gridExtra", "gtExtras", 
                    "kableExtra", "knitr", "pastecs", "psych", "readr", "tidyverse")
# Cita de Código
# Código tomado de: https://github.com/lkovalevski/textsimilaritiesinR/tree/691b5798553d81a86b6c151557c4a667f8c58643/ejecutarAnalisisTexto.R
# Autor: lkovalevski
#
newPackages <- listofpackages[ !(listofpackages %in% installed.packages()[, "Package"])]
if(length(newPackages)) install.packages(newPackages)
for (paquete in listofpackages) {
  suppressMessages(library(paquete, character.only = TRUE))
}

# Abrimos el archivo que se encuentra en la carpeta datos_entrada
archivo_datos <- readRDS("./datos_entrada/df_bcra_individuals.rds")

# Describimos el dataset
t(summary(archivo_datos))
str(archivo_datos)
t(head(archivo_datos))

# Vemos las columnas con datos nulos o faltantes
columnas_con_nulos <- archivo_datos %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "columna", values_to = "num_nulos") %>%
  filter(num_nulos > 0)

print(columnas_con_nulos)

# Nos aseguramos que las columnas numéricas solo contienen datos numéricos
archivo_datos <- archivo_datos %>%
    mutate(across(where(is.numeric), ~ as.numeric(as.character(.))))




#############################################################
# Realmente necesito que estén definidas como categóricas si para el análisis CPA 
# deben ser numéricas ordinales?
# Hacemos todas las modificaciones en otro dataframe, no el original
#############################################################




# Nos aseguramos que las columnas especificadas sean de tipo categórico
archivo_datos <- archivo_datos %>%
    mutate(across(c(tipo_persona, n_deudas_actual, situacion_mes_actual, tiene_garantia_actual, 
                   max_situacion_mes, max_sit_mes_con_garantia, max_sit_mes_sin_garantia, 
                   peor_situacion_respuesta, mora_mayor_30_dias, default), as.factor))

# Verificamos la estructura del dataset después de las conversiones
str(archivo_datos)

#############################################################
#
# Verificamos la cantidad de registros que no cumplen la condición del enunciado
#
#    Con respecto al trabajo final les queríamos avisar que si bien en el
#    enunciado dice que la muestra utilizada está compuesta por cuits cuyo 
#    "monto total adeudado en ese momento no superaba los 100.000 pesos 
#    argentinos.", en la base quedaron algunos casos (1705) que sí tienen 
#    deuda total en junio 2019 mayor a 100.000.
# 
#############################################################

cant <- sum(archivo_datos$deuda_total_actual > 100, na.rm = TRUE)
print(paste("Cantidad de registros estrictamente mayores a 100:", cant))

cant <- sum(archivo_datos$deuda_total_actual >= 100, na.rm = TRUE)
print(paste("Cantidad de registros mayores o iguales a 100:", cant))

#############################################################
# Y porque los números están redondeados a miles, voy a filtrar los valores
# mayores o iguales a 100 en el campo deuda_total_actual, luego de 
# verificar que a esa condición 1705 registros la cumplen.
#############################################################

# Filtramos filas con deuda_total_actual mayor a 100
filas_con_deuda_mayor_100 <- archivo_datos %>%
    filter(deuda_total_actual >= 100)

# Excluir registros con deuda_total_actual mayor a 100
df_seleccionados <- archivo_datos %>%
    filter(deuda_total_actual < 100)

# Vemos las filas con deuda_total_actual mayor a 100
print(filas_con_deuda_mayor_100)

# Eliminar la columna id_individuo
df_seleccionados <- df_seleccionados %>%
  select(-id_individuo)

# Hacemos un análisis exploratorio inicial de las variables
#gt_plt_summary(df_seleccionados, title="Figura 1. Análisis exploratorio inicial de las variables en conjunto excluyendo deudas mayores a 100000")

# Vemos las columnas con datos nulos o faltantes
columnas_con_nulos <- df_seleccionados %>%
    summarise_all(~ sum(is.na(.))) %>%
    pivot_longer(cols = everything(), names_to = "columna", values_to = "num_nulos") %>%
    filter(num_nulos > 0)

print(columnas_con_nulos)


#############################################################
# Creamos una nueva categoría [0: No aplica] para la variable max_sit_mes_con_garantia para aquellos 
# registros que no tienen deuda con garantía y tienen valor NA
#############################################################


# Asignamos 0 a max_sit_mes_con_garantia si deuda_con_garantia_actual es 0 y max_sit_mes_con_garantia es NA
df_seleccionados <- df_seleccionados %>%
    mutate(max_sit_mes_con_garantia = ifelse(is.na(max_sit_mes_con_garantia) & deuda_con_garantia_actual == 0, 0, max_sit_mes_con_garantia))

# Volver a ver las columnas con datos nulos o faltantes
columnas_con_nulos <- df_seleccionados %>%
    summarise_all(~ sum(is.na(.))) %>%
    pivot_longer(cols = everything(), names_to = "columna", values_to = "num_nulos") %>%
    filter(num_nulos > 0)

print(columnas_con_nulos)

# Filtramos y seleccionamos las columnas deseadas para registros con max_sit_mes_sin_garantia = NA
registros_con_na <- df_seleccionados %>%
    filter(is.na(max_sit_mes_sin_garantia)) %>%
    select(deuda_total_actual, deuda_con_garantia_actual, prop_con_garantia_actual, tiene_garantia_actual, max_sit_mes_sin_garantia)

# Vemos los registros filtrados
print(registros_con_na)

# Asignamos 0 a max_sit_mes_sin_garantia si prop_con_garantia_actual es 1 y max_sit_mes_sin_garantia es NA
df_seleccionados <- df_seleccionados %>%
    mutate(max_sit_mes_sin_garantia = ifelse(is.na(max_sit_mes_sin_garantia) & prop_con_garantia_actual == 1, 0, max_sit_mes_sin_garantia))

# Volver a ver las columnas con datos nulos o faltantes
columnas_con_nulos <- df_seleccionados %>%
    summarise_all(~ sum(is.na(.))) %>%
    pivot_longer(cols = everything(), names_to = "columna", values_to = "num_nulos") %>%
    filter(num_nulos > 0)

print(columnas_con_nulos)


#############################################################
# Cita de código
## Licencia: desconocida
## Autor: Arena, Cristian
## Actualización: Ajuste con parámetros de corte y coeficientes propios
#############################################################


# Función para calcular el rango de edad
calcular_rango_edad_2019 <- function(proxy_edad_actual) {
  if (is.na(proxy_edad_actual)) {
    return(NA)
  }
  
  # Calculamos año de nacimiento
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
  
  # Calculamos edad en 2019
  edad_2019 <- 2019 - anio_nacimiento
  
  # Asignamos rango de edad
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

# Creamos nueva columna con rango de edad calculado para cada individuo
df_seleccionados$rango_edad_2019 <- sapply(df_seleccionados$proxy_edad_actual, calcular_rango_edad_2019)

# Mostramos un resumen de los resultados
print("Resumen de rangos de edad al 2019:")
print(table(df_seleccionados$rango_edad_2019))

# Mostramos algunas filas de ejemplo
print("Ejemplos de registros con el rango de edad al 2019:")
print(head(df_seleccionados[, c("proxy_edad_actual", "rango_edad_2019")]))

# Aseguramos que la columna rango_edad_2019 sea de tipo categórico
df_seleccionados <- df_seleccionados %>%
    mutate(rango_edad_2019 = as.factor(rango_edad_2019))

# Hacemos un análisis exploratorio inicial de las variables
#gt_plt_summary(df_seleccionados, title="Figura 2. Análisis exploratorio inicial de las variables en conjunto excluyendo deudas mayores a 100000, sin valores nulos")



# Análisis univariado

df2 <- pastecs::stat.desc(df_seleccionados) %>% as.matrix %>% as.data.frame %>% round(2)

df2 <- format(df2, scientific = FALSE)

df2 <- df2 %>%
    select_if(is.numeric)

knitr::kable(t(data.frame(df2)), digits = 2)

# Utilizamos la función summary para describir la distribución univariada de cada columna
summary(df_seleccionados)

# Alternativa A

# Función para crear gráficos de distribución
plot_distribution <- function(data, column) {
  if (is.numeric(data[[column]])) {
    ggplot(data, aes_string(x = column)) +
      geom_histogram(binwidth = 10, fill = "blue", color = "black") +
      labs(title = paste("Distribución de", column), x = column, y = "Frecuencia")
  } else if (is.factor(data[[column]])) {
    ggplot(data, aes_string(x = column)) +
      geom_bar(fill = "blue", color = "black") +
      labs(title = paste("Distribución de", column), x = column, y = "Frecuencia")
  }
}

# Alternativa B

library(MASS)     # Permite calcular el número optimo de bins


#############################################################
# Calculamos el número óptimo de bins (intervalos) para el histograma utilizando 
# la regla de Freedman-Diaconis a través de la función nclass.FD del paquete MASS

# La Regla de Freedman-Diaconis es un método para determinar el tamaño óptimo de 
# los bins (intervalos) en un histograma. Busca un balance entre precisión y suavidad
# en la distribución de los datos, evitando histogramas con demasiados o muy pocos bins.

# Fórmula de Freedman-Diaconis
# Bin width=2×IQRn1/3
# Bin width=2×n1/3IQR​

# Donde:

#    IQR = Rango intercuartílico (diferencia entre el cuartil 3 y el cuartil 1).
#    n = Número total de observaciones (tamaño de la muestra).
#    Bin width = Ancho del intervalo del histograma.

# 📌 ¿Por qué se usa el IQR?

#    Es robusto frente a valores atípicos (outliers).
#    Proporciona una mejor representación de la dispersión de los datos que la
#    desviación estándar.
#############################################################


# Función para crear gráficos de distribución
plot_distribution <- function(data, column) {
  if (is.numeric(data[[column]])) {
    # Calculamos el número óptimo de bins usando la regla de Freedman-Diaconis
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

# Creamos una lista de gráficos de distribución para cada columna
plots <- lapply(colnames(df_seleccionados), function(column) {
  plot_distribution(df_seleccionados, column)
})

# Presentamos los gráficos en una matriz
do.call(grid.arrange, c(plots, ncol = 6))

# Función para crear boxplots
plot_boxplot <- function(data, column) {
  ggplot(data, aes_string(y = column)) +
    geom_boxplot(fill = "blue", color = "black") +
    labs(title = paste("Boxplot de", column), y = column)
}

# Creamos una lista de gráficos de boxplot para cada columna numérica
boxplots <- lapply(names(df_seleccionados), function(column) {
  if (is.numeric(df_seleccionados[[column]])) {
    plot_boxplot(df_seleccionados, column)
  }
})

# Filtramos los elementos NULL de la lista
boxplots <- Filter(Negate(is.null), boxplots)

# Presentamos los gráficos en una matriz
do.call(grid.arrange, c(boxplots, ncol = 6))

# Outliers

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

# Identificamos outliers para cada columna numérica
outliers_list <- lapply(names(df_seleccionados), function(column) {
  if (is.numeric(df_seleccionados[[column]])) {
    outliers <- identify_outliers(df_seleccionados, column)
    if (nrow(outliers) > 0) {
      return(list(column = column, outliers = outliers))
    }
  }
  return(NULL)
})

# Filtramos los elementos NULL de la lista
outliers_list <- Filter(Negate(is.null), outliers_list)

# Mostramos los outliers identificados
for (outlier_info in outliers_list) {
  cat("Outliers en la columna:", outlier_info$column, "\n")
  print(outlier_info$outliers)
  cat("\n")
}

# Correlaciones


#############################################################
# Eliminamos las columnas categoricas no ordinales y hacer ordinales aquellas 
# que puedan serlo
#############################################################


# Filtramos las columnas numéricas del DataFrame
col_numericas <- df_seleccionados %>%
  select_if(is.numeric)

# Calculamos la matriz de correlaciones
matriz_corr <- cor(col_numericas, use = "complete.obs")

# Mostramos la matriz de correlaciones
print(matriz_corr)

# Visualizamos la matriz de correlaciones
corrplot(matriz_corr, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

# Correlaciones 2 

# Guardamos el gráfico en un archivo PNG
png("pairs_panels.png", width = 1200, height = 1200)
par(mar = c(2, 2, 2, 2))
options(repr.plot.width = 10, repr.plot.height = 8)  # Ajusta el ancho y alto

# Creamos el gráfico de pares
pairs.panels(
  col_numericas,
  method = "pearson",  # correlation
  hist.col = "#00AFBB",
  density = TRUE,      # show density
  ellipses = F     # show correlation ellipses
)

# Correlaciones 3

library(GGally)
ggpairs(col_numericas)  # Gráfico de pares alternativo

# PCA

# Realizar el análisis de componentes principales (PCA)
pca_result <- prcomp(col_numericas, scale. = TRUE)

# Resumen del PCA
summary(pca_result)

# Porcentaje de la variabilidad total explicada por las dos primeras componentes
var_explicada <- summary(pca_result)$importance[2, 1:2]
cat("Porcentaje de la variabilidad total explicada por las dos primeras componentes:\n")
print(var_explicada)

# Creamos un DataFrame con las componentes principales
pca_df <- as.data.frame(pca_result$x)

# Añadimos la columna tipo_persona al DataFrame de PCA
pca_df <- pca_df %>%
  mutate(tipo_persona = df_seleccionados$tipo_persona)

# Visualizamos las dos primeras componentes principales
ggplot(pca_df, aes(x = PC1, y = PC2, color = tipo_persona)) +
  geom_point(alpha = 0.7) +
  labs(title = "PCA: Primeras dos componentes principales",
       x = "Componente Principal 1",
       y = "Componente Principal 2") +
  theme_minimal()

# Interpretación de los componentes
cat("Cargas de las variables en las dos primeras componentes:\n")
print(pca_result$rotation[, 1:2])


