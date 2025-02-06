
# Función para calcular el rango de edad
# Version 1: Cristian
# Version 1.1: Alejandro
#   Cambios: Ajustes de valores base y coeficientes
#            basados en la planilla de Becarios de UNC
#            URL: https://www.unc.edu.ar/sites/default/files/01_LISTADO%20DE%20BECARIOS%20BECAS%20INGRESANTES%20%202018.pdf
#



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

# Creamos nueva columna con rango de edad calculado para cada individuo
df_bcra$rango_edad_2019 <- sapply(df_bcra$proxy_edad_actual, calcular_rango_edad_2019)

# Mostramos un resumen de los resultados
print("Resumen de rangos de edad al 2019:")
print(table(df_bcra$rango_edad_2019))

# Mostramos algunas filas de ejemplo
print("\
Ejemplos de registros con el rango de edad al 2019:")
print(head(df_bcra[, c("proxy_edad_actual", "rango_edad_2019")]))