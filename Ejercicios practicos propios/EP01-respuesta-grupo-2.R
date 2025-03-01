# Grupo N�2

# Integrantes:
# Christofer Rodriguez
# Christian Mendez
# Israel Arias

#Se importa la librer�a
library(dplyr)

#Actividad 1
datos <- read.csv2("F:\\Descargas\\EP01 Datos Covid.csv",check.names = FALSE)


#2. Importar el conjunto de datos al entorno de programaci�n R y responder las siguientes preguntas:
#  . �Qu� variables se han cargado?
# R: Se han cargado regiones y cantidad de contagiados por fecha.

#  . �Qu� tipo tiene cada una de estas variables?
# R: Regi�n: Categorica Nominal.
#    Fechas: N�mericas discretas

#  . �Qu� escala parecen tener estas variables?
# R: Regi�n: Son de escala nominal, debido a que una jerarqu�a establecida en las regiones.
#   Fechas: Escala de raz�n, debido a que tiene su origen en un cero que viene siendo que no hay contagiados ese d�a.

#3. Asegurar que las variables tienen el tipo correcto en la tabla de datos (data frame) cargada
# R: Los datos corresponden al tipo de dato adecuado.

#4. Utilizando herramientas de R (se recomienda usar el paquete dplyr) responda las preguntas asignadas a su grupo.

#Grupo 2:
#  1. �Qu� d�a se produjo el mayor n�mero de casos con s�ntomas en la regi�n de Tarapac� entre el 01-mar-2021 y el 31-ago-2021?

# Seleccionar los datos de Tarapac�
tarapaca <- datos %>% filter ( Region == "Tarapac�")
#Se seleccionan los datos entre las fechas indicadas
tarapacaFechas <- tarapaca %>% select("01-03-2021":"31-08-2021")
# Se busca la columna que tenga el mayor valor, se extrae el nombre de la columna
maxTarapaca <- colnames(tarapacaFechas)[apply(tarapacaFechas,1,which.max)]
#Se imprime el d�a con mayor n�mero  de casos con sintomas por consola
cat("El d�a con mayor n�mero de casos con s�ntomas en la regi�n de Tarapac� es: ",maxTarapaca)


#  2. �Cu�l fue el total de casos con s�ntomas para cada mes de este periodo?

#Se realiza la suma de casos con s�ntomas para cada mes del periodo
sumaMarzo <- rowSums(tarapacaFechas %>% select("01-03-2021": "31-03-2021"))
sumaAbril <- rowSums(tarapacaFechas %>% select("01-04-2021": "30-04-2021"))
sumaMayo <- rowSums(tarapacaFechas %>% select("01-05-2021": "31-05-2021"))
sumaJunio <- rowSums(tarapacaFechas %>% select("01-06-2021": "30-06-2021"))
sumaJulio <- rowSums(tarapacaFechas %>% select("01-07-2021": "31-07-2021"))
sumaAgosto <- rowSums(tarapacaFechas %>% select("01-08-2021": "31-08-2021"))

# Se crea el vector de meses
meses <- c("Marzo 2021", "Abril 2021", "Mayo 2021", "Junio 2021", "Julio 2021", "Agosto 2021")
#Se crea el vector de suma
totales <- c(sumaMarzo, sumaAbril, sumaMayo, sumaJunio, sumaJulio, sumaAgosto)
#Se crea el dataframe resultante
totalesPorMes <- data.frame(meses, totales)
#Se imprime el dataframe por consola
totalesPorMes