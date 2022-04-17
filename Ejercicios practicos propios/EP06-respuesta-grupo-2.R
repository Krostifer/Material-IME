# Ejercicio práctico N°6

# Grupo N°2
# Integrantes:
# Christofer Rodriguez - Christian Méndez  - Israel Arias

#Importación de paquetes
if(!require(pwr)){
  install.packages("pwr",dependencies = TRUE)
  require(pwr)
}
if(!require(ggplot2)){
  install.packages("ggplot2",dependencies = TRUE)
  require(ggplot2)
}

#########################################################################
# 1. Estudios previos habían determinado que la proporción de autoras   #
# en la especialidad de obstetricia era de                              #
# 59%. ¿Respaldan estos datos tal estimación?                           #
#########################################################################

# H0: La proporción de autoras de la especilidad de obstetricia es de 59%
# Ha: La proporción de autoras de la especilidad de obstetricia no es 59%


# Fijar valores conocidos
 n <- 137 #Cantidad total de autores de articulos de obstetricia
 p_exito <- 71/137 #Probabilidad de escoger a una autora mujer
 alfa <- 0.05
 valor_nulo <- 0.59 # Valor nulo dado por estudio previo

 ### PRUEBA WILSON ####

 # Calcular cantidad de éxitos .
exitos <- p_exito * n
 
  # Prueba de Wilson en R.
prueba <- prop.test (exitos , n = n , p = valor_nulo ,
                            alternative = "two.sided", conf.level = 1 - alfa )
 
print (prueba)

# el valor de p = 0.1051 es mayor a alfa 0.05, entonces se falla en rechazar
# la hipotesis nula. En conclusión, los datos si respaldan que la proporción
# de autoras en la especialidad de obstretricia es de 59%.

#########################################################################
# 2. Según estos datos, ¿es igual la proporción de autoras en las áreas #
# de neurología y obstetricia?                                          #
#########################################################################

# H0: La proporción de autoras de las áreas de neurología y obstetricia son iguales
# Ha: La proporción de autoras de las áreas de neurología y obstetricia son distintos

# Fijar valores conocidos (neurología ,obstetricia)
n <-c(144 , 137)
exitos <- c(56 , 71)
alfa <- 0.05

 # Prueba de Wilson en R.
prueba2 <- prop.test (exitos , n = n , alternative = "two.sided",
                           conf.level = 1 - alfa)
print (prueba2)


# el valor de p = 0.040 es menor a alfa 0.05, entonces  rechaza
# la hipotesis nula en favor de la hipotesis alternativa.
# En conclusión, la proporción de autoras en las áreas de neurología
# y obstetricia es distinta.

#########################################################################
# 3. Suponiendo que la diferencia en la proporción de autoras en        #
# la especialidad de psiquiatría y la de neurología es de 0,25.         #
# ¿A cuántos autores deberíamos monitorear para obtener un intervalo    #
# de confianza del 95% y poder estadístico de 75%, si se intenta        #
# mantener aproximadamente la misma proporción de gente estudiada       #
# en cada caso?                                                         #
#########################################################################

alfa = 0.05
p1 <- 30/72#probabilidad siquiatria
p2 <- 56 /144
power.prop.test(n = NULL, p1 = p1, p2 =p2, sig.level = 1-alfa, power = 0.75,
                alternative = "two.sided")
bsamsize(p1,p2,72/(72+144),0.05,75)
