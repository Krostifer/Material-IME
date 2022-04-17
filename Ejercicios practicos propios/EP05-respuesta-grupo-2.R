# Ejercicio práctico N°5

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
###############################################################################
# 1. Si el ingeniero está seguro de que el verdadero volumen medio no puede   #
# ser inferior a 10 litros y piensa rechazar la hipótesis nula cuando la      #
# muestra presente una media mayor a 10,3 litros, ¿cuál es la probabilidad de #
# que cometa un error de tipo I?                                              #
###############################################################################

n <- 100 #Cantidad de muestra
desv_est <- 1 #1 litro
media <- 10 #Media de 10 litros
#Se calcula el error estandar
es <- desv_est/sqrt(n)              #PREGUNTAR
#Se fija un valor de secuencia con 3 desviaciones estandar de la media, para abarcar el 99.7% de las observaciones
x <- seq (9.7, 10.4, 0.01)
y <- dnorm (x , mean = media , sd = es)
normal <- data.frame(x,y)
#Gráfico de la distribución
g <- ggplot (normal, aes (x , y )) + geom_line (color = "red")
g <- g + xlab("Volumen medio [L]")
g <- g + scale_y_continuous(breaks = NULL)
g <- g + scale_x_continuous(breaks = seq(9.7,10.4,0.1))
g <- g + geom_vline ( xintercept = media, linetype = "dashed")



g <- g + geom_area ( data = subset (data.frame(x,y), x > 10.3) ,
                     aes ( y = y ) ,
                     colour = "red",
                     fill = "red",
                     alpha = 0.1)

g <- g + annotate("text", x = 10.35, y = 0.6, label = "Área de rechazo ->")

print(g)


#La probabilidad de cometer un error de tipo I esta dada por el valor de alfa que es igual
# a la probabilidad de tomar un valor dentro del area de rechazo de H0, o sea
# alfa = P(x > 10.3)
probMayor10_3 <- pnorm(10.3, media, es, lower.tail = FALSE)
alfa <- probMayor10_3
#alfa = 0.001349

# La probabilidad de que se cometa un error de tipo I es de 0.5, es decir
# que un 50% de las veces cometerá un error de tipo I.

###############################################################################
# 2. Si el verdadero volumen medio de los bidones fuera de 10,2 litros,       #
# ¿cuál sería la probabilidad de que el ingeniero, que obviamente             #
#  no conoce este dato, cometa un error de tipo II?                           #
###############################################################################


n <- 100
media_nula <- 10
media_efecto<- 10.2
desv_est <- 1 #1 litro
es <- desv_est/sqrt(n) 

#Se tiene el valor de alfa calculado para la primera pregunta
probMayor10_3 <- pnorm(10.3, media, es, lower.tail = FALSE)
alfa <- probMayor10_3
#alfa = 0.001349


#Se calculan los valores criticos de las regiones de rechazo
q_critico<- qnorm(probMayor10_3, media_nula, es, lower.tail = FALSE)

#gráfico
x <- seq (9.7, 10.4, 0.01)
y <- dnorm (x , mean = media_nula , sd = es)
normal <- data.frame(x,y)
g <- ggplot (normal, aes (x , y )) + geom_line (color = "red")
g <- g + xlab("Volumen medio [L]")
g <- g + scale_y_continuous(breaks =  NULL)
g <- g + scale_x_continuous(breaks = seq(9.7,10.4,0.1))


g <- g + geom_area ( data = subset (data.frame(x,y), x > 10.3) ,
                     aes ( y = y ) ,
                     colour = "red",
                     fill = "red",
                     alpha = 0.1)


g <- g + geom_vline ( xintercept = media_nula, linetype = "dashed")

print(g)

#grafico 2
x1 <- seq (9.7, 10.4, 0.01)
y1 <- dnorm (x1 , mean = media_efecto , sd = es)
normal1 <- data.frame(x1,y1)
g <- g + stat_function(fun = dnorm,
                       args = list(mean = media_efecto, sd = es),
                       colour = "blue", size = 1)

g <- g + geom_area ( data = subset ( data.frame (x1,y1 ) ,x > q_critico ),
                     aes ( x = x1 , y = y1 ),
                     colour = "blue", fill = "blue", alpha = 0.5)
g <- g + geom_vline ( xintercept = media_efecto , linetype = "dashed")

print(g)

#Cálculo del poder
poder <- pnorm (q_critico,
            mean = media_efecto,
            sd = es,
            lower.tail = FALSE)

pawer <- power.t.test(n=n,
                      delta = media_efecto-media_nula,
                      sd = desv_est,
                      sig.level = alfa,
                      power = NULL,
                      type = "one.sample",
                      alternative = "one.sided")

#Calcula beta
beta <- 1 - poder



# 3. Como no se conoce el verdadero volumen medio, genere un gráfico del poder estadístico 
# con las condiciones anteriores, pero suponiendo que el verdadero volumen medio podría 
# variar de 10 a 10,5 litros.

n <- 100
media_nula <- 10
desv_est <- 1 #1 litro
es <- desv_est/sqrt(n) #Error estandar

#Se tiene el valor de alfa calculado previamente


#Se crea un vector con la variación de las medias
varMedias <- seq(10,10.5,0.01)

#Se calcula el efecto
tamano_efecto <- (varMedias - media_nula) / desv_est

#Se calcula el poder
poder <- power.t.test(n = n, delta = tamano_efecto, sd = desv_est, sig.level = alfa, 
                      type="one.sample", alternative = "one.sided")$power


df_datos <- data.frame(tamano_efecto, poder)

g2 <- ggplot(df_datos, aes(tamano_efecto, poder))
g2 <- g2 + geom_line(colour="blue")
print (g2)




# 4. Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían revisarse para conseguir un poder
#estadístico de 0,75 y un nivel de significación de 0,05?

resultado <- power.t.test ( n = NULL ,
                              delta = media_efecto-media_nula, 
                              sd = desv_est,
                              sig.level = 0.05,
                              power = 0.75,
                              type = "one.sample",
                              alternative = "one.sided")

n <- ceiling ( resultado [["n"]])




# 5. ¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer un error de tipo I a un 1%
#solamente?

resultado <- power.t.test ( n = NULL ,
                              delta = media_efecto-media_nula,
                              sd = desv_est,
                              sig.level = 0.01,
                              power = 0.75,
                              type = "one.sample",
                              alternative = "one.sided")

n <- ceiling ( resultado [["n"]])
cat("La cantidad de bidones a revisar para que la probabilidad de cometer un error de tipo I sea de 1% es:", n)