# Parte A
# Las aves parasitas de crıa ponen huevos en nidos de otras especies (hospedador), las
# cuales incuban los huevos y crıan al pichon parasito. En un bosque de talas de la provincia
# de Buenos Aires hay dos especies hospederas que son indistinguibles a simple vista. Una
# de las principales diferencias entre estas especies radica en el grado de discriminacion y
# remocion de huevos parasitos de sus nidos. Una de las especies es aceptadora de huevos
# parasitos (Y = 0), ya que remueve del nido solo el 30% de los huevos parasitos, mientras
# que la otra especie es rechazadora (Y = 1) ya que remueve el 80% de los huevos parasitos
# presentes en su nido. Ademas, se sabe que el 90% de los nidos del bosque corresponden
# a la especie aceptadora , mientras que apenas el 10% restante son nidos de la especie
# rechazadora?.
# Calculo de probabilidades: Se elige al azar un nido del bosque y se colocan k = 8
# huevos parasitos. Denotemos con X a la variable aleatoria que indica el numero de huevos
# removidos del nido. Asuma que, en cada nido, la remocion (o no) de los diferentes huevos
# se realiza de manera independiente.
# 1. Calcule la probabilidad de que 5 de los huevos parasitos sean removidos del nido si se
# sabe que el nido parasitado es de la especie aceptadora.

#rechazadora y=0
py0 = 0.9
pr0 = 0.3

dbinom(5,8,pr0)

# aceptadora
py1 = 0.1
pr1 = 0.8 #prob de rechazar un huevo dado que soy acep


# 2. Calcule la probabilidad de que x de los huevos ajenos sean removidos del nido si se
# sabe que el nido parasitado es de la especie aceptadora, para 0 menor igual x menorigual 8, y represente
# las probabilidades calculadas en un diagrama de barras. Incluya un titulo pertinente
# en el grafico.
# 
# 3. Calcule la probabilidad de que 5 de los huevos parasitos sean removidos del nido si se
# sabe que el nido parasitado es de la especie rechazadora.
# 
  # dbinom(5,8,pr1)
# 
# 
# 4. Calcule la probabilidad de que x de los huevos parasitos sean removidos del nido si se
# sabe que el nido parasitado es de la especie rechazadora, para 0 menor igual x menorigual 8, y representa
# las probabilidades calculadas en un diagrama de barras. Incluya un tıtulo pertinente
# en el grafico.
# 
# 5. Calcule la probabilidad de que 5 de los huevos parasitos sean removidos del nido.
# #cuaderno
# 
# 6. Calcule la probabilidad de que x de los huevos parasitos sean removidos del nido,
# para 0 0 menor igual x menorigual 8, y represente las probabilidades calculadas en un diagrama de barras.
# Incluya un titulo pertinente en el grafico.





# Parte B
# Clasificacion: Toy Example
# 
# 13. Considere la regla de clasificacion que determina que un nido corresponde a la especie
# rechazadora (Y = 1) si la cantidad de huevos removidos es mayor o igual a t = 5. Caso
# contrario, determina que el nido es de la especie hospedadora aceptadora (Y = 0).
# 
# Implemente una funcion regla5 que dado x, el numero de huevos agredidos, devuelva
# el valor 1 si x >= 5, y devuelva el valor 0, caso contrario. Observe que la regla de
# clasificacion es una funcion g que para cada x determina un valor en {0, 1}.
# 
# Calcule R5, la tasa de error empirica correspondiente a esta regla, utilizando los datos de
# depredadosclasificadas.txt. Es decir, calcule la proporcion de datos mal clasificados
# segun esta regla.

datos <- read.csv('depredadosclasificadas.txt',sep="", header=T)

regla5 <-function(x){
    if(x >= 5){
      return (1)
    }
  else{
    return (0)
  }
}

funcion_regla5 <-function(df){
  nrep = dim(df)[1]
  lista_clasif <-rep(0,nrep)
  for (i in 1:nrep){
    if (df$especie[i] != regla5(df$removidos[i])){
       lista_clasif[i] = 1
        }
  }
  lista_clasif
}

mean(funcion_regla5(datos))
 
# 14. Considere la regla de clasificacion que determina que un nido corresponde a la especie
# rechazadora (Y = 1) si la cantidad de huevos removidos es par.
# Caso contrario, determina que el nido es de un hospedador aceptador (Y = 0).
# Implemente una funcion rechazadorpar que dado x, el numero de huevos agredidos, devuelva el valor
# 1 si x es par, y devuelva el valor 0, caso contrario. Calcule la tasa de error empirica
# correspondiente a esta regla, utilizando los datos de depredadosclasificadas.txt. Es
# decir, calcule la proporcion de datos mal clasificados segun esta regla.

rechazarpar <-function(x){
  ifelse(x%%2==0,1,0)
}

funcion_regla5 <-function(df){
  nrep = dim(df)[1]
  lista_clasif <-rep(0,nrep)
  for (i in 1:nrep){ # rechazadora 1 si es par
    if (df$especie[i] != rechazarpar(df$removidos[i])){ #1
      lista_clasif[i] = 1
    }
  }
  lista_clasif
}

mean(funcion_regla5(datos))

# 15.?Cual de las reglas consideradas en los items anteriores (regla5 o rechazadorpar)
# prefiere? ?Por que?

# No solo por la diferencia de criterio a nivel observador sino tambien en cuestion
# del error cuadraticom medio empirico 
mean(funcion_regla5(datos)) 
mean(funcion_regla5(datos))

# 16. Para cada t ??? {0, . . . , 8}, considere la regla de clasificacion que determina que un nido
# corresponde a la variedad rechazadora (Y = 1) si la cantidad de huevos removidos es
# mayor o igual a t. Caso contrario, determina que el nido es del hospedador aceptador
# (Y = 0). Implemente una funci?on reglacorte que dados x y t, el n?umero de huevos
# removidos y el punto de corte, respectivamente, devuelva el valor 1 si x ??? t, y devuelva
# el valor 0, caso contrario.
# Calcule Rbt, la tasa de error emp?irica correspondiente a regla
# con punto de corte en t utilizando los datos depredadosclasificadas. Es decir, calcule
# la proporci?on de datos mal clasificados seg?un cada regla, para 0 ??? t ??? 8.
reglacorte <-function(x,t){
  ifelse(x>=t,1,0)
}

funcion_regla16 <-function(df,corte){
  nrep = dim(df)[1]
  lista_clasif <-rep(0,nrep)
  for (i in 1:nrep){
    if (df$especie[i] != reglacorte(df$removidos[i], corte)){
      lista_clasif[i] = 1
    }
  }
  lista_clasif
}
funcion16 <-function(df){
  ecm <-rep(0,8)
  for (i in 0:8){
      ecm[i] = mean(funcion_regla16(datos,i))
  }
  ecm
}



funcion16(datos)

library(ggplot2)

df <- data.frame(seq(0, 8, by=1),funcion16(datos))
ggplot(df) +
 aes(x = seq.1..8..by...1., y = funcion16.datos.) +
 geom_point(shape = "circle", size = 4L, 
 colour = "#FF8C00") +
 geom_smooth(span = 0.71) +
 labs(x = "X", y = "ECM") +
 theme_gray()+
 scale_x_continuous(breaks = seq(0, 8, 1))
 
# 18. Una regla de clasificacion g puede expresarse mediante una tira de ceros y unos que in-
#   dica cuanto vale g para los diferentes valores de x. Por ejemplo, la tira (0, 0, 0, 1, 1, 1, 0, 1, 1)
# indica que g(0) = 0, g(1) = 0, g(2) = 0, g(3) = 1, g(4) = 1, etc. Implemente la funcion
# ErrorClassTRUE que tenga por input un vector de ceros y unos y de longitud 9 y
# devuelva el error de clasificacion medio de la regla g, dado por P (g(X) 6 = Y ).

funcion18 <-function(regla){
  # dbinom me da la prob de condicional si multuplico por marginal
  # luego sumo cada momento en donde da mal 
  errorclasstrue <- sum((regla=0)*dbinom(0:8,8,pr0)*py0)+ sum((regla=1)*dbinom(0:8,8,pr1)*py1)
  errorclasstrue
  }
regla <- c(0, 0, 0, 1, 1, 1, 0, 1, 1)
funcion18(regla)
