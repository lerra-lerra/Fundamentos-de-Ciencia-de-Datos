#### Manos a la obra - Parte I
# 1. Descarga datos correspondiente a 200 individuos.
# 2. Leer el archivo de datos en R.
datos<-read.csv("alturas_identificacion_42414_n_200.csv",fileEncoding = "UTF-8")

# 3. Identificar el nombre de las columnas del data.frame.
str(datos)
names(datos)

# 4. ¿Cu ́antos individuos conforman tu conjunto de observaciones
dim(datos)

#   5. Calcular el promedio de las alturas de los hijes para predecir la altura de un nuevo individuo.
mean(datos$altura) #166.885

# 6. ¿Con que valor se puede predecir la altura de un nuevo individuo masculino
masc = datos[datos$genero=="M",]
mean(masc$altura) #172.6696

#   7. ¿Con qu ́e valor se puede predecir la altura de un nuevo individuo masculino cuya madre es bajita?
mascbaj = datos[datos$genero=="M" & datos$contextura_madre=="bajita",]
mean(mascbaj$altura) #169.8944

#### Manos a la obra - Parte II
#   1. Graficar altura de mam ́a (en el eje x) vs. altura del hije (eje y), utilizando un color por cada g ́enero.
library(ggplot2)
ggplot(datos)+aes(x = datos$altura_madre, y = datos$altura, color=datos$genero )+geom_point(size = 0.6)


# Nos quedamos con los datos con genero =="M"
masc = datos[datos$genero=="M",]

# 2. Promedio movil centrado en 156 con ventana de tamno 1 (cm).
a = 1 
altura = 156
prom_movil = masc[masc$altura_madre>altura-a & masc$altura_madre<altura+a,]

# 2.1 Indique c ́uantos casos hay donde la madre registre una altura entre 155 y 157 cm., inclusive.
dim(prom_movil) # 17 datos

# 2.2 Calcule el promedio de la altura de los hijos cuyas madres registran una altura entre 155 y 157 cm.
mean(prom_movil$altura) # 171.3

#### Tarea 3:
# 1. Promedio m ́ovil centrado en 156 con ventana de tamno 3 (cm)
promedio_movil <-function(a,altura){
   datos<-read.csv("alturas_identificacion_42414_n_200.csv",fileEncoding = "UTF-8")
   masc = datos[datos$genero=="M",]
   prom_movil = masc[masc$altura_madre>altura-a & masc$altura_madre<altura+a,]
   mean(prom_movil$altura)
  }
ventana = 3
altura = 156
promedio_movil(ventana,altura)




# 1. Calentando motores
# 1. Descargar de esta p ́agina un conjunto de n = 500 observaciones, con todas las variables
# y leer el archivo en R.
datos<-read.csv("alturas_n_500.csv",fileEncoding = "UTF-8")

# 2. Identificar el nombre de las columnas del data.frame.
names(datos)
# 3. Calcular el promedio de las alturas de los hijes para predecir la altura de un nuevo
# individuo.
mean(datos$altura)

# 4. Realizar un histograma de las alturas de les hijes. ¿Cu ́antas modas se observan? ¿A
# qu ́e se puede atribuir?
hist(datos$altura)
#   5. Explor el comanado plot(density(variable)), utilizando como variable la columna
# de alturas con las que se realiz ́o el histograma. ¿Qu ́e est ́a pasando? ¿Cu ́antas modas
# observa? ¿A qu ́e se puede atribuir?
plot(density(datos$altura))
# se Ven dos modas producto seguramente dle genero

#   6. Realizar ahora un histograma de alturas por cada g ́enero. Es decir, un histograma para
# las alturas correspondientes al g ́enero Masculino y otro para las alturas correspondien-
#   tes al g ́enero Femenino.
masc = datos[datos$genero=="M",]
g = plot(density(masc$altura))
g + hist(datos$altura) 

fem = datos[datos$genero=="F",]
plot(density(fem$altura))

# 7. Superponer en cada histograma del  ́ıtem anterior una funci ́on de densidad normal con
# los par ́ametros que consideres pertinentes.
hist(masc$altura , probability = TRUE)
lines(density(masc$altura), col="blue", lwd=2)

hist(fem$altura , probability = TRUE)
lines(density(fem$altura), col="blue", lwd=2)

# 8. Indicar con qu ́e valor se puede predecir la altura de une hije Masculino, es decir, la
# altura de un hijo. Indicar con qu ́e valor se puede predecir la altura de une hije Femenino,
# es decir, la altura de una hija.


# 9. Indicar ahora con qu ́e valor se puede predecir la altura de un hijo (Masculino), sabiendo
# adem ́as que su mam ́a es de contextura peque ̃na. Comparar el valor de la predicci ́on
# con la predicci ́on obtenida en el  ́ıtem anterior. ¿Qu ́e se puede observar?





# 2. Vamos ahora a considerar la altura de la mama.

# 10. Graficar altura de mam ́a (en el eje x) vs. altura del hije (eje y), utilizando un color por
# cada g ́enero. ¿Qu ́e se puede observar?
library(ggplot2)
ggplot(datos)+aes(x = datos$altura_madre, y = datos$altura, color=datos$genero )+geom_point(size = 0.6)


#   En adelante, trabajaremos apenas con los datos de los hijos (g ́enero masculino).
# 11. Indicar si hay alguna madre de hijo var ́on cuya altura sea 156 cm.
# SI
# 12. Vamos ahora a predicir la altura de un hijo correspondiente a una mam ́a que mide
# x = 156 cm haciendo promedio m ́ovil centrado en 156 con ventana de tama ̃no h =0.5
# (cm).
# a) Indicar c ́uantos casos hay donde la madre registra una altura entre 155.5 y 156.5
# cm., inclusive.
# b) Calcular el promedio de la altura de los hijos cuyas madres registran una altura
# entre 155.5 y 156.5 cm.
# 13. Promedio m ́ovil centrado en 156 con ventana de tama ̃no h = 1 (cm).
# a) Indicar c ́uantos casos hay donde la madre registre una altura entre 155 y 157 cm.,
# inclusive.
# b) Calcular el promedio de la altura de los hijos cuyas madres registran una altura
# entre 155 y 157 cm.
# 14. Repetir los items anteriores pero considerando ahora que la altura de la mam ́a es 159
# cm. Es decir, nos movemos a calcular promedios en otro lado. Por eso hablamos de
# promedios m ́oviles.

# 3. Implementando funciones
# Hasta ahora hemos trabajado con dos posibles valores para la altura de la madre (156
#                                                                                  y 159) y dos posibles valores de ventana (h =0.5 y h = 1). Vamos ahora a implementar
# una funci ́on que permita predecir la altura de un hijo en funci ́on de la altura de la
# madre y el tama ̃no h de ventana elegida para hacer el promedio m ́ovil.
# 15. Implemente una funci ́on que permita predecir la altura de un hijo en funci ́on de la
# altura de la madre, los datos para la altura de los individuos y de las madres y el
# tama ̃no h de ventana elegida para hacer el promedio m ́ovil. Es decir, defina la funci ́on
# predigo altura masculino(altura, altura madre, altura mama nueva, h)
datos<-read.csv("alturas_n_500.csv",fileEncoding = "UTF-8")
masc = datos[datos$genero=="M",]
attach(masc)

promedio_movil <-function(altura_mama_nueva, h){
  datos<-read.csv("alturas_n_500.csv",fileEncoding = "UTF-8")
  masc = datos[datos$genero=="M",]
    prom_movil = masc[altura_madre>altura-h & altura_madre<altura+h,]
  mean(prom_movil$altura)
}

promedio_movil(156, 3)


# 16. Graficar la funci ́on predigo altura masculino, con h =0.5, evaluandola a lo largo de
# una una grilla sobre un intervalo que cubra todas las alturas observadas en las madres.
# 2
datos<-read.csv("alturas_n_500.csv",fileEncoding = "UTF-8")
masc = datos[datos$genero=="M",]
vector = masc$altura_madre

graficador <- function (vector){
  promedios <-c()

  for(i in vector){
    print(i)
    promedios <-c(promedios,promedio_movil(i,0.5))
    }
  plot(promedios,vector)
  promedios
  }
prom = graficador(vector)
# 17. Repita el item anterior usando h = 1. Repita usando h = 5. Grafique las tres funciones
# en un mismo gr ́afico utilizando un color diferente para cada valor de h.
# Hasta el momento hemos trabajado utilizando siempre un mismo conjunto de datos.
# Vamos ahora a implementar nuevas funciones que incluyan entre sus argumentos a los
# datos. Es decir,
# 18. Implemente una funci ́on que tenga por input un conjunto de valores de X, sus corres-
#   pondientes valores de Y, un nuevo valor x donde queremos predecir, y el tama ̃no h de la
# ventana que vamos a utilizar a la hora de hacer promedios moviles. predigoVentana(X,
#                                                                                   Y, xNuevo, h)
# 19. Pensar en alguna situaci ́on relacionada a su campo de trabajo donde pueda ser im-
#   portante predecir cierta variable respuesta con una (o varias) variables explicativas.
# Contanos el problema, el marco te ́orico y avisanos si ten ́es datos que puedas compar-
#   tir. Asi vemos que d ́















