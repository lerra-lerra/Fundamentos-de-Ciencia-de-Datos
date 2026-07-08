


datos <- read.csv('alturas_identificacion_42414_n_500.csv',sep=",", header=T)

ggplot(datos) +
  aes(x = genero, y = altura, fill = genero, colour = genero) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  theme_bw()


###### 1.2 KNN
promedio_vecinos <-function(datos,xnew, vecinos_cantidad ){
  df <- data.frame(datos$altura,datos$genero)
  df$datos.altura
  df$resta = abs(df$datos.altura-xnew) # Resta con valor ingresado de todas las alturas
  df <- df[order(df$resta),c(1,2,3)] # Ordeno el df
  df
  genero <- df[1:vecinos_cantidad,2] # Creo vector con los generos de los KNN vecinos mas cercanos
  # print(genero)
  prop <- sum(genero == "F")/ vecinos_cantidad
  prop
}
promedio_vecinos(datos,165, 100)

clasifico_vecinos <-function(datos,altura,vecinos){
  ifelse(promedio_vecinos(datos,altura, vecinos)>0.5,1,0) # 1 es F, 0 es M
}
clasifico_vecinos(datos,165, 10)
  
###### 1.3 MOVIL
promedio_movil <-function(datos,alturanueva,ventana){
  prom_movil = datos[datos$altura>(alturanueva-ventana) & datos$altura<(alturanueva+ventana),]
  prop <- sum(prom_movil$genero == "F")/ dim(prom_movil)[1]
  # print(sum(prom_movil$genero == "F"))
  prop
  # mean(prom_movil$altura)
  # prom_movil
  # print(min(prom_movil$altura))
  # print(max(prom_movil$altura))
  
    }
ventana = 1.5
altura = 165
promedio_movil(datos,altura,ventana)

clasifico_movil <-function(datos,altura,vecinos){
  ifelse(promedio_movil(datos,altura, vecinos)>0.5,1,0) # 1 es F, 0 es M
}
clasifico_movil(datos,165, 10)
 
######################################### 2
###### 2.4 VECINOS
ClasificoVecinos <- function(datos,alturanueva,ventana){
  promedio_vecinos(datos,alturanueva, ventana)
  clasifico_vecinos(datos,alturanueva, ventana)
  }

ClasificoVecinos(datos,169,10)

###### 2.5 MOVIL
ClasificoMovil <- function(datos,alturanueva,ventana){
  promedio_movil(datos,alturanueva, ventana)
  clasifico_movil(datos,alturanueva, ventana)
}

ClasificoMovil(datos,169,1.5)



##### Clase
indices = order(abs(altura-165))[1:10]
regla <- ifelse(mean(genero[indice]=="F")>= 0.5,1,0)


##################################### 3 
##### 3.6
hist(datos$altura, )

library(ggplot2)
masc = datos[datos$genero=="M",]
hist(masc$altura , probability = TRUE)
lines(density(masc$altura), col="blue", lwd=2)

fem = datos[datos$genero=="F",]
hist(fem$altura , probability = TRUE)
lines(density(fem$altura), col="pink", lwd=2)

##### 3.7
prop_fem <- sum(datos$genero == "F")/ dim(datos)[1]
prop_fem

##### 3.8 y 9 
# Probando codigo
prop_fem <- sum(datos$genero == "F")/ dim(datos)[1]
prop_fem

mean(fem$altura)
var(fem$altura)

dnorm(150,mean(fem$altura),var(fem$altura))

# Funcion
plugin <-function(datos,alturanueva){
  fem = datos[datos$genero=="F",]
  masc = datos[datos$genero=="M",]
  propfem = sum(datos$genero == "F")/ dim(datos)[1]
  propmasc = sum(datos$genero == "M")/ dim(datos)[1]
  ifelse( (dnorm(alturanueva,mean(fem$altura),var(fem$altura))*prop_fem) >= (dnorm(alturanueva,mean(masc$altura),var(masc$altura))*propmasc),1,0)
}

plugin(datos,166) # 1 es mujer

##################################### 4

test <- read.csv('alturas.testeo.csv',sep=",", header=T)


ClasificoVecinos(datos,166,10)

ClasificoMovil(datos,166,1.5)

plugin(datos, 166)


# funcion_testeo <-function(df){
nrep = dim(test)[1]
lista_clasif_knn <-rep(NaN,nrep)
lista_clasif_movil <-rep(NaN,nrep)
lista_clasif_plug <-rep(NaN,nrep)

  
for (i in 1:nrep){ # rechazadora 1 si es par
  print(test$altura[i])
  lista_clasif_knn[i] = ClasificoVecinos(test,test$altura[i],10)
  print(lista_clasif_knn[i])
  lista_clasif_movil[i] = ClasificoMovil(test,test$altura[i],1.5)
  print(lista_clasif_movil[i])
  lista_clasif_plug[i] = plugin(test,test$altura[i])
}

  # return(lista_clasif_knn,lista_clasif_movil,lista_clasif_plug)
# }

# a,b,c = funcion_testeo(test)
test$genero2 = ifelse(test$genero =="F",1,0)
test$genero2 = as.factor(test$genero2)
dfgraficar <- data.frame(test$altura,test$genero2,lista_clasif_knn,lista_clasif_movil,lista_clasif_plug)

dfgraficar$lista_clasif_knn = as.factor(dfgraficar$lista_clasif_knn)
dfgraficar$lista_clasif_movil = as.factor(dfgraficar$lista_clasif_movil)
dfgraficar$lista_clasif_plug = as.factor(dfgraficar$lista_clasif_plug)


ggplot(dfgraficar, aes(x = test.altura)) +                    # basic graphical object
  geom_point(aes(y= test.genero2), colour="green",shape=15) +  # first layer
  geom_point(aes(y= lista_clasif_knn), colour="red",position = position_dodge2(w = 0.1), show.legend = TRUE)+  # second layer
  geom_point(aes(y= lista_clasif_movil), colour="orange",position = position_dodge2(w = 0.2))+  # second layer
  geom_point(aes(y= lista_clasif_plug), colour="blue",position = position_dodge2(w = 0.3))+
   theme(legend.position="bottom")
  # scale_color_manual(values = c("Women" ="blue",'Men' = '#3399ff')) + 
  # scale_shape_manual(values = c('Women' = 17, 'Men' = 16))


######### ECM de cada metodo
reglacorte <-function(i,funcion){
  ifelse(( df$test.genero2[i] != df$funcion[i]) , 1 ,0)
  
}

funcion_contar_errores <-function(df){
  nrep = dim(df)[1]
  lista_clasif_knns <-rep(0,nrep)
  lista_clasif_movils <-rep(0,nrep)
  lista_clasif_plugs <-rep(0,nrep)
  
  for (i in 1:nrep){
    if (df$test.genero2[i] != df$lista_clasif_knn[i]){
      lista_clasif_knns[i] = 1
    }
    if (df$test.genero2[i] != df$lista_clasif_movil[i]){
      lista_clasif_movils[i] = 1
    }
    if (df$test.genero2[i] != df$lista_clasif_plug[i]){
      lista_clasif_plugs[i] = 1
    }
    
  }
  print("Errores knn ")
  print(sum(lista_clasif_knns))
  
  print("Errores movil ")
  print(sum(lista_clasif_movils))
  
  print("Errores plug ")
  print(sum(lista_clasif_plugs))
  
  
}


funcion_contar_errores(dfgraficar)




