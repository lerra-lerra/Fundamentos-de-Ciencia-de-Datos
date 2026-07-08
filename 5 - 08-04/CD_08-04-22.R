

# El  ́album de figuritas
#### Primera simplificacion: suponga que las figuritas se compran individualmente.
### 1 y 2. 
# Simular el n ́umero de una figurita elegida al azar si el  ́album es de 6 figuritas.¿Que comando sirve?
#Simular el llenado de un  ́album de 6 figuritas e indicar cu ́antas figuritas se debieron comprar para completarlo.
album_seis <-function(n=6){
  contador <- 0
  album <- rep(0,n)
  incompleto <- TRUE
  while (incompleto){
    contador <- contador+1
    figurita <-sample(1:n,1)
    album[figurita]<-TRUE
    incompleto <- sum(album)<n 
    }
  contador
}

album_seis(680)

### 3. 
#Implementar una funcion cuantasFigus(figusTotal) que, dado el tamano deĺ album (figusTotal), simule su llenado y devuelva la cantidad de figuritas que se
# debieron adquirir para completarlo. 
cuantasFigus <-function(figusTotal){
  contador <- 0
  album <- rep(FALSE,figusTotal)
  incompleto <- TRUE
  while (incompleto){
    contador <- contador+1
    figurita <-sample(1:figusTotal,1)
    album[figurita]<-TRUE
    incompleto <- sum(album)<figusTotal 
  }
  contador
}

cuantasFigus(680)


### 4. 
#Calcular Nrep=1000 veces la funcion anterior utilizando figusTotal=6 y guarde
# en una lista los resultados obtenidos en cada repeticion. Con los resultados
# obtenidos para un  ́album de seis figuritas, estimar:

tirar_muchos_album <-function(n=1000,figusTotal){
  sumas <-rep(NA, n) # sumas <-c() 
  for (i in 1:n)
  {
    sumas[i] = cuantasFigus(figusTotal) # sumas <- c(sumas,suma.cubilete())
  }
  sumas
}
vv = tirar_muchos_album(1000,6)
hist(vv)

# 4.a El numero de figuritas hay que comprar, en media, para completar el  ́album.
mean(vv)

# 4.b La probabilidad de completar el  ́album comprando 16 figuritas.

length(which(vv<=16))/1000
# length(which(vv==16))/1000
mean(vv<=16)

# 4.c El numero de figuritas que hay que comprar para tener probabilidad del 90%
# de completar el  ́album
prob_90 <-function(){
figus <-0
prob <- 0
  while (prob < 0.9){
    figus<-figus +1
    prob <- length(which(vv<=figus))/1000
  }
figus
}
prob_90()

# Sobre variabilidad y convergencia
# 5. 
# Repetir el item 4.a utilizando Nrep=200, 500, 1000, 5000, 10000. Grafique Nrep
# (en el eje x) vs. los promedios calculados para cada valor de Nrep.
ejercicioc <-function(p,n=1000){
  grilla<-c(200,500,1000,5000,10000)
  promedios <-c() 
  for (i in grilla)
  {
    promedios <- c(promedios,mean(tirar_muchas_monedas(i,6))) 
  }
  plot(grilla,promedios)
  promedios
}
ejercicioc()

# 6. Repetir el item anterior utilizando otro color para graficar los puntos. Vuelva a
# repetir. Comentar los resultados obtenidos.

# Caso intermedio: consideremos ahora que el  ́album es de figusTotal=640 figuritas
# y que las figuritas se siguen comprando individualmente.
# 7. 
# Calcular Nrep=100 veces la funcion cuantasFigus(figusTotal=640) utilizando
# figusTotal=640 y guardar los resultados obtenidos en cada repetici ́on en una
# lista. Con los resultados obtenidos estimar cu ́antas figuritas hay que comprar, en
# promedio, para completar el  ́album (de 640 figuritas).

vv = tirar_muchos_album(100,640)
hist(vv)
mean(vv)

# Con paquetes:
# 8. 
# Simular la generacion de un paquete con 5 figuritas, sabiendo que el  ́album es de
# 640. Notemos que, c ́omo en la vida real, pueden haber figuritas repetidas en un
# paquete.
# 9. Implementar una funcion genPaquete(figusTotal, figusPaquete) que dado el
# tamano del  ́album (figusTotal) y la cantidad de figuritas por paquete (figusPaquete)
# genere un paquete de figuritas al azar. Notemos que, c ́omo en la vida real, pueden
# haber figuritas repetidas en un paquete.

generar_paquete <-function(figusTotal=640,cant_figus=5){
  paquete <-c() 
  for (i in 1:cant_figus)
  {
    paquete <- c(paquete,sample(1:figusTotal,1)) 
  }
  paquete
}
generar_paquete()



# 10. Implementar una funci ́on cuantosPaquetes(figusTotal, figusPaquete) que
# dado el tama ̃no del  ́album (figusTotal) y la cantidad de figuritas por paquete
# (figusPaquete) simule el llenado del  ́album y devuelva cu ́antos paquetes se de-
#   bieron adquirir para completarlo.
cuantosPaquetes <-function(figusTotal, figusPaquete){
  contador <- 0
  album <- rep(FALSE,figusTotal)
  incompleto <- TRUE
    while (incompleto) {
    contador <- contador+1
    paq <-generar_paquete(figusTotal,figusPaquete)
      for (i in paq){
      album[i]<-TRUE
      }
    incompleto <- sum(album)<figusTotal 
  }
  contador
}

cuantosPaquetes(640,5)

# 11. Calcular Nrep=100 veces la funci ́on cuantosPaquetes, utilizando figusTotal=640,
# figusPaquete=5 y guarde los resultados obtenidos en una lista y calcule su prome-
#   dio.
tirar_muchos_album_paq <-function(n=100,figusTotal,figusPaquete){
  sumas <-rep(NA, n) # sumas <-c() 
  for (i in 1:n)
  {
    sumas[i] = cuantosPaquetes(figusTotal,figusPaquete) # sumas <- c(sumas,suma.cubilete())
  }
  sumas
}
vv = tirar_muchos_album_paq(100,640,5)
hist(vv)
mean(vv)









