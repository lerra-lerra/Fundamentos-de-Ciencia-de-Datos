#Clase 4

##################################### Sobre Monedas ############################################
# A lo largo de esta actividad identificaremos la cara con el valor 1 y la ceca con el valor 0.

###################### 1. Una moneda
# a) Implementar una funci ́on cara ceca que simule el lanzamiento de una moneda
# equilibrada, devolviendo los valores 1 y 0 para cara y ceca, respectivamente.
# Indicar si es necesario definir un input para esta funci ́on.
cara_seca <-function()
{
  return(sample(0:1, 1, replace=TRUE))
}
cara_seca()

# b) ¿Con qu ́e frecuencia (cu ́antas veces) espera observar el valor 1 (cara) si repite el
# experimento N rep = 1000 veces?


tirar_muchas_monedas <-function(n=40){
  sumas <-rep(NA, n) # sumas <-c() 
  for (i in 1:n)
  {
    sumas[i] = cara_seca() # sumas <- c(sumas,suma.cubilete())
  }
  sumas
}
vv = tirar_muchas_monedas(1000)
hist(vv)
#   c) Guardar en el vector muchas repeticiones el resultado de la funci ́on cara ceca
# a lo largo de N rep = 1000 repeticiones. Calcular la cantidad de veces (frecuen-
#cia) que se obtuvo el valor cara. Calcular la frecuencia relativa (frecuencia/N ep)
# correspondiente al valor cara.
probabilidad_cara <-function(vector,n=1000){
  a <- table(vector)
  count_cara <- a[names(a)==1]
  return( count_cara/n)
  #length(which(numbers==x))
}
probabilidad_cara(vv,1000)

###################### 2. Repetimos varias monedas
# a) Implementar una funcíon repito moneda(n) que dado un n ́umero de repeticiones
# n devuelva un vector con n posiciones de manera que en la componente i- ́esima
# haya un 1 si en la i- ́esima repetici ́on se obtiene cara y haya un 0 si se obtiene ceca.
repito_moneda <-function(n=1000){
  sumas <-rep(NA, n) # sumas <-c() 
  for (i in 1:n)
  {
    sumas[i] = cara_seca() # sumas <- c(sumas,suma.cubilete())
  }
  sumas
}
vv = repito_moneda(1000)

# b) Implementar una funci ́on cuantas caras(n) que dado un n ́umero de repeticiones
# n devuelva la cantidad de caras (unos) observadas en las n repeticiones.
cuantas_caras <-function(n=1000){
  vector= repito_moneda(1000)
  a <- table(vector)
  a[names(a)==1]
  #length(which(numbers==x))
}
cuantas_caras(1000)


###################### 3. Sigue tirando, sigue tirando
# a) Implementar la funci ́on perseverancia cara que emule el n ́umero de lanzamien-
#   tos necesarios hasta observar la primera cara. Indicar si es necesario definir un
# input para esta funci ́on
perseverancia_cara <-function(){
  suma = 0 
  cara=0
  while(cara==0){
    if(cara_seca() == 1)
    {cara=1}
    suma = suma + 1
  }
  return(suma)
}
perseverancia_cara()



# b) Guardar en el vector muchas perseverancia cara el resultado de la funci ́on
# perseverancia a lo largo de N rep = 1000 repeticiones. Calcular la frecuencia re-
#   lativa de cada uno de los valores obtenidos. Calcular el promedio de los resultados
# guardados en muchas perseverancia cara.
perseverancia_a_lo_largo <-function(n=1000){
  sumas <-rep(NA, n)
  frec_sumas<-rep(NA, n)
  for (i in 1:n)
  {
    sumas[i] = perseverancia_cara()
    frec_sumas[i] = 1/sumas[i]
  }
  frec_sumas
  # hist(sumas, breaks = 50)
}
perseverancia_a_lo_largo()
hist(pers)

###################### 4. Exito - Fracaso
# a) Implementar una funci ́on exito fracaso(p) que tenga por input un valor p que
# devuelva los valores 1 o 0, para  ́exito y fracaso, respectivamente, de forma tal que
# el  ́exito ocurra con probabilidad p.
funcion_exito_fracaso <-function(p){
  sample(0:1, 1, replace = FALSE, prob = c(1-p,p))
}

funcion_exito_fracaso(0.9)





# b) ¿Con qu ́e frecuencia (cu ́antas veces) espera observar el valor 1 si repite el experi-
#   mento N rep = 1000 veces?

frecuencia_funcion_exito_fracaso <-function(n=40,p=.5){
  sumas <-rep(NA, n) # sumas <-c() 
  for (i in 1:n)
  {
    sumas[i] = funcion_exito_fracaso(p) # sumas <- c(sumas,suma.cubilete())
  }
  hist(sumas)
  sumas
}
frecuencia_funcion_exito_fracaso(1000,0.9)

# c) Guardar en el vector muchas repeticiones bis el resultado de la funci ́on exito fracaso
# a lo largo de N rep = 1000 repeticiones, utilizando p = 0,8. 
# Calcular la cantidad de veces que (frecuencia) obtuvo el valor 1 ( ́exito).
# Calcular la frecuencia relativa (frecuencia/N ep) correspondiente al  ́exito.
muchas_repeticiones_bis <- frecuencia_funcion_exito_fracaso(1000,0.8)

cant_veces_uno = length(which(muchas_repeticiones_bis==1))
frec_relativa_cant_veces_uno = cant_veces_uno/1000


# 5. Muchas repeticiones
# a) Implementar una funci ́on repito exito fracaso(n, p) que dado un n ́umero de
# repeticiones n y un valor p devuelva un vector con n posiciones de manera que
# en la componente i- ́esima haya un 1 si en la i- ́esima repetici ́on se obtiene  ́exito y
# haya un 0 si se obtiene fracaso, siendo p la probabilidad de  ́exito en cada una de
# las n repeticiones (independientes).

# b) Implementar una funci ́on cuantos exitos(n, p) que dado un n ́umero de repeti-
#   ciones n y un valor p devuelva la cantidad de  ́exitos (unos) observadas en las n
# repeticiones, siendo p la probabilidad de  ́exito en cada una de las n repeticiones
# (independientes).


cuantos_exitos <-function(n=1000,p=.8){
  muchas_repeticiones_bis <- frecuencia_funcion_exito_fracaso(n,p)
  length(which(muchas_repeticiones_bis==1))

}

cuantos_exitos()

# 6. Sigue tirando, sigue tirando
# a) Implementar la funci ́on perseverancia exito(p) que dado un valor p emule
# el n ́umero de repeticiones necesarias hasta observar el primer  ́exito, siendo p la
# probabilidad de  ́exito en cada repetici ́on.
perseverancia_exito <-function(p){
  suma = 0 
  cara=0
  while(cara==0){
    if(funcion_exito_fracaso(p) == 1)
    {cara=1}
    suma = suma + 1
  }
  return(suma)
}
perseverancia_exito(0.1)

# b) Guardar en el vector muchas perseverancia exito el resultado de la funci ́on
# perseverancia exito a lo largo de N rep = 1000 repeticiones, utilizando p = 0,8.
# Calcular la frecuencia relativa de cada uno de los valores obtenidos. Calcular el
# promedio de los resultados guardados en muchas perseverancia exito.

muchas_perseverancias_exito <-function(p,n=1000){
  sumas <-rep(NA, n)
  frec_sumas<-rep(NA, n)
  for (i in 1:n)
  {
    sumas[i] = perseverancia_exito(p)
  }
  
  sumas
}

hist(muchas_perseverancias_exito(0.8))

# c) Repita el item anterior variando p en la grilla grilla<-seq(0.01,0.99,by=0.02)
# y graficar p (en la grilla) vs el promedio de perseverancia exito(p) en N rep =
#   1000. Proponga alguna curva para modelar este fen ́omeno y superponerla en otro
# color.
ejercicioc <-function(p,n=1000){
  grilla<-seq(0.01,0.99,by=0.02)
  promedios <-c() 
  for (i in grilla)
  {
    promedios <- c(promedios,mean(muchas_perseverancias_exito(i))) 
  }
  plot(promedios,grilla)
  promedios
  }
ejercicioc()













