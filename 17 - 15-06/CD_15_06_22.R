# Ejercicio de clase sobre la empirica
#-----------------------
datos <- c(0.57, 0.93, 0.81, 0.44,0.36, 0.52,0.65,0.69,0.82,0.12) #los datos son de una uniforme
# datos <- runif(100)
# Empirica devuelve la proba de datos < t
empirica <- function(datos,t){
  mean(datos<=t)
  }

ejex <- seq(0,1,0.01)
ejey <- c()
# Recorro cada x y calculo el y
for(i in ejex){
  ejey<- c(ejey,empirica(datos,i))
}

plot(ejex,ejey,cex=0.5)
#-----------------------

# Probando ecdf
  #-----------------------
  # ecdf: Empirical Cumulative Distribution Function
  plot(ecdf(datos))
#-----------------------

# 1
#-----------------------
datos = scan(nlines = 1)
# 126.4 82.4 78.1 51.1 90.9 76.2 104.5 87.4 110.5 25.0 69.3 53.5 39.8 63.6 46.7 72.9 79.6 83.6 80.7 60.3 79.0 74.4 49.6 54.7 71.8 49.1 103.9 51.6 82.4 83.6 77.8 79.3 89.6 85.5 58.0 120.7 110.5 65.4 39.9 40.1 88.7 71.4 83.0 55.9 89.9 84.8 105.2 113.7 124.7 114.5 115.6 102.4 101.4 89.8 71.5 70.9 98.3 55.5 66.1 78.4 120.5 97.0 110.0
par(mfrow=c(3,1))
hist(datos)
# i) de 20 a 140 con paso 10
hist(datos, breaks = c(10),xlim = c(20,140))
# ii) de 20 a 140 con paso 5. 
hist(datos, breaks = c(5),xlim = c(20,140))
# Comparar los tres histogramas obtenidos. ¿Tiene algun efecto el refinamiento de los bins?

#-----------------------

# 2
#-----------------------
par(mfrow=c(3,1))
puntos_corte <- seq(10,130,10)
hist(datos, breaks = puntos_corte, freq = FALSE)

puntos_corte <- puntos_corte +2
hist(datos, breaks = puntos_corte, freq = FALSE)

puntos_corte <- puntos_corte +2
hist(datos, breaks = puntos_corte, freq = FALSE)
# Alerta a que es sensible el histograma? yo me quiero acercar a la densidad
# No quiero poner muy chiquitos
# El histograma parte y hace el mismo corte para todos los puntos 
#-----------------------

# 3 probab.est(x,dat,h)
#-----------------------
probab.est <- function(x,datos,h){
  valor<- sum(datos>=(x-h) & datos <=(x+h))
  valor/(2*h*length(datos))
}
probab.est(40,datos,10) #0.04761
probab.est(40,datos,20) # 0.0055
#-----------------------
# 4 grilla_valores
#-----------------------
grilla_valores <- function(datos,h){
    valores <- c()
    for (i in datos){
    valores <- c(valores,probab.est(i,datos,h)) }
    valores
}
par(mfrow=c(3,1))
plot(datos,grilla_valores(datos,10))
plot(datos,grilla_valores(datos,20))
plot(datos,grilla_valores(datos,30))
#-----------------------

# 5 densidad.est.parzen
#-----------------------
densidad.est.parzen <- function(x,datos,h){
  datos <- (datos- x)/h
  datos
  # valor<- sum(datos>=(x-h) & datos <=(x+h))
  # valor/(h*length(datos))
}
densidad.est.parzen(100,datos,0.1)
par(mfrow=c(3,1))
plot(datos,grilla_valores(datos,10))
plot(datos,grilla_valores(datos,20))
plot(datos,grilla_valores(datos,30))
#-----------------------

