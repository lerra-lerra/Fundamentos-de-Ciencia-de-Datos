#############################  BERNOULLI
########################## Hacemos una tirada de bernoulli
p = 0.2 # dado 10 caras y si sale 1 o 2 exito
nrep=1000
# ?rbinom
muchas_bernoullies<- rbinom(n=nrep,size=1,prob=p)  
muchos_promedios_bernoullies <- rep(NaN,nrep)

for (i in 1:nrep){ 
  muchos_promedios_bernoullies[i] <- mean(muchas_bernoullies[0:i])
}

g = plot(1:nrep,muchos_promedios_bernoullies,ylim = c(0,1))


##################### Le agregamos otra tirada
muchas_bernoullies<- rbinom(n=nrep,size=1,prob=p)  
muchos_promedios_bernoullies <- rep(NaN,nrep)

for (i in 1:nrep){ 
  muchos_promedios_bernoullies[i] <- mean(muchas_bernoullies[0:i])
}

g+points(1:nrep,muchos_promedios_bernoullies,ylim = c(0,1),col=2)



##################### Le agregamos 20 tiradas distintas
p = 0.2 # dado 10 caras y si sale 1 o 2 exito
nrep=1000
# ?rbinom
muchas_bernoullies<- rbinom(n=nrep,size=1,prob=p)  
muchos_promedios_bernoullies <- rep(NaN,nrep)

for (i in 1:nrep){
  muchos_promedios_bernoullies[i] <- mean(muchas_bernoullies[0:i])
}

g = plot(1:nrep,muchos_promedios_bernoullies,ylim = c(0,1))
ngen= 20
for (j in 1:ngen){
  muchas_bernoullies<- rbinom(n=nrep,size=1,prob=p)  
  muchos_promedios_bernoullies <- rep(NaN,nrep)
  
  for (i in 1:nrep){ 
    muchos_promedios_bernoullies[i] <- mean(muchas_bernoullies[0:i])
  }
  g+points(1:nrep,muchos_promedios_bernoullies,ylim = c(0,1),col=j,cex = 0.3)
  
}


################################ Con dos Fors en lugar de slice
muchos_promedios_bernoullies2 <- rep(NaN,nrep)
suma =0
for (i in 1:nrep){ # rechazadora 1 si es par
  for (j in i){
    suma <- suma+ muchas_bernoullies[j]
  }
  muchos_promedios_bernoullies2[i] <- suma/i
}

plot(1:nrep,muchos_promedios_bernoullies2,ylim = c(0,1))



############################ UNIFORME

# Xi ∼ U(a, b) 
a = 67
b = 73
nrep=1000

muchas_tiradas<- runif(n=nrep,a,b)  
muchos_promedios <- rep(NaN,nrep)

for (i in 1:nrep){
  muchos_promedios[i] <- mean(muchas_tiradas[0:i])
}
g = plot(1:nrep,muchos_promedios,ylim = c(67,73))
ngen= 20
for (j in 1:ngen){
  muchas_tiradas<- runif(n=nrep,a,b)  
  muchos_promedios <- rep(NaN,nrep)
  
  for (i in 1:nrep){ # rechazadora 1 si es par
    muchos_promedios[i] <- mean(muchas_tiradas[0:i])
  }
  g+points(1:nrep,muchos_promedios,ylim = c(0,1),col=j,cex = 0.3)
  
}

# Frecuencias Relativas y Probabilidades
# 7. ¿Cuanto vale P(68 ≤ X ≤ 72)?
area(1) = base*altura
densidad uniforme = altura = 1(area)/base(67-73)= 1/6

prob = area = altura(1/6)*base(68-72)= 4/6

# 8. Guardar en el vector muchas uniformes, 
# N rep = 1000 datos con distribucion U(a, b)
# siendo a = 67, b = 73, utilizando el comando runif().

# 9. Guardar en el vector muchos frec rel uniformes la frecuencia relativa con la cual el
# dato aparece en el intervalo pedido (68, 72) a lo largo de las primeras n observaciones,
# con n = 1, . . . , N rep.

hist(muchas_tiradas,probability = TRUE)

# mean(muchas_tiradas > c(68:72), na.rm=TRUE)

muchos_promedios_uniforme <- rep(NaN,nrep)

for (i in 1:nrep){
  muchos_promedios_uniforme[i] <- sum(muchas_tiradas[0:i]>68 & muchas_tiradas[0:i]<72)/i
}

# 10. Graficar n (en el eje x) versus la frecuencia relativa de los primeros n datos. Utilizar
# ylim=c(0,1).
g = plot(1:nrep,muchos_promedios_uniforme,ylim = c(0,1))

# 11. Repetir los  ́ıtems 2-4 N gen = 10 veces, agregando al gr ́afico anterior las frecuencias
# relativas de cada individuo en un color diferente.
a = 67
b = 73
nrep=1000

muchas_tiradas<- runif(n=nrep,a,b)  
muchos_promedios_uniforme <- rep(NaN,nrep)

for (i in 1:nrep){
  muchos_promedios_uniforme[i] <- sum(muchas_tiradas[0:i]>68 & muchas_tiradas[0:i]<72)/i
}
g = plot(1:nrep,muchos_promedios_uniforme,ylim = c(0,1))
ngen= 20
for (j in 1:ngen){
  muchas_tiradas<- runif(n=nrep,a,b)  
  muchos_promedios_uniforme <- rep(NaN,nrep)
  
  for (i in 1:nrep){ # rechazadora 1 si es par
    muchos_promedios_uniforme[i] <- sum(muchas_tiradas[0:i]>68 & muchas_tiradas[0:i]<72)/i
  }
  g+points(1:nrep,muchos_promedios_uniforme,ylim = c(0,1),col=j,cex = 0.3)
  
}

# 12. Analizando el gr ́afico, ¿cu ́al parece ser el valor l ́ımite de estas sucesiones?
# 

















