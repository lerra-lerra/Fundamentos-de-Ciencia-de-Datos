#################################### UNIFORME
# 1. ¿Cuanto vale q0,9 = F???1 (0,9)?
a = 67
b = 73
qunif(0.9,a,b)


# 2. Guardar en el vector muchas uniformes, Nrep = 1000 datos 
# con distribucion U(a, b) siendo a = 67 y b = 73, 
# utilizando el comando runif().
## Promedios de uniformes
## Promedios de uniformes
nrep <- 1000
muchas_uniformes<- runif(nrep,a,b)


# 3. Guardar en el vector muchos cuantiles uniformes 
# los cuantiles muestrales correspondientes a las primeras n 
# observaciones del vector muchas uniformes, con n =
# 1, . . . , Nrep. Explorar el comando quantile(x= ,probs= ).

nrep <- 1000
muchas_cuantiles_uniformes<- rep(NA,nrep)
n=100
for (i in 1:nrep){
  vector_uniforme <- runif(n,67,73)
  sort(vector_uniforme)                      
  muchas_cuantiles_uniformes[i] <- vector_uniforme[0.9*n]
}

hist(muchas_cuantiles_uniformes,probability = TRUE,xlim=c(67,73),breaks = 50)
##################### con quantile
nrep <- 1000
muchas_cuantiles_uniformes<- rep(NA,nrep)
n=100
for (i in 1:nrep){
  muchas_cuantiles_uniformes[i] <- quantile(x=runif(n,67,73) ,probs=0.9 ) 
}
hist(muchas_cuantiles_uniformes,probability = TRUE,breaks = 50)

##################### entiendo que quireeen
nrep <- 1000
muchas_cuantiles_uniformes<- rep(NA,nrep)
for (i in 1:nrep){
  muchas_cuantiles_uniformes[i] <- quantile(x=muchas_uniformes[1:i] ,probs=0.9 ) 
}
hist(muchas_cuantiles_uniformes,probability = TRUE)




#   4. Graficar n (en el eje x) versus el cuantil emp´irico 
# de los primeros n datos. Utilizar ylim=c(65,75).
plot(1:nrep,muchas_cuantiles_uniformes,ylim = c(65,75))

#   5. Repetir los ´items 2 a 4 Ngen = 10 veces, agregando al gr´afico anterior los cuantiles de
#   cada individuo en un color diferente.
a = 67
b = 73
muchas_uniformes<- runif(nrep,a,b)
nrep <- 1000
muchas_cuantiles_uniformes<- rep(NA,nrep)
for (i in 1:nrep){
  muchas_cuantiles_uniformes[i] <- quantile(x=muchas_uniformes[1:i] ,probs=0.9 ) 
}
g = plot(1:nrep,muchas_cuantiles_uniformes,ylim = c(72,72.75),cex = 0.3)

ngen= 20
for (j in 1:ngen){
  muchas_uniformes<- runif(nrep,67,73)
  nrep <- 1000
  muchas_cuantiles_uniformes<- rep(NA,nrep)
  for (i in 1:nrep){
    muchas_cuantiles_uniformes[i] <- quantile(x=muchas_uniformes[1:i] ,probs=0.9 ) 
  }

  g+points(1:nrep,muchas_cuantiles_uniformes,ylim = c(0,1),col=j,cex = 0.3)
  
}



#################################### NORMAL
# 1. ¿Cuanto vale q0,9 = F???1 (0,9)?
a = 70
b = 1.2
qnorm(0.9,a,b)


# 2. Guardar en el vector muchas uniformes, Nrep = 1000 datos 
# con distribucion U(a, b) siendo a = 67 y b = 73, 
# utilizando el comando runif().
## Promedios de uniformes
## Promedios de uniformes
nrep <- 1000
muchas_normales<- rnorm(nrep,a,b)


# 3. Guardar en el vector muchos cuantiles uniformes 
# los cuantiles muestrales correspondientes a las primeras n 
# observaciones del vector muchas uniformes, con n =
# 1, . . . , Nrep. Explorar el comando quantile(x= ,probs= ).

##################### entiendo que quireeen
nrep <- 1000
muchas_cuantiles_normales<- rep(NA,nrep)
for (i in 1:nrep){
  muchas_cuantiles_normales[i] <- quantile(x=muchas_normales[1:i] ,probs=0.9 ) 
}
hist(muchas_cuantiles_normales,probability = TRUE)




#   4. Graficar n (en el eje x) versus el cuantil emp´irico 
# de los primeros n datos. Utilizar ylim=c(65,75).
plot(1:nrep,muchas_cuantiles_normales,ylim = c(60,80),cex=0.5)

#   5. Repetir los ´items 2 a 4 Ngen = 10 veces, agregando al gr´afico anterior los cuantiles de
#   cada individuo en un color diferente.
muchas_normales<- rnorm(nrep,a,b)
nrep <- 1000
muchas_cuantiles_normales<- rep(NA,nrep)
for (i in 1:nrep){
  muchas_cuantiles_normales[i] <- quantile(x=muchas_normales[1:i] ,probs=0.9 ) 
}
g = plot(1:nrep,muchas_cuantiles_normales,ylim = c(71,72),cex = 0.3)

ngen= 20
for (j in 1:ngen){
  muchas_normales<- rnorm(nrep,a,b)
  nrep <- 1000
  muchas_cuantiles_normales<- rep(NA,nrep)
  for (i in 1:nrep){
    muchas_cuantiles_normales[i] <- quantile(x=muchas_normales[1:i] ,probs=0.9 ) 
  }
  
  g+points(1:nrep,muchas_cuantiles_normales,ylim = c(0,1),col=j,cex = 0.3)
  
}


#################################### Exponencial
# 1. ¿Cuanto vale q0,9 = F???1 (0,9)?
a = 1/10
qexp(0.9,a)


# 2. Guardar en el vector muchas uniformes, Nrep = 1000 datos 
# con distribucion U(a, b) siendo a = 67 y b = 73, 
# utilizando el comando runif().
## Promedios de uniformes
## Promedios de uniformes
nrep <- 1000
muchas_exp<- rexp(nrep,a)


# 3. Guardar en el vector muchos cuantiles uniformes 
# los cuantiles muestrales correspondientes a las primeras n 
# observaciones del vector muchas uniformes, con n =
# 1, . . . , Nrep. Explorar el comando quantile(x= ,probs= ).

##################### entiendo que quireeen
nrep <- 1000
muchas_cuantiles_exp<- rep(NA,nrep)
for (i in 1:nrep){
  muchas_cuantiles_exp[i] <- quantile(x=muchas_exp[1:i] ,probs=0.9 ) 
}
hist(muchas_cuantiles_exp,probability = TRUE)




#   4. Graficar n (en el eje x) versus el cuantil emp´irico 
# de los primeros n datos. Utilizar ylim=c(65,75).
plot(1:nrep,muchas_cuantiles_exp,ylim = c(0,35))

#   5. Repetir los ´items 2 a 4 Ngen = 10 veces, agregando al gr´afico anterior los cuantiles de
#   cada individuo en un color diferente.
muchas_exp<- rexp(nrep,a)
nrep <- 1000
muchas_cuantiles_exp<- rep(NA,nrep)
for (i in 1:nrep){
  muchas_cuantiles_exp[i] <- quantile(x=muchas_exp[1:i] ,probs=0.9 ) 
}
g = plot(1:nrep,muchas_cuantiles_exp,ylim = c(20,30),cex = 0.3)

ngen= 20
for (j in 1:ngen){
  muchas_exp<- rexp(nrep,a)
  nrep <- 1000
  muchas_cuantiles_exp<- rep(NA,nrep)
  for (i in 1:nrep){
    muchas_cuantiles_exp[i] <- quantile(x=muchas_exp[1:i] ,probs=0.9 ) 
  }
  
  g+points(1:nrep,muchas_cuantiles_exp,ylim = c(0,1),col=j,cex = 0.3)
  
}
s
