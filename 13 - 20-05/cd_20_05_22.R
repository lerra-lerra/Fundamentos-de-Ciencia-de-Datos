rm(list=ls())
ls()

# Me invento los datos
lambda = 0.1
muchas_exponenciales_clase <- rexp(100,lambda)

# Poblacional
q <- qexp(0.9,lambda)
q

# Estimacion  sobre la muestra
qsomb<- quantile(muchas_exponenciales_clase,0.9)
qsomb

# Plug in del lambda estimado asumiendo una distribucion de los datos muestrales
lambdaestim <- 1/mean(muchas_exponenciales_clase)
# qrulo <- -log(1-0.9)/lambdaestim
qrulo <- qexp(0.9,lambdaestim)
qrulo
# aca estoy asumiendo la distribucion de los datos



# ----------------------------------------------
lambda = 0.1
muchas_expo <- rexp(100,lambda)

# --------------------------------------------
# Simulamos los datos
prom = c(4.00,1.10,0.20,10.00,7.00,14.50,63.50,7.30,8.90,12.60)

mean(prom)
se = sd(prom)/sqrt(length(prom))
se

# -----------------------------------------------
# Asumiendo conocido el sigmacuadr
n=5
sigmacuad = 9
alpha = 0.95
factor = qnorm(1-(alpha/2))

datos <- c(71.57,70.08,69.93,71.82,67.76)
estimacion <- mean(datos)
incertidumbre = sqrt(sigmacuad/length(datos)) # conozco la varianza = 9
a <- estimacion - factor*incertidumbre
b <- estimacion + factor*incertidumbre  

esta<- a<=70 & 70<=b

#---------------------
nrep = 1000
mu = 70
sigmacuad = 9
alpha = 0.05
n=5
resultados_esta<- rep(NA,nrep)

for (i in 1:nrep){
  datos <- rnorm(n,mu,sqrt(sigmacuad))
  factor = qnorm(1-(alpha/2))
  estimacion <- mean(datos)
  incertidumbre = sqrt(sigmacuad/length(datos)) # conozco la varianza = 9
  a <- estimacion - factor*incertidumbre
  b <- estimacion + factor*incertidumbre  
  # esta<- a<=70 & 70<=b
  resultados_esta[i]<- a<=70 & 70<=b 
}
mean(resultados_esta)

#---------------------
# ahora deberiamos probar con sigma cuadr reemplazarlo por 
# sd(datos)/sqrt(datos) pero no funciona igual, 
# ahora abajo no estamos poniendo la varianza verdadera sino una varianza
nrep = 1000
mu = 70
sigmacuad = 9
alpha = 0.05
n=5
resultados_esta<- rep(NA,nrep)

for (i in 1:nrep){
  datos <- rnorm(n,mu,sqrt(sigmacuad))
  factor = qnorm(1-(alpha/2))
  estimacion <- mean(datos)
  incertidumbre = (sd(datos)/sqrt(length(datos))) # Aca no pongo la varianza pob
  a <- estimacion - factor*incertidumbre
  b <- estimacion + factor*incertidumbre  
  # esta<- a<=70 & 70<=b
  resultados_esta[i]<- a<=70 & 70<=b 
}
mean(resultados_esta)

# estimada, por lo cual no es gratis, 
# se reemplaza el factor por una distribucion de T
# ---------------------------------------------
#
# reemplazamos ahroa qnorm por qt(alpha/2, n-1)
nrep = 1000
mu = 70
sigmacuad = 9
alpha = 0.05
n=5
resultados_esta<- rep(NA,nrep)

for (i in 1:nrep){
  datos <- rnorm(n,mu,sqrt(sigmacuad))
  factor = qt(1-(alpha/2), n-1) # coloco distribucion de T
  estimacion <- mean(datos)
  incertidumbre = (sd(datos)/sqrt(length(datos))) # Coloco var estim
  a <- estimacion - factor*incertidumbre
  b <- estimacion + factor*incertidumbre  
  resultados_esta[i]<- a<=70 & 70<=b 
}
mean(resultados_esta)
