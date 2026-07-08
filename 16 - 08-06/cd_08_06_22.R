################################# Estimacion Puntual
#Estimador 1:

#1.1
# titasomb <- E(xi,xn) (tal que xi sea >1) / n

# 1.2
estimador_tita_somb <-function(datos){
  sum(datos>1)/length(datos)
}

datos <- c(12.23,6.37, 6.10, 0.70, 3.48, 2.82, 9.55, 2.21, 0.72, 9.09)
#1.3
titasomb <- estimador_tita_somb(datos)

################################# Mundo Exponencial: Calentando motores
#1.4
lambda = 0.2
# E(x) = 1/0.2  V(x)= 1/(0.2)**2  P(x) = 1-(1-exp(1)**(-0.2*1))

proba <- 1- pexp(1,0.2) # porque la pexp va de -inf a el valor que indico

# 1.5
lambdaestimado = 1/mean(datos)
# E(x) = 1/lambda  V(x)= 1/(lambda)**2  P(x) = 1-(1-exp(1)**(-lambda*1))
proba <- 1- pexp(1,lambda) # porque la pexp va de -inf a el valor que indico


#################################  Mundo Exponencial: Haciendo Estad??stica
# 1.6
# P(x) = 1-(1-exp(1)**(-lambdaestimado*1))
titarulo <- 1- pexp(1,lambdaestimado)

# 1.7
estimador_tita_rulo<-function(df){
  lamnbdaestim <- 1/mean(df)
  1- pexp(1,lamnbdaestim)
}

#1.8
estrulo <- estimador_tita_rulo(datos)

# Simulacion 1:
#1.9
lambda <- 0.2
proba <- 1- pexp(1,lambda) 

# 1.10
datitos <- rexp(50,lambda)

# 1.11
nrep <- 1000
muchos_est_somb<-rep(NA,nrep)
muchos_est_rulo<-rep(NA,nrep)

for (i in 1:nrep) {
  datitos <- rexp(50,lambda)
  muchos_est_somb[i]<- estimador_tita_somb(datitos)
  muchos_est_rulo[i]<- estimador_tita_rulo(datitos)
}

#1.12
hist(muchos_est_somb,probability = TRUE,breaks = 50,col=3)
hist(muchos_est_rulo,probability = TRUE,breaks = 50,add=TRUE,col = 2)

# 1.13
#VERDAD
set.seed(123)
lambda <- 0.2

#Primer estimador. Tita sombrero
lambda<-0.2
tita_verdad <- 1- pexp(1,lambda) 

Nrep<-1000 #Num de replicaciones
n<-c(150,200,500,1000) #Vec con tamanos de las muestras
ecm_sombrero<-rep(NA,length(n))

for (j in 1:length(n)){
  tamano<-n[j] # tamaño de n
  empi_dist_tita<-rep(NA, Nrep)
  diferencia<-rep(NA, Nrep)
  for (i in 1:Nrep){
    #Genero mis datos de tamano n
    mis_exponenciales<-rexp(tamano,lambda)
    #Para cada replica calculo proba40
    empi_dist_tita[i]<- estimador_tita_somb(mis_exponenciales)
    #Calculo dif entre la estimacion de cada replicacion y la verdad
    diferencia[i]<-((empi_dist_tita[i]-tita_verdad)^2)
  }
  ecm_sombrero[j]<-sum((diferencia)/Nrep)
}
#Segundo estimador. Tita rulo
ecm_rulo<-rep(NA,length(n))
set.seed(123)
for (j in 1:length(n)) {
  tamano<-n[j]
  empi_dist_tita <- rep(NA, Nrep)
  diferencia<-rep(NA, Nrep)
  for (i in 1:Nrep){
    #Genero mis datos de tamano n
    mis_exponenciales<-rexp(tamano,lambda)
    #Para cada replica calculo proba40 exp con lambda estimado
    empi_dist_tita[i]<-estimador_tita_rulo(mis_exponenciales)
    diferencia[i]<-((empi_dist_tita[i]-tita_verdad)^2)
  }
  ecm_rulo[j]<-sum((diferencia)/Nrep)
}

tabla_errores<-data.frame(n,ecm_sombrero,ecm_rulo)
colnames(tabla_errores)<-c("Tamano muestra","ECM tita sombrero","ECM tita rulo")
tabla_errores


#################################  Mundo Normal
#1.14
mu <- 1/0.2
sigmacuad <- 1/(0.2)**2
sigma <- sqrt(sigmacuad)

proba1_normal <- 1- pnorm(1,mu,sigma)

#1.16
mu_estim <- mean(datos)
sigma_estim <- sd(datos)/sqrt(length(datos))
proba2_normal_estim <- 1- pnorm(1,mu_estim,sigam_estim) 

# Simulacion 2



############################### Estimacion por Intervalos EXPONENCIAL

# 2. Construir los siguientes tres posibles intervalos de confianza asintoticos 
# para el parametro de interes, con nivel 0.95
  # (a) Intervalos asintoticamente normal basado en ??sombn, 
# propuesto en el item 1.1, 
# considerando que se trata de un estimador para una probabilidad 
# (es decir, no deja de ser un pestim).

lambda<-0.2
tita_verdad <- 1- pexp(1,lambda) 

# 1.1
estimador_tita_somb <-function(datos){
  sum(datos>1)/length(datos)
}

cantidad_intervalos <- 1000
resultados_esta <- c()

for (i in 1:cantidad_intervalos){
n_exp = 1000
mis_exp <-rexp(n_exp,0.2) 
tita_somb <-estimador_tita_somb(mis_exp)
# Bootstrap
nboot<-5000
remuestreo<-rep(NA,n_exp)
for (i in 1:nboot){
  datos_estrella<-sample(mis_exp,size = length(mis_exp),replace = TRUE)
  remuestreo[i]<-estimador_tita_somb(datos_estrella)
}
error_varianza<-sqrt(var(remuestreo)) # 

alfa<-0.05
factor<-qnorm(1-alfa/2)
intervalo_1 <- tita_somb - factor*error_varianza
intervalo_2 <- tita_somb + factor*error_varianza
esta<- intervalo_1<=tita_verdad & tita_verdad<=intervalo_2
resultados_esta <- c(resultados_esta,esta)
}
mean(resultados_esta)
