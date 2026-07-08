df <- read.table("http://astrostatistics.psu.edu/datasets/GRB_afterglow.dat",
           header=T, skip=1)
df <- df[,2:3]


################################################Parte 1
#1. 
#Estimar P(X ??? 40). 
#Informar el standard error (que tambien hemos llamado incertidumbre) asociado a esta estimacion.

proba <- dim(df[df$f<40,])[1]/dim(df)[1]

# A mano
B <- 1000
tboot <-rep(NA,B)
for (i in 1:length(tboot)){
  xstar <- sample(x=df$f, size=dim(df)[1], replace=TRUE)
  tboot[i] <- mean( xstar<40)
}
hist(tboot)
se <- sqrt(var(tboot))
tbootvar <- sd(tboot)



#2. 
#Estimar a partir de los datos el flux medio. 
#Informar el standard error (que tambien hemos llamado incertidumbre) asociado a esta estimaci´on.

mean(df$f)
mean(df$s)

semean <- sd(df$f)/sqrt(length(df$f))

alpha = 0.05
factor = qt(1-(alpha/2), dim(df)[1]-1) # coloco distribucion de T
estimacion <- mean(df$f)
incertidumbre = (sd(df$f)/sqrt(length(df$f))) # Coloco var estim
a <- estimacion - factor*incertidumbre
b <- estimacion + factor*incertidumbre  


# 3. 
# Estimar a partir de los datos la mediana de flux. 
# Informar el standard error (que tambi´en hemos llamado incertidumbre) asociado a esta estimaci´on.

median(df$f)
# median(df$s)


# Libreria
library(boot)
b <- boot(data = df$f ,statistic = function(x,i) median(x[i]),R = 1000)
plot(b, index=1)
tableOfIndices<-boot.array(b, indices=T)
tableOfIndices[1,]
boot.ci(b)

boot.ci(b, type="bca")


# A mano
B <- 1000
tboot <-rep(NA,B)
for (i in 1:length(tboot)){
  xstar <- sample(x=df$f, size=dim(df)[1], replace=TRUE)
  tboot[i] <- median(xstar)
}
hist(tboot)
se <- sqrt(var(tboot))
tbootvar <- sd(tboot)

result_vec <- tboot[order(tboot)]
lower_bound <- result_vec[round(0.025*B)]
upper_bound <- result_vec[round(0.0975*B)]


# 4. 
# Estimar a partir de los datos la varianza de flux. 
# Informar el standard error (que tambi´en hemos llamado incertidumbre) asociado a esta estimaci´on.
var(df$f)

sqrt((var(df$f, na.rm = FALSE)))
sd(x=df$f, na.rm = FALSE)


# A mano
B <- 1000
tboot <-rep(NA,B)
for (i in 1:length(tboot)){
  vectorboot <- sample(x=df$f, size=dim(df)[1], replace=TRUE)
  tboot[i] <- sd(x=vectorboot, na.rm = FALSE)

}
tbootvar <- sd(tboot)


# 6. Realizar un histograma para los datos flux.
# ¿Los datos parecen tener alguna distribuci´on conocida? Explorar el comando density.

hist(df$f)
density(df$f)




################################################### Parte 2
df <- read.table("http://astrostatistics.psu.edu/datasets/GRB_afterglow.dat",header=T, skip=1)
df <- df[,2:3]

# 7. Estimar P(X ??? 40) asumiendo que los datos provienen de una distribuci´on exponencial. 
# Comparar con la estimaci´on obtenida en la Secci´on 1. Informar el standard error
# (que tambi´en hemos llamado incertidumbre) asociado a esta estimaci´on.
# esperanza = 1/lambda
lambda <- 1/mean(df$f)
pexp(40, rate=lambda)

# A mano
B <- 1000
tboot <-rep(NA,B)
for (i in 1:length(tboot)){
  xstar <- sample(x=df$f, size=dim(df)[1], replace=TRUE)
  lambda <- 1/mean(xstar)
  tboot[i] <- pexp(40, rate=lambda)
}
hist(tboot)
sd(tboot)

# 8. Realizar un histograma para los datos de flux y superponer la funci´on de densidad
# exponencial con el par´ametro que considere pertinente.

hist(df$f, prob = TRUE)
curve(dexp(x, rate = 0.03), col = 2, lty = 2, lwd = 2, add = TRUE)
curve(dexp(x, rate = lambda), col = 3, lty = 2, lwd = 2, add = TRUE)


# 9. Estimar el flux medio asumiendo que los datos provienen de una distribucion exponencial. 
# Informar el standard error (que tambien hemos llamado incertidumbre) asociado a esta estimacion.

mean(df$f)
semean <- sd(df$f)/sqrt(length(df$f))

# 10. Si X ??? E(??), entonces su mediana resuelve la ecuaci´on 1???e?????t = 0.5, 
# y por consiguiente vale ??? log(0.5)/?? = log(2)/??. 
# Estimar la mediana de flux asumiendo que los datos provienen de una distribuci´on exponencial.
# Informar el standard error (que tambi´en hemos llamado incertidumbre) asociado a esta estimaci´on.
lambda <- 1/mean(df$f) # 0.029
medianexp <- log(2)/lambda # 23.87

# A mano
B <- 1000
tboot <-rep(NA,B)
for (i in 1:length(tboot)){
  xstar <- sample(x=df$f, size=dim(df)[1], replace=TRUE)
  lambdaest <- 1/mean(xstar) # 0.029
  tboot[i] <- log(2)/lambdaest
  
}
tbootvar <- sd(tboot) 
# 2.57


# 11. Estimar la varianza de flux, asumiendo que los datos provienen de una distribuci´on
# exponencial. Informar el standard error (que tambi´en hemos llamado incertidumbre)
# asociado a esta estimacion.
varexp <- 1/(mean(df$f)**2)


# A mano
B <- 1000
tboot <-rep(NA,B)
for (i in 1:length(tboot)){
  xstar <- sample(x=df$f, size=dim(df)[1], replace=TRUE)
  tboot[i] <- 1/(mean(xstar)**2)
  
}
tbootvar <- sd(tboot)



####################################Error Cuadr´atico Medio


# Generaremos datos provenientes de una familia exponencial: X ??? E(??). Procuraremos estimar

# 12. Calcular E(X) y V(X).
# E(Y) = 1/lambda
# V(Y) = 1/lambda**2


# 13. Obtenga una formula para el parametro de interes en funcion de ??.
# ?? = P(X ??? 40).
# P(x<= 40) = 1 - e**(-lambda*40)
titaestim = 1-exp(1)**(-0.03*40)

# 14


# Qsomb
set.seed(123)
enes <- c(50,150,200,500)
titasombecm <- c()

for (n in enes){
  # Qsomb
  lambda <- 0.03 # Realidad
  realidad = pexp(40, rate=lambda) # Parametro realidad
  Nrep<-1000  
  
  titasomb <-rep(NA, Nrep) 
  dif_titasomb_realid <-rep(NA, Nrep)
  
  for (i in 1:Nrep){
    mis_exponenciales<-rexp(n,lambda)   #Genero exponenciales
    
    # Para cada replica calculo quantile
    titasomb[i]<-proba <- sum(mis_exponenciales<=40)/length(mis_exponenciales)
    
    # Calculo diferencia entre qsomb y la realidad  
    dif_titasomb_realid[i]<-((titasomb[i]-realidad)^2) #Calculo dif estimacion qsomb y la realidad 
  }
  # vector <- c(vector, v)
  titasombecm <- c(titasombecm, sum(dif_titasomb_realid)/Nrep )
  print(sum(dif_titasomb_realid)/Nrep)
}



# Qrulo
titaruloecm <- c()

for (n in enes){
  # Titarulo
  lambda <- 0.03 # Realidad
  realidad = pexp(40, rate=lambda) # Parametro realidad
  Nrep<-1000  
  
  titarulo <-rep(NA, Nrep) 
  dif_titarulo_realid <-rep(NA, Nrep)
  
  for (i in 1:Nrep){
    mis_exponenciales<-rexp(n,lambda)   #Genero exponenciales
    lambdaestim <- 1/mean(mis_exponenciales)
    # Para cada replica calculo quantile
    titarulo[i]<-  pexp(40, rate=lambdaestim)
    
    # Calculo diferencia entre qsomb y la realidad  
    dif_titarulo_realid[i]<-((titarulo[i]-realidad)^2) #Calculo dif estimacion qsomb y la realidad 
  }
  # vector <- c(vector, v)
  titaruloecm <- c(titaruloecm, sum(dif_titarulo_realid)/Nrep )
  print(sum(dif_titarulo_realid)/Nrep)
}

df <- data.frame(enes,titasombecm,titaruloecm)
colnames(df) <- c('N','ECM titasomb','ECM titarulo')
df


pgamma(40, shape=alfa,rate=0.03)
pgamma(p, shape=alfa,scale=1/lambda)


# hist(df$f, prob = TRUE)
curve(pgamma(x, shape=2.25,rate=0.03), col = 2, lty = 2, lwd = 2, add = TRUE)










