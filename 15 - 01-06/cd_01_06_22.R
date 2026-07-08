# Datos 
# ------------------------- 
datos <- c(3.318573,
4.309468,
9.676125,
5.211525,
5.387863,
10.14519,
6.382749,
1.204816,
2.939441,
3.663014,
8.672245,
6.079441,
6.202314,
5.332048,
3.332477,
10.36074,
6.493551,
-0.8998515,
7.104068,
3.581626,
1.796529,
4.346075,
1.921987,
2.813326,
3.124882,
-0.06007993,
7.513361,
5.460119,
1.585589,
8.761445,
6.279393,
4.114786,
7.685377,
7.6344,
7.464743,
7.065921,
6.661753,
4.814265,
4.082112,
3.858587,
2.915879,
4.376248,
1.203811,
11.50687,
8.623886,
1.630674,
3.791345,
3.600034,
7.339895,
4.749893,
5.759956,
4.91436,
4.871389,
9.105807,
4.322687,
9.549412,
0.3537416,
6.753841,
5.371563,
5.647825,
6.138918,
3.49303,
4.000378,
1.944274,
1.784626,
5.910586,
6.344629,
5.159013,
7.766802,
11.15025,
3.526907,
-1.927507,
8.017216,
2.872398,
2.935974,
8.076714,
4.145681,
1.337847,
5.54391,
4.583326,
5.017293,
6.155841,
3.88802,
6.93313,
4.33854,
5.995346,
8.290517,
6.305544,
4.022205,
8.446423,
7.980512,
6.645191,
5.716195,
3.116282,
9.081957,
3.199221,
11.562,
9.597832,
4.292899,
1.920737)
# ------------------------- 
mean(datos)

# Asumimos conocido el sigma = 9 
# Con Formula
# ------------------------- 
sigma <- 9
mus <- seq(-10,100,length.out= 100)
veros <- c()

for (mu in mus){
  difcondatos <- datos-mu
  valor <-  0.5*((sum(difcondatos)**2)/sigma)
  veros <- c(veros,valor)
}

plot(mus,veros)


plot(mus,log(veros))
# Con which
mean(datos) # 5.271218
mus[which.min(veros)] # 5.271453 con 100000
# ------------------------- 

# Con Dnorm
# ------------------------- 
sigma <- 9
mus <- seq(-10,100,length.out= 100)
verosimilitud <- c()
for (mu in mus){
  densidad <- dnorm(datos,mu,sigma)
  verosimilitud <- c(verosimilitud,prod(densidad))
}
plot(mus,verosimilitud)
plot(mus,log(verosimilitud))

mus[which.max(verosimilitud)]

# ------------------------- 





################################### Parte 1 
df <- read.table("http://astrostatistics.psu.edu/datasets/GRB_afterglow.dat",
                 header=T, skip=1)

# df <- read.table("GRB_afterglow.dat",header=T, skip=1)
df <- df[,2:3]


# 1. Estimar P (X â‰¤ 40).
proba <- dim(df[df$f<40,])[1]/dim(df)[1]
# Bootstrap
##################
B <- 1000
tboot <-rep(NA,B)
for (i in 1:length(tboot)){
  xstar <- sample(x=df$f, size=dim(df)[1], replace=TRUE)
  tboot[i] <- mean( xstar<40)
}
hist(tboot)
se <- sqrt(var(tboot))
tbootvar <- sd(tboot)
##################


# 2. Estimar a partir de los datos el flux medio.
mean(df$f)
mean(df$s)
semean <- sd(df$f)/sqrt(length(df$f))
# IC?
alpha = 0.05
factor = qt(1-(alpha/2), dim(df)[1]-1) # coloco distribucion de T
estimacion <- mean(df$f)
incertidumbre = (sd(df$f)/sqrt(length(df$f))) # Coloco var estim
a <- estimacion - factor*incertidumbre
b <- estimacion + factor*incertidumbre  

# 3. Estimar a partir de los datos la mediana de flux. Â¿Usar ÌÄ±a para esto la estimaci Ìon
# obtenida en el  ÌÄ±tem anterior?
median(df$f)
# Bootstrap
#################
B <- 1000
tboot <-rep(NA,B)
for (i in 1:length(tboot)){
  xstar <- sample(x=df$f, size=dim(df)[1], replace=TRUE)
  tboot[i] <- median(xstar)
}
hist(tboot)
se <- sqrt(var(tboot))
tbootvar <- sd(tboot)
#################

# 4. Estimar a partir de los datos la varianza de flux.
var(df$f)

sqrt((var(df$f, na.rm = FALSE)))
sd(x=df$f, na.rm = FALSE)

# Bootstrap
#################
# A mano
B <- 1000
tboot <-rep(NA,B)
for (i in 1:length(tboot)){
  xstar <- sample(x=df$f, size=dim(df)[1], replace=TRUE)
  tboot[i] <- sd(x=xstar, na.rm = FALSE)
  
}
tbootvar <- sd(tboot)
#################

# 5. Realizar un histograma para los datos flux. Â¿Los datos parecen tener alguna distribu-
#   ci Ìon conocida? Explorar el comando density.
hist(df$f)
density(df$f)




################################# 2. Parte 2
# 2.1. Estimaci Ìon bajo modelo exponencial: E(Î»)

# 6. Estimar por el m Ìetodo de M Ìaxima Verosimilitud P (X â‰¤ 40). Comparar con la estima-
# ci Ìon obtenida en la Secci Ìon 1



# Estimacion de lambda por max verosimilutd
# ------------------------- 
lambdas <- seq(0.001 ,1,length.out= 500)
verosimilitudexp <- c()

for (lambda in lambdas){
  n <- dim(df)[1]
  valor <-  n*log(lambda)-(lambda*sum(df$f))
  verosimilitudexp <- c(verosimilitudexp,valor)
}

plot(lambdas,verosimilitudexp)
plot(lambdas,log(verosimilitudexp*-1))

# Con which
lambdas[which.max(verosimilitudexp)] 
# -------------------------

lambdaestimveros <- lambdas[which.max(verosimilitudexp)] 
prob40 = 1-exp(1)**(-lambdaestimveros*40)
prob40


# 7.Realizar un histograma para los datos de flux y superponer la funci Ìon de densidad
# exponencial con el par Ìametro que considere pertinente.
hist(df$f, prob = TRUE)
curve(dexp(x, rate = lambdaestimveros), col = 2, lty = 2, lwd = 2, add = TRUE)

# 8. Estimar por el metodo de maximal Verisimilitude el flux medio a partir de los datos.
medioexp <- 1/lambdaestimveros
medioexp
mean(df$f)

# 9. Si X âˆ¼ E(Î»), entonces su mediana resuelve la ecuaci Ìon 1âˆ’eâˆ’Î»t = 0.5, y por consiguiente
# vale âˆ’ log(0.5)/Î» = log(2)/Î». Estimar por el m Ìetodo de M Ìaxima Verosimilitud la
# mediana de flux
median(df$f)
medianexp <- log(2)/lambdaestimveros

# 10. Estimar por el metodo de M Ìaxima Verosimilitud la varianza de flux.
varexp <- 1/(lambdaestimveros**2)

############################################ 3. Parte 3
# EstimaciÌon bajo modelo Gamma Î“(Î±, Î»):

# 1. Implementar una funci´on MME.gamma que tenga por argumento un conjunto de
# datos x = (x1, . . . , xn) y devuelva los estimadores de momentos de ?? y ??.
# 2. Suponiendo que los datos de flux provienen de una poblaci´on con distribuci´on Gamma,
# estimar ?? y ?? usando la funci´on MME.gamma.

mme.gamma<- function(datos){
  n<- length(datos)
  dif <- datos-mean(datos)
  dif <- dif**2
  alfaestim <-n*(mean(datos)**2)/sum(dif)
  
  lambdaestim <-n*(mean(datos))/sum(dif)
  
  return(c(alfaestim,lambdaestim))
}

alfaestimado <- mme.gamma(df$f)[1]
lambdaestimado <- mme.gamma(df$f)[2]



# 3. Estimar P(X ??? 40) usando las estimaciones obtenidas y el m´etodo plug-in, es decir
# reemplazando cada par´ametro por su valor estimado. Comparar con las estimaciones
# obtenidas en la Gu´ia de clase.
pgamma(40, shape=alfaestimado, scale= 1/lambdaestimado)


# 4. Realizar un histograma para los datos de flux y superponer la funci´on de densidad
# gamma con los par´ametros que considere pertinentes.
hist(df$f, prob = TRUE)
curve(dgamma(x, shape=alfaestimado, scale= 1/lambdaestimado), col = 2, lty = 2, lwd = 2, add = TRUE)




# 5. Bajo el mismo supuesto, 
# calcular la estimaci´on por m´axima verosimilitud de ?? y ??. 
# Explorar el comando fitdistr de la libreria MASS. 
#Comparar con los valores obtenidos en ´items anteriores.
# Repetir los tres ´items anteriores y comparar los resultados obtenidos.

# Estimacion de lambda por max verosimilutd
# ------------------------- 
longitud <- 5
lambdas <- seq(0.001 ,1,length.out= longitud)
alfas <- seq(1 ,100,length.out= longitud)

verosimilitudexp <- c()

for (lambda in lambdas){
  for (alfa in alfas){
    n <- dim(df)[1]
    valor <-  n*log(lambda)-(lambda*sum(df$f))
      verosimilitudexp <- c(verosimilitudexp,valor)
  }
}

plot(lambdas,verosimilitudexp)
plot(lambdas,log(verosimilitudexp*-1))

# Con which
lambdas[which.max(verosimilitudexp)] 
# -------------------------



# 6. Estimar por el m´etodo de M´axima Verosimilitud el flux medio a partir de los datos.
# ¿Con qu´e valor podr´ia comparar esta estimaci´on?



# 7. Estimar por el m´etodo plug-in la mediana de flux. Explorar el comando qgamma())






