
## Uniformes
nrep <- 10000
muchas <- runif(nrep,67,73)
hist(muchas,probability = TRUE)



## Promedios de uniformes
nrep <- 100000
promedios<- rep(NA,nrep)
n=5
for (i in 1:nrep){
promedios[i] <- mean(runif(n,67,73))
}

hist(promedios,probability = TRUE,xlim=c(67,73))



## Exp
nrep <- 100000
muchas <- rexp(nrep,1/10)
hist(muchas,probability = TRUE)

nrep <- 100000
promedios<- rep(NA,nrep)
n=3
for (i in 1:nrep){
  promedios[i] <- mean(rexp(n,1/10))
}

hist(promedios,probability = TRUE,breaks = 100)


## RLnorm
nrep <- 10000
muchas <- rlnorm(nrep,1/10)
hist(muchas,probability = TRUE)

nrep <- 100000
promedios<- rep(NA,nrep)
n=100
for (i in 1:nrep){
  promedios[i] <- mean(rlnorm(n,1/10))
}

hist(promedios,probability = TRUE)

## RCauchy: la densidad cauchy tiene colas pesadas, degenera el dibujo
nrep <- 1000
muchas <- rcauchy(nrep,1/10)
hist(muchas,probability = TRUE,breaks = 100000,xlim = c(-5,5))

## RLnorm
nrep <- 10000
promedios<- rep(NA,nrep)
n=60
for (i in 1:nrep){
  promedios[i] <- mean(rpois(n,0.1))}
hist(promedios,probability = TRUE)

## RBinom
nrep <- 10000
promedios<- rep(NA,nrep)
n=40
for (i in 1:nrep){
  promedios[i] <- mean(rbinom(n,1,0.5))}
hist(promedios,probability = TRUE)

