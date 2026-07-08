titas <- seq(0,1,length.out= 100)
veros <- c()
maximo <- 0
titamaximo <- 0
for (tita in titas){
  valor <- (tita**74)*((1-tita)**26)
  veros <- c(veros,valor)
  if (valor>maximo){
    maximo <- valor
    titamaximo <- tita
  }
}

plot(titas,veros)


plot(titas,log(veros))

# Cuando da maximo
maximo # 1.29324e-25
titamaximo # 0.7373737

# Con which
which.max(veros) # 74
titas[74] # 0.7373737
