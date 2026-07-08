depredado<-read.csv("D:\\Lorenzo\\Cursos\\CD Sued\\7 - 22-04\\depredadosclasificadas.txt", header = TRUE, sep = " ")

#Y =0 si el hospedador es aceptador, 
#Y = 1 indica que el hospedador es rechazador
#Buscar el error segun la regla clasificacion g1
nrep<-length(depredado$removidos)
errores<-rep(0,nrep)
for (i in 1:nrep) {
  if((depredado$removidos[i]<=3 & depredado$especie[i]==1)
     |(depredado$removidos[i]>=4 & depredado$especie[i]==0))
  {errores[i]<-1}
}
mean(errores)
