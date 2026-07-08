# Clase 3
# Ejericio 1

funcion_ej1 <-function(x,a,b)
{
  if(x<a){salida<-0}
  else if (x>=a & x<=b ){salida<-(x-10)/(15-10)}
  else {salida<-1}
  return(salida)
}

grilla_1<-seq(5,20,by=0.01)
yy_1<-lapply(grilla_1,funcion_ej1,10,15)
plot(grilla_1,yy_1, type="l",xlab="x",ylab="funcion_ej1")

# Ejercicio 2
# Tarea 2: Combinemos bucles y condiciones.
# 1. Implementar una funci ́on suma.positivos que dado un vector v suma sus componente
# positivas; si no hay componentes positivas debe devolver el valor cero.
# 2. Calcular la suma de los n ́umeros naturales positivos comprendidos entre los valores
# que le indicamos de manera personalizada en la hoja suma positivos que encontrar ́as
# ac ́a y completar con el resultado obtenido en la columna D de la mencionada hoja de
# c ́alculo.
# 3. (∗) Implementar una funci ́on suma.si.hay.positivos que dado un vector v suma sus
# componente positivas; si no hay componentes positivas debe devolver un cartel que
# diga ”No hay positivos”.
#1
funcion_ej2 <-function(vect){
  suma = 0
  for (i in vect) {
    if(i>0){
      suma = suma + i}
    }
  return(suma)
}
vv<- c(10,1,-1,8)
funcion_ej2(vv)

numeros <-seq(-636,3976,by=1)
funcion_ej2(numeros)
sum(seq(1,3976,by=1))

#3
suma.si.hay.positivos <-function(vect){
  suma = 0
  for (i in vect) {
    if(i>0){
      suma = suma + i}
  }

  if (suma == 0){
    return("No hay positivos")
  }
  else {return(suma)}
}
vv <-seq(-636,-3976,by=-1)
suma.si.hay.positivos(vv)


# Tarea 3:

#1 Crear una funcíon suma.cubilete que simule arrojar el cubilete y d ́e por resultado la
# suma de las 5 caras obtenidas.

suma.cubilete <- function(n=5){
return(sum(sample(1:6, size=n, replace=T)))
}
suma.cubilete()

# 2. Invocar una vez la funci ́on suma.cubilete y complet ́a la planilla aqu ́ı con el resultado
# obtenido.
# 3. Leemos la planilla y hacemos un histograma con los datos le ́ıdos.
setwd("/home/clinux01/Descargas")
planilla <- read.csv('sumando.csv',dec=",", header=T)
planilla_scan <- scan()
20
26
12
18
14
15
18
21
13
23
16
16
17
20
16
20
15
12
17
10
17
17
27
19
hist(planilla_scan, breaks = 25)


# 4. Cada uno simula todo el proceso ahora con R: genera los 40 resultados del cubilete,
# calcula la suma y grafica un histograma con los resultados de su simulaci ́on.
# 5. Repetir en lugar de 40, 1000 veces el  ́ıtem anterior.
#set.seed(123)

tirar_muchos_dados <-function(n=40){
  sumas <-rep(NA, n) # sumas <-c() 
  for (i in 1:n)
  {
    sumas[i] = suma.cubilete() # sumas <- c(sumas,suma.cubilete())
  }
  hist(sumas, breaks = 25)
}
tirar_muchos_dados(10000)

# 6. (∗) Implementar una funci ́on todos.seis que cuenta la cantidad necesaria de lanza-
#   mientos hasta obtener el valor seis en todas la caras. ¿C ́omo puede utilizar la funci ́on
# suma cubilete para verificar si sale seis en todas las caras?

todos.seis <-function(){
  suma = 0 
  todoseis=0
  while(todoseis==0){
    if(suma.cubilete() == 30)
    {todoseis=1}
    suma = suma + 1
  }
  return(suma)
}
todos.seis()

tirar_muchos_seis <-function(n=40){
  sumas <-rep(NA, n) 
  for (i in 1:n)
  {
    sumas[i] = todos.seis()
  }
  hist(sumas, breaks = 50)
}
tirar_muchos_seis(100)

