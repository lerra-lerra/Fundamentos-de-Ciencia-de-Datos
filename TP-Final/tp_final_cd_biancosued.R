# Cargamos el DF
df <-  read.csv("movie_metadata.csv")

# Hacemos un str del df para ver las variables
str(df)

# Reordenamos las variables 
df <- df[, c('movie_title','imdb_score','director_name','genres','actor_1_name','duration','language','country','content_rating','budget','gross','title_year','color','num_critic_for_reviews','director_facebook_likes','actor_3_facebook_likes','actor_2_name','actor_1_facebook_likes','num_voted_users','cast_total_facebook_likes','actor_3_name','facenumber_in_poster','plot_keywords','movie_imdb_link','num_user_for_reviews','actor_2_facebook_likes','aspect_ratio','movie_facebook_likes')] # leave the row index blank to keep all rows
head(df)

# Chequeamos si hay filas duplicadas
print(dim(df))
print('Removemos Filas duplicadas')
sum(duplicated(df)) # 45 Duplicated Rows
df <- df[!duplicated(df), ]
dim(df)

# Miramos Si hay valores faltanes
colSums(sapply(df, is.na))
# str(df['imdb_score'])
# hist(df['imdb_score'])

# Descartamos las budget y gross que tenian valores nulos
df <- df[!is.na(df$gross), ]
df<- df[!is.na(df$budget), ]
dim(df)

# Calculamos la ganancia de cada pelicula para trabajar con una variable en lugar de 2
df['profit'] = df['budget']/df['gross']

# Miramos las categorias de content rating
table(df['content_rating'])

# According to the history of naming these different content ratings, we find M = GP = PG, X = NC-17. 
# We want to replace M and GP with PG, replace X with NC-17, because these two are what we use nowadays.
# https://help.imdb.com/article/contribution/titles/certificates/GU757M8ZJ9ZPXB39#
df$content_rating[df$content_rating == 'M'] <- 'PG' 
df$content_rating[df$content_rating == 'GP']  <- 'PG' 
df$content_rating[df$content_rating == 'X']   <- 'NC-17'
table(df['content_rating'])

# Nos que damos con las categorias mas representativas
# attach(df) 
df_sub=df[which(df$content_rating==c("PG-13", "PG", "R")),]
# attach(df_sub)

# Ploteamos varios graficos
par(mfrow=c(3,3))
plot(df_sub$imdb_score,df_sub$gross)
boxplot(df_sub$gross ~ df_sub$content_rating)
boxplot(df_sub$budget ~ df_sub$content_rating)
boxplot(df_sub$imdb_score ~ df_sub$content_rating)
boxplot(df_sub$duration ~ df_sub$content_rating)
boxplot(df_sub$movie_facebook_likes ~ df_sub$content_rating)
boxplot(df_sub$cast_total_facebook_likes ~ df_sub$content_rating)
boxplot(df_sub$profit ~ df_sub$content_rating)

library('ggplot2')
# attach(df)
pairs(~  df_sub$profit + df_sub$gross + df_sub$budget+ df_sub$imdb_score +df_sub$duration+ df_sub$movie_facebook_likes + df_sub$cast_total_facebook_likes)

which(df_sub$budget==max(df_sub$budget))
print(df_sub[859,])
df_sub=df_sub[-859,]
pairs(~ df_sub$gross + df_sub$budget+ df_sub$imdb_score +df_sub$duration+ df_sub$movie_facebook_likes + df_sub$cast_total_facebook_likes)
# Sacando este valor con DF_SUB yo quedaria aca, no se si seguiria sacando

which(df_sub$budget == max(df_sub$budget))
print(df_sub[864,])
# df_sub= df_sub[-864,]

pairs(~  df_sub$profit + df_sub$gross + df_sub$budget+ df_sub$imdb_score +df_sub$duration+ df_sub$movie_facebook_likes + df_sub$cast_total_facebook_likes)

names(df_sub)

# Porque aca escalaste df y no df_SUB?
# df_def=scale(df[,c("budget","gross", "imdb_score", "duration","movie_facebook_likes", "cast_total_facebook_likes")])
# df_def=cbind(df_def, df["content_rating"])
# colnames(df_def)[7]="content_rating"

# lolo
df_def=scale(df_sub[,c("profit","budget","gross", "imdb_score", "duration","movie_facebook_likes", "cast_total_facebook_likes")])
df_def=cbind(df_def, df_sub["content_rating"])
colnames(df_def)[8]="content_rating"
df_def

# install.packages("GGally")
library(GGally) 
ggcorr(df_def, label = TRUE, label_round = 2, label_size = 3.5, size = 2, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

"""## *Guia*


Ajustar Las columnas de profit.

Hacer plot correlacion variables Para elegir explicatorias

Luego KNN 
"""

dist_euclidea <- function (vector1, vector2){
  distancia <- sqrt(sum((vector1 - vector2)^2))  
  # Calcula la distancia entre elemento del Test y elemento del Train (distancia entre vectores)
  distancia
}

a <- c(2, 6, 7, 7, 5, 13, 14, 17, 11, 8)
b <- c(3, 5, 5, 3, 7, 12, 13, 19, 22, 7)
dist_euclidea(a,b)

# 2. Explorar el comando dist de la libreria stats. Calcular nuevamente la distancia. Chequear con el punto 1.
# compute the Euclidean Distance using R's base function
# x <- rbind(a,b)
# stats::dist(x, method = "euclidean")

# 3. 
install.packages('philentropy')
library(philentropy)
getDistMethods()

ProbMatrix <- rbind(1:10/sum(1:10), 20:29/sum(20:29),30:39/sum(30:39))
rownames(ProbMatrix) <- paste0("Example", 1:3)
ProbMatrix
print('Con el metodo distance')
distance(ProbMatrix, method = "euclidean", use.row.names = TRUE)
print('Con el metodo stats::dist')
stats::dist(ProbMatrix, method = "euclidean")

print('Whereas distance() returns a symmetric distance matrix, stats::dist() returns only one part of the symmetric matrix.')
# Try as.dist.obj = TRUE en calculo de distancia

dist_euclidea_muchos <- function (vectorTest, dfTrain){
  # Calcula la distancia para cada elemento del Train la distancia con el vectortest (llamando a dist_euclidea)
  # agrega vectorcondistancias al dfTrain
  distancias=c()
  for (i in 1:dim(dfTrain)[1]){
    dist = dist_euclidea(vectorTest, (dfTrain[i,1:3]))
    distancias=c(distancias, dist)
  }
  dfTrain$distancias=distancias
  return (dfTrain)
}

dist_euclidea_muchos <- function (vectorTest, dfTrain){
  # Calcula la distancia para cada elemento del Train la distancia con el vectortest (llamando a dist_euclidea)
  # agrega vectorcondistancias al dfTrain
  dfTrain$distancias <- apply(dfTrain[,1:3], 1, function(x)sqrt(sum((x-vectorTest)^2)))
  dfTrain
}

clasif_item_test <- function (vectorTest, dfTrain, kvec){
  # Procesa el resultado de dist_euclidea_muchos (llamando a dist_euclidea_muchos)
  # Ordena el DF por la columna distancia euclidea, corta en el K, y clasifica segun proporcion de los elementos
  dfTrain=dist_euclidea_muchos(vectorTest, dfTrain)
  # print(dfTrain)
  dfTrain=dfTrain[order(dfTrain$distancias),]
  tabla=table(dfTrain[1:kvec,"content_rating"])
  return (names(tabla[order(tabla, decreasing = TRUE)])[1])
}

clasif_test_muchos <- function (dfTest, dfTrain, kvec){
  # LLama a clasif_item_test y agrega el valor de la prediccion a dftest(new column)
  outputs=c()
  # print(dim(dfTest)[1])
  for(vecTest in 1:dim(dfTest)[1]){
    # print(dfTest[vecTest,1:3])
    out=clasif_item_test(dfTest[vecTest,1:3], dfTrain, kvec) # pasar valores de los parametros con los que clasificara
    outputs=c(outputs, out)
  }
  print('clasifique test')
  dfTest$clasificacion=outputs
  return (dfTest)
}

error_clasif <- function (dfTest){
  # Calcula el error de la clasificacion empirico entre la clasif y la prediccion
  return (mean(dfTest$clasificacion != dfTest$content_rating))
}

cross_validation <- function (df, kfold, kvec){
  # Separa en k segmentos, llama clasif_test_muchos para cada segmento como dfTest y clasifica,
  # luego calcula el error
  # luego promedio de cada segmento
  rows=dim(df)[1] %/% kfold
  indices=seq(1, dim(df)[1], by=1) # obtengo los indices de mi df
  #   print(dim(df)[1])
  shuffled_indices=sample(indices, replace=FALSE) # hago una mezcla de indices
  for (k in 1:kfold){
    print('k')
    print(k)
    test_indices=shuffled_indices[(1+(k-1)*rows):(k*rows)] # me quedo con los indices que seran utilizados para el test
    # print(test_indices)
    train_indices=shuffled_indices[which(!shuffled_indices %in% test_indices)]
    test_result=clasif_test_muchos(df[test_indices,], df[train_indices,], kvec)
    print(error_clasif(test_result))
    error_clasif(test_result)
  }
}

df_defcross= df_def[,c("profit", "duration", "cast_total_facebook_likes","content_rating")]
# df_def
cross_validation(df_defcross, 5, 10)



####################################

grafico_error_kvecinos <- function (df, kfold, maxvecinos ){
  
  errores<- c()
  for (kvec in 5:maxvecinos){
    print('kvec')
    print(kvec)
    errores <- c(errores,cross_validation(df, 5, kvec))
  }
  errores
}
errores <- grafico_error_kvecinos(df_defcross, 5, 10)
# 4:09 
k<-5:10 
plot(errores2,1:5)


###########################

## *Guia*

dist_euclidea <- function (vector1, vector2){
  distancia <- sqrt(sum((vector1 - vector2)^2))  
  # Calcula la distancia entre elemento del Test y elemento del Train (distancia entre vectores)
  distancia
}

a <- c(2, 6, 7, 7, 5, 13, 14, 17, 11, 8)
b <- c(3, 5, 5, 3, 7, 12, 13, 19, 22, 7)
dist_euclidea(a,b)

# 2. Explorar el comando dist de la libreria stats. Calcular nuevamente la distancia. Chequear con el punto 1.
# compute the Euclidean Distance using R's base function
x <- rbind(a,b)
stats::dist(x, method = "euclidean")

# 3. 
install.packages('philentropy')
library(philentropy)
getDistMethods()

ProbMatrix <- rbind(1:10/sum(1:10), 20:29/sum(20:29),30:39/sum(30:39))
rownames(ProbMatrix) <- paste0("Example", 1:3)
ProbMatrix
print('Con el metodo distance')
distance(ProbMatrix, method = "euclidean", use.row.names = TRUE)
print('Con el metodo stats::dist')
stats::dist(ProbMatrix, method = "euclidean")

print('Whereas distance() returns a symmetric distance matrix, stats::dist() returns only one part of the symmetric matrix.')
# Try as.dist.obj = TRUE en calculo de distancia

dist_euclidea_muchos <- function (vectorTest, dfTrain){
  # Calcula la distancia para cada elemento del Train la distancia con el vectortest (llamando a dist_euclidea)
  # agrega vectorcondistancias al dfTrain
  distancias=c()
  for (i in 1:dim(dfTrain)[1]){
    dist = dist_euclidea(vectorTest, (dfTrain[i,1:3]))
    distancias=c(distancias, dist)
  }
  dfTrain$distancias=distancias
  return (dfTrain)
}

dist_euclidea_muchos <- function (vectorTest, dfTrain){
  # Calcula la distancia para cada elemento del Train la distancia con el vectortest (llamando a dist_euclidea)
  # agrega vectorcondistancias al dfTrain
  dfTrain$distancias <- apply(dfTrain[,1:3], 1, function(x)sqrt(sum((x-vectorTest)^2)))
  dfTrain
}

clasif_item_test <- function (vectorTest, dfTrain, kvec){
  # Procesa el resultado de dist_euclidea_muchos (llamando a dist_euclidea_muchos)
  # Ordena el DF por la columna distancia euclidea, corta en el K, y clasifica segun proporcion de los elementos
  dfTrain=dist_euclidea_muchos(vectorTest, dfTrain)
  print(dfTrain)
  dfTrain=dfTrain[order(dfTrain$distancias),]
  tabla=table(dfTrain[1:kvec,"content_rating"])
  return (names(tabla[order(tabla, decreasing = TRUE)])[1])
}

clasif_test_muchos <- function (dfTest, dfTrain, kvec){
  # LLama a clasif_item_test y agrega el valor de la prediccion a dftest(new column)
  outputs=c()
  print(dim(dfTest)[1])
  for(vecTest in 1:dim(dfTest)[1]){
    print(dfTest[vecTest,1:3])
    out=clasif_item_test(dfTest[vecTest,1:3], dfTrain, kvec) # pasar valores de los parametros con los que clasificara
    outputs=c(outputs, out)
  }
  dfTest$clasificacion=outputs
  return (dfTest)
}


error_clasif <- function (dfTest){
  # Calcula el error de la clasificacion empirico entre la clasif y la prediccion
  return (mean(dfTest$clasificacion != dfTest$content_rating))
}

cross_validation <- function (df, kfold, kvec){
  # Separa en k segmentos, llama clasif_test_muchos para cada segmento como dfTest y clasifica,
  # luego calcula el error
  # luego promedio de cada segmento
  rows=dim(df)[1] %/% kfold
  indices=seq(1, dim(df)[1], by=1) # obtengo los indices de mi df
  #   print(dim(df)[1])
  shuffled_indices=sample(indices, replace=FALSE) # hago una mezcla de indices
  for (k in 1:kfold){
    print(k)
    test_indices=shuffled_indices[(1+(k-1)*rows):(k*rows)] # me quedo con los indices que seran utilizados para el test
    # print(test_indices)
    train_indices=shuffled_indices[which(!shuffled_indices %in% test_indices)]
    test_result=clasif_test_muchos(df[test_indices,], df[train_indices,], kvec)
    print(error_clasif(test_result))
  }
}
df_cross= df_def[,c("profit", "duration", "cast_total_facebook_likes","content_rating")]
# df_def
cross_validation(df_cross, 5, 10)

grafico_error_kvecinos <- function (df, kfold, maxvecinos){
  # hace el cross-validation para cada kvec
  # plotea el ecme vs Kvecinos  
  errores<- c()
  for (kvec in 5:maxvecinos){
    print('kvec')
    print(kvec)
    errores <- c(errores,cross_validation(df, 5, kvec))
  }
  errores
}
errores <- grafico_error_kvecinos(df_defcross, 5, 10)
# 4:09 
k<-5:10 
plot(errores2,1:5)

# Version usando libreria
library (class)
cross_validation_paq <- function (df, kfold, kvec){
  rows=dim(df)[1] %/% kfold
  indices=seq(1, dim(df)[1], by=1) # obtengo los indices de mi df
  shuffled_indices=sample(indices, replace=FALSE) # hago una mezcla de indices
  errores_k <- c()
  for (k in 1:kfold){
    test_indices=shuffled_indices[(1+(k-1)*rows):(k*rows)] # me quedo con los indices que seran utilizados para el test
    train_indices=shuffled_indices[which(!shuffled_indices %in% test_indices)]
    
    
    df_train <- df[train_indices,]
    df_test <- df[test_indices,]
    
    
    train.X <- df_train[,1:7]
    train.vector = df_train[,"content_rating"]
    test.X <- df_test[,1:7]
    knn.pred <- knn (train.X, test.X, train.vector , k = kvec)
    df_test$clasificacion = knn.pred
    # print(table (knn.pred , df_test$content_rating))
    errores_k <- c(errores_k,error_clasif(df_test))
  }
  errores_k
}

df_cross= df_def[,c("profit","budget","gross", "imdb_score", "duration","movie_facebook_likes", "cast_total_facebook_likes","content_rating")]
# df_def=scale(df_sub[,c("profit","budget","gross", "imdb_score", "duration","movie_facebook_likes", "cast_total_facebook_likes")])
# df_def
cross_validation_paq(df_cross, 5, 10)

grafico_error_kvecinos_paq <- function (df, kfold, maxvecinos ){
  # hace el cross-validation para cada kvec
  # plotea el ecme vs Kvecinos  
  errores<- c()
  for (kvec in 1:maxvecinos){
    # print('kvec')
    # print(kvec)
    errores <- c(errores,mean(cross_validation_paq(df, 5, kvec)))
  }
  errores
}

# df_cross= df_def[,c("profit", "duration", "cast_total_facebook_likes","content_rating")]
cross_validation_paq(df_cross, 5, 10)
maximo_vecinos <- 100
errores <- grafico_error_kvecinos_paq(df_cross, 5, maximo_vecinos)

# Grafico
k<-1:maximo_vecinos 
df2<-data.frame(k,errores)
df2
plot(k, errores)

