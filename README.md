Este es un proyecto de Machine Learning no supervisado en el que se hicieron clusters de clientes de un centro de depilación láser, utilizando el método K means. 
Los resultados fueron los siguientes:
1er grupo: Clientes que viven en distintas ciudades de Uruguay, son los que gastan más de los cuatro clusters y consumen en cualquier momento de la semana (compuesto por 333 personas).
2ndo grupo: Clientes que efectuaron sus compras en Montevideo o alrededores, tienen un nivel de gasto levemente menor que el primer cluster, y consumen sobre el fin de semana
(compuesto por 7670 personas)
3er grupo: Clientes que viven en MVD y alrededores, no tienen un monto total de compras alto, y consumen sobre el fin de semana (7270 personas).
4to grupo: Clientes que viven lejos de la capital, son los que gastan menos y suelen consumir entre semana (6755 personas).

rm(list=ls())
setwd("C:/Users/rafae/OneDrive/Documentos/MLNS")

x <- read.csv("segmentacion.csv")

View(x)

#cargamos las librerías

library(dplyr)
library(MASS)
library(factoextra)
library(ggplot2)
library(class)
library("stats")
library("plotly")


## OBSERVAMOS LOS DATOS ## 
#buscamos si hay datos NA
is.na(x$importe)
#encontramos que sí

#vemos las dimensiones
dim(x)

#verificamos que las columnas estén en valores numéricos 
class(x$importe)
class(x$t)
class(x$dist)


## EMPEZAMOS A MODIFICAR LOS DATOS ##
#eliminamos los NA
x <- x %>% na.omit(x)

#eliminamos la columna x, n y customer ID
x <- dplyr::select(x, dist, importe, t)

plot(x)

#normalizamos
x = scale(x)

#sacamos una muestra de 10.000 observaciones
n <- 10000
muestra<- sample(1:nrow(x), size = n, replace =FALSE)
#asignamos la muestra al dataframe
m <- x[muestra,]

##COMENZAMOS A CLUSTERIZAR 

#buscamos si hay estructura en los datos 
#(si tiene estructura me va a dar tendiente a 1, sino tendiente a 0,5) 

a <-get_clust_tendency(m, 1000, graph = FALSE)
#nos da 0.89, hay estructura 

pairs(m)
#vemos la estructura 

#hacemos matriz de distancia 
#VAT
get_clust_tendency(m, 1000, graph = TRUE)
#vemos los clusters

#hago una matriz ficticia y comparo la estructura 
matriz = matrix(runif(2000), 1000, 2)
plot(matriz)
b<- get_clust_tendency(matriz, 200, graph = FALSE)
#nos da hopkings mas bajo 0.51

#Hopkins, tengo que sacar variables constantes o que hacen ruido
#para eso corro hopkins en cada una 
a <-get_clust_tendency(m, 1000, graph = FALSE)
a1 =get_clust_tendency(m[,-1], 1000, graph = FALSE)
#da 0.96
a2 =get_clust_tendency(m[,-2], 1000, graph = FALSE)
#da 0.7
a3 =get_clust_tendency(m[,-3], 1000, graph = FALSE)
#da 0.94
#el que me da menor hopkins es a2, me quedo con la columna 2 y 3

memory.limit(size = 30000)

#Hago VAT de la estructura después de modificarla 
a1 =get_clust_tendency(x[,-1], 100, graph = TRUE)

#ploteamos para ver la columna 2 y 3 

plot(m[,-1])


#Validacion por codo
codo <- fviz_nbclust(m[,-1], FUNcluster = kmeans, method = 'wss', k.max = 1e1)
codo 

#Validacion por silueta 
silueta <- fviz_nbclust(m[,-1], FUNcluster = kmeans, method = 'silhouette', k.max = 10)
silueta 

#kmeans sobre todos los datos (no sobre la muestra)

#a la base original elimino la variable 1

#x <- dplyr::select(x, importe, t)

#guess R

k = 3
n = 1e2
v = matrix(0, n, 2)
for (i in 1:n) {
  set.seed(i)
  q = kmeans(x[,-1], k)
  v[i,] = c(i, q$tot.withinss)
}

v  
#---identificamos la mejor solución
which.min(v[,2])
#el set seed 3 

#---volvemos a ejecutar K-Means con el mejor seed
set.seed(1)
qok = kmeans(x[,-1], k)

#---visualizamos!
plot(x, col = qok$cluster)

#---resultados (interpretar)
q$centers
#graficamos centros 
points(q$centers, pch = 16, cex = 5, col = 5)

#---recuperamos media y desvío para poner esos centros 
#en el espacio original, porque no puede haber  números negativos
a = attributes(x)

#---traducimos centros a espacio original para interpretar
#---desnormalizamos
t(q$centers)*a$`scaled:scale` + a$`scaled:center`
