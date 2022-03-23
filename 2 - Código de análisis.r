##########################################################################
############--------------Universidad ORT Uruguay---------------##########
############--------Obligatorio Analítica de Negocios 2---------##########
############----------------Prof. Guillermo Magnou--------------##########
##########################################################################

#=========================================================================
#############
# Preámbulo #
#############

# Borramos datos de la memoria
rm(list = ls())

# Establecemos directorio de trabajo
#setwd("C:/Users/Cecilia Machado/Desktop/Obligatorio Analítica 2")
setwd("C:/Users/el_to/OneDrive/Escritorio/Drive/ORT/8vo Sem/Analitica de Negocios 2/Obligatorio/")

# Cargamos libreria a utilizar
library(ISLR)
library(cluster)
library(ranger)

# Cargamos los datos
datos = Hitters

## Visualizamos los datos 
View(datos)

# 
summary(datos)

# Fin del preambulo
#====================================================================

#**********
# Parte 1 
#**********

#********************
#Análisis descriptivo 
#********************

summary(datos)

# Diagrama de caja de las principales variables
par(mfrow = c(3, 4))
boxplot(datos$Salary, col = "brown3", main = "Diagrama de caja de Salary", ylab="Salary")
boxplot(datos$CRuns, col = "brown3", main = "Diagrama de caja de CRuns", ylab="CRuns")
boxplot(datos$CAtBat, col = "brown3", main = "Diagrama de caja de CAtBat", ylab="CAtBat")
boxplot(datos$CHits, col = "brown3", main = "Diagrama de caja de CHits", ylab="CHits")
boxplot(datos$CRBI, col = "brown3", main = "Diagrama de caja de CRBI", ylab="CRBI")
boxplot(datos$CWalks, col = "brown3", main = "Diagrama de caja de CWalks", ylab="CWalks")
boxplot(datos$CHmRun, col = "brown3", main = "Diagrama de caja de CHmRun", ylab="CHmRun")
boxplot(datos$Years, col = "brown3", main = "Diagrama de caja de Years", ylab="Years")
boxplot(datos$Hits, col = "brown3", main = "Diagrama de caja de Hits", ylab="Hits")
boxplot(datos$Runs, col = "brown3", main = "Diagrama de caja de Runs", ylab="Runs")
boxplot(datos$AtBat, col = "brown3", main = "Diagrama de caja de AtBat", ylab="AtBat")


#**********
# Parte 2 
#**********

#********************
#Análisis de cluster 
#********************

# Creamos copia de base de datos para convertir variables dicotómicas en dummies

datos_dummies <- datos

# Convertimos variables dicotómicas en dummies

datos_dummies$League <- ifelse(datos_dummies$League == 'A', 1, 0)
datos_dummies$Division <- ifelse(datos_dummies$Division == 'E', 1, 0)
datos_dummies$NewLeague <- ifelse(datos_dummies$NewLeague == 'A', 1, 0)

View(datos_dummies)

# Escalamos base de datos 
datos_dummies_scaled <- scale(datos_dummies)

# Verificamos que los datos están escalados
colMeans(datos_dummies_scaled)
apply(datos_dummies_scaled, 2, var)

#Verificamos la posibilidad de imputar los NA para no perder observaciones

# Omitimos datos NA
datos_dummies_scaled_NO_NA <- na.omit(datos_dummies_scaled)

#Clusterizamos sin tomar la variable de interés (salary)
#clustereu = agnes (datos_dummies_scaled_NO_NA[,-19], metric = "euclidean")
#clustereu$ac

#!#Clusterizamos sin tomar la variable de interés (salary)
clustereu = agnes (datos_dummies_scaled[,-19], metric = "euclidean")
clustereu$ac

#Se visualiza que la observación correspondiente a "Peter Rose" es un outlier, por lo que descartamos
#la métrica Euclidea y pasamos a Manhattan.

#Se realiza dendograma para visualizar y seleccionar cantidad de clusters.
dendo1 = pltree(clustereu, main = "Dendograma")

# Se visualiza que el dendograma inicial es poco claro, se hace dificil de interpretar
dendo1 = pltree(clustereu, main = "Dendograma", hang = -1)

#Encontramos un outlier por lo que debemos proceder con cambios en la metrica

# Se realiza clusterización con otras métricas para mejorar el ac utlizando "manhattan"
clusterman = agnes (datos_dummies_scaled[,-19], metric = "manhattan" )
clusterman$ac

# Se realiza clusterización con otros metodos para mejorar el ac utlizando "complete"
clusterman2 = agnes (datos_dummies_scaled[,-19], metric = "manhattan", method = "complete")
clusterman2$ac

# Mejora la clusterización

# Se realiza clusterización con otros metodos para mejorar el ac utlizando "ward"
clusterman3 = agnes (datos_dummies_scaled[,-19], metric = "manhattan", method = "ward")
clusterman3$ac

# Se visualiza dendograma con la mejor custerización
dendo2 = pltree(clusterman3, main = "Dendograma", hang = -1)

# Comenzamos el proceso de poda del dendograma buscando clusters en los que:
  #ni todas sus observaciones sean NA
  #ni los clusters nos queden demasiado grandes, ya que sino estaremos imputando el mismo
  #valor para todos los NA lo cual no tendría mucho sentido
  #similar cantidad de NA por cluster

arbol_cortado1 = cutree(as.hclust(clusterman3), h = 50)
table(arbol_cortado1)

arbol_cortado2 = cutree(as.hclust(clusterman3), h = 40)
table(arbol_cortado2)

arbol_cortado3 = cutree(as.hclust(clusterman3), h = 35)
table(arbol_cortado3)

arbol_cortado4 = cutree(as.hclust(clusterman3), h = 30)
table(arbol_cortado4)

arbol_cortado5 = cutree(as.hclust(clusterman3), h = 25)
table(arbol_cortado5)


# Se imputa la culsterización a la base de datos
#datos_dummies = as.data.frame(datos_dummies) 
datos_dummies$clusters1 = arbol_cortado1
datos_dummies$clusters2 = arbol_cortado2
datos_dummies$clusters3 = arbol_cortado3
datos_dummies$clusters4 = arbol_cortado4
datos_dummies$clusters5 = arbol_cortado5

View(datos_dummies)
summary(datos_dummies)

########################################################

# Una vez descartado el cálculo de los NA, 
# procedemos a omitir la observaciones con datos faltantes

########################################################

#Razones por las que omitimos los datos faltantes:
  # 1) No nos fue posible inputar los NA
  # 2) La muestra es lo suficientemente grande para permitirnos perder el 20% de las obs
  # 3) Al inputar la variable de interés ("salary") estamos, mejorando la eficacia de los modelos 
  # porque inpute la variable de interés.

#====================================================================
# Reiniciamos el ambiente

# Borramos datos de la memoria
rm(list = ls())

# Establecemos directorio de trabajo
#setwd("C:/Users/Cecilia Machado/Desktop/Obligatorio Analítica 2")
setwd("C:/Users/el_to/OneDrive/Escritorio/Drive/ORT/8vo Sem/Analitica de Negocios 2/Obligatorio/")

# Cargamos libreria a utilizar
library(ISLR)
library(cluster)
library(ranger)
# Cargamos los datos
datos = Hitters

# Visualizamos los datos 
View(datos)
#====================================================================

#**********
# Parte 3 
#**********

#****************************
#3.1) MODELO 1: RANDOM FOREST
#****************************

# Omitimos datos NA
datos_nNA = na.omit(datos)

# Separamos base en test y train
set.seed(123451)
train <- sample(nrow(datos_nNA), nrow(datos_nNA)*0.70)
test <- (-train)

# Modelo Random Forest
RF=ranger(log(Salary)~., data=datos_nNA[train,],seed=123451)
RF

# Predicción
pred_ranger = predictions(predict(RF,data=datos_nNA))
pred_ranger

# Errores relativo en train y test


round(100*sqrt(mean((log(datos_nNA$Salary[train])-pred_ranger[train])^2))/mean(log(datos_nNA$Salary)),2) #train

round(100*sqrt(RF$prediction.error)/mean(log(datos_nNA$Salary)),2)  #verdadero train

round(100*sqrt(mean((log(datos_nNA$Salary[test])-pred_ranger[test])^2))/mean(log(datos_nNA$Salary)),2) #test


#       Train  (CV)  / Test / 
# RFi    3,28 (7.73) / 7,21 /

# Optimizacion de parametros de nuestro Random Forest inicial

opt = expand.grid(
  'num_trees' = c(300, 425, 490, 550, 700),
  'mtry'      = c(2, 3, 4, 7),  #Verificar con otros valores (mayores y menores al inicial)
  'min.node.size' =c(2,4,6))

#opt

oob_error = rep(NA, nrow(opt))

min_i = 0
min_error = 99999
correcto = 0
for(i in 1:nrow(opt)){
  
  modelo <- ranger(
    formula   = log(Salary) ~ .,
    data      = datos_nNA[train,], 
    num.trees = opt$num_trees[i],
    mtry      = opt$mtry[i],
    min.node.size = opt$min.node.size[i],
    seed      = 123451)

  oob_error[i] <- round(100*sqrt(modelo$prediction.error)/mean(log(datos_nNA$Salary)),2)
  
  if(oob_error[i] < min_error){
    min_error = oob_error[i]
    min_i = i
    correcto = cbind(modelo$num.trees, modelo$mtry, modelo$min.node.size)
  }
}

print(correcto)


# Modelo final con los datos que mejoran el error

RF <- ranger(
  formula   = log(Salary) ~ .,
  data      = datos_nNA[train,], 
  num.trees = correcto[1,1],
  mtry      = correcto[1,2],
  min.node.size = correcto[1,3],
  importance='permutation',
  seed      = 123451)

RF



oob_errorFinal <- round(100*sqrt(RF$prediction.error)/mean(log(datos_nNA$Salary)),2)
oob_errorFinal

# predicion
pred_ranger=predictions(predict(RF,data=datos_nNA))

# errores relativo en train y test

round(100*sqrt(mean((log(datos_nNA$Salary[train])-pred_ranger[train])^2))/mean(log(datos_nNA$Salary)),2) #train

round(100*sqrt(RF$prediction.error)/mean(log(datos_nNA$Salary)),2)  #verdadero train

round(100*sqrt(mean((log(datos_nNA$Salary[test])-pred_ranger[test])^2))/mean(log(datos_nNA$Salary)),2) #test


#       Train  (CV)  / Test / 
# RFi    3,28 (7.73) / 7,21 /
# RFf    2.93 (7.67) / 7.31 /

# Importancia de las variables
sort(RF$variable.importance)     #importancia de las variables ordenadas

# Gráfico de las variables de mayor importancia
library(vip)
vip(RF, horizontal = FALSE)


#Reiniciamos ambiente
#====================================================================
# Reiniciamos el ambiente

rm(list = ls())

# Establecemos directorio de trabajo
#setwd("C:/Users/Cecilia Machado/Desktop/Obligatorio Analítica 2")
setwd("C:/Users/el_to/OneDrive/Escritorio/Drive/ORT/8vo Sem/Analitica de Negocios 2/Obligatorio/")

# Cargamos libreria a utilizar
library(ISLR)
library(cluster)
library(ranger)
library(glmnet)

# Cargamos los datos
datos = Hitters

# Visualizamos los datos 
View(datos)
#====================================================================

#************************************
#3.2) MODELO 2: RANDOM FOREST con ACP
#************************************

# Omitimos datos Na

datosNNa=na.omit(datos)

# Crear las dummys para ACP

datosNNa$League <- ifelse(datosNNa$League == 'A', 1, 0)
datosNNa$Division <- ifelse(datosNNa$Division == 'E', 1, 0)
datosNNa$NewLeague <- ifelse(datosNNa$NewLeague == 'A', 1, 0)


# Separamos los datos en train y test
set.seed(123451)   
train <- sample(nrow(datosNNa), nrow(datosNNa)*0.7) 
test <- (-train)

# realizamos el analisis de componentes principales
acp <- prcomp(datosNNa[train,-19], scale = TRUE)

summary(acp)

p=7

datos=as.data.frame(cbind(acp$x[,1:p],datosNNa[train,19]))
colnames(datos)=c(paste('PC',1:p,sep = ''),'Salary')

# Modelo
RF=ranger(log(Salary)~., data=datos,seed=123451)
RF

# Optimizacion de parametros

opt = expand.grid(
  'num_trees' = c(300, 400, 500,550, 600,625, 650,700, 800, 850),
  'mtry'      = c(1,2, 3, 4),
  'min.node.size' =c(3,5,8))

#opt

oob_error = rep(NA, nrow(opt))
min_i = 0
min_error = 99999
correcto = 0
for(i in 1:nrow(opt)){
  
  modelo <- ranger(
    formula   = log(Salary) ~ .,
    data      = datos, 
    num.trees = opt$num_trees[i],
    mtry      = opt$mtry[i],
    min.node.size = opt$min.node.size[i],
    seed      = 123451)

  oob_error[i] <- round(100*sqrt(modelo$prediction.error)/mean(log(datosNNa$Salary)),2)
  if(oob_error[i] < min_error){
    min_error = oob_error[i]
    min_i = i
    correcto = cbind(modelo$num.trees, modelo$mtry, modelo$min.node.size)
  }
  
}
print(correcto)

# Modelo final

RF <- ranger(
  formula   = log(Salary) ~ .,
  data      = datos, 
  num.trees = correcto[1,1],
  mtry      = correcto[1,2],
  min.node.size = correcto[1,3],
  importance='permutation',
  seed      = 123451)

RF

# escalando los resultados
base_scale=scale(datosNNa[,-19],center=acp$center,scale=acp$scale)

# creando los componentes para todas las observaciones
base_ACP=as.data.frame(base_scale%*%acp$rotation[,1:p])
colnames(base_ACP)=paste('PC',1:p,sep = '')

# predicion
pred_rangerACP=predictions(predict(RF,data=base_ACP))

# errores relativo en train y test

round(100*sqrt(mean((log(datosNNa$Salary[train])-pred_rangerACP[train])^2))/mean(log(datosNNa$Salary)),2) #train

round(100*sqrt(RF$prediction.error)/mean(log(datosNNa$Salary)),2)  #verdadero train

round(100*sqrt(mean((log(datosNNa$Salary[test])-pred_rangerACP[test])^2))/mean(log(datosNNa$Salary)),2) #test

# Importancia de las variables
sort(RF$variable.importance)     #importancia de las variables ordenadas

barplot(RF$variable.importance)


#          Train  (CV)  / Test / 
# RFi       3,28 (7.73) / 7,21 /
# RFf       2.93 (7.67) / 7.31 /
# RFacp(5)  3.57 (8.32) / 7.63 /
# RFacp(6)  3.67 (8.67) / 8.17 /
# RFacp(7)  4.28 (8.92) / 8.57 /


#Reiniciamos el ambiente
#====================================================================

# Borramos datos de la memoria
rm(list = ls())

# Establecemos directorio de trabajo
#setwd("C:/Users/Cecilia Machado/Desktop/Obligatorio Analítica 2")
setwd("C:/Users/el_to/OneDrive/Escritorio/Drive/ORT/8vo Sem/Analitica de Negocios 2/Obligatorio/")

# Cargamos libreria a utilizar
library(ISLR)
library(cluster)
library(ranger)
library(glmnet)

# Cargamos los datos
datos = Hitters

# Visualizamos los datos 
View(datos)
#====================================================================

#***********************************
#3.3) MODELO 3: RIDGE
#***********************************

# Omitimos datos Na

datosNNa=na.omit(datos)

# Crear las dummys

datosNNa$League <- ifelse(datosNNa$League == 'A', 1, 0)
datosNNa$Division <- ifelse(datosNNa$Division == 'E', 1, 0)
datosNNa$NewLeague <- ifelse(datosNNa$NewLeague == 'A', 1, 0)

# Separamos los datos en train y test
set.seed(123451)   
train <- sample(nrow(datosNNa), nrow(datosNNa)*0.7) 
test <- (-train)

# Modelo rigde

reg_ridge <- glmnet(
  x           = datosNNa[train,-19],
  y           = log(datosNNa[train,19]),
  alpha       = 0,
  nlambda     = 100,
  standardize = TRUE
)

# cross -validation
set.seed(123451)
cv_error <- cv.glmnet(
  x      = as.matrix(datosNNa[train,-19]),
  y      = log(datosNNa[train,19]),
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)
cv_error

reg_ridge.min <- glmnet(
  x           = as.matrix(datosNNa[train,-19]),
  y           = log(datosNNa[train,19]),
  alpha       = 0,
  lambda      = cv_error$lambda.min,
  standardize = TRUE
)
# beta0
reg_ridge.min$a0

# distintos betas
reg_ridge.min$beta

# predicciones

pred_ridge.min <- predict(reg_ridge.min,as.matrix(datosNNa[,-19]))

# errores relativo en train y test

round(100*sqrt(mean((log(datosNNa$Salary[train])-pred_ridge.min[train])^2))/mean(log(datosNNa$Salary)),2) #train

round(100*sqrt(cv_error$cvm[cv_error$index[1]])/mean(log(datosNNa$Salary)),2)  #verdadero train

round(100*sqrt(mean((log(datosNNa$Salary[test])-pred_ridge.min[test])^2))/mean(log(datosNNa$Salary)),2) #test



#          Train  (CV)  / Test / 
# RFi       3,28 (7.73) / 7,21 /
# RFf       2.93 (7.67) / 7.31 /
# RFacp(5)  3.57 (8.32) / 7.63 /
# RFacp(6)  3.67 (8.67) / 8.17 /
# RFacp(7)  4.28 (8.92) / 8.57 /

# Reiniciamos el ambiente
#====================================================================
# Borramos datos de la memoria
rm(list = ls())

# Establecemos directorio de trabajo
#setwd("C:/Users/Cecilia Machado/Desktop/Obligatorio Analítica 2")
setwd("C:/Users/el_to/OneDrive/Escritorio/Drive/ORT/8vo Sem/Analitica de Negocios 2/Obligatorio/")

# Cargamos libreria a utilizar
library(ISLR)
library(cluster)
library(ranger)
library(glmnet)
library(car)

# Cargamos los datos
datos = Hitters

# Visualizamos los datos 
View(datos)
#====================================================================

#*******************************
#3.4) MODELO 4: REGRESIÓN LINEAL
#*******************************

# Omitimos datos Na

datosNNa=na.omit(datos)

# Crear las dummys

datosNNa$League <- ifelse(datosNNa$League == 'A', 1, 0)
datosNNa$Division <- ifelse(datosNNa$Division == 'E', 1, 0)
datosNNa$NewLeague <- ifelse(datosNNa$NewLeague == 'A', 1, 0)

# Separamos los datos en train y test
set.seed(123451)   
train <- sample(nrow(datosNNa), nrow(datosNNa)*0.7) 
test <- (-train)

# Regresión con todos los parámetros
reg <- lm(log(Salary)~., data= datosNNa, subset=train)
summary(reg)

# Algoritmo de selección de variables
reg_step=step(reg, direction = "both", trace = 1)

# Control de multicolinealidad

vif(reg_step)

# Regresión lineal con selección de variables independientes

reg2=lm(formula = log(Salary) ~  Hits + HmRun + Walks + Years + 
          League + Division + PutOuts + Assists + 
          Errors, data = datosNNa, subset = train)
vif(reg2)
summary(reg2)

# Regresión lineal FINAL con selección de variables independientes

reg3=lm(formula = log(Salary) ~  Hits + Walks + Years + 
          League + PutOuts + Errors, data = datosNNa, subset = train)
vif(reg3)
summary(reg3)

# predicciones

pred_lm <- predict(reg3,datosNNa[,-19])

# errores relativo en train y test

RL_error_train = round(100*sqrt(mean((log(datosNNa$Salary[train])-pred_lm[train])^2))/mean(log(datosNNa$Salary)),2) #train

RL_error_test =round(100*sqrt(mean((log(datosNNa$Salary[test])-pred_lm[test])^2))/mean(log(datosNNa$Salary)),2) #test

#**************************************
# Parte 4: Tabla comparativa de errores 
#*************************************

#Errores de cada modelo
RF_error = c(2.93, 7.67, 7.31)
RFacp_error = c(4.28, 8.92, 8.57)
Ridge_error = c(9.61, 10.36, 11.62)
RegLin_error = c(9.92, NA, 11.56)

# Combinación de errores
tabla_errores = cbind(RF_error,RFacp_error, Ridge_error, RegLin_error)

# Creación de tabla con todos los modelos
tabla_final <- matrix(tabla_errores, ncol=3, byrow=TRUE)
colnames(tabla_final) <- c('Train','CV','Test')
rownames(tabla_final) <- c('Random Forest','Random Forest con ACP','Ridge', 'Regresión Lineal')
tabla_final

# Creación de tabla comparando Random Fores y Random Forest ACP
tabla_errores2 = cbind(RF_error, RFacp_error)
tabla_final <- matrix(tabla_errores2, ncol=3, byrow=TRUE)
colnames(tabla_final) <- c('Train','CV','Test')
rownames(tabla_final) <- c('Random Forest','Random Forest con ACP')
tabla_final

# ELEGIMOS EL MODELO DE RANDOM FOREST

###################
# FIN DEL ANALISIS#
###################