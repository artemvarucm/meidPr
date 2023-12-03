# Artem Vartanov y Mario Baldocchi
# bibliotecas necesarias
library(nortest) # test normalidad
library(lmtest) # test homoscedasticidad
library(MASS) # box-cox
library(leaps) # regsubsets
library(moments) # skewness
library(car) # influenceIndexPlot, vif...
library(pls) # pcr, pls
library(glmnet) # ridge, lasso, elasticnet...
library(caret) # reg. logistica confusionMatrix

####### FASE: IMPORT Y PREPROCESS
datos <- read.table(file = "/Users/tyomikjan/UNIVERSITY/R/TRABAJO DATOS/meidPr/Family Income and Expenditure.csv", fill = TRUE, header = TRUE, sep = ",")

dim(datos)

summary(datos)

# Nuestra variable Respuesta es Total.Household.Income, que es el salario anual por vivienda filipina

# Tratamos columnas como categoricas
datos$Region = as.factor(datos$Region)
datos$Agricultural.Household.indicator = as.factor(datos$Agricultural.Household.indicator)
datos$Household.Head.Sex = as.factor(datos$Household.Head.Sex)
datos$Household.Head.Marital.Status = as.factor(datos$Household.Head.Marital.Status)
datos$Household.Head.Highest.Grade.Completed = as.factor(datos$Household.Head.Highest.Grade.Completed)
datos$Household.Head.Job.or.Business.Indicator = as.factor(datos$Household.Head.Job.or.Business.Indicator)
datos$Household.Head.Occupation = as.factor(datos$Household.Head.Occupation)
datos$Household.Head.Class.of.Worker = as.factor(datos$Household.Head.Class.of.Worker)
datos$Type.of.Household = as.factor(datos$Type.of.Household)
datos$Type.of.Building.House = as.factor(datos$Type.of.Building.House)
datos$Type.of.Roof = as.factor(datos$Type.of.Roof)
datos$Type.of.Walls = as.factor(datos$Type.of.Walls)
datos$Tenure.Status = as.factor(datos$Tenure.Status)
datos$Toilet.Facilities = as.factor(datos$Toilet.Facilities)
datos$Electricity = as.factor(datos$Electricity)
datos$Main.Source.of.Income = as.factor(datos$Main.Source.of.Income)
datos$Main.Source.of.Water.Supply = as.factor(datos$Main.Source.of.Water.Supply)

summary(datos)

# Vemos que hay algunas columnas como Household.Head.Occupation y Household.Head.Class.of.Worker que tienen muchos nulos.
# Quitamos las columnas que tengan algun nulo
sum(!complete.cases(datos)) # 3355
sum(!complete.cases(datos[,-c(31, 32)])) # 0
datos = datos[,-c(31, 32)] # quitamos columnas con N/A
sum(!complete.cases(datos))

# Seleccionamos las columnas numericas solo
datos.solo.num = datos[sapply(datos, is.numeric)]

# Histogramas
par(mfrow=c(3, 5))
for (x in colnames(datos)) {
  hist(table(datos[, x]), xlab=x)
}

# Plots
par(mfrow=c(3, 5))
for (x in colnames(datos)) {
  plot(datos[, x], xlab=x)
}

# Quitamos outliers segun todas las columnas
datos.sin.out = datos.solo.num
coefIQR = 3 # coeficiente por el cual se multiplicará el IQR para quitar outliers
for (x in colnames(datos.sin.out)) { # Quitamos columnas con muy pocas alternativas (casi categoricas, Number of TVs)
  col = datos.sin.out[, x]
  print(x)
  print(dim(datos.sin.out))
  
  if (abs(skewness(col)) > 2) { # en simetricas, skewness esta entre (-2, 2)
    # si no es simetrica, la transformamos con box cox, 
    # pero la columna del dataframe queda igual
    col = dlookr::transform(col, method="Box-Cox")
  }
  q1 = quantile(col, 0.25)
  q3 = quantile(col, 0.75)
  iqr = q3 - q1
  if (iqr > 0) { # si los dos quantiles coinciden, evitamos quitar todos los datos
    datos.sin.out = datos.sin.out[col < q3 + coefIQR*iqr,]
    col = col[col < q3 + coefIQR*iqr]
    datos.sin.out = datos.sin.out[col > q1 - coefIQR*iqr,]
  }
}

# Quitamos las columnas agregadas (mucha correlacion con otras, literalmente combinacion lineal)

total.num.fam.mem = datos.sin.out["Total.Number.of.Family.members"] # guardamos para logistica

aggrCols = c("Total.Food.Expenditure", "Total.Number.of.Family.members")
for (col in aggrCols) {
  indR = grep(col, colnames(datos.sin.out))
  datos.sin.out = datos.sin.out[, -indR]
}

rownames(datos.sin.out)=1:nrow(datos.sin.out) # renombramos indices

####### FASE END: IMPORT Y PREPROCESS

####### FASE BEGIN: (AUTO)CORRELACIONES

# 43 columnas numericas
length(datos.solo.num) 

# Analizamos autocorrelaciones entre variables (excluimos la respuesta)
# Quitamos la variable respuesta
indR = grep("Total.Household.Income", colnames(datos.sin.out))
datos.sin.autocorr =  datos.sin.out[, -indR]

# Mapa de calor de las autocorrelaciones
heatmap(cor(datos.sin.autocorr, method="spearman"))
# No se ve bien, bastante dificil

# Guardara las parejas de variables altamente correlacionadas (pendientes a eliminar)
autocor.pairs = data.frame()
# Lista de columnas autocorrelacionadas (para heatmap)
autocor.cols = c()
i = 1
while (length(datos.sin.autocorr) >= i){
  # quitamos de las correlaciones la misma columna, 
  # porque una variable consigo misma tiene correlacion 1 siempre
  if (any(abs(cor(datos.sin.autocorr, method="spearman")[i, -i]) > 0.7)) { 
    autocor.cols = c(autocor.cols, colnames(datos.sin.autocorr)[i])
    for (col in rownames(as.matrix(which(abs(cor(datos.sin.autocorr, method="spearman")[i, -i]) > 0.7)))) {
      if (length(autocor.pairs) == 0) {
        autocor.pairs = as.data.frame(c(col))
        colnames(autocor.pairs)[length(colnames(autocor.pairs))] = colnames(datos.sin.autocorr)[i]
      } else if (length(autocor.pairs[1, col]) == 0 || autocor.pairs[1, col] != colnames(datos.sin.autocorr)[i]) { # si no esta anadido todavia
        autocor.pairs = cbind(autocor.pairs, c(col))
        colnames(autocor.pairs)[length(colnames(autocor.pairs))] = colnames(datos.sin.autocorr)[i]
      }
    }
  }
  i = i + 1
}

autocor.pairs

# Vemos que hay dos parejas que estan muy correlacionadas
heatmap(cor(datos.sin.autocorr[, autocor.cols]))
# Viendo las parejas, quitamos las que tengan MENOS correlacion con la var. respuesta
cor(datos.sin.out)["Total.Household.Income", autocor.cols]

indDelete = c(
  grep("Total.Rice.Expenditure", colnames(datos.sin.autocorr)), 
  grep("Housing.and.water.Expenditure", colnames(datos.sin.autocorr)) # quitamos esta porque sale correlacionada con dos a la vez
)

datos.sin.autocorr =  datos.sin.autocorr[, -indDelete]

# Probamos otra vez

# Lista de columnas autocorrelacionadas
autocor.cols = c()
i = 1
while (length(datos.sin.autocorr) >= i){
  # quitamos de las correlaciones la misma columna, 
  # porque una variable consigo misma tiene correlacion 1 siempre
  if (any(abs(cor(datos.sin.autocorr, method="spearman")[i, -i]) > 0.7)) { 
    autocor.cols = c(autocor.cols, colnames(datos.sin.autocorr)[i])
  }
  i = i + 1
}

length(autocor.cols) # 0 - no hay columnas autocorrelacionadas (correlacion en abs mas de 0.7)

length(datos.sin.autocorr) # quedan 38 columnas

sum(cor(datos.sin.autocorr, method="spearman") > 0.7) # solo estan las diagonales

#View(datos.sin.autocorr)

# Volvemos la columna a predecir
datos.sin.autocorr = cbind(datos.sin.out$Total.Household.Income, datos.sin.autocorr)
# Renombramos columna
colnames(datos.sin.autocorr)[1] = "Total.Household.Income"

# Columnas sin autocorrelacion
cols.sin.autocorr = colnames(datos.sin.autocorr) # Lo usaremos mas adelante en la RLM_BEST

# Tabla de correlaciones para columnas (incluye variable respuesta)
corr.sin.autocorr = cor(datos.sin.autocorr, method="spearman")

# Vemos las columnas correlacionadas linealmente con la variable respuesta Total.Household.Incom
corr.sin.autocorr[,"Total.Household.Income"]

# Como hay demasiadas columnas numericas, es muy dificil sacar conclusiones
# Seleccionamos columnas con correlaciones en valor absoluto mayores que 0.7 con la variable Total.Household.Income
# Lista de columnas 
# (Total.Household.Income esta dentro porque su correlacion es 1)
high.corr.cols = rownames(data.frame(corr.sin.autocorr[abs(corr.sin.autocorr[,"Total.Household.Income"]) > 0.7, "Total.Household.Income"]))
high.corr.cols
# Las usaremos mas adelante en la RLM.high.corr

####### FASE END: (AUTO)CORRELACIONES

####### FASE BEGIN: RLM FULL

# Regresion multiple para el conjunto completo (sin columnas categoricas ni nominales) (modelo 0)
# Tambien estan las autocorrelacionadas aqui
RLM_FULL = lm(Total.Household.Income ~ ., data=datos.sin.out)

# Analisis de los p-valores
summary(RLM_FULL)

# Intentamos predecir (sobre todo el modelo de datos)
RLM_FULL.predict = predict(RLM_FULL,datos)
# raiz del error cuadratico medio de la prediccion
RLM_FULL.error = (sum((datos$Total.Household.Income - RLM_FULL.predict) ^ 2)/length(RLM_FULL.predict)) ^ 0.5
RLM_FULL.error # 134325.9

# Vemos si hay colinealidad con el VIF
vif(RLM_FULL)

####### FASE END: RLM FULL

####### FASE BEGIN: RLM REDUCIDA
# Separamos los datos en train y test (80/20, 75/25, 70/30...)
set.seed(20)

train_indices = sample(1:nrow(datos.sin.out), floor(nrow(datos.sin.out) * 0.8), replace=FALSE)

data_train = datos.sin.out[train_indices,]

rownames(data_train)=1:nrow(data_train) # renombramos indices (para outliers)

data_test = datos.sin.out[-train_indices,] # olvidamos de momento del test

rownames(data_test)=1:nrow(data_test) # renombramos indices







# Regresion lineal multiple

# Primero intentamos usar las columnas altamente correlacionadas con Total.Household.Income, 
# (estan en high.corr.cols)

RLM_high.cor = lm(Total.Household.Income ~., data = data_train[, high.corr.cols])

# Intentamos predecir (error sobre data_test)
RLM_high.cor.predict = predict(RLM_high.cor,data_test)
# raiz del error cuadratico medio de la prediccion
RLM_high.cor.error = (sum((data_test$Total.Household.Income - RLM_high.cor.predict) ^ 2)/length(RLM_high.cor.predict)) ^ 0.5
RLM_high.cor.error # 272143.5 

# Quitamos obsevaciones influyentes
# Viendo el grafico, vamos a realizar una limieza de residuos menores que -8 y mayores que 8 (simetrico)
# outlierTest no se puede usar, pues no son normales las variables
car::influenceIndexPlot(RLM_high.cor, vars="student")
rEst = rstudent(RLM_high.cor)  # Residuos estudentizados
outliers=c(which(rEst>8),which(rEst<(-8)))
data.copy = data_train[-outliers,]
rownames(data.copy)=1:nrow(data.copy) # renombramos indices

RLM_high.cor.rest = lm(Total.Household.Income ~., data = data.copy[, high.corr.cols])

# Intentamos predecir (error sobre data_test)
RLM_high.cor.predict = predict(RLM_high.cor.rest,data_test)
# raiz del error cuadratico medio de la prediccion
RLM_high.cor.error = (sum((data_test$Total.Household.Income - RLM_high.cor.predict) ^ 2)/length(RLM_high.cor.predict)) ^ 0.5
RLM_high.cor.error # 272512.1 
# El error ha aumentado, prediccion peor
# Las observaciones que hemos quitado no fueron correctas
# Descartamos esta eliminacion de obs. influyentes

# Quitamos observaciones influyentes con alto leverage
car::influenceIndexPlot(RLM_high.cor, vars="hat")
hats = hatvalues(RLM_high.cor)
outliers=which(hats>0.02)
data.copy = data_train[-outliers,]
rownames(data.copy)=1:nrow(data.copy) # renombramos indices

RLM_high.cor = lm(Total.Household.Income ~., data = data.copy[, high.corr.cols])

# Intentamos predecir (error sobre data_test)
RLM_high.cor.predict = predict(RLM_high.cor,data_test)
# raiz del error cuadratico medio de la prediccion
RLM_high.cor.error = (sum((data_test$Total.Household.Income - RLM_high.cor.predict) ^ 2)/length(RLM_high.cor.predict)) ^ 0.5
RLM_high.cor.error # 269037.3
# En este caso ha mejorado. 
# Nos quedamos con este modelo, pero no nos interesa seguir con este modelo. 
# Error bastante critico

# Cross Validation + Regsubsets para encontrar el mejor modelo (best.RLM)

# Definimos una funcion para predecir a partir de regsubsets object, input newdata e id - n de variables
predict.regsubsets <- function(object, newdata, id,...){
  form <-as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object,id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

# Nos quedamos solo con columnas numericas sin autocorrelacion
data_train.solo.num = data_train[, cols.sin.autocorr]

n <- nrow(data_train.solo.num) 
k <- 4 # número de grupos, 4 porque se queda con 26 variables en vez de 40

set.seed(5)
# asigna a cada fila de da  ta train un grupo
folds <- sample(x=1:k, size=nrow(data_train.solo.num), replace = TRUE)

# Realizamos regsubsets(construccion del modelo mediante foreward, backward...)
numVars = dim(data_train.solo.num)[2] - 1 # restamos 1 por la variable respuesta, solo cuenta las explicativas
cv.errors <- matrix(NA, k, numVars, dimnames = list(NULL,paste(1:numVars)))
# betas para el modelo con minimo error
for (j in 1:k){
  # j es el grupo que dejamos fuera del conjunto de entrenar
  best.fit <- regsubsets(Total.Household.Income~., data=data_train.solo.num[folds !=j,], nvmax = numVars)
  for (i in 1:numVars){ # hay un limite en regsubsets de 8 variables
    pred <- predict.regsubsets(best.fit, newdata=data_train.solo.num[folds==j,], id=i) #datos de test
    cv.errors[j,i] <- (mean((data_train.solo.num$Total.Household.Income[folds == j]-pred)^2)) ^ 0.5
  }
}  
# Raices cuadradas de errores
cv.errors  
# Hacemos las medias 
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
min.err.betas = coef(best.fit, which.min(mean.cv.errors))
# Modelo con which.min(mean.cv.errors) variables
data.regsubsets = data_train.solo.num[, c("Total.Household.Income", names(min.err.betas[-1] ))] # quitamos intercept

RLM_regsubsets = lm(Total.Household.Income ~ ., data=data.regsubsets) 

summary(RLM_regsubsets)
# Intentamos predecir (error sobre data_test)
RLM_regsubsets.predict = predict(RLM_regsubsets,data_test)
# raiz del error cuadratico medio de la prediccion
RLM_regsubsets.error = (sum((data_test$Total.Household.Income - RLM_regsubsets.predict) ^ 2)/length(RLM_regsubsets.predict)) ^ 0.5
RLM_regsubsets.error # 147132.9


# Quitamos obsevaciones influyentes
# Viendo el grafico, vamos a realizar una limieza de residuos menores que -8 y mayores que 8 (simetrico)
# outlierTest no se puede usar, pues no son normales las variables
car::influenceIndexPlot(RLM_regsubsets, vars="student") 
rEst = rstudent(RLM_regsubsets)  # Residuos estudentizados
outliers=c(which(rEst>10),which(rEst<(-10)))
data.rest = data.regsubsets[-outliers,]
rownames(data.rest)=1:nrow(data.rest) # renombramos indices

RLM_regsubsets = lm(Total.Household.Income ~ ., data=data.rest) 

# Intentamos predecir (error sobre data_test)
RLM_regsubsets.predict = predict(RLM_regsubsets,data_test)
# raiz del error cuadratico medio de la prediccion
RLM_regsubsets.error = (sum((data_test$Total.Household.Income - RLM_regsubsets.predict) ^ 2)/length(RLM_regsubsets.predict)) ^ 0.5
RLM_regsubsets.error # 141859.3
# El error ha reducido, prediccion mejor. nos quedamos con este modelo

# Quitamos observaciones influyentes con alto leverage
car::influenceIndexPlot(RLM_regsubsets, vars="hat")
hats = hatvalues(RLM_regsubsets)
outliers=which(hats>0.21)
data.hat = data.rest[-outliers,]
rownames(data.hat)=1:nrow(data.hat) # renombramos indices

RLM_regsubsets = lm(Total.Household.Income ~ ., data=data.hat) 
# Intentamos predecir (error sobre data_test)
RLM_regsubsets.predict = predict(RLM_regsubsets,data_test)
# raiz del error cuadratico medio de la prediccion
RLM_regsubsets.error = (sum((data_test$Total.Household.Income - RLM_regsubsets.predict) ^ 2)/length(RLM_regsubsets.predict)) ^ 0.5
RLM_regsubsets.error # 140866.3
# El error ha reducido, prediccion mejor. nos quedamos con este modelo
# Estudiamos la colinealidad
vif(RLM_regsubsets)

summary(RLM_regsubsets)

# Veamos los tests.
residuos<-RLM_regsubsets$residuals
lillie.test(residuos) # no cumple
#homocedatsicidad
bptest(RLM_regsubsets) # no cumple
##independencia
dwtest(RLM_regsubsets) # cumple

# Transformamos a box-cox para ver si podemos hacer que den los tests
data.copy = data.hat

x<-data.copy$Total.Household.Income
sum(x <= 0) # 0 - se puede aplicar box cox
b<-boxcox(lm(x~1))
lambda<-b$x[which.max(b$y)]
# El valor de lambda que usaremos
lambda
ynew<-(x^lambda-1)/lambda # transformamos la variable respuesta

data.copy$Total.Household.Income = ynew # cambiamos la variable respuesta en el dataset

RLM_regsubsets.box.cox <- lm(Total.Household.Income ~ ., data=data.copy) # formamos nuevo modelo


# Veamos los tests.
residuos<-RLM_regsubsets.box.cox$residuals
lillie.test(residuos) # no cumple
#homocedatsicidad
bptest(RLM_regsubsets.box.cox) # no cumple
##independencia
dwtest(RLM_regsubsets.box.cox) # cumple

# Quitamos outliers segun variable respuesta, es simetrica ahora
skewness(ynew) # simetrica
boxplot(ynew) 
IQR = quantile(ynew, 0.75) - quantile(ynew, 0.25)
# Limite inferior
lowerBound = quantile(ynew, 0.25) - 1.5*IQR
# Limite superior
upperBound = quantile(ynew, 0.75) + 1.5*IQR
data.copy = data.copy[ynew < upperBound,]
ynew = ynew[ynew < upperBound]
data.copy = data.copy[ynew > lowerBound,]
RLM_regsubsets.box.cox <- lm(Total.Household.Income ~ ., data=data.copy) # formamos nuevo modelo

# Veamos los tests.
residuos<-RLM_regsubsets.box.cox$residuals
lillie.test(residuos) # no cumple
#homocedatsicidad
bptest(RLM_regsubsets.box.cox) # no cumple
##independencia
dwtest(RLM_regsubsets.box.cox) # cumple


# Transformamos las variables explicativas
# Guardamos lambda para las columnas, para la prediccion posterior
lambdas = data.frame("Total.Household.Income"=c(lambda))


indR = grep("Total.Household.Income", colnames(data.copy)) # la respuesta ya esta transformada

for (c in colnames(data.copy[, -indR])) {
  print(c)
  if (abs(skewness(data.copy[,c])) < 2 && !any(data.copy[,c] < 0)) { # si no es simetrica, transformamos
    # box cox no vale para valores negativos o cero
    x = data.copy[,c]
    if (any(x == 0)) {
      # reemplazamos los 0 con algo muy proximo a ello
      # porque logaritmo de 0 no existe
      x[x == 0] = 1e-20
    }
    
    b<-boxcox(lm(x~1))
    lambda<-b$x[which.max(b$y)]
    lambdas = cbind(lambdas, c(lambda))
    colnames(lambdas)[length(colnames(lambdas))] = c # renombramos nueva columna
    transf.col<-(x^lambda-1)/lambda # transformamos la variable respuesta
    
    data.copy[,c] = transf.col # cambiamos la variable respuesta en el dataset
    # reconstruimos modelo
    RLM_regsubsets.box.cox <- lm(Total.Household.Income ~ ., data=data.copy)
    if (lillie.test(RLM_regsubsets.box.cox$residuals)$p.value > 0.0001) {
      break
    } else {
      
      print("lillie-test p-value")
      print(lillie.test(RLM_regsubsets.box.cox$residuals)$p.value)
      # quitamos outliers
      IQR = quantile(transf.col, 0.75) - quantile(transf.col, 0.25)
      if (IQR > 0) {
        # Limite inferior
        lowerBound = quantile(transf.col, 0.25) - 1.5*IQR
        # Limite superior
        upperBound = quantile(transf.col, 0.75) + 1.5*IQR
        data.copy = data.copy[data.copy[,c] < upperBound,]
        data.copy = data.copy[data.copy[,c] > lowerBound,]
        rownames(data.copy) = 1:nrow(data.copy) # renombramos indices
        RLM_regsubsets.box.cox <- lm(Total.Household.Income ~ ., data=data.copy)
        if (lillie.test(RLM_regsubsets.box.cox$residuals)$p.value > 0.05) {
          break
        }
      }
    }
  }
}

# Veamos los tests.
residuos<-RLM_regsubsets.box.cox$residuals
lillie.test(residuos) # no cumple, pero mejorado
#homocedatsicidad
bptest(RLM_regsubsets.box.cox) # no cumple
##independencia
dwtest(RLM_regsubsets.box.cox) # cumple

# Quitamos observaciones influyentes
car::influenceIndexPlot(RLM_regsubsets.box.cox, vars="student")
rest = rstudent(RLM_regsubsets.box.cox)
outliers=c(which(rest < -2),which(rest > 2))
data.rest = data.copy[-outliers,]
rownames(data.rest)=1:nrow(data.rest) # renombramos indices

RLM_regsubsets.box.cox <- lm(Total.Household.Income ~ ., data=data.rest)

# Veamos los tests.
residuos<-RLM_regsubsets.box.cox$residuals
lillie.test(residuos)  # p-valor = 0.5, cumple
#homocedatsicidad
bptest(RLM_regsubsets.box.cox) # no cumple
##independencia
dwtest(RLM_regsubsets.box.cox) # cumple

summary(RLM_regsubsets.box.cox)

# transformamos a partir de lambdas
data_test.transformed = data_test
for (c in colnames(lambdas)) {
  x = data_test.transformed[, c]
  if (any(x == 0)) {
    # reemplazamos los 0 con algo muy proximo a ello
    # porque logaritmo de 0 no existe
    x[x == 0] = 1e-20
  }
  b<-boxcox(lm(x~1))
  lambda = lambdas[1,c]
  transf.col<-(x^lambda-1)/lambda # transformamos la variable respuesta
  data_test.transformed[,c] = transf.col # cambiamos la variable respuesta en el dataset
}

# Intentamos predecir (error sobre data_test)
RLM_regsubsets.predict = predict(RLM_regsubsets.box.cox,data_test.transformed)
# raiz del error cuadratico medio de la prediccion

lambda = lambdas[1,"Total.Household.Income"] # para convertir el error a algo entendible
# deshacemos box cox
pred.transf = abs(RLM_regsubsets.predict * lambda + 1) ^ (1/lambda)
errores = (data_test$Total.Household.Income - pred.transf)

RLM_regsubsets.error = (sum(errores ^ 2)/length(RLM_regsubsets.predict)) ^ 0.5
RLM_regsubsets.error # Error cosmico







# Era para probar la normalidad del modelo high corr, no ha funcionado...
# data.copy.transform = data.copy[, high.corr.cols]
# 
# x<-data.copy.transform$Total.Household.Income
# sum(x <= 0) # 0 - se puede aplicar box cox
# b<-boxcox(lm(x~1))
# lambda<-b$x[which.max(b$y)]
# # El valor de lambda que usaremos
# lambda
# ynew<-(x^lambda-1)/lambda # transformamos la variable respuesta
# 
# data.copy.transform$Total.Household.Income = ynew # cambiamos la variable respuesta en el dataset
# 
# RLM_high.cor.box.cox <- lm(Total.Household.Income ~ ., data=data.copy.transform) # formamos nuevo modelo
# 
# 
# # Veamos los tests.
# residuos<-RLM_high.cor.box.cox$residuals
# lillie.test(residuos) # no cumple
# #homocedatsicidad
# bptest(RLM_high.cor.box.cox) # no cumple
# ##independencia
# dwtest(RLM_high.cor.box.cox) # cumple
# 
# # Quitamos outliers segun variable respuesta, es simetrica ahora
# skewness(ynew) # simetrica
# boxplot(ynew) 
# IQR = quantile(ynew, 0.75) - quantile(ynew, 0.25)
# # Limite inferior
# lowerBound = quantile(ynew, 0.25) - 1.5*IQR
# # Limite superior
# upperBound = quantile(ynew, 0.75) + 1.5*IQR
# data.copy.transform = data.copy.transform[ynew < upperBound,]
# ynew = ynew[ynew < upperBound]
# data.copy.transform = data.copy.transform[ynew > lowerBound,]
# RLM_high.cor.box.cox <- lm(Total.Household.Income ~ ., data=data.copy.transform) # formamos nuevo modelo
# 
# # Veamos los tests.
# residuos<-RLM_high.cor.box.cox$residuals
# lillie.test(residuos) # no cumple
# #homocedatsicidad
# bptest(RLM_high.cor.box.cox) # no cumple
# ##independencia
# dwtest(RLM_high.cor.box.cox) # cumple
# 
# 
# # Transformamos las variables explicativas
# # Guardamos lambda para las columnas, para la prediccion posterior
# lambdas = data.frame("Total.Household.Income"=c(lambda))
# 
# 
# indR = grep("Total.Household.Income", colnames(data.copy.transform)) # la respuesta ya esta transformada
# 
# for (c in colnames(data.copy.transform[, -indR])) {
#   print(c)
#   if (abs(skewness(data.copy.transform[,c])) < 2 && !any(data.copy.transform[,c] < 0)) { # si no es simetrica, transformamos
#     # box cox no vale para valores negativos o cero
#     x = data.copy.transform[,c]
#     if (any(x == 0)) {
#       # reemplazamos los 0 con algo muy proximo a ello
#       # porque logaritmo de 0 no existe
#       x[x == 0] = 1e-20
#     }
#     
#     b<-boxcox(lm(x~1))
#     lambda<-b$x[which.max(b$y)]
#     lambdas = cbind(lambdas, c(lambda))
#     colnames(lambdas)[length(colnames(lambdas))] = c # renombramos nueva columna
#     transf.col<-(x^lambda-1)/lambda # transformamos la variable respuesta
#     
#     data.copy.transform[,c] = transf.col # cambiamos la variable respuesta en el dataset
#     # reconstruimos modelo
#     RLM_high.cor.box.cox <- lm(Total.Household.Income ~ ., data=data.copy.transform)
#     if (lillie.test(RLM_high.cor.box.cox$residuals)$p.value > 0.05) {
#       break
#     } else {
#       
#       print("lillie-test p-value")
#       print(lillie.test(RLM_high.cor.box.cox$residuals)$p.value)
#       # quitamos outliers
#       IQR = quantile(transf.col, 0.75) - quantile(transf.col, 0.25)
#       if (IQR > 0) {
#         # Limite inferior
#         lowerBound = quantile(transf.col, 0.25) - 1.5*IQR
#         # Limite superior
#         upperBound = quantile(transf.col, 0.75) + 1.5*IQR
#         data.copy.transform = data.copy.transform[data.copy.transform[,c] < upperBound,]
#         data.copy.transform = data.copy.transform[data.copy.transform[,c] > lowerBound,]
#         rownames(data.copy.transform) = 1:nrow(data.copy.transform) # renombramos indices
#         RLM_high.cor.box.cox <- lm(Total.Household.Income ~ ., data=data.copy.transform)
#         if (lillie.test(RLM_high.cor.box.cox$residuals)$p.value > 0.0001) {
#           break
#         }
#       }
#     }
#   }
# }
# 
# # Veamos los tests.
# residuos<-RLM_high.cor.box.cox$residuals
# 
# lillie.test(residuos) # no cumple
# #homocedatsicidad
# bptest(RLM_high.cor.box.cox) # no cumple
# ##independencia
# dwtest(RLM_high.cor.box.cox) # cumple
# 
# 
# # Quitamos observaciones influyentes
# car::influenceIndexPlot(RLM_high.cor.box.cox, vars="student")
# rest = rstudent(RLM_high.cor.box.cox)
# outliers=c(which(rest < -3),which(rest > 3))
# data.rest = data.copy.transform[-outliers,]
# rownames(data.rest)=1:nrow(data.rest) # renombramos indices
# 
# RLM_high.cor.box.cox <- lm(Total.Household.Income ~ ., data=data.rest)
# 
# # Veamos los tests.
# residuos<-RLM_high.cor.box.cox$residuals
# lillie.test(residuos)  # p-valor = 0.5, cumple
# #homocedatsicidad
# bptest(RLM_high.cor.box.cox) # no cumple
# ##independencia
# dwtest(RLM_high.cor.box.cox) # cumple

####### FASE END: RLM REDUCIDA

####### FASE BEGIN: RLM REGULARIZADA
# Regresion multiple Ridge, Lasso y Elasticnet (2do, 3er y 4to modelos) 

# En este caso no funciona lo de y ~ x1 + x2 ..., hay que pasar la matriz de datos
x.train<-model.matrix(Total.Household.Income~.,data_train[, cols.sin.autocorr])[,-1] # train sin respuesta
y.train<-data_train$Total.Household.Income
x.test<-model.matrix(Total.Household.Income~.,data_test[, cols.sin.autocorr])[,-1] # test sin respuesta
y.test<-data_test$Total.Household.Income

# Ridge alpha = 0
RLM_RIDGE<-glmnet(x.train,y.train,alpha=0)

# validacion cruzada para ridge
set.seed(1) # para seleccion aleatoria de folds
cv.out<-cv.glmnet(x.train,y.train,alpha=0)

plot(cv.out)
# Mejor lambda que minimiza el error de la validación cruzada
(bestlam<-cv.out$lambda.min)
# Predecimos con el bestlam
RLM_RIDGE.pred<-predict(RLM_RIDGE,s=bestlam,newx=x.test)
# Calculamos error de test
(RLM_RIDGE.error = sqrt(mean((RLM_RIDGE.pred-y.test)^2))) # 154438.2

# Lasso alpha = 1
RLM_LASSO<-glmnet(x.train,y.train,alpha=1)

# validacion cruzada para lasso
set.seed(1)
cv.out<-cv.glmnet(x.train,y.train,alpha=1)

plot(cv.out)
# Mejor lambda que minimiza el error de la validación cruzada
(bestlam<-cv.out$lambda.min)
# Predecimos con el bestlam
RLM_LASSO.pred<-predict(RLM_LASSO,s=bestlam,newx=x.test)
# Calculamos error de test
(RLM_LASSO.error = sqrt(mean((RLM_LASSO.pred-y.test)^2))) # 148707.8

# Predice un poco mejor que el modelo RIDGE

# Lo bueno es que quita variables (asignandole en coeficiente un 0)
out<-glmnet(x.train,y.train,alpha=1,lambda=bestlam)
lasso.coef<-predict(out,type="coefficients",s=bestlam)
lasso.coef
length(lasso.coef[lasso.coef!=0]) # Quedaron 30 variables de 38 iniciales

# Elasticnet
# Suponemos que glmnet por debajo usa algo asi:
# min(RSS + R_PEN * (1 - alpha) + L_PEN * alpha)
# RSS - (suma de residuos al cuadrado)
# R_PEN - penalizacion ridge
# L_PEN - penalizacion lasso
# Por tanto podremos combinar lasso y ridge seleccionando alpha entre (0,1)

models <- list()

for (i in 0:20) {
  name <- paste0("alpha", i/20)
  models[[name]] <- cv.glmnet(x.train,y.train, type.measure="mse", alpha=i/20)
}

results <- data.frame()

for (i in 0:20) {
  name <- paste0("alpha", i/20)
  ## Utilizamos cada modelo para predecir ’y’ dado el conjunto de datos de prueba
  predicted <- predict(models[[name]], s=models[[name]]$lambda.min, newx=x.test)
  ## Calculamos el ECM
  mse <- mean((predicted-y.test)^2)
  ## Almacenamos los resultados
  temp <- data.frame(alpha=i/20, lambda=models[[name]]$lambda.min, mse=mse, name=name)
  results <- rbind(results, temp)
}
print(results)
plot(results$alpha, results$mse)
pos.min=which.min(results$mse)
(minimo=results[pos.min,])  
sqrt(minimo$mse) # 148588.7

# Con elastic hemos obtenido el mejor modelo de los tres, veamos cuantas variables quedan
out<-glmnet(x.train,y.train,alpha=minimo$alpha)
elasnet.coef<-predict(out,type="coefficients",s=minimo$lambda)
elasnet.coef
length(elasnet.coef[elasnet.coef!=0]) # Quedaron 31 variables de 38 iniciales

####### FASE END: RLM REGULARIZADA

####### FASE BEGIN: PCR y PLS
# Regresion multiple PLS, PCR (5to y 6to modelos)

# PCR
# usamos todas las columnas numericas, no quitamos las autocorrelacionadas
data.pca = data_train

# Medias y cuazivarianzas muy distintas (obvio por columnas como Number.of.Television y Total.Rice.Expenditure
apply(data.pca,2,mean)
apply(data.pca,2,var)

# Por tanto necesitamos usar el escalado a la hora de realizar el modelo
set.seed(2)
RLM_PCR<-pcr(Total.Household.Income~.,data=data.pca,scale=TRUE,validation="CV")

summary(RLM_PCR)

validationplot(RLM_PCR)
# Prediccion sobre test
RLM_PCR.pred<-predict(RLM_PCR,data_test,ncomp=12)
RLM_PCR.error = sqrt(mean((RLM_PCR.pred-data_test$Total.Household.Income)^2))
RLM_PCR.error # 199790.6

y = data_test$Total.Household.Income
yhat = RLM_PCR.pred
R.squared = 1 - sum((y-yhat)^2)/sum((y-mean(y))^2)
n = length(data_test$Total.Household.Income)
p = dim(data.pca)[2] - 1 # resto uno por la respuesta
adj.r.squared = 1 - (1 - R.squared) * ((n - 1)/(n-p-1))

# PLS
set.seed(1)
RLM_PLS<-plsr(Total.Household.Income~.,data=data.pca,scale=TRUE,validation="CV")

summary(RLM_PLS)

validationplot(RLM_PLS)
# Prediccion sobre test
RLM_PLS.pred<-predict(RLM_PLS,data_test,ncomp=16)
RLM_PLS.error = sqrt(mean((RLM_PLS.pred-data_test$Total.Household.Income)^2))
RLM_PLS.error # 147659.5

# PLS predice mejor que PCR

# Ambas disminuyen la dimensionalidad a 22 componentes

####### FASE END: PCR y PLS

####### FASE BEGIN: RLM LOGISTICA

datos.log = datos.sin.out #Copiamos los datos originales 
datos.log["Total.Number.of.Family.members"] = total.num.fam.mem
datos.log$Total.Household.Income = datos.log$Total.Household.Income / 12 #Calculamos el sueldo mensual
datos.log[,"Total.Household.Income"] = datos.log$Total.Household.Income/datos.log$Total.Number.of.Family.members #Sueldo mensual per capita (sueldo mensual / numero de miembros en la unidad familiar)
#4500 es el 5% del prestamo
datos.log$Total.Household.Income[datos.log$Total.Household.Income < 4500] = 0
datos.log$Total.Household.Income[datos.log$Total.Household.Income >= 4500] = 1

datos.log$Total.Household.Income[datos.log$Total.Household.Income == 0] = "Deny" #Todos aquellos que no puedan acceder al préstamo se les pone Deny
datos.log$Total.Household.Income[datos.log$Total.Household.Income == 1] = "Accept" #Todos aquellos que puedan acceder al préstamo se les pone Accept
datos.log$Total.Household.Income = as.factor(datos.log$Total.Household.Income) #Convertimos la variable Total.Household.Income en una categórica

#Dividimos en train y test
set.seed(2)
train<-sample(nrow(datos.log),ceiling(nrow(datos.log)/2))
datos.log.train=datos.log[train,]
datos.log.test=datos.log[-train,]

glm.fits<-glm( #Entrenamos el modelo
  Total.Household.Income~.,
  data=datos.log.train,family=binomial
)

summary(glm.fits)
glm.probs<-predict(glm.fits,type="response") #Calculamos la probabilidad de que la familia pueda acceder a préstamo
Income = datos.log.test$Total.Household.Income 
contrasts(Income)
glm.pred<-rep("Accept",length(Income)) #Crea un vector con 6007 elementos "Accept"
glm.pred[glm.probs>.5]="Deny" #Transforma en Deny todos los elementos donde la probabilidad predicha<0.5

glm.pred<-as.factor(glm.pred)
confusionMatrix(glm.pred,Income)

#Como la matriz de confusión nos muestra que nuestro modelo se parece mucho a tirar una moneda, quitamos las variables con p-valor muy alto para ver si mejora
glm.fits<-glm(
  Total.Household.Income~ Bread.and.Cereals.Expenditure
  + Clothing..Footwear.and.Other.Wear.Expenditure
  + Housing.and.water.Expenditure + Medical.Care.Expenditure + Transportation.Expenditure 
  + Miscellaneous.Goods.and.Services.Expenditure + Special.Occasions.Expenditure 
  + Total.Income.from.Entrepreneurial.Acitivites + Total.Number.of.Family.members + Members.with.age.less.than.5.year.old
  + Members.with.age.5...17.years.old,
  
  data=datos.log.train,family=binomial
)

summary(glm.fits)
glm.probs<-predict(glm.fits,type="response") #Calculamos la probabilidad de que la familia pueda acceder a préstamo
Income = datos.log.test$Total.Household.Income 
contrasts(Income)
glm.pred<-rep("Accept",length(Income)) #Crea un vector con 6007 elementos "Accept"
glm.pred[glm.probs>.5]="Deny" #Transforma en Deny todos los elementos donde la probabilidad predicha<0.5

glm.pred<-as.factor(glm.pred)
confusionMatrix(glm.pred,Income) #Realizamos la matriz de confusion

#Validacion cruzada
set.seed(2) # los folds son aleatorios
folds <- createFolds(datos.log.train$Total.Household.Income, k = 20) #creamos los 20 folds
model<-train(   Total.Household.Income ~ .,
                data=datos.log.train,
                method="glm",
                family=binomial,
                trControl=trainControl(method="repeatedcv",number=20, index = folds))
model
glm.pred<-predict(model, datos.log.test)
confusionMatrix(glm.pred, datos.log.test$Total.Household.Income)

####### FASE END: RLM LOGISTICA