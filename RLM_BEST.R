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

# Intentamos predecir (sobre todo el modelo de datos)
# RLM_high.cor.predict = predict(RLM_high.cor,datos.sin.out)
# RLM_high.cor.error = (sum((datos.sin.out$Total.Household.Income - RLM_high.cor.predict) ^ 2)/length(RLM_high.cor.predict)) ^ 0.5
# RLM_high.cor.error # 235121.1

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
# Nos quedamos con este modelo, pero no nos interesa seguir con este modelo. Error bastante critico

# 5.1 Cross Validation + Regsubsets para encontrar el mejor modelo (best.RLM)
##LOOCV

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

# # Intentamos predecir (sobre todo el modelo de datos)
# RLM_regsubsets.predict = predict(RLM_regsubsets,datos.sin.out)
# RLM_regsubsets.error = (sum((datos.sin.out$Total.Household.Income - RLM_regsubsets.predict) ^ 2)/length(RLM_regsubsets.predict)) ^ 0.5
# RLM_regsubsets.error # 173931.8


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
#lambdas = data.frame("Total.Household.Income"=c(lambda))
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
        if (lillie.test(RLM_regsubsets.box.cox$residuals)$p.value > 0.0001) {
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
RLM_regsubsets.error 
# Error cosmico...
