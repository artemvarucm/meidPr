# Separamos los datos en train y test (80/20, 75/25, 70/30...)
set.seed(20)

train_indices = sample(1:nrow(datos.sin.out), floor(nrow(datos.sin.out) * 0.8), replace=FALSE)

data_train = datos.sin.out[train_indices,]

rownames(data_train)=1:nrow(data_train) # renombramos indices

data_test = datos.sin.out[-train_indices,] # olvidamos de momento del test

rownames(data_test)=1:nrow(data_test) # renombramos indices







# Regresion multiple

# Primero intentamos usar las columnas altamente correlacionadas con Total.Household.Income, 
# (estan en high.corr.cols)

RLM_high.cor = lm(Total.Household.Income ~., data = data_train[, high.corr.cols])

# Intentamos predecir (error sobre data_test)
RLM_high.cor.predict = predict(RLM_high.cor,data_test)
# raiz del error cuadratico medio de la prediccion
RLM_high.cor.error = (sum((data_test$Total.Household.Income - RLM_high.cor.predict) ^ 2)/length(RLM_high.cor.predict)) ^ 0.5
RLM_high.cor.error # 272143.5 

# Intentamos predecir (sobre todo el modelo de datos)
RLM_high.cor.predict = predict(RLM_high.cor,datos.sin.out)
RLM_high.cor.error = (sum((datos.sin.out$Total.Household.Income - RLM_high.cor.predict) ^ 2)/length(RLM_high.cor.predict)) ^ 0.5
RLM_high.cor.error # 235121.1

# Veamos los tests.
residuos<-RLM_high.cor$residuals
lillie.test(residuos) # no cumple
#homocedatsicidad
bptest(RLM_high.cor) # no cumple
##independencia
dwtest(RLM_high.cor) # cumple


# Transformamos a box-cox para ver si podemos hacer que den los tests
data.copy = data_train

x<-data.copy$Total.Household.Income
b<-boxcox(lm(x~1))
lambda<-b$x[which.max(b$y)]
# El valor de lambda que usaremos
lambda
ynew<-(x^lambda-1)/lambda # transformamos la variable respuesta

data.copy$Total.Household.Income = ynew # cambiamos la variable respuesta en el dataset

RLM_high.cor.box.cox <- lm(Total.Household.Income ~ ., data=data.copy) # formamos nuevo modelo


# Veamos los tests.
residuos<-RLM_high.cor.box.cox$residuals
lillie.test(residuos) # no cumple
#homocedatsicidad
bptest(RLM_high.cor.box.cox) # no cumple
##independencia
dwtest(RLM_high.cor.box.cox) # cumple

# Quitamos outliers segun variable respuesta, es simetrica ahora
skewness(ynew) # simetrica

boxplot(ynew) # entre 4.5 y 5.1
IQR = quantile(ynew, 0.75) - quantile(ynew, 0.25)
# Limite inferior
lowerBound = quantile(ynew, 0.25) - 1.5*IQR
# Limite superior
upperBound = quantile(ynew, 0.75) + 1.5*IQR
data.copy = data.copy[ynew < upperBound,]
ynew = ynew[ynew < upperBound]
data.copy = data.copy[ynew > lowerBound,]

RLM_high.cor.box.cox <- lm(Total.Household.Income ~ ., data=data.copy) # formamos nuevo modelo

# Veamos los tests.
residuos<-RLM_high.cor.box.cox$residuals
lillie.test(residuos) # no cumple, pero ha mejorado
#homocedatsicidad
bptest(RLM_high.cor.box.cox) # no cumple
##independencia
dwtest(RLM_high.cor.box.cox) # cumple


# Transformamos las variables explicativas
# Guardamos lambda para las columnas, para la prediccion posterior
#lambdas = data.frame("Total.Household.Income"=c(lambda))
# CONTINUAR .... 












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

# Intentamos predecir (sobre todo el modelo de datos)
RLM_regsubsets.predict = predict(RLM_regsubsets,datos.sin.out)
RLM_regsubsets.error = (sum((datos.sin.out$Total.Household.Income - RLM_regsubsets.predict) ^ 2)/length(RLM_regsubsets.predict)) ^ 0.5
RLM_regsubsets.error # 173931.8

# Veamos los tests.
residuos<-RLM_regsubsets$residuals
lillie.test(residuos) # no cumple
#homocedatsicidad
bptest(RLM_regsubsets) # no cumple
##independencia
dwtest(RLM_regsubsets) # cumple

# Transformamos a box-cox para ver si podemos hacer que den los tests
data.copy = data.regsubsets

x<-data.copy$Total.Household.Income
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

boxplot(ynew) # entre 4.5 y 5.1
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
lillie.test(residuos) # no cumple, pero ha mejorado
#homocedatsicidad
bptest(RLM_regsubsets.box.cox) # no cumple
##independencia
dwtest(RLM_regsubsets.box.cox) # cumple


# Transformamos las variables explicativas
# Guardamos lambda para las columnas, para la prediccion posterior
#lambdas = data.frame("Total.Household.Income"=c(lambda))
# CONTINUAR .... 


# 5.8 Calculamos el error cuadratico medio del modelo hallado
# Error cuadratico medio del modelo sobre TEST
# gracias al abs, no da NA, y tampoco afecta, pues el salario es siempre positivo
# Si no, da error NA por intentar hacer raices pares sobre un termino negativo
#RLM_regsubsets.pred = abs(predict(RLM_regsubsets.box.cox,data_test) * lambda + 1) ^ (1/lambda) # deshacemos box-cox
#RLM_regsubsets.error = sqrt(sum((data_test$Total.Household.Income - RLM_regsubsets.pred) ^ 2)/length(RLM_regsubsets.pred))
#RLM_regsubsets.error

# Error cuadratico medio del modelo sobre modelo Completo
#RLM_regsubsets.pred = abs(predict(RLM_regsubsets.box.cox,datos.sin.out) * lambda + 1) ^ (1/lambda) # deshacemos box-cox
#RLM_regsubsets.error = sqrt(sum((datos.sin.out$Total.Household.Income - RLM_regsubsets.pred) ^ 2)/length(RLM_regsubsets.pred))
#RLM_regsubsets.error
# Error cosmico...



