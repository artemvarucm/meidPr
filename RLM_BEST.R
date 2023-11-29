
# 4. Separamos los datos en train y test (80/20, 75/25, 70/30...)
# test se guarda para el final
# set.seed() y sample(1:nrow(datos), prob=c(0.2, 0.8))....
set.seed(20)

train_indices = sample(1:nrow(datos), floor(nrow(datos) * 0.8), replace=FALSE)

data_train = datos[train_indices,]

rownames(data_train)=1:nrow(data_train) # renombramos indice

data_test = datos[-train_indices,] # olvidamos de momento del test

# 5. Regresion multiple (1er modelo)

# Primero intentamos usar las columnas altamente correlacionadas con Total.Household.Income , que estan en high.corr.cols

RLM_high.cor = lm(Total.Household.Income ~., data = data_train[, high.corr.cols])

# Intentamos predecir (error sobre data_test)
RLM_high.cor.predict = predict(RLM_high.cor,data_test)
# raiz del error cuadratico medio de la prediccion
RLM_high.cor.error = (sum((data_test$Total.Household.Income - RLM_high.cor.predict) ^ 2)/length(RLM_high.cor.predict)) ^ 0.5
RLM_high.cor.error # 175604.9 

# Intentamos predecir (sobre todo el modelo de datos)
RLM_high.cor.predict = predict(RLM_high.cor,datos)
RLM_high.cor.error = (sum((datos$Total.Household.Income - RLM_high.cor.predict) ^ 2)/length(RLM_high.cor.predict)) ^ 0.5
RLM_high.cor.error # 208002.1

# El error ha crecido casi el doble, respecto RLM_FULL
# Veamos si hay outliers que afecten el modelo
hist(data_train$Total.Household.Income, 100)
# En el histograma se ve que el salario mayor que 1 millon es poco frecuente
# De hecho
nrow(data_train) # 11733 total de filas 
sum(data_train$Total.Household.Income > 1000000) # 231
# Serian alrededor de un 2% del total. Intentemos quitarlos y contruir de nuevo el modelo
data_train.sin.out = data_train[data_train$Total.Household.Income < 1000000, ]

RLM_high.cor = lm(Total.Household.Income ~., data = data_train.sin.out[, high.corr.cols])

# Intentamos predecir (error sobre data_test)
RLM_high.cor.predict = predict(RLM_high.cor,data_test)
# raiz del error cuadratico medio de la prediccion
RLM_high.cor.error = (sum((data_test$Total.Household.Income - RLM_high.cor.predict) ^ 2)/length(RLM_high.cor.predict)) ^ 0.5
RLM_high.cor.error # 182378.6 

# Intentamos predecir (sobre todo el modelo de datos)
RLM_high.cor.predict = predict(RLM_high.cor,datos)
RLM_high.cor.error = (sum((datos$Total.Household.Income - RLM_high.cor.predict) ^ 2)/length(RLM_high.cor.predict)) ^ 0.5
RLM_high.cor.error # 214248.4

# En ambos casos, es mas alto el error, que si quitar outliers. Descartamos este modelo.

# IMPORTANTE: Podemos hacer forwarding(anadir variables) a este modelo

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
k <- 5 #número de grupos igual a n # aqui poner 5 porque lo dijo la profe...
set.seed(5)
# asigna a cada fila de da  ta train un grupo
folds <- sample(x=1:k, size=nrow(data_train.solo.num), replace = TRUE)
# IMPORTANTE: REEEMPLAZAR 20 despues
cv.errors <- matrix(NA, k, 20, dimnames = list(NULL,paste(1:20)))
# betas para el modelo con minimo error
for (j in 1:k){
  # j es el grupo que dejamos fuera del conjunto de entrenar
  best.fit <- regsubsets(Total.Household.Income~., data=data_train.solo.num[folds !=j,], nvmax = 20) # comprobado con 42, el de 16 es el mejor
  for (i in 1:20){ # hay un limite en regsubsets de 8 variables
    pred <- predict.regsubsets(best.fit, newdata=data_train.solo.num[folds==j,], id=i) #datos de test
    cv.errors[j,i] <- (mean((data_train.solo.num$Total.Household.Income[folds == j]-pred)^2)) ^ 0.5
  }
  
  #cv.errors
  #mean.cv.errors <- apply(cv.errors, 2, mean) #calcula la media de los betas_i
  #mean.cv.errors
  #min.err.betas = coef(best.fit, which.min(mean.cv.errors))
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

# predecimos
# Intentamos predecir (error sobre data_test)
RLM_regsubsets.predict = predict(RLM_regsubsets,data_test)
# raiz del error cuadratico medio de la prediccion
RLM_regsubsets.error = (sum((data_test$Total.Household.Income - RLM_regsubsets.predict) ^ 2)/length(RLM_regsubsets.predict)) ^ 0.5
RLM_regsubsets.error # 97824.69

# Intentamos predecir (sobre todo el modelo de datos)
RLM_regsubsets.predict = predict(RLM_regsubsets,datos)
RLM_regsubsets.error = (sum((datos$Total.Household.Income - RLM_regsubsets.predict) ^ 2)/length(RLM_regsubsets.predict)) ^ 0.5
RLM_regsubsets.error # 134921.7

# Prediccion mejor, que con RLM.high.cor pero sobre el modelo completo seguimos perdiendo contra RLM_FULL






# 5.2 Realizar tests de normalidad, homoscedasticidad y autocorrelacion 
#estudiamos la normalidad en los residuos
residuos<-RLM_regsubsets$residuals
lillie.test(residuos)
#homocedatsicidad
bptest(RLM_regsubsets)
##independencia
dwtest(RLM_regsubsets)
############### CONTINUAR O NO - DEPENDE DEL RESULTADO DE TESTS
# ? 5.3 Si no ha funcionado, vemos si hay outliers
# ? 5.4 Quitamos esos outliers
rest=rstudent(RLM_regsubsets) # calculamos los residuos estudentizados
outliers=c(which(rest>3),which(rest<(-3)))

datos_sin_out=data.regsubsets[-outliers, ]

rownames(datos_sin_out)=1:nrow(datos_sin_out) #renombramos indices

RLM_regsubsets.sin.out <- lm(Total.Household.Income ~ ., data=datos_sin_out)

# ? 5.5 Volvemos a Realizar tests de normalidad, homoscedasticidad y autocorrelacion
#estudiamos la normalidad en los residuos
# box-plot no funciona para distribuciones asimetricas (analizar skewness si esta fuera de (-2, 2) es asimetrica )
#install.packages("moments")
#library(moments)
#skewness(best.RLM)

residuos<-RLM_regsubsets.sin.out$residuals
lillie.test(residuos)
#homocedatsicidad
bptest(RLM_regsubsets.sin.out)
##independencia
dwtest(RLM_regsubsets.sin.out)

# No han cambiado los resultados

# ? 5.6 Realizamos transformacion BOX-COX
data.copy.new.regsubsets = data.regsubsets
# Predice bien, si realizamos este filtro, que me parece bastante MAL
# data.copy.new.regsubsets = data.copy.new.regsubsets[data.copy.new.regsubsets$Total.Household.Income < 100000,]

x<-data.copy.new.regsubsets$Total.Household.Income
b<-boxcox(lm(x~1))
lambda<-b$x[which.max(b$y)]
# El valor de lambda que usaremos
lambda
ynew<-(x^lambda-1)/lambda # transformamos la variable respuesta

skewness(ynew) # - simetrica

boxplot(ynew) # entre 4.5 y 5.1
# Lo de abajo de IQR EMPEORA el resultado
IQR = quantile(ynew, 0.75) - quantile(ynew, 0.25)
# Limite inferior
lowerBound = quantile(ynew, 0.25) - 1.5*IQR
# Limite superior
upperBound = quantile(ynew, 0.75) + 1.5*IQR

data.copy.new.regsubsets$Total.Household.Income = ynew # cambiamos la variable respuesta en el dataset
data.copy.new.regsubsets = data.copy.new.regsubsets[data.copy.new.regsubsets$Total.Household.Income < upperBound, ]
data.copy.new.regsubsets = data.copy.new.regsubsets[data.copy.new.regsubsets$Total.Household.Income > lowerBound, ]

RLM_regsubsets.box.cox <- lm(Total.Household.Income ~ ., data=data.copy.new.regsubsets) # formamos nuevo modelo






# ? 5.7 Volvemos a Realizar tests de normalidad, homoscedasticidad y autocorrelacion 
#estudiamos la normalidad en los residuos
residuos<-RLM_regsubsets.box.cox$residuals
lillie.test(residuos)
#homocedatsicidad
bptest(RLM_regsubsets.box.cox)
##independencia
dwtest(RLM_regsubsets.box.cox)
############### FIN PARTE DEPENDIENTE


# 5.8 Calculamos el error cuadratico medio del modelo hallado
# Error cuadratico medio del modelo sobre TEST
# gracias al abs, no da NA, y tampoco afecta, pues el salario es siempre positivo
# Si no, da error NA por intentar hacer raices pares sobre un termino negativo
RLM_regsubsets.pred = abs(predict(RLM_regsubsets.box.cox,data_test) * lambda + 1) ^ (1/lambda) # deshacemos box-cox
RLM_regsubsets.error = sqrt(sum((data_test$Total.Household.Income - RLM_regsubsets.pred) ^ 2)/length(RLM_regsubsets.pred))
RLM_regsubsets.error # 361146.9

# Error cuadratico medio del modelo sobre modelo Completo
RLM_regsubsets.pred = (predict(RLM_regsubsets.box.cox,datos) * lambda + 1) ^ (1/lambda) # deshacemos box-cox
RLM_regsubsets.error = sqrt(sum((datos$Total.Household.Income - RLM_regsubsets.pred) ^ 2)/length(RLM_regsubsets.pred))
RLM_regsubsets.error # 385657.2
# ERROR COSMICO...

# residualPlot
# library(car)
# influencePlot(RLM_model)
# outlier.test


