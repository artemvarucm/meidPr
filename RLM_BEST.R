
# 4. Separamos los datos en train y test (80/20, 75/25, 70/30...)
# test se guarda para el final
# set.seed() y sample(1:nrow(datos), prob=c(0.2, 0.8))....
set.seed(20)

train_indices = sample(1:nrow(datos.sin.autocorr), floor(nrow(datos.sin.autocorr) * 0.8), replace=FALSE)

data_train = datos.sin.autocorr[train_indices,]

rownames(data_train)=1:nrow(data_train) # renombramos indice

data_test = datos.sin.autocorr[-train_indices,] # olvidamos de momento del test

# 5. Regresion multiple (1er modelo)


# 5.1 Cross Validation + Regsubsets para encontrar el mejor modelo (best.RLM)

#x2) Usando validaci on cruzada dejando uno fuera elegir el mejor modelo de regresi
# on de entre los que obtenemos con la funci on regsubsets cuando cuando
#hwfat es la variable respuesta y age, HT, abs, triceps y subscap las variables
#explicativas.
##LOOCV

predict.regsubsets <- function(object, newdata, id,...){
  form <-as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object,id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

n <- nrow(data_train) 
k <- 5 #número de grupos igual a n # aqui poner 5 porque lo dijo la profe...
set.seed(5)
# asigna a cada fila de da  ta train un grupo
folds <- sample(x=1:k, size=nrow(data_train), replace = TRUE)
# IMPORTANTE: REEEMPLAZAR 8 despues
cv.errors <- matrix(NA, k, 16, dimnames = list(NULL,paste(1:16)))
# betas para el modelo con minimo error
min.err.betas = c()
for (j in 1:k){
  # j es el grupo que dejamos fuera del conjunto de entrenar
  best.fit <- regsubsets(Total.Household.Income~., data=data_train[folds !=j,], nvmax = 16) # comprobado con 42, el de 16 es el mejor
  for (i in 1:16){ # hay un limite en regsubsets de 8 variables
    pred <- predict.regsubsets(best.fit, newdata=data_train[folds==j,], id=i) #datos de test
    cv.errors[j,i] <- (mean((data_train$Total.Household.Income[folds == j]-pred)^2)) ^ 0.5
  }
  
  #cv.errors
  mean.cv.errors <- apply(cv.errors, 2, mean) #calcula la media de los betas_i
  #mean.cv.errors
  min.err.betas = coef(best.fit, which.min(mean.cv.errors))[-1] # quitamos intercept
}  
# Raices cuadradas de errores
cv.errors  
# Raices 
mean.cv.errors <- apply(cv.errors, 2, mean)

# Modelo con 16 variables
new.model.16  = lm(Total.Household.Income ~ ., data=data_train[, c("Total.Household.Income", names(min.err.betas))])
# predecimos
new.model.16.error = (sum((predict(new.model.16, data_test) - data_test$Total.Household.Income) ^ 2)/ nrow(data_test)) ^ 0.5

# 5.2 Realizar tests de normalidad, homoscedasticidad y autocorrelacion 
#estudiamos la normalidad en los residuos
residuos<-new.model.16$residuals
lillie.test(residuos)
#homocedatsicidad
bptest(new.model.16)
##independencia
dwtest(new.model.16)
############### CONTINUAR O NO - DEPENDE DEL RESULTADO DE TESTS
# ? 5.3 Si no ha funcionado, vemos si hay outliers
# ? 5.4 Quitamos esos outliers
rest=rstudent(best.RLM) # calculamos los residuos estudentizados
(outliers=c(which(rest>3),which(rest<(-3))))

datos_sin_out=datos_sin_out[-outliers, ]

rownames(datos_sin_out)=1:nrow(datos_sin_out) #renombramos indices

best.RLM <- lm(Total.Household.Income ~ ., data=datos_sin_out)

# ? 5.5 Volvemos a Realizar tests de normalidad, homoscedasticidad y autocorrelacion
#estudiamos la normalidad en los residuos
# box-plot no funciona para distribuciones asimetricas (analizar skewness si esta fuera de (-2, 2) es asimetrica )
install.packages("moments")
library(moments)
skewness(best.RLM)

residuos<-best.RLM$residuals
lillie.test(residuos)
#homocedatsicidad
bptest(best.RLM)
##independencia
dwtest(best.RLM)

############### CONTINUAR O NO - DEPENDE DEL RESULTADO DE TESTS
# ? 5.6 Realizamos transformacion BOX-COX
x<-datos_sin_out$Total.Household.Income
b<-boxcox(lm(x~1))
lambda<-b$x[which.max(b$y)]
# El valor de lambda que usaremos
lambda
ynew<-(x^lambda-1)/lambda # transformamos la variable respuesta
datos_sin_out$Total.Household.Income = ynew # cambiamos la variable respuesta en el dataset
best.RLM <- lm(Total.Household.Income ~ ., data=datos_sin_out) # formamos nuevo modelo

# ? 5.7 Volvemos a Realizar tests de normalidad, homoscedasticidad y autocorrelacion 
#estudiamos la normalidad en los residuos
residuos<-best.RLM$residuals
lillie.test(residuos)
#homocedatsicidad
bptest(best.RLM)
##independencia
dwtest(best.RLM)
############### FIN PARTE DEPENDIENTE


# 5.8 Calculamos el error cuadratico medio del modelo hallado
# Error cuadratico medio del modelo sobre TRAIN
sigmaTrainRLM = summary(best.RLM)$sigma 
# Error cuadratico medio del modelo sobre TEST
yPredictRLM = predict(best.RLM,data_test) 
sigmaTestRLM = (sum((data_test$Total.Household.Income - yPredictRLM) ^ 2)/length(yPredictRLM)) ^ 0.5

