# 6. Regresion multiple Ridge, Lasso y Elasticnet (2do, 3er y 4to modelos) 
# RLB_RIDGE, RLM_ELASTICNET, RLM_LASSO...
# 6.1 Cross Validation + Regsubsets para encontrar el mejor modelo
# No hace falta comprobar los tests
# 6.2 Calculamos el error cuadratico medio del modelo hallado

# Ridge alpha = 0
install.packages("glmnet") #solo si no est´a ya instalado
library(glmnet)


x.train<-model.matrix(Total.Household.Income~.,data_train[, cols.sin.autocorr])[,-1]
y.train<-data_train$Total.Household.Income
x.test<-model.matrix(Total.Household.Income~.,data_test[, cols.sin.autocorr])[,-1]
y.test<-data_test$Total.Household.Income


RLM_RIDGE<-glmnet(x.train,y.train,alpha=0)

# validacion cruzada para ridge
set.seed(1)
cv.out<-cv.glmnet(x.train,y.train,alpha=0)
plot(cv.out)
(bestlam<-cv.out$lambda.min)
# Predecimos con el bestlam
RLM_RIDGE.pred<-predict(RLM_RIDGE,s=bestlam,newx=x.test)
# Calculamos error de test
(RLM_RIDGE.error = sqrt(mean((RLM_RIDGE.pred-y.test)^2))) # 97497.05

# Intentamos predecir (sobre todo el modelo de datos)
RLM_RIDGE.pred = predict(RLM_RIDGE,s=bestlam,newx=rbind(x.train, x.test))
(RLM_RIDGE.error = sqrt(mean((RLM_RIDGE.pred-c(y.train, y.test))^2))) # 134463.4
# Predice un poco mejor que el modelo de minimos cuadrados
# El problema es que no elimina variables


# Lasso alpha = 1
RLM_LASSO<-glmnet(x.train,y.train,alpha=1)

# validacion cruzada para lasso
set.seed(1)
cv.out<-cv.glmnet(x.train,y.train,alpha=1)
plot(cv.out)
(bestlam<-cv.out$lambda.min)
# Predecimos con el bestlam
RLM_LASSO.pred<-predict(RLM_LASSO,s=bestlam,newx=x.test)
# Calculamos error de test
(RLM_LASSO.error = sqrt(mean((RLM_LASSO.pred-y.test)^2))) # 97336.29

# Intentamos predecir (sobre todo el modelo de datos)
RLM_LASSO.pred = predict(RLM_LASSO,s=bestlam,newx=rbind(x.train, x.test))
(RLM_LASSO.error = sqrt(mean((RLM_LASSO.pred-c(y.train, y.test))^2))) # 134365.7
# Predice un poco mejor que el modelo RIDGE

# Lo bueno es que quita variables (asignandole en coeficiente un 0)
out<-glmnet(x.train,y.train,alpha=1,lambda=grid)
lasso.coef<-predict(out,type="coefficients",s=bestlam)
lasso.coef
length(lasso.coef[lasso.coef!=0]) # Quedaron 28 variables de 38 iniciales

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
sqrt(minimo$mse) # 97256.58

# Intentamos predecir (sobre todo el modelo de datos)
RLM_ELASTIC.pred = predict(models[[minimo$name]],s=minimo$lambda,newx=rbind(x.train, x.test))
(RLM_ELASTIC.error = sqrt(mean((RLM_ELASTIC.pred-c(y.train, y.test))^2))) # 134368.5

# Con elastic hemos obtenido el mejor modelo, veamos cuantas variables
out<-glmnet(x.train,y.train,alpha=minimo$alpha)
elasnet.coef<-predict(out,type="coefficients",s=minimo$lambda)
elasnet.coef
length(elasnet.coef[elasnet.coef!=0]) # Quedaron 27 variables de 38 iniciales





