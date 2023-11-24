# 6. Regresion multiple Ridge, Lasso y Elasticnet (2do, 3er y 4to modelos) 
# RLB_RIDGE, RLM_ELASTICNET, RLM_LASSO...
# 6.1 Cross Validation + Regsubsets para encontrar el mejor modelo
# No hace falta comprobar los tests
# 6.2 Calculamos el error cuadratico medio del modelo hallado

#ridge
install.packages("glmnet") #solo si no est´a ya instalado
library(glmnet)

x<-model.matrix(Total.Household.Income ~ .,datos)[,-1]
y<-datos$Total.Household.Income


grid<-10^seq(10,-2,length=100)
ridge.mod<-glmnet(x,y,alpha=0,lambda=grid)

#lasso
lasso.mod<-glmnet(x,y,alpha=1,lambda=grid)

#elastic
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
