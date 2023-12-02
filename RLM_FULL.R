
# 3. Regresion multiple para el conjunto completo (sin columnas categoricas ni nominales) (modelo 0)
# Tambien estan las autocorrelacionadas aqui
RLM_FULL = lm(Total.Household.Income ~ ., data=datos.sin.out)

# 3.1 Analisis de los p-valores con summary(modeloFull)
summary(RLM_FULL)
# 3.2 Analisis del ajuste con R-cuadrado, el p-valor que pone abajo del todo ...
summary(RLM_FULL)$r.squared

# Intentamos predecir (sobre todo el modelo de datos)
RLM_FULL.predict = predict(RLM_FULL,datos)
# raiz del error cuadratico medio de la prediccion
RLM_FULL.error = (sum((datos$Total.Household.Income - RLM_FULL.predict) ^ 2)/length(RLM_FULL.predict)) ^ 0.5
RLM_FULL.error # 131924.9

# Vemos que hay autocorrelacion con el VIF
library(car)
vif(RLM_FULL)
