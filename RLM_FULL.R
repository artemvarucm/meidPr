
# 3. Regresion multiple para el conjunto completo (sin columnas categoricas ni nominales) (modelo 0)
# Tambien estan las autocorrelacionadas aqui
RLM_FULL = lm(Total.Household.Income ~ ., data=datos.sin.out)

# 3.1 Analisis de los p-valores con summary(modeloFull)
summary(RLM_FULL)

# Intentamos predecir (sobre todo el modelo de datos)
RLM_FULL.predict = predict(RLM_FULL,datos)
# raiz del error cuadratico medio de la prediccion
RLM_FULL.error = (sum((datos$Total.Household.Income - RLM_FULL.predict) ^ 2)/length(RLM_FULL.predict)) ^ 0.5
RLM_FULL.error # 134325.9

# Vemos si hay colinealidad con el VIF
vif(RLM_FULL)
