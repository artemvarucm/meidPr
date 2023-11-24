
# 3. Regresion multiple para el conjunto completo (sin columnas categoricas ni nominales) (modelo 0)
RLM_FULL = lm(Total.Household.Income ~ ., data=datos.solo.num)

# 3.1 Analisis de los p-valores con summary(modeloFull)
summary(RLM_FULL)
# 3.2 Analisis del ajuste con R-cuadrado, el p-valor que pone abajo del todo ...
summary(RLM_FULL)$r.squared

#Intentamos predecir
yPredictFull = predict(RLM_FULL,datos.solo.num)
# raiz del error cuadratico medio de la prediccion
(sum((datos.solo.num$Total.Household.Income - yPredictFull) ^ 2)/length(yPredictFull)) ^ 0.5
