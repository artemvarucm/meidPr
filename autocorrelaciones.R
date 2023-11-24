# 2. Analisis de las correlaciones. Explicacion.
# cor(datos), dependiendo del caso spearman, pearson...

# Seleccionamos las columnas numericas solo
datos.solo.num = datos[sapply(datos, is.numeric)]
# 43 columnas numericas
length(datos.solo.num) 

# Analizamos autocorrelaciones entre variables (excluimos la respuesta)
# IMPORTANTE: por que usamos coeficiente de correlacion de SPEARMAN ? 
datos.solo.num = datos.solo.num[datos.solo.num$Total.Household.Income < 1000000, ]
# Quitamos la variable respuesta
datos.sin.autocorr =  datos.solo.num[, -1]
i = 1
while (length(datos.sin.autocorr) >= i + 1){ # como minimo quedan dos columnas
  if (any(abs(cor(datos.sin.autocorr, method="spearman")[i, -i]) > 0.7)) { # quitamos de las correlaciones la misma columna, porque una variable consigo misma tiene correlacion 1 siempre
    datos.sin.autocorr = datos.sin.autocorr[, -i]
  } else {
    i = i + 1
  }
}
length(datos.sin.autocorr) # quedan 38 columnas
sum(cor(datos.sin.autocorr, method="spearman") > 0.7) # solo estan las diagonales

View(datos.sin.autocorr)
# Volvemos la columna a predecir
datos.sin.autocorr = cbind(datos.solo.num$Total.Household.Income, datos.sin.autocorr)
# Renombramos columna
colnames(datos.sin.autocorr)[1] = "Total.Household.Income"

# Tabla de correlaciones para columnas numericas
corr_only_numeric = cor(datos.solo.num, method="spearman")

# Vemos las columnas correlacionadas linealmente con la variable respuesta Total.Household.Incom
corr_only_numeric[,"Total.Household.Income"]

# IMPORTANTE: INTENTAR VER COLUMNAS CORRELACIONADAS ENTRE SI, INFLUENCIADAS POR TERCERA COLUMNA
# SE HACE CON CORRELACION AISLADA

# Como hay demasiadas columnas numericas, es muy dificil sacar conclusiones
# Seleccionamos correlaciones en valor absoluto mayores que 0.7 con la variable Total.Household.Income
# Lista de columnas 
# (Total.Household.Income esta dentro porque su correlacion es 1)
corr_cols = rownames(data.frame(corr_only_numeric[abs(corr_only_numeric[,"Total.Household.Income"]) > 0.7, "Total.Household.Income"]))
corr_cols

# Seleccionamos del dataset solo las columnas anteriores
datos_cor = datos[,corr_cols] # seleccionamos las columnas correlacionadas
summary(datos_cor)

# AUTO CORRELACIONES
# Mapa de calor de las autocorrelaciones
heatmap(cor(datos_cor[-1], method="spearman"))
View(abs(cor(datos_cor[-1], method="spearman")) > 0.7) # evitamos la misma columna Total.Household.Income 
# Meat y Food muy correlacionadas
#datos_cor = datos_cor[,c(-3, -5, -6, -8, -9, -10)] # eliminamos columnas correlacionadas

