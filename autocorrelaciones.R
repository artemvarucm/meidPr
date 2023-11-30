# 2. Analisis de las correlaciones. Explicacion.
# cor(datos), dependiendo del caso spearman, pearson...


# 43 columnas numericas
length(datos.solo.num) 

# AUTO CORRELACIONES
# Analizamos autocorrelaciones entre variables (excluimos la respuesta)
# IMPORTANTE: por que usamos coeficiente de correlacion de SPEARMAN ? 

# Quitamos la variable respuesta
indR = grep("Total.Household.Income", colnames(datos.solo.num))
datos.sin.autocorr =  datos.solo.num[, -indR]

# Mapa de calor de las autocorrelaciones
heatmap(cor(datos.sin.autocorr, method="spearman"))

i = 1
while (length(datos.sin.autocorr) >= i + 1){ # como minimo quedan dos columnas
  if (any(abs(cor(datos.sin.autocorr, method="spearman")[i, -i]) > 0.7)) { # quitamos de las correlaciones la misma columna, porque una variable consigo misma tiene correlacion 1 siempre
    print(colnames(datos.sin.autocorr)[i])
    datos.sin.autocorr = datos.sin.autocorr[, -i]
  } else {
    i = i + 1
  }
}
length(datos.sin.autocorr) # quedan 38 columnas
sum(cor(datos.sin.autocorr, method="spearman") > 0.7) # solo estan las diagonales

#View(datos.sin.autocorr)

# Volvemos la columna a predecir
datos.sin.autocorr = cbind(datos.solo.num$Total.Household.Income, datos.sin.autocorr)
# Renombramos columna
colnames(datos.sin.autocorr)[1] = "Total.Household.Income"

# Columnas sin autocorrelacion
cols.sin.autocorr = colnames(datos.sin.autocorr) # Lo usaremos mas adelante en la RLM_BEST

# Tabla de correlaciones para columnas (incluye variable respuesta)
corr.sin.autocorr = cor(datos.sin.autocorr, method="spearman")

# Vemos las columnas correlacionadas linealmente con la variable respuesta Total.Household.Incom
corr.sin.autocorr[,"Total.Household.Income"]

# IMPORTANTE: INTENTAR VER COLUMNAS CORRELACIONADAS ENTRE SI, INFLUENCIADAS POR TERCERA COLUMNA
# SE HACE CON CORRELACION AISLADA

# Como hay demasiadas columnas numericas, es muy dificil sacar conclusiones
# Seleccionamos correlaciones en valor absoluto mayores que 0.7 con la variable Total.Household.Income
# Lista de columnas 
# (Total.Household.Income esta dentro porque su correlacion es 1)
high.corr.cols = rownames(data.frame(corr.sin.autocorr[abs(corr.sin.autocorr[,"Total.Household.Income"]) > 0.7, "Total.Household.Income"]))
high.corr.cols

# Seleccionamos del dataset solo las columnas anteriores
datos.high.cor = datos[,high.corr.cols] # seleccionamos las columnas correlacionadas
summary(datos.high.cor)
# Lo usaremos mas adelante en la RLM_BEST


