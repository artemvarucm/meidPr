# 2. Analisis de las correlaciones. Explicacion.
# cor(datos), dependiendo del caso spearman, pearson...


# 43 columnas numericas
length(datos.solo.num) 

# AUTO CORRELACIONES
# Analizamos autocorrelaciones entre variables (excluimos la respuesta)
# IMPORTANTE: por que usamos coeficiente de correlacion de SPEARMAN ? 

# Quitamos la variable respuesta
indR = grep("Total.Household.Income", colnames(datos.sin.out))
datos.sin.autocorr =  datos.sin.out[, -indR]

# Mapa de calor de las autocorrelaciones
heatmap(cor(datos.sin.autocorr, method="spearman"))
# No se ve bien, bastante dificil

# Guardara las parejas de variables altamente correlacionadas (pendientes a eliminar)
autocor.pairs = data.frame()
# Lista de columnas autocorrelacionadas (para heatmap)
autocor.cols = c()
i = 1
while (length(datos.sin.autocorr) >= i){
  # quitamos de las correlaciones la misma columna, 
  # porque una variable consigo misma tiene correlacion 1 siempre
  if (any(abs(cor(datos.sin.autocorr, method="spearman")[i, -i]) > 0.7)) { 
    autocor.cols = c(autocor.cols, colnames(datos.sin.autocorr)[i])
    for (col in rownames(as.matrix(which(abs(cor(datos.sin.autocorr, method="spearman")[i, -i]) > 0.7)))) {
      if (length(autocor.pairs) == 0) {
        autocor.pairs = as.data.frame(c(col))
        colnames(autocor.pairs)[length(colnames(autocor.pairs))] = colnames(datos.sin.autocorr)[i]
      } else if (length(autocor.pairs[1, col]) == 0 || autocor.pairs[1, col] != colnames(datos.sin.autocorr)[i]) { # si no esta anadido todavia
        autocor.pairs = cbind(autocor.pairs, c(col))
        colnames(autocor.pairs)[length(colnames(autocor.pairs))] = colnames(datos.sin.autocorr)[i]
      }
    }
  }
  i = i + 1
}

autocor.pairs

# Vemos que hay dos parejas que estan muy correlacionadas
heatmap(cor(datos.sin.autocorr[, autocor.cols]))
# Viendo las parejas, quitamos las que tengan MENOS correlacion con la var. respuesta
cor(datos.sin.out)["Total.Household.Income", autocor.cols]

indDelete = c(
  grep("Total.Rice.Expenditure", colnames(datos.sin.autocorr)), 
  grep("Housing.and.water.Expenditure", colnames(datos.sin.autocorr)) # quitamos esta porque sale correlacionada con dos a la vez
)

datos.sin.autocorr =  datos.sin.autocorr[, -indDelete]

# Probamos otra vez

# Lista de columnas autocorrelacionadas
autocor.cols = c()
i = 1
while (length(datos.sin.autocorr) >= i){
  # quitamos de las correlaciones la misma columna, 
  # porque una variable consigo misma tiene correlacion 1 siempre
  if (any(abs(cor(datos.sin.autocorr, method="spearman")[i, -i]) > 0.7)) { 
    autocor.cols = c(autocor.cols, colnames(datos.sin.autocorr)[i])
  }
  i = i + 1
}

length(autocor.cols) # 0 - no hay columnas autocorrelacionadas (correlacion en abs mas de 0.7)

length(datos.sin.autocorr) # quedan 38 columnas

sum(cor(datos.sin.autocorr, method="spearman") > 0.7) # solo estan las diagonales

#View(datos.sin.autocorr)

# Volvemos la columna a predecir
datos.sin.autocorr = cbind(datos.sin.out$Total.Household.Income, datos.sin.autocorr)
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


