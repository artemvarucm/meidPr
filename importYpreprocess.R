# Artem Vartanov y Mario Baldocchi

library(nortest) # test normalidad
library(lmtest) # test homoscedasticidad
library(MASS) # box-cox
library(leaps) # regsubsets
library(moments) # skewness
library(car) # influenceIndexPlot, vif...
library(pls) # pcr, pls
library(glmnet) # ridge, lasso, elasticnet...
library(caret) # reg. logistica confusionMatrix

datos <- read.table(file = "/Users/tyomikjan/UNIVERSITY/R/TRABAJO DATOS/meidPr/Family Income and Expenditure.csv", fill = TRUE, header = TRUE, sep = ",")

dim(datos)

summary(datos)

# Nuestra variable Respuesta es Total.Household.Income, que es el salario anual por vivienda filipina

# Tratamos columnas como categoricas
datos$Region = as.factor(datos$Region)
datos$Agricultural.Household.indicator = as.factor(datos$Agricultural.Household.indicator)
datos$Household.Head.Sex = as.factor(datos$Household.Head.Sex)
datos$Household.Head.Marital.Status = as.factor(datos$Household.Head.Marital.Status)
datos$Household.Head.Highest.Grade.Completed = as.factor(datos$Household.Head.Highest.Grade.Completed)
datos$Household.Head.Job.or.Business.Indicator = as.factor(datos$Household.Head.Job.or.Business.Indicator)
datos$Household.Head.Occupation = as.factor(datos$Household.Head.Occupation)
datos$Household.Head.Class.of.Worker = as.factor(datos$Household.Head.Class.of.Worker)
datos$Type.of.Household = as.factor(datos$Type.of.Household)
datos$Type.of.Building.House = as.factor(datos$Type.of.Building.House)
datos$Type.of.Roof = as.factor(datos$Type.of.Roof)
datos$Type.of.Walls = as.factor(datos$Type.of.Walls)
datos$Tenure.Status = as.factor(datos$Tenure.Status)
datos$Toilet.Facilities = as.factor(datos$Toilet.Facilities)
datos$Electricity = as.factor(datos$Electricity)
datos$Main.Source.of.Income = as.factor(datos$Main.Source.of.Income)
datos$Main.Source.of.Water.Supply = as.factor(datos$Main.Source.of.Water.Supply)

summary(datos)

# Vemos que hay algunas columnas como Household.Head.Occupation y Household.Head.Class.of.Worker que tienen muchos nulos.
# Quitamos las columnas que tengan algun nulo
sum(!complete.cases(datos)) # 3355
sum(!complete.cases(datos[,-c(31, 32)])) # 0
datos = datos[,-c(31, 32)] # quitamos columnas con N/A
sum(!complete.cases(datos))

# Seleccionamos las columnas numericas solo
datos.solo.num = datos[sapply(datos, is.numeric)]

# Histogramas
par(mfrow=c(3, 5))
for (x in colnames(datos)) {
    hist(table(datos[, x]), xlab=x)
}

# Plots
par(mfrow=c(3, 5))
for (x in colnames(datos)) {
  plot(datos[, x], xlab=x)
}

# Quitamos outliers segun todas las columnas
datos.sin.out = datos.solo.num
coefIQR = 3 # coeficiente por el cual se multiplicará el IQR para quitar outliers
for (x in colnames(datos.sin.out)) { # Quitamos columnas con muy pocas alternativas (casi categoricas, Number of TVs)
  col = datos.sin.out[, x]
  print(x)
  print(dim(datos.sin.out))
  
  if (abs(skewness(col)) > 2) { # en simetricas, skewness esta entre (-2, 2)
    # si no es simetrica, la transformamos con box cox, 
    # pero la columna del dataframe queda igual
    col = dlookr::transform(col, method="Box-Cox")
  }
  q1 = quantile(col, 0.25)
  q3 = quantile(col, 0.75)
  iqr = q3 - q1
  if (iqr > 0) { # si los dos quantiles coinciden, evitamos quitar todos los datos
    datos.sin.out = datos.sin.out[col < q3 + coefIQR*iqr,]
    col = col[col < q3 + coefIQR*iqr]
    datos.sin.out = datos.sin.out[col > q1 - coefIQR*iqr,]
  }
}

# Quitamos las columnas agregadas (mucha correlacion con otras, literalmente combinacion lineal)

total.num.fam.mem = datos.sin.out["Total.Number.of.Family.members"] # guardamos para logistica

aggrCols = c("Total.Food.Expenditure", "Total.Number.of.Family.members")
for (col in aggrCols) {
  indR = grep(col, colnames(datos.sin.out))
  datos.sin.out = datos.sin.out[, -indR]
}

rownames(datos.sin.out)=1:nrow(datos.sin.out) # renombramos indices




