# Artem Vartanov y Mario Baldocchi
# 1. Descripcion del estudio, motivacion...
# Hablar un poco de la tabla de datos, hacer as.factor, summary(datos)...

library(nortest) # test normalidad
library(lmtest) # test homoscedasticidad
library(MASS) # box-cox
library(leaps) # regsubsets

datos <- read.table(file = "./Family Income and Expenditure.csv", fill = TRUE, header = TRUE, sep = ",")

# IMPORTANTE CUASIVARIANZA O VARIANZA
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
# Quitamos las filas que tengan algun nulo
datos = datos[complete.cases(datos),]

# Distribucion del salario, asimetrica a la derecha
hist(datos$Total.Household.Income, 100)


# Distribucion por regiones
barplot(table(datos$Region),las=2)

barplot(table(datos$Household.Head.Sex),las=2)

barplot(table(datos$Electricity),las=2)

barplot(table(datos$Type.of.Building.House),las=2)