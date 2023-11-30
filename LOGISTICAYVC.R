###EJERCICIOS NAIVE BAYES
# 8. Regresion logistica (7to modelo) (No estoy seguro si hay que hacer la logistica o la discriminante)
# 8.1 Convertir variable respuesta en una variable categorica (ej. salario alto, salario bajo)
# 8.2 Realizar una tabla de contingencia para ver la eficiencia
# 8.3 Dependiendo del caso, seran mas criticos los falsos negativos, que los falsos positivos o viceversa

#Se destruyen las casas. El banco concede prestamos de 90000 (1489 euros) para reconstruir las casas. Para conceder el prestamo, 
#cada miembro de la familia debe "ingresar" un 5% del prestamo 
datos.log = datos
datos.log$Total.Household.Income = datos.log$Total.Household.Income / 12 #Calculamos el sueldo mensual
datos.log$Total.Household.Income = datos.log$Total.Household.Income/datos$Total.Number.of.Family.members #"Sueldo mensual" de cada miembro de la familia 

#4500 es el 5% del prestamo
datos.log$Total.Household.Income[datos.log$Total.Household.Income < 4500] = 0
datos.log$Total.Household.Income[datos.log$Total.Household.Income >= 4500] = 1

datos.log$Total.Household.Income[datos.log$Total.Household.Income == 0] = "Deny"
datos.log$Total.Household.Income[datos.log$Total.Household.Income ==1] = "Accept"
datos.log$Total.Household.Income = as.factor(datos.log$Total.Household.Income)


glm.fits<-glm(
  Total.Household.Income~Total.Food.Expenditure + Bread.and.Cereals.Expenditure + Total.Rice.Expenditure + Meat.Expenditure + Total.Fish.and..marine.products.Expenditure
  + Fruit.Expenditure + Vegetables.Expenditure + Restaurant.and.hotels.Expenditure + Alcoholic.Beverages.Expenditure + Tobacco.Expenditure + Clothing..Footwear.and.Other.Wear.Expenditure
  + Housing.and.water.Expenditure + Imputed.House.Rental.Value + Medical.Care.Expenditure + Transportation.Expenditure + Communication.Expenditure 
  + Education.Expenditure + Miscellaneous.Goods.and.Services.Expenditure + Special.Occasions.Expenditure + Crop.Farming.and.Gardening.expenses 
  + Total.Income.from.Entrepreneurial.Acitivites + Household.Head.Age + Total.Number.of.Family.members + Members.with.age.less.than.5.year.old
  + Members.with.age.5...17.years.old + House.Floor.Area + House.Age + Number.of.bedrooms + Number.of.Television + Number.of.CD.VCD.DVD + Number.of.Component.Stereo.set
  + Number.of.Refrigerator.Freezer + Number.of.Washing.Machine + Number.of.Airconditioner + Number.of.Car..Jeep..Van + Number.of.Landline.wireless.telephones
  + Number.of.Cellular.phone + Number.of.Personal.Computer + Number.of.Stove.with.Oven.Gas.Range + Number.of.Motorized.Banca + Number.of.Motorcycle.Tricycle,
  
  data=datos.log,family=binomial
)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
glm.probs<-predict(glm.fits,type="response")
glm.probs[1:10]
Income = datos.log$Total.Household.Income
contrasts(Income)
glm.pred<-rep("Deny",length(Income)) #Crea un vector con 1250 elementos "Down"
glm.pred[glm.probs>.5]="Accept" #Transforma en Up todos los elementos donde la probabilidad predicha>0.5

#install.packages("caret") #si no se ha instalado previamente
library(caret)
glm.pred<-as.factor(glm.pred)
confusionMatrix(glm.pred,Income)

#COMO LA MATRIZ DE CONFUSION DA MAL, QUITAMOS LAS VARIABLES CON P-VALOR MUY ALTO
glm.fits<-glm(
  Total.Household.Income~Total.Food.Expenditure + Bread.and.Cereals.Expenditure
  + Clothing..Footwear.and.Other.Wear.Expenditure
  + Housing.and.water.Expenditure + Medical.Care.Expenditure + Transportation.Expenditure 
  + Miscellaneous.Goods.and.Services.Expenditure + Special.Occasions.Expenditure 
  + Total.Income.from.Entrepreneurial.Acitivites + Total.Number.of.Family.members + Members.with.age.less.than.5.year.old
  + Members.with.age.5...17.years.old,
  
  data=datos.log,family=binomial
)
summary(glm.fits)

glm.probs<-predict(glm.fits,type="response")
glm.probs[1:10]
Income = datos.log$Total.Household.Income
contrasts(Income)
glm.pred<-rep("Accept",length(Income)) #Crea un vector con 1250 elementos "Down"
glm.pred[glm.probs>.5]="Deny" #Transforma en Up todos los elementos donde la probabilidad predicha>0.5

#install.packages("caret") #si no se ha instalado previamente
library(caret)
glm.pred<-as.factor(glm.pred)
confusionMatrix(glm.pred,Income)


#Validacion cruzada
folds5 <- createFolds(datos.log$Total.Household.Income, k = 5) #creamos los 10 folds
model5<-train(  Total.Household.Income~Total.Food.Expenditure + Bread.and.Cereals.Expenditure
                + Clothing..Footwear.and.Other.Wear.Expenditure
                + Housing.and.water.Expenditure + Medical.Care.Expenditure + Transportation.Expenditure 
                + Miscellaneous.Goods.and.Services.Expenditure + Special.Occasions.Expenditure 
                + Total.Income.from.Entrepreneurial.Acitivites + Total.Number.of.Family.members + Members.with.age.less.than.5.year.old
                + Members.with.age.5...17.years.old,
               data=datos.log,
               method="glm",
               family=binomial,
               trControl=trainControl(method="repeatedcv",number=5, index = folds5))
model5
glm.pred5<-predict(model5, datos.log)
confusionMatrix(glm.pred5, datos.log$Total.Household.Income)

# Usar estadisticos asociados
# Podemos usar el error cuadratico medio porque hay dos variables. 0 - una categoria, 1 - otra categoria
