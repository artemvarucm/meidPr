
install.packages("caret") #si no se ha instalado previamente
library(caret)

datos.log = datos.sin.out #Copiamos los datos originales 
datos.log["Total.Number.of.Family.members"] = total.num.fam.mem
datos.log$Total.Household.Income = datos.log$Total.Household.Income / 12 #Calculamos el sueldo mensual
datos.log[,"Total.Household.Income"] = datos.log$Total.Household.Income/datos.log$Total.Number.of.Family.members #Sueldo mensual per capita (sueldo mensual / numero de miembros en la unidad familiar)
#4500 es el 5% del prestamo
datos.log$Total.Household.Income[datos.log$Total.Household.Income < 4500] = 0
datos.log$Total.Household.Income[datos.log$Total.Household.Income >= 4500] = 1

datos.log$Total.Household.Income[datos.log$Total.Household.Income == 0] = "Deny" #Todos aquellos que no puedan acceder al préstamo se les pone Deny
datos.log$Total.Household.Income[datos.log$Total.Household.Income == 1] = "Accept" #Todos aquellos que puedan acceder al préstamo se les pone Accept
datos.log$Total.Household.Income = as.factor(datos.log$Total.Household.Income) #Convertimos la variable Total.Household.Income en una categórica

#Dividimos en train y test
train<-sample(nrow(datos.log),ceiling(nrow(datos.log)/2))
datos.log.train=datos.log[train,]
datos.log.test=datos.log[-train,]

glm.fits<-glm( #Entrenamos el modelo
  Total.Household.Income~.,
  data=datos.log.train,family=binomial
)

summary(glm.fits)
glm.probs<-predict(glm.fits,type="response") #Calculamos la probabilidad de que la familia pueda acceder a préstamo
Income = datos.log.test$Total.Household.Income 
contrasts(Income)
glm.pred<-rep("Accept",length(Income)) #Crea un vector con 6007 elementos "Accept"
glm.pred[glm.probs>.5]="Deny" #Transforma en Deny todos los elementos donde la probabilidad predicha<0.5

glm.pred<-as.factor(glm.pred)
confusionMatrix(glm.pred,Income)

#Como la matriz de confusión nos muestra que nuestro modelo se parece mucho a tirar una moneda, quitamos las variables con p-valor muy alto para ver si mejora
glm.fits<-glm(
  Total.Household.Income~ Bread.and.Cereals.Expenditure
  + Clothing..Footwear.and.Other.Wear.Expenditure
  + Housing.and.water.Expenditure + Medical.Care.Expenditure + Transportation.Expenditure 
  + Miscellaneous.Goods.and.Services.Expenditure + Special.Occasions.Expenditure 
  + Total.Income.from.Entrepreneurial.Acitivites + Total.Number.of.Family.members + Members.with.age.less.than.5.year.old
  + Members.with.age.5...17.years.old,
  
  data=datos.log.train,family=binomial
)

summary(glm.fits)
glm.probs<-predict(glm.fits,type="response") #Calculamos la probabilidad de que la familia pueda acceder a préstamo
Income = datos.log.test$Total.Household.Income 
contrasts(Income)
glm.pred<-rep("Accept",length(Income)) #Crea un vector con 6007 elementos "Accept"
glm.pred[glm.probs>.5]="Deny" #Transforma en Deny todos los elementos donde la probabilidad predicha<0.5

glm.pred<-as.factor(glm.pred)
confusionMatrix(glm.pred,Income) #Realizamos la matriz de confusion

#Validacion cruzada
folds <- createFolds(datos.log.train$Total.Household.Income, k = 20) #creamos los 20 folds
model<-train(   Total.Household.Income~ Bread.and.Cereals.Expenditure + Meat.Expenditure + Total.Fish.and..marine.products.Expenditure
                + Fruit.Expenditure + Vegetables.Expenditure + Restaurant.and.hotels.Expenditure + Alcoholic.Beverages.Expenditure + Tobacco.Expenditure + Clothing..Footwear.and.Other.Wear.Expenditure
                + Housing.and.water.Expenditure + Medical.Care.Expenditure + Transportation.Expenditure + Communication.Expenditure 
                + Education.Expenditure + Miscellaneous.Goods.and.Services.Expenditure + Special.Occasions.Expenditure + Crop.Farming.and.Gardening.expenses 
                + Total.Income.from.Entrepreneurial.Acitivites + Household.Head.Age + Total.Number.of.Family.members + Members.with.age.less.than.5.year.old
                + Members.with.age.5...17.years.old + House.Floor.Area + House.Age + Number.of.bedrooms + Number.of.Television + Number.of.CD.VCD.DVD + Number.of.Component.Stereo.set
                + Number.of.Refrigerator.Freezer + Number.of.Washing.Machine + Number.of.Airconditioner + Number.of.Car..Jeep..Van + Number.of.Landline.wireless.telephones
                + Number.of.Cellular.phone + Number.of.Personal.Computer + Number.of.Stove.with.Oven.Gas.Range + Number.of.Motorized.Banca + Number.of.Motorcycle.Tricycle,
                
                data=datos.log.train,
                method="glm",
                family=binomial,
                trControl=trainControl(method="repeatedcv",number=20, index = folds))
model
glm.pred<-predict(model, datos.log.test)
confusionMatrix(glm.pred, datos.log.test$Total.Household.Income)

# Usar estadisticos asociados
# Podemos usar el error cuadratico medio porque hay dos variables. 0 - una categoria, 1 - otra categoria
