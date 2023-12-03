# 7. Regresion multiple PLS, PCR (5to y 6to modelos)

# PCR
# usamos todas las columnas numericas, no quitamos las autocorrelacionadas
data.pca = data_train

# Medias y varianzas muy distintas (obvio por columnas como Number.of.Television y Total.Rice.Expenditure
apply(data.pca,2,mean)
apply(data.pca,2,var)

# Por tanto necesitamos usar el escalado a la hora de realizar el modelo
set.seed(2)
RLM_PCR<-pcr(Total.Household.Income~.,data=data.pca,scale=TRUE,validation="CV")

summary(RLM_PCR)

validationplot(RLM_PCR)
# Prediccion sobre test
RLM_PCR.pred<-predict(RLM_PCR,data_test,ncomp=12)
RLM_PCR.error = sqrt(mean((RLM_PCR.pred-data_test$Total.Household.Income)^2))
RLM_PCR.error # 199790.6

# # Sobre el conjunto total
# RLM_PCR.pred<-predict(RLM_PCR,datos,ncomp=30)
# RLM_PCR.error = sqrt(mean((RLM_PCR.pred-datos$Total.Household.Income)^2))
# RLM_PCR.error #

# PLS
set.seed(1)
RLM_PLS<-plsr(Total.Household.Income~.,data=data.pca,scale=TRUE,validation="CV")

summary(RLM_PLS)

validationplot(RLM_PLS)
# Prediccion sobre test
RLM_PLS.pred<-predict(RLM_PLS,data_test,ncomp=16)
RLM_PLS.error = sqrt(mean((RLM_PLS.pred-data_test$Total.Household.Income)^2))
RLM_PLS.error # 147659.5

# Sobre el conjunto total
# RLM_PLS.pred<-predict(RLM_PLS,datos,ncomp=5)
# RLM_PLS.error = sqrt(mean((RLM_PLS.pred-datos$Total.Household.Income)^2))
# RLM_PLS.error # 132266

# PLS predice mejor que PCR

# Ambas disminuyen la dimensionalidad a 22 componentes



