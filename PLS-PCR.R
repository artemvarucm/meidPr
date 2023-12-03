# 7. Regresion multiple PLS, PCR (5to y 6to modelos)

# PCR

library(pls)
# usamos todas las columnas numericas, no quitamos las autocorrelacionadas
data.numeric = data_train

set.seed(2)
RLM_PCR<-pcr(Total.Household.Income~.,data=data.numeric,scale=TRUE,validation="CV")

summary(RLM_PCR)

validationplot(RLM_PCR,val.type="MSEP")
# Prediccion sobre test
RLM_PCR.pred<-predict(RLM_PCR,data_test,ncomp=30)
RLM_PCR.error = sqrt(mean((RLM_PCR.pred-data_test$Total.Household.Income)^2))
RLM_PCR.error # 103558.2

# Sobre el conjunto total
RLM_PCR.pred<-predict(RLM_PCR,datos,ncomp=30)
RLM_PCR.error = sqrt(mean((RLM_PCR.pred-datos$Total.Household.Income)^2))
RLM_PCR.error # 137102.1

# PLS
set.seed(2)
RLM_PLS<-plsr(Total.Household.Income~.,data=data.numeric,scale=TRUE,validation="CV")

summary(RLM_PLS)

validationplot(RLM_PLS,val.type="MSEP")
# Prediccion sobre test
RLM_PLS.pred<-predict(RLM_PLS,data_test,ncomp=5)
RLM_PLS.error = sqrt(mean((RLM_PLS.pred-data_test$Total.Household.Income)^2))
RLM_PLS.error # 95451.36

# Sobre el conjunto total
RLM_PLS.pred<-predict(RLM_PLS,datos,ncomp=5)
RLM_PLS.error = sqrt(mean((RLM_PLS.pred-datos$Total.Household.Income)^2))
RLM_PLS.error # 132266

# PLS predice mejor que PCR, ademas, disminuyendo la dimensionalidad a 5 componentes



