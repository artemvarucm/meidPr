# Artem Vartanov y Mario Baldocchi
# bibliotecas necesarias
library(nortest) # test normalidad
library(lmtest) # test homoscedasticidad
library(MASS) # box-cox
library(leaps) # regsubsets

# Aqui uniremos los archivos en el siguiente orden
# importYpreprocess
# autocorrelaciones
# RLM_FULL
# RLM_BEST
# RLM_REGULAR
# PLS-PCR
# LOGISTICA


# Anadir en la memoria una tabla asi, cogiendo como mejor el modelo con el menor error, 
# explicando si es critico o no(hacer raiz cuadrada antes)            
#########################################
#VARIABLES MODEL # X1 # X2 # X3 # ERROR #
#########################################
# RLM            #  x #    # x  # 10000 #
#########################################
# RLM_RIDGE      #    # x  #  x # 40000 #
#########################################
# RLM_LASSO      #    # x  #  x # 5000  #
#########################################
#     ***



