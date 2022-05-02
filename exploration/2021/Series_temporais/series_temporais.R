############################################################################################################################
###### IMPORTANDO OS DADOS ######
#################################
library(readxl)
library(dplyr)
library(forecast)
library(ggplot2)
library(fpp2)

setwd('C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/SIH_filtrado/')

list.files()

dados_pol <- read_excel("SIH_unificado_poluentesmedio.xlsx", sheet = "Defasagem-completa") #dados de saude e polui

setwd('C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/Meteorologicos/')

meteo <- read_excel("dados.meteo.aeroporto.15a19.xlsx")

dados <- data.frame(dados_pol, meteo[1:1462, 5:12])

attach(dados)

############################################################################################################################
###### Serie temporal SIH_GV_J_Old ######
##########################################
ts_SIH_GV_J_Old <- ts(dados$SIH_GV_J_Old)

autoplot(ts_SIH_GV_J_Old)

par(mfrow=c(1,2))

acf(ts_SIH_GV_J_Old)
pacf(ts_SIH_GV_J_Old)

par(mfrow=c(1,1))

ARIMA_fit_SIH_GV_J_Old <- auto.arima(ts_SIH_GV_J_Old)

checkresiduals(ARIMA_fit_SIH_GV_J_Old)

autoplot(forecast(ARIMA_fit_SIH_GV_J_Old, h =7))

##################################################
####Interpretando a saida da funcao auto.arima####
##################################################
# a notacao de saida do modelo ajustado: ARIMA(p,d,q)(P,D,Q)[m]
# onde:
# p = parametros autoregressivos
# d = a ordem de integracao
# q = numero de parametros de medias moveis
# P = numero de parametros autorregressivos sazonais
# D = ordem de integracao sazonal
# Q = parametros de medias moveis sazonais
# m = ordem sazonal
############################################################################################################################
###### Serie temporal SIH_GV_I_Old ######
##########################################
ts_SIH_GV_I_Old <- ts(dados$SIH_GV_I_Old)

autoplot(ts_SIH_GV_I_Old)

par(mfrow=c(1,2))

acf(ts_SIH_GV_I_Old)
pacf(ts_SIH_GV_I_Old)

par(mfrow=c(1,1))

ARIMA_fit_SIH_GV_I_Old <- auto.arima(ts_SIH_GV_I_Old)

ARIMA_fit_SIH_GV_I_Old

checkresiduals(ARIMA_fit_SIH_GV_I_Old)

autoplot(forecast(ARIMA_fit_SIH_GV_I_Old, h =7))
############################################################################################################################
###### Serie temporal SIH_Sul_J_Old ######
##########################################
ts_SIH_Sul_J_Old <- ts(dados$SIH_Sul_J_Old)

autoplot(ts_SIH_Sul_J_Old)

par(mfrow=c(1,2))

acf(ts_SIH_Sul_J_Old)
pacf(ts_SIH_Sul_J_Old)

par(mfrow=c(1,1))

ARIMA_fit_SIH_Sul_J_Old <- auto.arima(ts_SIH_Sul_J_Old)

ARIMA_fit_SIH_Sul_J_Old

checkresiduals(ARIMA_fit_SIH_Sul_J_Old)

autoplot(forecast(ARIMA_fit_SIH_Sul_J_Old, h =7))

############################################################################################################################
###### Serie temporal SIH_Sul_I_Old ######
##########################################
ts_SIH_Sul_I_Old <- ts(dados$SIH_Sul_I_Old)

autoplot(ts_SIH_Sul_I_Old)

par(mfrow=c(1,2))

acf(ts_SIH_Sul_I_Old)
pacf(ts_SIH_Sul_I_Old)

par(mfrow=c(1,1))

ARIMA_fit_SIH_Sul_I_Old <- auto.arima(ts_SIH_Sul_I_Old)

ARIMA_fit_SIH_Sul_I_Old

checkresiduals(ARIMA_fit_SIH_Sul_I_Old)

autoplot(forecast(ARIMA_fit_SIH_Sul_I_Old, h =7))

############################################################################################################################
###### Serie temporal SIH_GV_J_Kid ######
##########################################
ts_SIH_GV_J_Kid <- ts(dados$SIH_GV_J_Kid)

autoplot(ts_SIH_GV_J_Kid)

par(mfrow=c(1,2))

acf(ts_SIH_GV_J_Kid)
pacf(ts_SIH_GV_J_Kid)

par(mfrow=c(1,1))

ARIMA_fit_SIH_GV_J_Kid <- auto.arima(ts_SIH_GV_J_Kid)

ARIMA_fit_SIH_GV_J_Kid

checkresiduals(ARIMA_fit_SIH_GV_J_Kid)

autoplot(forecast(ARIMA_fit_SIH_GV_J_Kid, h =7))

############################################################################################################################
###### Serie temporal SIH_GV_I_Kid ######
##########################################
ts_SIH_GV_I_Kid <- ts(dados$SIH_GV_I_Kid)

autoplot(ts_SIH_GV_I_Kid)

par(mfrow=c(1,2))

acf(ts_SIH_GV_I_Kid)
pacf(ts_SIH_GV_I_Kid)

par(mfrow=c(1,1))

ARIMA_fit_SIH_GV_I_Kid <- auto.arima(ts_SIH_GV_I_Kid)

ARIMA_fit_SIH_GV_I_Kid

checkresiduals(ARIMA_fit_SIH_GV_I_Kid)

autoplot(forecast(ARIMA_fit_SIH_GV_I_Kid, h =7))

############################################################################################################################
###### Serie temporal SIH_Sul_J_Kid ######
##########################################
ts_SIH_Sul_J_Kid <- ts(dados$SIH_Sul_J_Kid)

autoplot(ts_SIH_Sul_J_Kid)

par(mfrow=c(1,2))

acf(ts_SIH_Sul_J_Kid)
pacf(ts_SIH_Sul_J_Kid)

par(mfrow=c(1,1))

ARIMA_fit_SIH_Sul_J_Kid <- auto.arima(ts_SIH_Sul_J_Kid)

ARIMA_fit_SIH_Sul_J_Kid

checkresiduals(ARIMA_fit_SIH_Sul_J_Kid)

autoplot(forecast(ARIMA_fit_SIH_Sul_J_Kid, h =7))
############################################################################################################################
###### Serie temporal SIH_Sul_I_Kid ######
##########################################
ts_SIH_Sul_I_Kid <- ts(dados$SIH_Sul_I_Kid)

autoplot(ts_SIH_Sul_I_Kid)

par(mfrow=c(1,2))

acf(ts_SIH_Sul_I_Kid)
pacf(ts_SIH_Sul_I_Kid)

par(mfrow=c(1,1))

ARIMA_fit_SIH_Sul_I_Kid <- auto.arima(ts_SIH_Sul_I_Kid)

ARIMA_fit_SIH_Sul_I_Kid

checkresiduals(ARIMA_fit_SIH_Sul_I_Kid)

autoplot(forecast(ARIMA_fit_SIH_Sul_I_Kid, h =7))

############################################################################################################################
######## Serie temporal pm10.gv ##########
##########################################
ts_pm10.gv <- ts(dados$pm10.gv)

autoplot(ts_pm10.gv)

par(mfrow=c(1,2))

acf(ts_pm10.gv)
pacf(ts_pm10.gv)

par(mfrow=c(1,1))

ARIMA_fit_pm10.gv <- auto.arima(ts_pm10.gv)

ARIMA_fit_pm10.gv

checkresiduals(ARIMA_fit_pm10.gv)

autoplot(forecast(ARIMA_fit_pm10.gv, h =7))

############################################################################################################################
######## Serie temporal pm25.gv ##########
##########################################
ts_pm25.gv <- ts(dados$pm25.gv)

autoplot(ts_pm25.gv)

par(mfrow=c(1,2))

acf(ts_pm25.gv)
pacf(ts_pm25.gv)

par(mfrow=c(1,1))

ARIMA_fit_pm25.gv <- auto.arima(ts_pm25.gv)

ARIMA_fit_pm25.gv

checkresiduals(ARIMA_fit_pm25.gv)

autoplot(forecast(ARIMA_fit_pm25.gv, h =7))

############################################################################################################################
######## Serie temporal so2.gv ##########
##########################################
ts_so2.gv <- ts(dados$so2.gv)

autoplot(ts_so2.gv)

par(mfrow=c(1,2))

acf(ts_so2.gv)
pacf(ts_so2.gv)

par(mfrow=c(1,1))

ARIMA_fit_so2.gv <- auto.arima(ts_so2.gv)

ARIMA_fit_so2.gv

checkresiduals(ARIMA_fit_so2.gv)

autoplot(forecast(ARIMA_fit_so2.gv, h =7))

############################################################################################################################
######## Serie temporal no2.gv ##########
##########################################
ts_no2.gv <- ts(dados$no2.gv)

autoplot(ts_no2.gv)

par(mfrow=c(1,2))

acf(ts_no2.gv)
pacf(ts_no2.gv)

par(mfrow=c(1,1))

ARIMA_fit_no2.gv <- auto.arima(ts_no2.gv)

ARIMA_fit_no2.gv

checkresiduals(ARIMA_fit_no2.gv)

autoplot(forecast(ARIMA_fit_no2.gv, h =7))

############################################################################################################################
######## Serie temporal co.gv ##########
##########################################
ts_co.gv <- ts(dados$co.gv)

autoplot(ts_co.gv)

par(mfrow=c(1,2))

acf(ts_co.gv)
pacf(ts_co.gv)

par(mfrow=c(1,1))

ARIMA_fit_co.gv <- auto.arima(ts_co.gv)

ARIMA_fit_co.gv

checkresiduals(ARIMA_fit_co.gv)

autoplot(forecast(ARIMA_fit_co.gv, h =7))

############################################################################################################################
######## Serie temporal o3.gv ##########
##########################################
ts_o3.gv <- ts(dados$o3.gv)

autoplot(ts_o3.gv)

par(mfrow=c(1,2))

acf(ts_o3.gv)
pacf(ts_o3.gv)

par(mfrow=c(1,1))

ARIMA_fit_o3.gv <- auto.arima(ts_o3.gv)

ARIMA_fit_o3.gv

checkresiduals(ARIMA_fit_o3.gv)

autoplot(forecast(ARIMA_fit_o3.gv, h =7))
############################################################################################################################
######## Serie temporal Temp.Ar ##########
##########################################
ts_Temp.Ar <- ts(dados$Temp.Ar)

autoplot(ts_Temp.Ar)

par(mfrow=c(1,2))

acf(ts_Temp.Ar)
pacf(ts_Temp.Ar)

par(mfrow=c(1,1))

ARIMA_fit_Temp.Ar <- auto.arima(ts_Temp.Ar)

ARIMA_fit_Temp.Ar

checkresiduals(ARIMA_fit_Temp.Ar)

autoplot(forecast(ARIMA_fit_Temp.Ar, h =7))
############################################################################################################################
######## Serie temporal Umidade.Ar ##########
##########################################
ts_Umidade.Ar <- ts(dados$Umidade.Ar)

autoplot(ts_Umidade.Ar)

par(mfrow=c(1,2))

acf(ts_Umidade.Ar)
pacf(ts_Umidade.Ar)

par(mfrow=c(1,1))

ARIMA_fit_Umidade.Ar <- auto.arima(ts_Umidade.Ar)

ARIMA_fit_Umidade.Ar

checkresiduals(ARIMA_fit_Umidade.Ar)

autoplot(forecast(ARIMA_fit_Umidade.Ar, h =7))

############################################################################################################################
######## Serie temporal pm10.sul ##########
##########################################
ts_pm10.sul <- ts(dados$pm10.sul)

autoplot(ts_pm10.sul)

par(mfrow=c(1,2))

acf(ts_pm10.sul)
pacf(ts_pm10.sul)

par(mfrow=c(1,1))

ARIMA_fit_pm10.sul <- auto.arima(ts_pm10.sul)

ARIMA_fit_pm10.sul

checkresiduals(ARIMA_fit_pm10.sul)

autoplot(forecast(ARIMA_fit_pm10.sul, h =7))

############################################################################################################################
######## Serie temporal pm25.sul ##########
##########################################
ts_pm25.sul <- ts(dados$pm25.sul)

autoplot(ts_pm25.sul)

par(mfrow=c(1,2))

acf(ts_pm25.sul)
pacf(ts_pm25.sul)

par(mfrow=c(1,1))

ARIMA_fit_pm25.sul <- auto.arima(ts_pm25.sul)

ARIMA_fit_pm25.sul

checkresiduals(ARIMA_fit_pm25.sul)

autoplot(forecast(ARIMA_fit_pm25.sul, h =7))

############################################################################################################################
######## Serie temporal so2.sul ##########
##########################################
ts_so2.sul <- ts(dados$so2.sul)

autoplot(ts_so2.sul)

par(mfrow=c(1,2))

acf(ts_so2.sul)
pacf(ts_so2.sul)

par(mfrow=c(1,1))

ARIMA_fit_so2.sul <- auto.arima(ts_so2.sul)

ARIMA_fit_so2.sul

checkresiduals(ARIMA_fit_so2.sul)

autoplot(forecast(ARIMA_fit_so2.sul, h =7))

############################################################################################################################
######## Serie temporal no2.sul ##########
##########################################
ts_no2.sul <- ts(dados$no2.sul)

autoplot(ts_no2.sul)

par(mfrow=c(1,2))

acf(ts_no2.sul)
pacf(ts_no2.sul)

par(mfrow=c(1,1))

ARIMA_fit_no2.sul <- auto.arima(ts_no2.sul)

ARIMA_fit_no2.sul

checkresiduals(ARIMA_fit_no2.sul)

autoplot(forecast(ARIMA_fit_no2.sul, h =7))

############################################################################################################################
######## Serie temporal co.sul ##########
##########################################
ts_co.sul <- ts(dados$co.sul)

autoplot(ts_co.sul)

par(mfrow=c(1,2))

acf(ts_co.sul)
pacf(ts_co.sul)

par(mfrow=c(1,1))

ARIMA_fit_co.sul <- auto.arima(ts_co.sul)

ARIMA_fit_co.sul

checkresiduals(ARIMA_fit_co.sul)

autoplot(forecast(ARIMA_fit_co.sul, h =7))

############################################################################################################################
######## Serie temporal o3.sul ##########
##########################################
ts_o3.sul <- ts(dados$o3.sul)

autoplot(ts_o3.sul)

par(mfrow=c(1,2))

acf(ts_o3.sul)
pacf(ts_o3.sul)

par(mfrow=c(1,1))

ARIMA_fit_o3.sul <- auto.arima(ts_o3.sul)

ARIMA_fit_o3.sul

checkresiduals(ARIMA_fit_o3.sul)

autoplot(forecast(ARIMA_fit_o3.sul, h =7))
