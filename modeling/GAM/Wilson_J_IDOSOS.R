######################################################
#####carregando as planilhas##########################
######################################################
library(readxl)

library(dplyr)

setwd('C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/SIH_filtrado/')

dados_pol <- read_excel("SIH_unificado_poluentesmedio.xlsx", sheet = "Defasagem-completa") #dados de saude e polui

setwd('C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/Meteorologicos/')

meteo <- read_excel("dados.meteo.aeroporto.15a19.xlsx")

######################################################
##### separando os dados RGC e SUL ###################
######################################################
 
dados_rgv <- data.frame(dados_pol[,c(6:9,10:15,22:27,34:39,46:51,58:63,70:75,82:87,94:99)], meteo[1:1462, c(9,12)])

dados_sul <- data.frame(dados_pol[,c(2:5,16:21,28:33,40:45,52:57,64:69,76:81,88:93,100:105)], meteo[1:1462, c(9,12)])


######################################################
##### Retirando ozonio dos dados   ###################
######################################################

dados_GV <-data.frame(dados_rgv[,-c(10,16,22,28,34,40,46,52)])

attach(dados_GV)

######################################################
##### carregando pacotes para modelos gam ############
######################################################

library("gam")

library("mgcv")

################# IDOSOS CID J ##########################
########### MODELOS POLUENTES EXCETO OZONIO #############
######################## Lag 0 ##########################

modelo.idoso.J.lag0<- mgcv::gam(SIH_GV_J_Old ~ pm25.gv + pm10.gv + so2.gv + no2.gv + co.gv + Temp.Ar + Umidade.Ar, data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.idoso.J.lag0) 

residuo.idoso.J.lag0 <- residuals(modelo.idoso.J.lag0)

par(mfrow = c(1,4))

plot(residuo.idoso.J.lag0)
acf(residuo.idoso.J.lag0)
qqnorm(residuo.idoso.J.lag0)
qqline(residuo.idoso.J.lag0)
hist(residuo.idoso.J.lag0)
 
################# IDOSOS CID J ##########################
########### MODELOS POLUENTES EXCETO OZONIO #############
######################## Lag 1 ##########################

modelo.idoso.J.lag1<- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.1 + pm10.gv.d.1 + so2.gv.d.1 + no2.gv.d.1 + co.gv.d.1 + Temp.Ar + Umidade.Ar, data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.idoso.J.lag1) 

residuo.idoso.J.lag1 <- residuals(modelo.idoso.J.lag1)

plot(residuo.idoso.J.lag1)
acf(residuo.idoso.J.lag1)
qqnorm(residuo.idoso.J.lag1)
hist(residuo.idoso.J.lag1)

################# IDOSOS CID J ##########################
########### MODELOS POLUENTES EXCETO OZONIO #############
######################## Lag 2 ##########################

modelo.idoso.J.lag2<- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.2 + pm10.gv.d.2 + so2.gv.d.2 + no2.gv.d.2 + co.gv.d.2 + Temp.Ar + Umidade.Ar, data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.idoso.J.lag2) 

residuo.idoso.J.lag2 <- residuals(modelo.idoso.J.lag2)

plot(residuo.idoso.J.lag2)
acf(residuo.idoso.J.lag2)
qqnorm(residuo.idoso.J.lag2)
hist(residuo.idoso.J.lag2)

################# IDOSOS CID J ##########################
########### MODELOS POLUENTES EXCETO OZONIO #############
######################## Lag 3 ##########################

modelo.idoso.J.lag3<- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.3 + pm10.gv.d.3 + so2.gv.d.3 + no2.gv.d.3 + co.gv.d.3 + Temp.Ar + Umidade.Ar, data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.idoso.J.lag3) 

residuo.idoso.J.lag3 <- residuals(modelo.idoso.J.lag3)

plot(residuo.idoso.J.lag3)
acf(residuo.idoso.J.lag3)
qqnorm(residuo.idoso.J.lag3)
hist(residuo.idoso.J.lag3)

################# IDOSOS CID J ##########################
########### MODELOS POLUENTES EXCETO OZONIO #############
######################## Lag 4 ##########################

modelo.idoso.J.lag4<- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.4 + pm10.gv.d.4 + so2.gv.d.4 + no2.gv.d.4 + co.gv.d.4 + Temp.Ar + Umidade.Ar, data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.idoso.J.lag4) 

residuo.idoso.J.lag4 <- residuals(modelo.idoso.J.lag4)

plot(residuo.idoso.J.lag4)
acf(residuo.idoso.J.lag4)
qqnorm(residuo.idoso.J.lag4)
hist(residuo.idoso.J.lag4)

################# IDOSOS CID J ##########################
########### MODELOS POLUENTES EXCETO OZONIO #############
######################## Lag 5 ##########################

modelo.idoso.J.lag5<- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.5 + pm10.gv.d.5 + so2.gv.d.5 + no2.gv.d.5 + co.gv.d.5 + Temp.Ar + Umidade.Ar, data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.idoso.J.lag5) 

residuo.idoso.J.lag5 <- residuals(modelo.idoso.J.lag5)

plot(residuo.idoso.J.lag5)
acf(residuo.idoso.J.lag5)
qqnorm(residuo.idoso.J.lag5)
hist(residuo.idoso.J.lag5)

################# IDOSOS CID J ##########################
########### MODELOS POLUENTES EXCETO OZONIO #############
######################## Lag 6 ##########################

modelo.idoso.J.lag6<- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.6 + pm10.gv.d.6 + so2.gv.d.6 + no2.gv.d.6 + co.gv.d.6 + Temp.Ar + Umidade.Ar, data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.idoso.J.lag6) 

residuo.idoso.J.lag6 <- residuals(modelo.idoso.J.lag6)

plot(residuo.idoso.J.lag6)
acf(residuo.idoso.J.lag6)
qqnorm(residuo.idoso.J.lag6)
hist(residuo.idoso.J.lag6)

################# IDOSOS CID J ##########################
########### MODELOS POLUENTES EXCETO OZONIO #############
######################## Lag 7 ##########################

modelo.idoso.J.lag7<- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.7 + pm10.gv.d.7 + so2.gv.d.7 + no2.gv.d.7 + co.gv.d.7 + Temp.Ar + Umidade.Ar, data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.idoso.J.lag7) 

residuo.idoso.J.lag7 <- residuals(modelo.idoso.J.lag7)

plot(residuo.idoso.J.lag7)
acf(residuo.idoso.J.lag7)
qqnorm(residuo.idoso.J.lag7)
hist(residuo.idoso.J.lag7)



