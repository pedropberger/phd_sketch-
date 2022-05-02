######################################################
#####carregando as planilhas##########################
######################################################
library(readxl)

library(dplyr)

setwd('C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/SIH_filtrado/')

dados_pol <- read_excel("SIH_unificado_poluentesmedio.xlsx", sheet = "Defasagem-completa") #dados de saude e poluicao

setwd('C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/Meteorologicos/')

meteo <- read_excel("dados.meteo.aeroporto.15a19.xlsx")

######################################################
##### separando os dados RGv e SUL ###################
######################################################

dados_GV <- data.frame(dados_pol[,c(6:9,10:15,22:27,34:39,46:51,58:63,70:75,82:87,94:99,106:113)], meteo[1:1462, c(9,12)])

#dados_SUL <- data.frame(dados_pol[,c(2:5,16:21,28:33,40:45,52:57,64:69,76:81,88:93,100:113)], meteo[1:1462, c(9,12)])

######################################################
##### Retirando ozonio dos dados   ###################
######################################################

######dados_GV <-data.frame(dados_GV[,-c(10,16,22,28,34,40,46,52)])
######dados_SUL <-data.frame(dados_SUL[,-c(6,12,18,24,30,36,42,48,54,60,66)])

######################################################
##### carregando pacotes para modelos gam ############
######################################################

library("gam")

library("mgcv")

attach(dados_GV)

################# crianças CID J #######################
###############todos os poluentes exceto pm10 ##########
########################################################

#################### Lag 0 #############################
modelo.kid.J.fullpm10.lag0 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv + so2.gv + no2.gv + co.gv + o3.gv + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.J.fullpm10.lag0) 

residuo.kid.J.fullpm10.lag0 <- residuals(modelo.kid.J.fullpm10.lag0)

par(mfrow = c(1,4))

plot(residuo.kid.J.fullpm10.lag0)
acf(residuo.kid.J.fullpm10.lag0)
qqnorm(residuo.kid.J.fullpm10.lag0)
hist(residuo.kid.J.fullpm10.lag0)
shapiro.test(residuo.kid.J.fullpm10.lag0)

#################### Lag 1 #############################

modelo.kid.J.fullpm10.lag1 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv.d.1  + so2.gv.d.1 + no2.gv.d.1 + co.gv.d.1 + o3.gv.d.1 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.J.fullpm10.lag1) 

residuo.kid.J.fullpm10.lag1 <- residuals(modelo.kid.J.fullpm10.lag1)

par(mfrow = c(1,4))

plot(residuo.kid.J.fullpm10.lag1)
acf(residuo.kid.J.fullpm10.lag1)
qqnorm(residuo.kid.J.fullpm10.lag1)
hist(residuo.kid.J.fullpm10.lag1)
shapiro.test(residuo.kid.J.fullpm10.lag1)


#################### Lag 2 #############################

modelo.kid.J.fullpm10.lag2 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv.d.2 + so2.gv.d.2 + no2.gv.d.2 + co.gv.d.2 + o3.gv.d.2 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.J.fullpm10.lag2) 

residuo.kid.J.fullpm10.lag2 <- residuals(modelo.kid.J.fullpm10.lag2)

par(mfrow = c(1,4))

plot(residuo.kid.J.fullpm10.lag2)
acf(residuo.kid.J.fullpm10.lag2)
qqnorm(residuo.kid.J.fullpm10.lag2)
hist(residuo.kid.J.fullpm10.lag2)
shapiro.test(residuo.kid.J.fullpm10.lag2)

#################### Lag 3 #############################

modelo.kid.J.fullpm10.lag3 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv.d.3 + so2.gv.d.3 + no2.gv.d.3 + co.gv.d.3 + o3.gv.d.3 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.J.fullpm10.lag3) 

residuo.kid.J.fullpm10.lag3 <- residuals(modelo.kid.J.fullpm10.lag3)

par(mfrow = c(1,4))

plot(residuo.kid.J.fullpm10.lag3)
acf(residuo.kid.J.fullpm10.lag3)
qqnorm(residuo.kid.J.fullpm10.lag3)
hist(residuo.kid.J.fullpm10.lag3)
shapiro.test(residuo.kid.J.fullpm10.lag3)

#################### Lag 4 #############################

modelo.kid.J.fullpm10.lag4 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv.d.4 + so2.gv.d.4 + no2.gv.d.4 + co.gv.d.4 + o3.gv.d.4 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.J.fullpm10.lag4) 

residuo.kid.J.fullpm10.lag4 <- residuals(modelo.kid.J.fullpm10.lag4)

par(mfrow = c(1,4))

plot(residuo.kid.J.fullpm10.lag4)
acf(residuo.kid.J.fullpm10.lag4)
qqnorm(residuo.kid.J.fullpm10.lag4)
hist(residuo.kid.J.fullpm10.lag4)
shapiro.test(residuo.kid.J.fullpm10.lag4)

#################### Lag 5 #############################

modelo.kid.J.fullpm10.lag5 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv.d.5 + so2.gv.d.5 + no2.gv.d.5 + co.gv.d.5 + o3.gv.d.5 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.J.fullpm10.lag5) 

residuo.kid.J.fullpm10.lag5 <- residuals(modelo.kid.J.fullpm10.lag5)

par(mfrow = c(1,4))

plot(residuo.kid.J.fullpm10.lag5)
acf(residuo.kid.J.fullpm10.lag5)
qqnorm(residuo.kid.J.fullpm10.lag5)
hist(residuo.kid.J.fullpm10.lag5)
shapiro.test(residuo.kid.J.fullpm10.lag5)

#################### Lag 6 #############################

modelo.kid.J.fullpm10.lag6 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv.d.6 + so2.gv.d.6 + no2.gv.d.6 + co.gv.d.6 + o3.gv.d.6 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.J.fullpm10.lag6) 

residuo.kid.J.fullpm10.lag6 <- residuals(modelo.kid.J.fullpm10.lag6)

par(mfrow = c(1,4))

plot(residuo.kid.J.fullpm10.lag6)
acf(residuo.kid.J.fullpm10.lag6)
qqnorm(residuo.kid.J.fullpm10.lag6)
hist(residuo.kid.J.fullpm10.lag6)
shapiro.test(residuo.kid.J.fullpm10.lag6)


#################### Lag 7 #############################

modelo.kid.J.fullpm10.lag7 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv.d.7 + so2.gv.d.7 + no2.gv.d.7 + co.gv.d.7 + o3.gv.d.7 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.J.fullpm10.lag7) 

residuo.kid.J.fullpm10.lag7 <- residuals(modelo.kid.J.fullpm10.lag7)

par(mfrow = c(1,4))

plot(residuo.kid.J.fullpm10.lag7)
acf(residuo.kid.J.fullpm10.lag7)
qqnorm(residuo.kid.J.fullpm10.lag7)
hist(residuo.kid.J.fullpm10.lag7)
shapiro.test(residuo.kid.J.fullpm10.lag7)


################# crianças CID J #######################
################## AIC E ANOVA #########################
###############todos os poluentes exceto pm10##########

library(bbmle)

AIC.mod.kid.J.fullpm10 <- AICctab(modelo.kid.J.fullpm10.lag0,modelo.kid.J.fullpm10.lag1,modelo.kid.J.fullpm10.lag2,
                                  modelo.kid.J.fullpm10.lag3,modelo.kid.J.fullpm10.lag4,modelo.kid.J.fullpm10.lag5,
                                  modelo.kid.J.fullpm10.lag6,modelo.kid.J.fullpm10.lag7,
                                  base = T, weights=T)
AIC.mod.kid.J.fullpm10 

anova.gam.mod.kid.J.fullpm10 <- anova.gam(modelo.kid.J.fullpm10.lag0,modelo.kid.J.fullpm10.lag1,modelo.kid.J.fullpm10.lag2,
                                          modelo.kid.J.fullpm10.lag3,modelo.kid.J.fullpm10.lag4,modelo.kid.J.fullpm10.lag5,
                                          modelo.kid.J.fullpm10.lag6,modelo.kid.J.fullpm10.lag7)

anova.gam.mod.kid.J.fullpm10

anova.mod.kid.J.fullpm10 <-anova(modelo.kid.J.fullpm10.lag0,modelo.kid.J.fullpm10.lag1,modelo.kid.J.fullpm10.lag2,
                                 modelo.kid.J.fullpm10.lag3,modelo.kid.J.fullpm10.lag4,modelo.kid.J.fullpm10.lag5,
                                 modelo.kid.J.fullpm10.lag6,modelo.kid.J.fullpm10.lag7)

anova.mod.kid.J.fullpm10