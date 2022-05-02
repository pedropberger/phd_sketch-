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

################# idosos CID J #######################
##################### Pm25 #############################
########################################################

#################### Lag 0 #############################
modelo.Old.J.pm25.lag0 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.pm25.lag0) 

residuo.Old.J.pm25.lag0 <- residuals(modelo.Old.J.pm25.lag0)

par(mfrow = c(1,4))

plot(residuo.Old.J.pm25.lag0)
acf(residuo.Old.J.pm25.lag0)
qqnorm(residuo.Old.J.pm25.lag0)
hist(residuo.Old.J.pm25.lag0)
shapiro.test(residuo.Old.J.pm25.lag0)

#################### Lag 1 #############################

modelo.Old.J.pm25.lag1 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.1 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.pm25.lag1) 

residuo.Old.J.pm25.lag1 <- residuals(modelo.Old.J.pm25.lag1)

par(mfrow = c(1,4))

plot(residuo.Old.J.pm25.lag1)
acf(residuo.Old.J.pm25.lag1)
qqnorm(residuo.Old.J.pm25.lag1)
hist(residuo.Old.J.pm25.lag1)
shapiro.test(residuo.Old.J.pm25.lag1)


#################### Lag 2 #############################

modelo.Old.J.pm25.lag2 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.2 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.pm25.lag2) 

residuo.Old.J.pm25.lag2 <- residuals(modelo.Old.J.pm25.lag2)

par(mfrow = c(1,4))

plot(residuo.Old.J.pm25.lag2)
acf(residuo.Old.J.pm25.lag2)
qqnorm(residuo.Old.J.pm25.lag2)
hist(residuo.Old.J.pm25.lag2)
shapiro.test(residuo.Old.J.pm25.lag2)

#################### Lag 3 #############################

modelo.Old.J.pm25.lag3 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.3 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.pm25.lag3) 

residuo.Old.J.pm25.lag3 <- residuals(modelo.Old.J.pm25.lag3)

par(mfrow = c(1,4))

plot(residuo.Old.J.pm25.lag3)
acf(residuo.Old.J.pm25.lag3)
qqnorm(residuo.Old.J.pm25.lag3)
hist(residuo.Old.J.pm25.lag3)
shapiro.test(residuo.Old.J.pm25.lag3)

#################### Lag 4 #############################

modelo.Old.J.pm25.lag4 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.4 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.pm25.lag4) 

residuo.Old.J.pm25.lag4 <- residuals(modelo.Old.J.pm25.lag4)

par(mfrow = c(1,4))

plot(residuo.Old.J.pm25.lag4)
acf(residuo.Old.J.pm25.lag4)
qqnorm(residuo.Old.J.pm25.lag4)
hist(residuo.Old.J.pm25.lag4)
shapiro.test(residuo.Old.J.pm25.lag4)

#################### Lag 5 #############################

modelo.Old.J.pm25.lag5 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.5 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.pm25.lag5) 

residuo.Old.J.pm25.lag5 <- residuals(modelo.Old.J.pm25.lag5)

par(mfrow = c(1,4))

plot(residuo.Old.J.pm25.lag5)
acf(residuo.Old.J.pm25.lag5)
qqnorm(residuo.Old.J.pm25.lag5)
hist(residuo.Old.J.pm25.lag5)
shapiro.test(residuo.Old.J.pm25.lag5)

#################### Lag 6 #############################

modelo.Old.J.pm25.lag6 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.6 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.pm25.lag6) 

residuo.Old.J.pm25.lag6 <- residuals(modelo.Old.J.pm25.lag6)

par(mfrow = c(1,4))

plot(residuo.Old.J.pm25.lag6)
acf(residuo.Old.J.pm25.lag6)
qqnorm(residuo.Old.J.pm25.lag6)
hist(residuo.Old.J.pm25.lag6)
shapiro.test(residuo.Old.J.pm25.lag6)


#################### Lag 7 #############################

modelo.Old.J.pm25.lag7 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.7 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.pm25.lag7) 

residuo.Old.J.pm25.lag7 <- residuals(modelo.Old.J.pm25.lag7)

par(mfrow = c(1,4))

plot(residuo.Old.J.pm25.lag7)
acf(residuo.Old.J.pm25.lag7)
qqnorm(residuo.Old.J.pm25.lag7)
hist(residuo.Old.J.pm25.lag7)
shapiro.test(residuo.Old.J.pm25.lag7)


################# idosos CID J #######################
################## AIC E ANOVA #########################
#################### pm25 ##############################

library(bbmle)

AIC.mod.Old.J.pm25 <- AICctab(modelo.Old.J.pm25.lag0,modelo.Old.J.pm25.lag1,modelo.Old.J.pm25.lag2,modelo.Old.J.pm25.lag3, 
                              modelo.Old.J.pm25.lag4,modelo.Old.J.pm25.lag5,modelo.Old.J.pm25.lag6,
                              modelo.Old.J.pm25.lag7,
                              base = T, weights=T)
AIC.mod.Old.J.pm25 

anova.gam.mod.Old.J.pm25 <- anova.gam(modelo.Old.J.pm25.lag0,modelo.Old.J.pm25.lag1,modelo.Old.J.pm25.lag2,modelo.Old.J.pm25.lag3, 
                                      modelo.Old.J.pm25.lag4,modelo.Old.J.pm25.lag5,modelo.Old.J.pm25.lag6,
                                      modelo.Old.J.pm25.lag7)

anova.gam.mod.Old.J.pm25

anova.mod.Old.J.pm25 <-anova(modelo.Old.J.pm25.lag0,modelo.Old.J.pm25.lag1,modelo.Old.J.pm25.lag2,modelo.Old.J.pm25.lag3, 
                             modelo.Old.J.pm25.lag4,modelo.Old.J.pm25.lag5,modelo.Old.J.pm25.lag6,
                             modelo.Old.J.pm25.lag7)

anova.mod.Old.J.pm25



################# idosos CID J #######################
##################### Pm10 #############################
########################################################

#################### Lag 0 #############################
modelo.Old.J.pm10.lag0 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.pm10.lag0) 

residuo.Old.J.pm10.lag0 <- residuals(modelo.Old.J.pm10.lag0)

par(mfrow = c(1,4))

plot(residuo.Old.J.pm10.lag0)
acf(residuo.Old.J.pm10.lag0)
qqnorm(residuo.Old.J.pm10.lag0)
hist(residuo.Old.J.pm10.lag0)
shapiro.test(residuo.Old.J.pm10.lag0)

#################### Lag 1 #############################

modelo.Old.J.pm10.lag1 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.1 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.pm10.lag1) 

residuo.Old.J.pm10.lag1 <- residuals(modelo.Old.J.pm10.lag1)

par(mfrow = c(1,4))

plot(residuo.Old.J.pm10.lag1)
acf(residuo.Old.J.pm10.lag1)
qqnorm(residuo.Old.J.pm10.lag1)
hist(residuo.Old.J.pm10.lag1)
shapiro.test(residuo.Old.J.pm10.lag1)


#################### Lag 2 #############################

modelo.Old.J.pm10.lag2 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.2 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.pm10.lag2) 

residuo.Old.J.pm10.lag2 <- residuals(modelo.Old.J.pm10.lag2)

par(mfrow = c(1,4))

plot(residuo.Old.J.pm10.lag2)
acf(residuo.Old.J.pm10.lag2)
qqnorm(residuo.Old.J.pm10.lag2)
hist(residuo.Old.J.pm10.lag2)
shapiro.test(residuo.Old.J.pm10.lag2)

#################### Lag 3 #############################

modelo.Old.J.pm10.lag3 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.3 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.pm10.lag3) 

residuo.Old.J.pm10.lag3 <- residuals(modelo.Old.J.pm10.lag3)

par(mfrow = c(1,4))

plot(residuo.Old.J.pm10.lag3)
acf(residuo.Old.J.pm10.lag3)
qqnorm(residuo.Old.J.pm10.lag3)
hist(residuo.Old.J.pm10.lag3)
shapiro.test(residuo.Old.J.pm10.lag3)

#################### Lag 4 #############################

modelo.Old.J.pm10.lag4 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.4 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.pm10.lag4) 

residuo.Old.J.pm10.lag4 <- residuals(modelo.Old.J.pm10.lag4)

par(mfrow = c(1,4))

plot(residuo.Old.J.pm10.lag4)
acf(residuo.Old.J.pm10.lag4)
qqnorm(residuo.Old.J.pm10.lag4)
hist(residuo.Old.J.pm10.lag4)
shapiro.test(residuo.Old.J.pm10.lag4)

#################### Lag 5 #############################

modelo.Old.J.pm10.lag5 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.5 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.pm10.lag5) 

residuo.Old.J.pm10.lag5 <- residuals(modelo.Old.J.pm10.lag5)

par(mfrow = c(1,4))

plot(residuo.Old.J.pm10.lag5)
acf(residuo.Old.J.pm10.lag5)
qqnorm(residuo.Old.J.pm10.lag5)
hist(residuo.Old.J.pm10.lag5)
shapiro.test(residuo.Old.J.pm10.lag5)

#################### Lag 6 #############################

modelo.Old.J.pm10.lag6 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.6 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.pm10.lag6) 

residuo.Old.J.pm10.lag6 <- residuals(modelo.Old.J.pm10.lag6)

par(mfrow = c(1,4))

plot(residuo.Old.J.pm10.lag6)
acf(residuo.Old.J.pm10.lag6)
qqnorm(residuo.Old.J.pm10.lag6)
hist(residuo.Old.J.pm10.lag6)
shapiro.test(residuo.Old.J.pm10.lag6)


#################### Lag 7 #############################

modelo.Old.J.pm10.lag7 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.7 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.pm10.lag7) 

residuo.Old.J.pm10.lag7 <- residuals(modelo.Old.J.pm10.lag7)

par(mfrow = c(1,4))

plot(residuo.Old.J.pm10.lag7)
acf(residuo.Old.J.pm10.lag7)
qqnorm(residuo.Old.J.pm10.lag7)
hist(residuo.Old.J.pm10.lag7)
shapiro.test(residuo.Old.J.pm10.lag7)


################# idosos CID J #######################
################## AIC E ANOVA #########################
#################### pm10 ##############################

library(bbmle)

AIC.mod.Old.J.pm10 <- AICctab(modelo.Old.J.pm10.lag0,modelo.Old.J.pm10.lag1,modelo.Old.J.pm10.lag2,modelo.Old.J.pm10.lag3, 
                              modelo.Old.J.pm10.lag4,modelo.Old.J.pm10.lag5,modelo.Old.J.pm10.lag6,
                              modelo.Old.J.pm10.lag7,
                              base = T, weights=T)
AIC.mod.Old.J.pm10 

anova.gam.mod.Old.J.pm10 <- anova.gam(modelo.Old.J.pm10.lag0,modelo.Old.J.pm10.lag1,modelo.Old.J.pm10.lag2,modelo.Old.J.pm10.lag3, 
                                      modelo.Old.J.pm10.lag4,modelo.Old.J.pm10.lag5,modelo.Old.J.pm10.lag6,
                                      modelo.Old.J.pm10.lag7)

anova.gam.mod.Old.J.pm10

anova.mod.Old.J.pm10 <-anova(modelo.Old.J.pm10.lag0,modelo.Old.J.pm10.lag1,modelo.Old.J.pm10.lag2,modelo.Old.J.pm10.lag3, 
                             modelo.Old.J.pm10.lag4,modelo.Old.J.pm10.lag5,modelo.Old.J.pm10.lag6,
                             modelo.Old.J.pm10.lag7)

anova.mod.Old.J.pm10


################# idosos CID J #######################
##################### so2 #############################
########################################################

#################### Lag 0 #############################
modelo.Old.J.so2.lag0 <- mgcv::gam(SIH_GV_J_Old ~ so2.gv + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.so2.lag0) 

residuo.Old.J.so2.lag0 <- residuals(modelo.Old.J.so2.lag0)

par(mfrow = c(1,4))

plot(residuo.Old.J.so2.lag0)
acf(residuo.Old.J.so2.lag0)
qqnorm(residuo.Old.J.so2.lag0)
hist(residuo.Old.J.so2.lag0)
shapiro.test(residuo.Old.J.so2.lag0)

#################### Lag 1 #############################

modelo.Old.J.so2.lag1 <- mgcv::gam(SIH_GV_J_Old ~ so2.gv.d.1 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.so2.lag1) 

residuo.Old.J.so2.lag1 <- residuals(modelo.Old.J.so2.lag1)

par(mfrow = c(1,4))

plot(residuo.Old.J.so2.lag1)
acf(residuo.Old.J.so2.lag1)
qqnorm(residuo.Old.J.so2.lag1)
hist(residuo.Old.J.so2.lag1)
shapiro.test(residuo.Old.J.so2.lag1)


#################### Lag 2 #############################

modelo.Old.J.so2.lag2 <- mgcv::gam(SIH_GV_J_Old ~ so2.gv.d.2 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.so2.lag2) 

residuo.Old.J.so2.lag2 <- residuals(modelo.Old.J.so2.lag2)

par(mfrow = c(1,4))

plot(residuo.Old.J.so2.lag2)
acf(residuo.Old.J.so2.lag2)
qqnorm(residuo.Old.J.so2.lag2)
hist(residuo.Old.J.so2.lag2)
shapiro.test(residuo.Old.J.so2.lag2)

#################### Lag 3 #############################

modelo.Old.J.so2.lag3 <- mgcv::gam(SIH_GV_J_Old ~ so2.gv.d.3 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.so2.lag3) 

residuo.Old.J.so2.lag3 <- residuals(modelo.Old.J.so2.lag3)

par(mfrow = c(1,4))

plot(residuo.Old.J.so2.lag3)
acf(residuo.Old.J.so2.lag3)
qqnorm(residuo.Old.J.so2.lag3)
hist(residuo.Old.J.so2.lag3)
shapiro.test(residuo.Old.J.so2.lag3)

#################### Lag 4 #############################

modelo.Old.J.so2.lag4 <- mgcv::gam(SIH_GV_J_Old ~ so2.gv.d.4 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.so2.lag4) 

residuo.Old.J.so2.lag4 <- residuals(modelo.Old.J.so2.lag4)

par(mfrow = c(1,4))

plot(residuo.Old.J.so2.lag4)
acf(residuo.Old.J.so2.lag4)
qqnorm(residuo.Old.J.so2.lag4)
hist(residuo.Old.J.so2.lag4)
shapiro.test(residuo.Old.J.so2.lag4)

#################### Lag 5 #############################

modelo.Old.J.so2.lag5 <- mgcv::gam(SIH_GV_J_Old ~ so2.gv.d.5 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.so2.lag5) 

residuo.Old.J.so2.lag5 <- residuals(modelo.Old.J.so2.lag5)

par(mfrow = c(1,4))

plot(residuo.Old.J.so2.lag5)
acf(residuo.Old.J.so2.lag5)
qqnorm(residuo.Old.J.so2.lag5)
hist(residuo.Old.J.so2.lag5)
shapiro.test(residuo.Old.J.so2.lag5)

#################### Lag 6 #############################

modelo.Old.J.so2.lag6 <- mgcv::gam(SIH_GV_J_Old ~ so2.gv.d.6 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.so2.lag6) 

residuo.Old.J.so2.lag6 <- residuals(modelo.Old.J.so2.lag6)

par(mfrow = c(1,4))

plot(residuo.Old.J.so2.lag6)
acf(residuo.Old.J.so2.lag6)
qqnorm(residuo.Old.J.so2.lag6)
hist(residuo.Old.J.so2.lag6)
shapiro.test(residuo.Old.J.so2.lag6)


#################### Lag 7 #############################

modelo.Old.J.so2.lag7 <- mgcv::gam(SIH_GV_J_Old ~ so2.gv.d.7 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.so2.lag7) 

residuo.Old.J.so2.lag7 <- residuals(modelo.Old.J.so2.lag7)

par(mfrow = c(1,4))

plot(residuo.Old.J.so2.lag7)
acf(residuo.Old.J.so2.lag7)
qqnorm(residuo.Old.J.so2.lag7)
hist(residuo.Old.J.so2.lag7)
shapiro.test(residuo.Old.J.so2.lag7)


################# idosos CID J #######################
################## AIC E ANOVA #########################
#################### so2 ##############################

library(bbmle)

AIC.mod.Old.J.so2 <- AICctab(modelo.Old.J.so2.lag0,modelo.Old.J.so2.lag1,modelo.Old.J.so2.lag2,modelo.Old.J.so2.lag3, 
                             modelo.Old.J.so2.lag4,modelo.Old.J.so2.lag5,modelo.Old.J.so2.lag6,
                             modelo.Old.J.so2.lag7,
                             base = T, weights=T)
AIC.mod.Old.J.so2 

anova.gam.mod.Old.J.so2 <- anova.gam(modelo.Old.J.so2.lag0,modelo.Old.J.so2.lag1,modelo.Old.J.so2.lag2,modelo.Old.J.so2.lag3, 
                                     modelo.Old.J.so2.lag4,modelo.Old.J.so2.lag5,modelo.Old.J.so2.lag6,
                                     modelo.Old.J.so2.lag7)

anova.gam.mod.Old.J.so2

anova.mod.Old.J.so2 <-anova(modelo.Old.J.so2.lag0,modelo.Old.J.so2.lag1,modelo.Old.J.so2.lag2,modelo.Old.J.so2.lag3, 
                            modelo.Old.J.so2.lag4,modelo.Old.J.so2.lag5,modelo.Old.J.so2.lag6,
                            modelo.Old.J.so2.lag7)

anova.mod.Old.J.so2

################# idosos CID J #######################
##################### no2 #############################
########################################################

#################### Lag 0 #############################
modelo.Old.J.no2.lag0 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.no2.lag0) 

residuo.Old.J.no2.lag0 <- residuals(modelo.Old.J.no2.lag0)

par(mfrow = c(1,4))

plot(residuo.Old.J.no2.lag0)
acf(residuo.Old.J.no2.lag0)
qqnorm(residuo.Old.J.no2.lag0)
hist(residuo.Old.J.no2.lag0)
shapiro.test(residuo.Old.J.no2.lag0)

#################### Lag 1 #############################

modelo.Old.J.no2.lag1 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.1 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.no2.lag1) 

residuo.Old.J.no2.lag1 <- residuals(modelo.Old.J.no2.lag1)

par(mfrow = c(1,4))

plot(residuo.Old.J.no2.lag1)
acf(residuo.Old.J.no2.lag1)
qqnorm(residuo.Old.J.no2.lag1)
hist(residuo.Old.J.no2.lag1)
shapiro.test(residuo.Old.J.no2.lag1)


#################### Lag 2 #############################

modelo.Old.J.no2.lag2 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.2 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.no2.lag2) 

residuo.Old.J.no2.lag2 <- residuals(modelo.Old.J.no2.lag2)

par(mfrow = c(1,4))

plot(residuo.Old.J.no2.lag2)
acf(residuo.Old.J.no2.lag2)
qqnorm(residuo.Old.J.no2.lag2)
hist(residuo.Old.J.no2.lag2)
shapiro.test(residuo.Old.J.no2.lag2)

#################### Lag 3 #############################

modelo.Old.J.no2.lag3 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.3 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.no2.lag3) 

residuo.Old.J.no2.lag3 <- residuals(modelo.Old.J.no2.lag3)

par(mfrow = c(1,4))

plot(residuo.Old.J.no2.lag3)
acf(residuo.Old.J.no2.lag3)
qqnorm(residuo.Old.J.no2.lag3)
hist(residuo.Old.J.no2.lag3)
shapiro.test(residuo.Old.J.no2.lag3)

#################### Lag 4 #############################

modelo.Old.J.no2.lag4 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.4 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.no2.lag4) 

residuo.Old.J.no2.lag4 <- residuals(modelo.Old.J.no2.lag4)

par(mfrow = c(1,4))

plot(residuo.Old.J.no2.lag4)
acf(residuo.Old.J.no2.lag4)
qqnorm(residuo.Old.J.no2.lag4)
hist(residuo.Old.J.no2.lag4)
shapiro.test(residuo.Old.J.no2.lag4)

#################### Lag 5 #############################

modelo.Old.J.no2.lag5 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.5 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.no2.lag5) 

residuo.Old.J.no2.lag5 <- residuals(modelo.Old.J.no2.lag5)

par(mfrow = c(1,4))

plot(residuo.Old.J.no2.lag5)
acf(residuo.Old.J.no2.lag5)
qqnorm(residuo.Old.J.no2.lag5)
hist(residuo.Old.J.no2.lag5)
shapiro.test(residuo.Old.J.no2.lag5)

#################### Lag 6 #############################

modelo.Old.J.no2.lag6 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.6 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.no2.lag6) 

residuo.Old.J.no2.lag6 <- residuals(modelo.Old.J.no2.lag6)

par(mfrow = c(1,4))

plot(residuo.Old.J.no2.lag6)
acf(residuo.Old.J.no2.lag6)
qqnorm(residuo.Old.J.no2.lag6)
hist(residuo.Old.J.no2.lag6)
shapiro.test(residuo.Old.J.no2.lag6)


#################### Lag 7 #############################

modelo.Old.J.no2.lag7 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.7 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.no2.lag7) 

residuo.Old.J.no2.lag7 <- residuals(modelo.Old.J.no2.lag7)

par(mfrow = c(1,4))

plot(residuo.Old.J.no2.lag7)
acf(residuo.Old.J.no2.lag7)
qqnorm(residuo.Old.J.no2.lag7)
hist(residuo.Old.J.no2.lag7)
shapiro.test(residuo.Old.J.no2.lag7)


################# idosos CID J #######################
################## AIC E ANOVA #########################
#################### no2 ##############################

library(bbmle)

AIC.mod.Old.J.no2 <- AICctab(modelo.Old.J.no2.lag0,modelo.Old.J.no2.lag1,modelo.Old.J.no2.lag2,modelo.Old.J.no2.lag3, 
                             modelo.Old.J.no2.lag4,modelo.Old.J.no2.lag5,modelo.Old.J.no2.lag6,
                             modelo.Old.J.no2.lag7,
                             base = T, weights=T)
AIC.mod.Old.J.no2 

anova.gam.mod.Old.J.no2 <- anova.gam(modelo.Old.J.no2.lag0,modelo.Old.J.no2.lag1,modelo.Old.J.no2.lag2,modelo.Old.J.no2.lag3, 
                                     modelo.Old.J.no2.lag4,modelo.Old.J.no2.lag5,modelo.Old.J.no2.lag6,
                                     modelo.Old.J.no2.lag7)

anova.gam.mod.Old.J.no2

anova.mod.Old.J.no2 <-anova(modelo.Old.J.no2.lag0,modelo.Old.J.no2.lag1,modelo.Old.J.no2.lag2,modelo.Old.J.no2.lag3, 
                            modelo.Old.J.no2.lag4,modelo.Old.J.no2.lag5,modelo.Old.J.no2.lag6,
                            modelo.Old.J.no2.lag7)

anova.mod.Old.J.no2

################# idosos CID J #######################
##################### co #############################
########################################################

#################### Lag 0 #############################
modelo.Old.J.co.lag0 <- mgcv::gam(SIH_GV_J_Old ~ co.gv + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.co.lag0) 

residuo.Old.J.co.lag0 <- residuals(modelo.Old.J.co.lag0)

par(mfrow = c(1,4))

plot(residuo.Old.J.co.lag0)
acf(residuo.Old.J.co.lag0)
qqnorm(residuo.Old.J.co.lag0)
hist(residuo.Old.J.co.lag0)
shapiro.test(residuo.Old.J.co.lag0)

#################### Lag 1 #############################

modelo.Old.J.co.lag1 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.1 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.co.lag1) 

residuo.Old.J.co.lag1 <- residuals(modelo.Old.J.co.lag1)

par(mfrow = c(1,4))

plot(residuo.Old.J.co.lag1)
acf(residuo.Old.J.co.lag1)
qqnorm(residuo.Old.J.co.lag1)
hist(residuo.Old.J.co.lag1)
shapiro.test(residuo.Old.J.co.lag1)


#################### Lag 2 #############################

modelo.Old.J.co.lag2 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.2 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.co.lag2) 

residuo.Old.J.co.lag2 <- residuals(modelo.Old.J.co.lag2)

par(mfrow = c(1,4))

plot(residuo.Old.J.co.lag2)
acf(residuo.Old.J.co.lag2)
qqnorm(residuo.Old.J.co.lag2)
hist(residuo.Old.J.co.lag2)
shapiro.test(residuo.Old.J.co.lag2)

#################### Lag 3 #############################

modelo.Old.J.co.lag3 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.3 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.co.lag3) 

residuo.Old.J.co.lag3 <- residuals(modelo.Old.J.co.lag3)

par(mfrow = c(1,4))

plot(residuo.Old.J.co.lag3)
acf(residuo.Old.J.co.lag3)
qqnorm(residuo.Old.J.co.lag3)
hist(residuo.Old.J.co.lag3)
shapiro.test(residuo.Old.J.co.lag3)

#################### Lag 4 #############################

modelo.Old.J.co.lag4 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.4 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.co.lag4) 

residuo.Old.J.co.lag4 <- residuals(modelo.Old.J.co.lag4)

par(mfrow = c(1,4))

plot(residuo.Old.J.co.lag4)
acf(residuo.Old.J.co.lag4)
qqnorm(residuo.Old.J.co.lag4)
hist(residuo.Old.J.co.lag4)
shapiro.test(residuo.Old.J.co.lag4)

#################### Lag 5 #############################

modelo.Old.J.co.lag5 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.5 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.co.lag5) 

residuo.Old.J.co.lag5 <- residuals(modelo.Old.J.co.lag5)

par(mfrow = c(1,4))

plot(residuo.Old.J.co.lag5)
acf(residuo.Old.J.co.lag5)
qqnorm(residuo.Old.J.co.lag5)
hist(residuo.Old.J.co.lag5)
shapiro.test(residuo.Old.J.co.lag5)

#################### Lag 6 #############################

modelo.Old.J.co.lag6 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.6 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.co.lag6) 

residuo.Old.J.co.lag6 <- residuals(modelo.Old.J.co.lag6)

par(mfrow = c(1,4))

plot(residuo.Old.J.co.lag6)
acf(residuo.Old.J.co.lag6)
qqnorm(residuo.Old.J.co.lag6)
hist(residuo.Old.J.co.lag6)
shapiro.test(residuo.Old.J.co.lag6)


#################### Lag 7 #############################

modelo.Old.J.co.lag7 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.7 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.co.lag7) 

residuo.Old.J.co.lag7 <- residuals(modelo.Old.J.co.lag7)

par(mfrow = c(1,4))

plot(residuo.Old.J.co.lag7)
acf(residuo.Old.J.co.lag7)
qqnorm(residuo.Old.J.co.lag7)
hist(residuo.Old.J.co.lag7)
shapiro.test(residuo.Old.J.co.lag7)


################# idosos CID J #######################
################## AIC E ANOVA #########################
#################### co ##############################

library(bbmle)

AIC.mod.Old.J.co <- AICctab(modelo.Old.J.co.lag0,modelo.Old.J.co.lag1,modelo.Old.J.co.lag2,modelo.Old.J.co.lag3, 
                            modelo.Old.J.co.lag4,modelo.Old.J.co.lag5,modelo.Old.J.co.lag6,
                            modelo.Old.J.co.lag7,
                            base = T, weights=T)
AIC.mod.Old.J.co 

anova.gam.mod.Old.J.co <- anova.gam(modelo.Old.J.co.lag0,modelo.Old.J.co.lag1,modelo.Old.J.co.lag2,modelo.Old.J.co.lag3, 
                                    modelo.Old.J.co.lag4,modelo.Old.J.co.lag5,modelo.Old.J.co.lag6,
                                    modelo.Old.J.co.lag7)

anova.gam.mod.Old.J.co

anova.mod.Old.J.co <-anova(modelo.Old.J.co.lag0,modelo.Old.J.co.lag1,modelo.Old.J.co.lag2,modelo.Old.J.co.lag3, 
                           modelo.Old.J.co.lag4,modelo.Old.J.co.lag5,modelo.Old.J.co.lag6,
                           modelo.Old.J.co.lag7)

anova.mod.Old.J.co

################# idosos CID J #######################
##################### o3 #############################
########################################################

#################### Lag 0 #############################
modelo.Old.J.o3.lag0 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.o3.lag0) 

residuo.Old.J.o3.lag0 <- residuals(modelo.Old.J.o3.lag0)

par(mfrow = c(1,4))

plot(residuo.Old.J.o3.lag0)
acf(residuo.Old.J.o3.lag0)
qqnorm(residuo.Old.J.o3.lag0)
hist(residuo.Old.J.o3.lag0)
shapiro.test(residuo.Old.J.o3.lag0)

#################### Lag 1 #############################

modelo.Old.J.o3.lag1 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.1 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.o3.lag1) 

residuo.Old.J.o3.lag1 <- residuals(modelo.Old.J.o3.lag1)

par(mfrow = c(1,4))

plot(residuo.Old.J.o3.lag1)
acf(residuo.Old.J.o3.lag1)
qqnorm(residuo.Old.J.o3.lag1)
hist(residuo.Old.J.o3.lag1)
shapiro.test(residuo.Old.J.o3.lag1)


#################### Lag 2 #############################

modelo.Old.J.o3.lag2 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.2 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.o3.lag2) 

residuo.Old.J.o3.lag2 <- residuals(modelo.Old.J.o3.lag2)

par(mfrow = c(1,4))

plot(residuo.Old.J.o3.lag2)
acf(residuo.Old.J.o3.lag2)
qqnorm(residuo.Old.J.o3.lag2)
hist(residuo.Old.J.o3.lag2)
shapiro.test(residuo.Old.J.o3.lag2)

#################### Lag 3 #############################

modelo.Old.J.o3.lag3 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.3 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.o3.lag3) 

residuo.Old.J.o3.lag3 <- residuals(modelo.Old.J.o3.lag3)

par(mfrow = c(1,4))

plot(residuo.Old.J.o3.lag3)
acf(residuo.Old.J.o3.lag3)
qqnorm(residuo.Old.J.o3.lag3)
hist(residuo.Old.J.o3.lag3)
shapiro.test(residuo.Old.J.o3.lag3)

#################### Lag 4 #############################

modelo.Old.J.o3.lag4 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.4 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.o3.lag4) 

residuo.Old.J.o3.lag4 <- residuals(modelo.Old.J.o3.lag4)

par(mfrow = c(1,4))

plot(residuo.Old.J.o3.lag4)
acf(residuo.Old.J.o3.lag4)
qqnorm(residuo.Old.J.o3.lag4)
hist(residuo.Old.J.o3.lag4)
shapiro.test(residuo.Old.J.o3.lag4)

#################### Lag 5 #############################

modelo.Old.J.o3.lag5 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.5 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.o3.lag5) 

residuo.Old.J.o3.lag5 <- residuals(modelo.Old.J.o3.lag5)

par(mfrow = c(1,4))

plot(residuo.Old.J.o3.lag5)
acf(residuo.Old.J.o3.lag5)
qqnorm(residuo.Old.J.o3.lag5)
hist(residuo.Old.J.o3.lag5)
shapiro.test(residuo.Old.J.o3.lag5)

#################### Lag 6 #############################

modelo.Old.J.o3.lag6 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.6 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.o3.lag6) 

residuo.Old.J.o3.lag6 <- residuals(modelo.Old.J.o3.lag6)

par(mfrow = c(1,4))

plot(residuo.Old.J.o3.lag6)
acf(residuo.Old.J.o3.lag6)
qqnorm(residuo.Old.J.o3.lag6)
hist(residuo.Old.J.o3.lag6)
shapiro.test(residuo.Old.J.o3.lag6)


#################### Lag 7 #############################

modelo.Old.J.o3.lag7 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.7 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.Old.J.o3.lag7) 

residuo.Old.J.o3.lag7 <- residuals(modelo.Old.J.o3.lag7)

par(mfrow = c(1,4))

plot(residuo.Old.J.o3.lag7)
acf(residuo.Old.J.o3.lag7)
qqnorm(residuo.Old.J.o3.lag7)
hist(residuo.Old.J.o3.lag7)
shapiro.test(residuo.Old.J.o3.lag7)


################# idosos CID J #######################
################## AIC E ANOVA #########################
#################### o3 ##############################

library(bbmle)

AIC.mod.Old.J.o3 <- AICctab(modelo.Old.J.o3.lag0,modelo.Old.J.o3.lag1,modelo.Old.J.o3.lag2,modelo.Old.J.o3.lag3, 
                            modelo.Old.J.o3.lag4,modelo.Old.J.o3.lag5,modelo.Old.J.o3.lag6,
                            modelo.Old.J.o3.lag7,
                            base = T, weights=T)
AIC.mod.Old.J.o3 

anova.gam.mod.Old.J.o3 <- anova.gam(modelo.Old.J.o3.lag0,modelo.Old.J.o3.lag1,modelo.Old.J.o3.lag2,modelo.Old.J.o3.lag3, 
                                    modelo.Old.J.o3.lag4,modelo.Old.J.o3.lag5,modelo.Old.J.o3.lag6,
                                    modelo.Old.J.o3.lag7)

anova.gam.mod.Old.J.o3

anova.mod.Old.J.o3 <-anova(modelo.Old.J.o3.lag0,modelo.Old.J.o3.lag1,modelo.Old.J.o3.lag2,modelo.Old.J.o3.lag3, 
                           modelo.Old.J.o3.lag4,modelo.Old.J.o3.lag5,modelo.Old.J.o3.lag6,
                           modelo.Old.J.o3.lag7)

anova.mod.Old.J.o3
