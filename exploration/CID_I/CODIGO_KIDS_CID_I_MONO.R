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

################# crian�as CID I #######################
##################### Pm25 #############################
########################################################

#################### Lag 0 #############################
modelo.kid.I.pm25.lag0 <- mgcv::gam(SIH_GV_I_Kid ~ pm25.gv + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.pm25.lag0) 

residuo.kid.I.pm25.lag0 <- residuals(modelo.kid.I.pm25.lag0)

par(mfrow = c(1,4))

plot(residuo.kid.I.pm25.lag0)
acf(residuo.kid.I.pm25.lag0)
qqnorm(residuo.kid.I.pm25.lag0)
hist(residuo.kid.I.pm25.lag0)
shapiro.test(residuo.kid.I.pm25.lag0)

#################### Lag 1 #############################

modelo.kid.I.pm25.lag1 <- mgcv::gam(SIH_GV_I_Kid ~ pm25.gv.d.1 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.pm25.lag1) 

residuo.kid.I.pm25.lag1 <- residuals(modelo.kid.I.pm25.lag1)

par(mfrow = c(1,4))

plot(residuo.kid.I.pm25.lag1)
acf(residuo.kid.I.pm25.lag1)
qqnorm(residuo.kid.I.pm25.lag1)
hist(residuo.kid.I.pm25.lag1)
shapiro.test(residuo.kid.I.pm25.lag1)


#################### Lag 2 #############################

modelo.kid.I.pm25.lag2 <- mgcv::gam(SIH_GV_I_Kid ~ pm25.gv.d.2 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.pm25.lag2) 

residuo.kid.I.pm25.lag2 <- residuals(modelo.kid.I.pm25.lag2)

par(mfrow = c(1,4))

plot(residuo.kid.I.pm25.lag2)
acf(residuo.kid.I.pm25.lag2)
qqnorm(residuo.kid.I.pm25.lag2)
hist(residuo.kid.I.pm25.lag2)
shapiro.test(residuo.kid.I.pm25.lag2)

#################### Lag 3 #############################

modelo.kid.I.pm25.lag3 <- mgcv::gam(SIH_GV_I_Kid ~ pm25.gv.d.3 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.pm25.lag3) 

residuo.kid.I.pm25.lag3 <- residuals(modelo.kid.I.pm25.lag3)

par(mfrow = c(1,4))

plot(residuo.kid.I.pm25.lag3)
acf(residuo.kid.I.pm25.lag3)
qqnorm(residuo.kid.I.pm25.lag3)
hist(residuo.kid.I.pm25.lag3)
shapiro.test(residuo.kid.I.pm25.lag3)

#################### Lag 4 #############################

modelo.kid.I.pm25.lag4 <- mgcv::gam(SIH_GV_I_Kid ~ pm25.gv.d.4 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.pm25.lag4) 

residuo.kid.I.pm25.lag4 <- residuals(modelo.kid.I.pm25.lag4)

par(mfrow = c(1,4))

plot(residuo.kid.I.pm25.lag4)
acf(residuo.kid.I.pm25.lag4)
qqnorm(residuo.kid.I.pm25.lag4)
hist(residuo.kid.I.pm25.lag4)
shapiro.test(residuo.kid.I.pm25.lag4)

#################### Lag 5 #############################

modelo.kid.I.pm25.lag5 <- mgcv::gam(SIH_GV_I_Kid ~ pm25.gv.d.5 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.pm25.lag5) 

residuo.kid.I.pm25.lag5 <- residuals(modelo.kid.I.pm25.lag5)

par(mfrow = c(1,4))

plot(residuo.kid.I.pm25.lag5)
acf(residuo.kid.I.pm25.lag5)
qqnorm(residuo.kid.I.pm25.lag5)
hist(residuo.kid.I.pm25.lag5)
shapiro.test(residuo.kid.I.pm25.lag5)

#################### Lag 6 #############################

modelo.kid.I.pm25.lag6 <- mgcv::gam(SIH_GV_I_Kid ~ pm25.gv.d.6 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.pm25.lag6) 

residuo.kid.I.pm25.lag6 <- residuals(modelo.kid.I.pm25.lag6)

par(mfrow = c(1,4))

plot(residuo.kid.I.pm25.lag6)
acf(residuo.kid.I.pm25.lag6)
qqnorm(residuo.kid.I.pm25.lag6)
hist(residuo.kid.I.pm25.lag6)
shapiro.test(residuo.kid.I.pm25.lag6)


#################### Lag 7 #############################

modelo.kid.I.pm25.lag7 <- mgcv::gam(SIH_GV_I_Kid ~ pm25.gv.d.7 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.pm25.lag7) 

residuo.kid.I.pm25.lag7 <- residuals(modelo.kid.I.pm25.lag7)

par(mfrow = c(1,4))

plot(residuo.kid.I.pm25.lag7)
acf(residuo.kid.I.pm25.lag7)
qqnorm(residuo.kid.I.pm25.lag7)
hist(residuo.kid.I.pm25.lag7)
shapiro.test(residuo.kid.I.pm25.lag7)


################# crian�as CID I #######################
################## AIC E ANOVA #########################
#################### pm25 ##############################

library(bbmle)

AIC.mod.kid.I.pm25 <- AICctab(modelo.kid.I.pm25.lag0,modelo.kid.I.pm25.lag1,modelo.kid.I.pm25.lag2,modelo.kid.I.pm25.lag3, 
                              modelo.kid.I.pm25.lag4,modelo.kid.I.pm25.lag5,modelo.kid.I.pm25.lag6,
                              modelo.kid.I.pm25.lag7,
                              base = T, weights=T)
AIC.mod.kid.I.pm25 

anova.gam.mod.kid.I.pm25 <- anova.gam(modelo.kid.I.pm25.lag0,modelo.kid.I.pm25.lag1,modelo.kid.I.pm25.lag2,modelo.kid.I.pm25.lag3, 
                                      modelo.kid.I.pm25.lag4,modelo.kid.I.pm25.lag5,modelo.kid.I.pm25.lag6,
                                      modelo.kid.I.pm25.lag7)

anova.gam.mod.kid.I.pm25

anova.mod.kid.I.pm25 <-anova(modelo.kid.I.pm25.lag0,modelo.kid.I.pm25.lag1,modelo.kid.I.pm25.lag2,modelo.kid.I.pm25.lag3, 
                             modelo.kid.I.pm25.lag4,modelo.kid.I.pm25.lag5,modelo.kid.I.pm25.lag6,
                             modelo.kid.I.pm25.lag7)

anova.mod.kid.I.pm25



################# crian�as CID I #######################
##################### Pm10 #############################
########################################################

#################### Lag 0 #############################
modelo.kid.I.pm10.lag0 <- mgcv::gam(SIH_GV_I_Kid ~ pm10.gv + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.pm10.lag0) 

residuo.kid.I.pm10.lag0 <- residuals(modelo.kid.I.pm10.lag0)

par(mfrow = c(1,4))

plot(residuo.kid.I.pm10.lag0)
acf(residuo.kid.I.pm10.lag0)
qqnorm(residuo.kid.I.pm10.lag0)
hist(residuo.kid.I.pm10.lag0)
shapiro.test(residuo.kid.I.pm10.lag0)

#################### Lag 1 #############################

modelo.kid.I.pm10.lag1 <- mgcv::gam(SIH_GV_I_Kid ~ pm10.gv.d.1 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.pm10.lag1) 

residuo.kid.I.pm10.lag1 <- residuals(modelo.kid.I.pm10.lag1)

par(mfrow = c(1,4))

plot(residuo.kid.I.pm10.lag1)
acf(residuo.kid.I.pm10.lag1)
qqnorm(residuo.kid.I.pm10.lag1)
hist(residuo.kid.I.pm10.lag1)
shapiro.test(residuo.kid.I.pm10.lag1)


#################### Lag 2 #############################

modelo.kid.I.pm10.lag2 <- mgcv::gam(SIH_GV_I_Kid ~ pm10.gv.d.2 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.pm10.lag2) 

residuo.kid.I.pm10.lag2 <- residuals(modelo.kid.I.pm10.lag2)

par(mfrow = c(1,4))

plot(residuo.kid.I.pm10.lag2)
acf(residuo.kid.I.pm10.lag2)
qqnorm(residuo.kid.I.pm10.lag2)
hist(residuo.kid.I.pm10.lag2)
shapiro.test(residuo.kid.I.pm10.lag2)

#################### Lag 3 #############################

modelo.kid.I.pm10.lag3 <- mgcv::gam(SIH_GV_I_Kid ~ pm10.gv.d.3 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.pm10.lag3) 

residuo.kid.I.pm10.lag3 <- residuals(modelo.kid.I.pm10.lag3)

par(mfrow = c(1,4))

plot(residuo.kid.I.pm10.lag3)
acf(residuo.kid.I.pm10.lag3)
qqnorm(residuo.kid.I.pm10.lag3)
hist(residuo.kid.I.pm10.lag3)
shapiro.test(residuo.kid.I.pm10.lag3)

#################### Lag 4 #############################

modelo.kid.I.pm10.lag4 <- mgcv::gam(SIH_GV_I_Kid ~ pm10.gv.d.4 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.pm10.lag4) 

residuo.kid.I.pm10.lag4 <- residuals(modelo.kid.I.pm10.lag4)

par(mfrow = c(1,4))

plot(residuo.kid.I.pm10.lag4)
acf(residuo.kid.I.pm10.lag4)
qqnorm(residuo.kid.I.pm10.lag4)
hist(residuo.kid.I.pm10.lag4)
shapiro.test(residuo.kid.I.pm10.lag4)

#################### Lag 5 #############################

modelo.kid.I.pm10.lag5 <- mgcv::gam(SIH_GV_I_Kid ~ pm10.gv.d.5 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.pm10.lag5) 

residuo.kid.I.pm10.lag5 <- residuals(modelo.kid.I.pm10.lag5)

par(mfrow = c(1,4))

plot(residuo.kid.I.pm10.lag5)
acf(residuo.kid.I.pm10.lag5)
qqnorm(residuo.kid.I.pm10.lag5)
hist(residuo.kid.I.pm10.lag5)
shapiro.test(residuo.kid.I.pm10.lag5)

#################### Lag 6 #############################

modelo.kid.I.pm10.lag6 <- mgcv::gam(SIH_GV_I_Kid ~ pm10.gv.d.6 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.pm10.lag6) 

residuo.kid.I.pm10.lag6 <- residuals(modelo.kid.I.pm10.lag6)

par(mfrow = c(1,4))

plot(residuo.kid.I.pm10.lag6)
acf(residuo.kid.I.pm10.lag6)
qqnorm(residuo.kid.I.pm10.lag6)
hist(residuo.kid.I.pm10.lag6)
shapiro.test(residuo.kid.I.pm10.lag6)


#################### Lag 7 #############################

modelo.kid.I.pm10.lag7 <- mgcv::gam(SIH_GV_I_Kid ~ pm10.gv.d.7 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.pm10.lag7) 

residuo.kid.I.pm10.lag7 <- residuals(modelo.kid.I.pm10.lag7)

par(mfrow = c(1,4))

plot(residuo.kid.I.pm10.lag7)
acf(residuo.kid.I.pm10.lag7)
qqnorm(residuo.kid.I.pm10.lag7)
hist(residuo.kid.I.pm10.lag7)
shapiro.test(residuo.kid.I.pm10.lag7)


################# crian�as CID I #######################
################## AIC E ANOVA #########################
#################### pm10 ##############################

library(bbmle)

AIC.mod.kid.I.pm10 <- AICctab(modelo.kid.I.pm10.lag0,modelo.kid.I.pm10.lag1,modelo.kid.I.pm10.lag2,modelo.kid.I.pm10.lag3, 
                              modelo.kid.I.pm10.lag4,modelo.kid.I.pm10.lag5,modelo.kid.I.pm10.lag6,
                              modelo.kid.I.pm10.lag7,
                              base = T, weights=T)
AIC.mod.kid.I.pm10 

anova.gam.mod.kid.I.pm10 <- anova.gam(modelo.kid.I.pm10.lag0,modelo.kid.I.pm10.lag1,modelo.kid.I.pm10.lag2,modelo.kid.I.pm10.lag3, 
                                      modelo.kid.I.pm10.lag4,modelo.kid.I.pm10.lag5,modelo.kid.I.pm10.lag6,
                                      modelo.kid.I.pm10.lag7)

anova.gam.mod.kid.I.pm10

anova.mod.kid.I.pm10 <-anova(modelo.kid.I.pm10.lag0,modelo.kid.I.pm10.lag1,modelo.kid.I.pm10.lag2,modelo.kid.I.pm10.lag3, 
                             modelo.kid.I.pm10.lag4,modelo.kid.I.pm10.lag5,modelo.kid.I.pm10.lag6,
                             modelo.kid.I.pm10.lag7)

anova.mod.kid.I.pm10


################# crian�as CID I #######################
##################### so2 #############################
########################################################

#################### Lag 0 #############################
modelo.kid.I.so2.lag0 <- mgcv::gam(SIH_GV_I_Kid ~ so2.gv + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.so2.lag0) 

residuo.kid.I.so2.lag0 <- residuals(modelo.kid.I.so2.lag0)

par(mfrow = c(1,4))

plot(residuo.kid.I.so2.lag0)
acf(residuo.kid.I.so2.lag0)
qqnorm(residuo.kid.I.so2.lag0)
hist(residuo.kid.I.so2.lag0)
shapiro.test(residuo.kid.I.so2.lag0)

#################### Lag 1 #############################

modelo.kid.I.so2.lag1 <- mgcv::gam(SIH_GV_I_Kid ~ so2.gv.d.1 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.so2.lag1) 

residuo.kid.I.so2.lag1 <- residuals(modelo.kid.I.so2.lag1)

par(mfrow = c(1,4))

plot(residuo.kid.I.so2.lag1)
acf(residuo.kid.I.so2.lag1)
qqnorm(residuo.kid.I.so2.lag1)
hist(residuo.kid.I.so2.lag1)
shapiro.test(residuo.kid.I.so2.lag1)


#################### Lag 2 #############################

modelo.kid.I.so2.lag2 <- mgcv::gam(SIH_GV_I_Kid ~ so2.gv.d.2 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.so2.lag2) 

residuo.kid.I.so2.lag2 <- residuals(modelo.kid.I.so2.lag2)

par(mfrow = c(1,4))

plot(residuo.kid.I.so2.lag2)
acf(residuo.kid.I.so2.lag2)
qqnorm(residuo.kid.I.so2.lag2)
hist(residuo.kid.I.so2.lag2)
shapiro.test(residuo.kid.I.so2.lag2)

#################### Lag 3 #############################

modelo.kid.I.so2.lag3 <- mgcv::gam(SIH_GV_I_Kid ~ so2.gv.d.3 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.so2.lag3) 

residuo.kid.I.so2.lag3 <- residuals(modelo.kid.I.so2.lag3)

par(mfrow = c(1,4))

plot(residuo.kid.I.so2.lag3)
acf(residuo.kid.I.so2.lag3)
qqnorm(residuo.kid.I.so2.lag3)
hist(residuo.kid.I.so2.lag3)
shapiro.test(residuo.kid.I.so2.lag3)

#################### Lag 4 #############################

modelo.kid.I.so2.lag4 <- mgcv::gam(SIH_GV_I_Kid ~ so2.gv.d.4 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.so2.lag4) 

residuo.kid.I.so2.lag4 <- residuals(modelo.kid.I.so2.lag4)

par(mfrow = c(1,4))

plot(residuo.kid.I.so2.lag4)
acf(residuo.kid.I.so2.lag4)
qqnorm(residuo.kid.I.so2.lag4)
hist(residuo.kid.I.so2.lag4)
shapiro.test(residuo.kid.I.so2.lag4)

#################### Lag 5 #############################

modelo.kid.I.so2.lag5 <- mgcv::gam(SIH_GV_I_Kid ~ so2.gv.d.5 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.so2.lag5) 

residuo.kid.I.so2.lag5 <- residuals(modelo.kid.I.so2.lag5)

par(mfrow = c(1,4))

plot(residuo.kid.I.so2.lag5)
acf(residuo.kid.I.so2.lag5)
qqnorm(residuo.kid.I.so2.lag5)
hist(residuo.kid.I.so2.lag5)
shapiro.test(residuo.kid.I.so2.lag5)

#################### Lag 6 #############################

modelo.kid.I.so2.lag6 <- mgcv::gam(SIH_GV_I_Kid ~ so2.gv.d.6 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.so2.lag6) 

residuo.kid.I.so2.lag6 <- residuals(modelo.kid.I.so2.lag6)

par(mfrow = c(1,4))

plot(residuo.kid.I.so2.lag6)
acf(residuo.kid.I.so2.lag6)
qqnorm(residuo.kid.I.so2.lag6)
hist(residuo.kid.I.so2.lag6)
shapiro.test(residuo.kid.I.so2.lag6)


#################### Lag 7 #############################

modelo.kid.I.so2.lag7 <- mgcv::gam(SIH_GV_I_Kid ~ so2.gv.d.7 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.so2.lag7) 

residuo.kid.I.so2.lag7 <- residuals(modelo.kid.I.so2.lag7)

par(mfrow = c(1,4))

plot(residuo.kid.I.so2.lag7)
acf(residuo.kid.I.so2.lag7)
qqnorm(residuo.kid.I.so2.lag7)
hist(residuo.kid.I.so2.lag7)
shapiro.test(residuo.kid.I.so2.lag7)


################# crian�as CID I #######################
################## AIC E ANOVA #########################
#################### so2 ##############################

library(bbmle)

AIC.mod.kid.I.so2 <- AICctab(modelo.kid.I.so2.lag0,modelo.kid.I.so2.lag1,modelo.kid.I.so2.lag2,modelo.kid.I.so2.lag3, 
                             modelo.kid.I.so2.lag4,modelo.kid.I.so2.lag5,modelo.kid.I.so2.lag6,
                             modelo.kid.I.so2.lag7,
                             base = T, weights=T)
AIC.mod.kid.I.so2 

anova.gam.mod.kid.I.so2 <- anova.gam(modelo.kid.I.so2.lag0,modelo.kid.I.so2.lag1,modelo.kid.I.so2.lag2,modelo.kid.I.so2.lag3, 
                                     modelo.kid.I.so2.lag4,modelo.kid.I.so2.lag5,modelo.kid.I.so2.lag6,
                                     modelo.kid.I.so2.lag7)

anova.gam.mod.kid.I.so2

anova.mod.kid.I.so2 <-anova(modelo.kid.I.so2.lag0,modelo.kid.I.so2.lag1,modelo.kid.I.so2.lag2,modelo.kid.I.so2.lag3, 
                            modelo.kid.I.so2.lag4,modelo.kid.I.so2.lag5,modelo.kid.I.so2.lag6,
                            modelo.kid.I.so2.lag7)

anova.mod.kid.I.so2

################# crian�as CID I #######################
##################### no2 #############################
########################################################

#################### Lag 0 #############################
modelo.kid.I.no2.lag0 <- mgcv::gam(SIH_GV_I_Kid ~ no2.gv + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.no2.lag0) 

residuo.kid.I.no2.lag0 <- residuals(modelo.kid.I.no2.lag0)

par(mfrow = c(1,4))

plot(residuo.kid.I.no2.lag0)
acf(residuo.kid.I.no2.lag0)
qqnorm(residuo.kid.I.no2.lag0)
hist(residuo.kid.I.no2.lag0)
shapiro.test(residuo.kid.I.no2.lag0)

#################### Lag 1 #############################

modelo.kid.I.no2.lag1 <- mgcv::gam(SIH_GV_I_Kid ~ no2.gv.d.1 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.no2.lag1) 

residuo.kid.I.no2.lag1 <- residuals(modelo.kid.I.no2.lag1)

par(mfrow = c(1,4))

plot(residuo.kid.I.no2.lag1)
acf(residuo.kid.I.no2.lag1)
qqnorm(residuo.kid.I.no2.lag1)
hist(residuo.kid.I.no2.lag1)
shapiro.test(residuo.kid.I.no2.lag1)


#################### Lag 2 #############################

modelo.kid.I.no2.lag2 <- mgcv::gam(SIH_GV_I_Kid ~ no2.gv.d.2 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.no2.lag2) 

residuo.kid.I.no2.lag2 <- residuals(modelo.kid.I.no2.lag2)

par(mfrow = c(1,4))

plot(residuo.kid.I.no2.lag2)
acf(residuo.kid.I.no2.lag2)
qqnorm(residuo.kid.I.no2.lag2)
hist(residuo.kid.I.no2.lag2)
shapiro.test(residuo.kid.I.no2.lag2)

#################### Lag 3 #############################

modelo.kid.I.no2.lag3 <- mgcv::gam(SIH_GV_I_Kid ~ no2.gv.d.3 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.no2.lag3) 

residuo.kid.I.no2.lag3 <- residuals(modelo.kid.I.no2.lag3)

par(mfrow = c(1,4))

plot(residuo.kid.I.no2.lag3)
acf(residuo.kid.I.no2.lag3)
qqnorm(residuo.kid.I.no2.lag3)
hist(residuo.kid.I.no2.lag3)
shapiro.test(residuo.kid.I.no2.lag3)

#################### Lag 4 #############################

modelo.kid.I.no2.lag4 <- mgcv::gam(SIH_GV_I_Kid ~ no2.gv.d.4 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.no2.lag4) 

residuo.kid.I.no2.lag4 <- residuals(modelo.kid.I.no2.lag4)

par(mfrow = c(1,4))

plot(residuo.kid.I.no2.lag4)
acf(residuo.kid.I.no2.lag4)
qqnorm(residuo.kid.I.no2.lag4)
hist(residuo.kid.I.no2.lag4)
shapiro.test(residuo.kid.I.no2.lag4)

#################### Lag 5 #############################

modelo.kid.I.no2.lag5 <- mgcv::gam(SIH_GV_I_Kid ~ no2.gv.d.5 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.no2.lag5) 

residuo.kid.I.no2.lag5 <- residuals(modelo.kid.I.no2.lag5)

par(mfrow = c(1,4))

plot(residuo.kid.I.no2.lag5)
acf(residuo.kid.I.no2.lag5)
qqnorm(residuo.kid.I.no2.lag5)
hist(residuo.kid.I.no2.lag5)
shapiro.test(residuo.kid.I.no2.lag5)

#################### Lag 6 #############################

modelo.kid.I.no2.lag6 <- mgcv::gam(SIH_GV_I_Kid ~ no2.gv.d.6 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.no2.lag6) 

residuo.kid.I.no2.lag6 <- residuals(modelo.kid.I.no2.lag6)

par(mfrow = c(1,4))

plot(residuo.kid.I.no2.lag6)
acf(residuo.kid.I.no2.lag6)
qqnorm(residuo.kid.I.no2.lag6)
hist(residuo.kid.I.no2.lag6)
shapiro.test(residuo.kid.I.no2.lag6)


#################### Lag 7 #############################

modelo.kid.I.no2.lag7 <- mgcv::gam(SIH_GV_I_Kid ~ no2.gv.d.7 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.no2.lag7) 

residuo.kid.I.no2.lag7 <- residuals(modelo.kid.I.no2.lag7)

par(mfrow = c(1,4))

plot(residuo.kid.I.no2.lag7)
acf(residuo.kid.I.no2.lag7)
qqnorm(residuo.kid.I.no2.lag7)
hist(residuo.kid.I.no2.lag7)
shapiro.test(residuo.kid.I.no2.lag7)


################# crian�as CID I #######################
################## AIC E ANOVA #########################
#################### no2 ##############################

library(bbmle)

AIC.mod.kid.I.no2 <- AICctab(modelo.kid.I.no2.lag0,modelo.kid.I.no2.lag1,modelo.kid.I.no2.lag2,modelo.kid.I.no2.lag3, 
                             modelo.kid.I.no2.lag4,modelo.kid.I.no2.lag5,modelo.kid.I.no2.lag6,
                             modelo.kid.I.no2.lag7,
                             base = T, weights=T)
AIC.mod.kid.I.no2 

anova.gam.mod.kid.I.no2 <- anova.gam(modelo.kid.I.no2.lag0,modelo.kid.I.no2.lag1,modelo.kid.I.no2.lag2,modelo.kid.I.no2.lag3, 
                                     modelo.kid.I.no2.lag4,modelo.kid.I.no2.lag5,modelo.kid.I.no2.lag6,
                                     modelo.kid.I.no2.lag7)

anova.gam.mod.kid.I.no2

anova.mod.kid.I.no2 <-anova(modelo.kid.I.no2.lag0,modelo.kid.I.no2.lag1,modelo.kid.I.no2.lag2,modelo.kid.I.no2.lag3, 
                            modelo.kid.I.no2.lag4,modelo.kid.I.no2.lag5,modelo.kid.I.no2.lag6,
                            modelo.kid.I.no2.lag7)

anova.mod.kid.I.no2

################# crian�as CID I #######################
##################### co #############################
########################################################

#################### Lag 0 #############################
modelo.kid.I.co.lag0 <- mgcv::gam(SIH_GV_I_Kid ~ co.gv + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.co.lag0) 

residuo.kid.I.co.lag0 <- residuals(modelo.kid.I.co.lag0)

par(mfrow = c(1,4))

plot(residuo.kid.I.co.lag0)
acf(residuo.kid.I.co.lag0)
qqnorm(residuo.kid.I.co.lag0)
hist(residuo.kid.I.co.lag0)
shapiro.test(residuo.kid.I.co.lag0)

#################### Lag 1 #############################

modelo.kid.I.co.lag1 <- mgcv::gam(SIH_GV_I_Kid ~ co.gv.d.1 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.co.lag1) 

residuo.kid.I.co.lag1 <- residuals(modelo.kid.I.co.lag1)

par(mfrow = c(1,4))

plot(residuo.kid.I.co.lag1)
acf(residuo.kid.I.co.lag1)
qqnorm(residuo.kid.I.co.lag1)
hist(residuo.kid.I.co.lag1)
shapiro.test(residuo.kid.I.co.lag1)


#################### Lag 2 #############################

modelo.kid.I.co.lag2 <- mgcv::gam(SIH_GV_I_Kid ~ co.gv.d.2 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.co.lag2) 

residuo.kid.I.co.lag2 <- residuals(modelo.kid.I.co.lag2)

par(mfrow = c(1,4))

plot(residuo.kid.I.co.lag2)
acf(residuo.kid.I.co.lag2)
qqnorm(residuo.kid.I.co.lag2)
hist(residuo.kid.I.co.lag2)
shapiro.test(residuo.kid.I.co.lag2)

#################### Lag 3 #############################

modelo.kid.I.co.lag3 <- mgcv::gam(SIH_GV_I_Kid ~ co.gv.d.3 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.co.lag3) 

residuo.kid.I.co.lag3 <- residuals(modelo.kid.I.co.lag3)

par(mfrow = c(1,4))

plot(residuo.kid.I.co.lag3)
acf(residuo.kid.I.co.lag3)
qqnorm(residuo.kid.I.co.lag3)
hist(residuo.kid.I.co.lag3)
shapiro.test(residuo.kid.I.co.lag3)

#################### Lag 4 #############################

modelo.kid.I.co.lag4 <- mgcv::gam(SIH_GV_I_Kid ~ co.gv.d.4 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.co.lag4) 

residuo.kid.I.co.lag4 <- residuals(modelo.kid.I.co.lag4)

par(mfrow = c(1,4))

plot(residuo.kid.I.co.lag4)
acf(residuo.kid.I.co.lag4)
qqnorm(residuo.kid.I.co.lag4)
hist(residuo.kid.I.co.lag4)
shapiro.test(residuo.kid.I.co.lag4)

#################### Lag 5 #############################

modelo.kid.I.co.lag5 <- mgcv::gam(SIH_GV_I_Kid ~ co.gv.d.5 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.co.lag5) 

residuo.kid.I.co.lag5 <- residuals(modelo.kid.I.co.lag5)

par(mfrow = c(1,4))

plot(residuo.kid.I.co.lag5)
acf(residuo.kid.I.co.lag5)
qqnorm(residuo.kid.I.co.lag5)
hist(residuo.kid.I.co.lag5)
shapiro.test(residuo.kid.I.co.lag5)

#################### Lag 6 #############################

modelo.kid.I.co.lag6 <- mgcv::gam(SIH_GV_I_Kid ~ co.gv.d.6 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.co.lag6) 

residuo.kid.I.co.lag6 <- residuals(modelo.kid.I.co.lag6)

par(mfrow = c(1,4))

plot(residuo.kid.I.co.lag6)
acf(residuo.kid.I.co.lag6)
qqnorm(residuo.kid.I.co.lag6)
hist(residuo.kid.I.co.lag6)
shapiro.test(residuo.kid.I.co.lag6)


#################### Lag 7 #############################

modelo.kid.I.co.lag7 <- mgcv::gam(SIH_GV_I_Kid ~ co.gv.d.7 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.co.lag7) 

residuo.kid.I.co.lag7 <- residuals(modelo.kid.I.co.lag7)

par(mfrow = c(1,4))

plot(residuo.kid.I.co.lag7)
acf(residuo.kid.I.co.lag7)
qqnorm(residuo.kid.I.co.lag7)
hist(residuo.kid.I.co.lag7)
shapiro.test(residuo.kid.I.co.lag7)


################# crian�as CID I #######################
################## AIC E ANOVA #########################
#################### co ##############################

library(bbmle)

AIC.mod.kid.I.co <- AICctab(modelo.kid.I.co.lag0,modelo.kid.I.co.lag1,modelo.kid.I.co.lag2,modelo.kid.I.co.lag3, 
                            modelo.kid.I.co.lag4,modelo.kid.I.co.lag5,modelo.kid.I.co.lag6,
                            modelo.kid.I.co.lag7,
                            base = T, weights=T)
AIC.mod.kid.I.co 

anova.gam.mod.kid.I.co <- anova.gam(modelo.kid.I.co.lag0,modelo.kid.I.co.lag1,modelo.kid.I.co.lag2,modelo.kid.I.co.lag3, 
                                    modelo.kid.I.co.lag4,modelo.kid.I.co.lag5,modelo.kid.I.co.lag6,
                                    modelo.kid.I.co.lag7)

anova.gam.mod.kid.I.co

anova.mod.kid.I.co <-anova(modelo.kid.I.co.lag0,modelo.kid.I.co.lag1,modelo.kid.I.co.lag2,modelo.kid.I.co.lag3, 
                           modelo.kid.I.co.lag4,modelo.kid.I.co.lag5,modelo.kid.I.co.lag6,
                           modelo.kid.I.co.lag7)

anova.mod.kid.I.co

################# crian�as CID I #######################
##################### o3 #############################
########################################################

#################### Lag 0 #############################
modelo.kid.I.o3.lag0 <- mgcv::gam(SIH_GV_I_Kid ~ o3.gv + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.o3.lag0) 

residuo.kid.I.o3.lag0 <- residuals(modelo.kid.I.o3.lag0)

par(mfrow = c(1,4))

plot(residuo.kid.I.o3.lag0)
acf(residuo.kid.I.o3.lag0)
qqnorm(residuo.kid.I.o3.lag0)
hist(residuo.kid.I.o3.lag0)
shapiro.test(residuo.kid.I.o3.lag0)

#################### Lag 1 #############################

modelo.kid.I.o3.lag1 <- mgcv::gam(SIH_GV_I_Kid ~ o3.gv.d.1 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.o3.lag1) 

residuo.kid.I.o3.lag1 <- residuals(modelo.kid.I.o3.lag1)

par(mfrow = c(1,4))

plot(residuo.kid.I.o3.lag1)
acf(residuo.kid.I.o3.lag1)
qqnorm(residuo.kid.I.o3.lag1)
hist(residuo.kid.I.o3.lag1)
shapiro.test(residuo.kid.I.o3.lag1)


#################### Lag 2 #############################

modelo.kid.I.o3.lag2 <- mgcv::gam(SIH_GV_I_Kid ~ o3.gv.d.2 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.o3.lag2) 

residuo.kid.I.o3.lag2 <- residuals(modelo.kid.I.o3.lag2)

par(mfrow = c(1,4))

plot(residuo.kid.I.o3.lag2)
acf(residuo.kid.I.o3.lag2)
qqnorm(residuo.kid.I.o3.lag2)
hist(residuo.kid.I.o3.lag2)
shapiro.test(residuo.kid.I.o3.lag2)

#################### Lag 3 #############################

modelo.kid.I.o3.lag3 <- mgcv::gam(SIH_GV_I_Kid ~ o3.gv.d.3 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.o3.lag3) 

residuo.kid.I.o3.lag3 <- residuals(modelo.kid.I.o3.lag3)

par(mfrow = c(1,4))

plot(residuo.kid.I.o3.lag3)
acf(residuo.kid.I.o3.lag3)
qqnorm(residuo.kid.I.o3.lag3)
hist(residuo.kid.I.o3.lag3)
shapiro.test(residuo.kid.I.o3.lag3)

#################### Lag 4 #############################

modelo.kid.I.o3.lag4 <- mgcv::gam(SIH_GV_I_Kid ~ o3.gv.d.4 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.o3.lag4) 

residuo.kid.I.o3.lag4 <- residuals(modelo.kid.I.o3.lag4)

par(mfrow = c(1,4))

plot(residuo.kid.I.o3.lag4)
acf(residuo.kid.I.o3.lag4)
qqnorm(residuo.kid.I.o3.lag4)
hist(residuo.kid.I.o3.lag4)
shapiro.test(residuo.kid.I.o3.lag4)

#################### Lag 5 #############################

modelo.kid.I.o3.lag5 <- mgcv::gam(SIH_GV_I_Kid ~ o3.gv.d.5 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.o3.lag5) 

residuo.kid.I.o3.lag5 <- residuals(modelo.kid.I.o3.lag5)

par(mfrow = c(1,4))

plot(residuo.kid.I.o3.lag5)
acf(residuo.kid.I.o3.lag5)
qqnorm(residuo.kid.I.o3.lag5)
hist(residuo.kid.I.o3.lag5)
shapiro.test(residuo.kid.I.o3.lag5)

#################### Lag 6 #############################

modelo.kid.I.o3.lag6 <- mgcv::gam(SIH_GV_I_Kid ~ o3.gv.d.6 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.o3.lag6) 

residuo.kid.I.o3.lag6 <- residuals(modelo.kid.I.o3.lag6)

par(mfrow = c(1,4))

plot(residuo.kid.I.o3.lag6)
acf(residuo.kid.I.o3.lag6)
qqnorm(residuo.kid.I.o3.lag6)
hist(residuo.kid.I.o3.lag6)
shapiro.test(residuo.kid.I.o3.lag6)


#################### Lag 7 #############################

modelo.kid.I.o3.lag7 <- mgcv::gam(SIH_GV_I_Kid ~ o3.gv.d.7 + s(Temp.Ar) + s(Umidade.Ar), data = dados_GV, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.I.o3.lag7) 

residuo.kid.I.o3.lag7 <- residuals(modelo.kid.I.o3.lag7)

par(mfrow = c(1,4))

plot(residuo.kid.I.o3.lag7)
acf(residuo.kid.I.o3.lag7)
qqnorm(residuo.kid.I.o3.lag7)
hist(residuo.kid.I.o3.lag7)
shapiro.test(residuo.kid.I.o3.lag7)


################# crian�as CID I #######################
################## AIC E ANOVA #########################
#################### o3 ##############################

library(bbmle)

AIC.mod.kid.I.o3 <- AICctab(modelo.kid.I.o3.lag0,modelo.kid.I.o3.lag1,modelo.kid.I.o3.lag2,modelo.kid.I.o3.lag3, 
                            modelo.kid.I.o3.lag4,modelo.kid.I.o3.lag5,modelo.kid.I.o3.lag6,
                            modelo.kid.I.o3.lag7,
                            base = T, weights=T)
AIC.mod.kid.I.o3 

anova.gam.mod.kid.I.o3 <- anova.gam(modelo.kid.I.o3.lag0,modelo.kid.I.o3.lag1,modelo.kid.I.o3.lag2,modelo.kid.I.o3.lag3, 
                                    modelo.kid.I.o3.lag4,modelo.kid.I.o3.lag5,modelo.kid.I.o3.lag6,
                                    modelo.kid.I.o3.lag7)

anova.gam.mod.kid.I.o3

anova.mod.kid.I.o3 <-anova(modelo.kid.I.o3.lag0,modelo.kid.I.o3.lag1,modelo.kid.I.o3.lag2,modelo.kid.I.o3.lag3, 
                           modelo.kid.I.o3.lag4,modelo.kid.I.o3.lag5,modelo.kid.I.o3.lag6,
                           modelo.kid.I.o3.lag7)

anova.mod.kid.I.o3