##############################################################################################
############################# IMPORTANDO OS DADOS ############################################

library(readxl)
library(dplyr)

setwd('C:/Users/rcfil/Dropbox/1Doutorado/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/SIH_filtrado/')

list.files()

dados_pol <- read_excel("SIH_unificado_poluentesmedio.xlsx", sheet = "Defasagem-completa") #dados de saude e polui

setwd('C:/Users/rcfil/Dropbox/1Doutorado/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/Meteorologicos/')

meteo <- read_excel("dados.meteo.aeroporto.15a19.xlsx")


dados <- data.frame(dados_pol, meteo[1:1462, 5:12])

dados.pm10 <- data.frame(SIH_GV_I_Old, SIH_GV_J_Kid, SIH_GV_J_Old,
                         pm10.gv, pm10.gv.d.1, pm10.gv.d.2, pm10.gv.d.3,
                         pm10.gv.d.4, pm10.gv.d.5, pm10.gv.d.6, pm10.gv.d.7,
                         Temp.Ar, Temp.maxima, Temp.minima, Umidade.Ar,
                         Precip.acumulada)

so2.ts <- ts(dados_pol$so2.gv, start = c(2015,1), frequency = 365)

#####################################################################################
############################# MODELOS COM O GAM ####################################

library("gam")
library("mgcv")

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM10 COM IDOSOS ###################
###################################### Lag 0 ########################################
modelo.old.pm10.1 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.1)
residuo <- residuals(modelo.old.pm10.1)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm10.2 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.2)
residuo <- residuals(modelo.old.pm10.2)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm10.3 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.3)
residuo <- residuals(modelo.old.pm10.3)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm10.4 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.4)
residuo <- residuals(modelo.old.pm10.4)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM10 COM IDOSOS ###################
###################################### Lag 1 ########################################
modelo.old.pm10.5 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.1 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.5) 
residuo <- residuals(modelo.old.pm10.5)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.pm10.6 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.1 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.6)
residuo <- residuals(modelo.old.pm10.6)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm10.7 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.1 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.7)
residuo <- residuals(modelo.old.pm10.7)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm10.8 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.1 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.8)
residuo <- residuals(modelo.old.pm10.8)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM10 COM IDOSOS ###################
###################################### Lag 4 ########################################
modelo.old.pm10.9 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.4 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.9)
residuo <- residuals(modelo.old.pm10.9)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.pm10.10 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.4 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.10)
residuo <- residuals(modelo.old.pm10.10)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm10.11 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.4 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.11)
residuo <- residuals(modelo.old.pm10.11)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm10.12 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.4 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.12)
residuo <- residuals(modelo.old.pm10.12)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM10 COM IDOSOS ###################
###################################### Lag 5 ########################################
modelo.old.pm10.13 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.5 + Temp.Ar + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.13)
residuo <- residuals(modelo.old.pm10.13)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

modelo.old.pm10.14 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.5 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.14)
residuo <- residuals(modelo.old.pm10.14)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm10.15 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.5 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.15)
residuo <- residuals(modelo.old.pm10.15)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm10.16 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.4 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.16)
residuo <- residuals(modelo.old.pm10.16)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM10 COM IDOSOS ###################
###################################### Lag 6 ########################################
modelo.old.pm10.17 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.6 + Temp.Ar + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.17) 
residuo <- residuals(modelo.old.pm10.17)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.pm10.18 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.6 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.18)
residuo <- residuals(modelo.old.pm10.18)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm10.19 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.6 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.19)
residuo <- residuals(modelo.old.pm10.19)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm10.20 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.6 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.20)
residuo <- residuals(modelo.old.pm10.20)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM10 COM IDOSOS ###################
###################################### Lag 7 ########################################
modelo.old.pm10.21 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.7 + Temp.Ar + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.21) 
residuo <- residuals(modelo.old.pm10.21)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.pm10.22 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.7 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.22)
residuo <- residuals(modelo.old.pm10.22)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm10.23 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.7 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.23)
residuo <- residuals(modelo.old.pm10.23)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm10.24 <- mgcv::gam(SIH_GV_J_Old ~ pm10.gv.d.7 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm10.24)
residuo <- residuals(modelo.old.pm10.24)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)



library(bbmle)
AIC.mod.pm10.old <- AICctab(modelo.old.pm10.1, modelo.old.pm10.2, modelo.old.pm10.3,
                            modelo.old.pm10.4, modelo.old.pm10.5, modelo.old.pm10.6,
                            modelo.old.pm10.7, modelo.old.pm10.8, modelo.old.pm10.9,
                            modelo.old.pm10.10, modelo.old.pm10.11, modelo.old.pm10.12,
                            modelo.old.pm10.13, modelo.old.pm10.14, modelo.old.pm10.15,
                            modelo.old.pm10.16, modelo.old.pm10.17, modelo.old.pm10.18,
                            modelo.old.pm10.19, modelo.old.pm10.20, modelo.old.pm10.21,
                            modelo.old.pm10.22, modelo.old.pm10.23, modelo.old.pm10.24,
                            base = T, weights=T)
AIC.mod.pm10.old

anova.gam(modelo.old.pm10.1, modelo.old.pm10.2, modelo.old.pm10.3,
          modelo.old.pm10.4, modelo.old.pm10.5, modelo.old.pm10.6,
          modelo.old.pm10.7, modelo.old.pm10.8, modelo.old.pm10.9,
          modelo.old.pm10.10, modelo.old.pm10.11, modelo.old.pm10.12,
          modelo.old.pm10.13, modelo.old.pm10.14, modelo.old.pm10.15,
          modelo.old.pm10.16, modelo.old.pm10.17, modelo.old.pm10.18,
          modelo.old.pm10.19, modelo.old.pm10.20, modelo.old.pm10.21,
          modelo.old.pm10.22, modelo.old.pm10.23, modelo.old.pm10.24)

anova(modeloo.ld.pm10.1, modelo.old.pm10.2, modelo.old.pm10.3,
      modelo.old.pm10.4, modelo.old.pm10.5, modelo.old.pm10.6,
      modelo.old.pm10.7, modelo.old.pm10.8, modelo.old.pm10.9,
      modelo.old.pm10.10, modelo.old.pm10.11, modelo.old.pm10.12,
      modelo.old.pm10.13, modelo.old.pm10.14, modelo.old.pm10.15,
      modelo.old.pm10.16, modelo.old.pm10.17, modelo.old.pm10.18,
      modelo.old.pm10.19, modelo.old.pm10.20, modelo.old.pm10.21,
      modelo.old.pm10.22, modelo.old.pm10.23, modelo.old.pm10.24)


################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM10 COM CRIANCA ###################
###################################### Lag 0 ########################################
modelo.kid.pm10.1 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm10.1) 
residuo <- residuals(modelo.kid.pm10.1)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

modelo.kid.pm10.2 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm10.2)
residuo <- residuals(modelo.kid.pm10.2)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm10.3 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm10.3)
residuo <- residuals(modelo.kid.pm10.3)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm10.4 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm10.4)
residuo <- residuals(modelo.kid.pm10.4)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM10 COM CRIANCA ###################
###################################### Lag 6 ########################################
modelo.kid.pm10.5 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv.d.6 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm10.5) 
residuo <- residuals(modelo.kid.pm10.5)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm10.6 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv.d.6 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm10.6)
residuo <- residuals(modelo.kid.pm10.6)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm10.7 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv.d.6 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm10.7)
residuo <- residuals(modelo.kid.pm10.7)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm10.8 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv.d.6 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm10.8)
residuo <- residuals(modelo.kid.pm10.8)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM10 COM CRIANCA ###################
###################################### Lag 7 ########################################
modelo.kid.pm10.9 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv.d.7 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm10.9)
residuo <- residuals(modelo.kid.pm10.9)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm10.10 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv.d.7 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm10.10)
residuo <- residuals(modelo.kid.pm10.10)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm10.11 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv.d.7 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm10.11)
residuo <- residuals(modelo.kid.pm10.11)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm10.12 <- mgcv::gam(SIH_GV_J_Kid ~ pm10.gv.d.7 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm10.12)
residuo <- residuals(modelo.kid.pm10.12)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)
library(bbmle)

AIC.mod.pm10.kid <- AICctab(modelo.kid.pm10.1, modelo.kid.pm10.2, modelo.kid.pm10.3,
                            modelo.kid.pm10.4, modelo.kid.pm10.5, modelo.kid.pm10.6,
                            modelo.kid.pm10.7, modelo.kid.pm10.8, modelo.kid.pm10.9,
                            modelo.kid.pm10.10, modelo.kid.pm10.11, modelo.kid.pm10.12,
                            base = T, weights=T)
AIC.mod.pm10.kid

anova.gam(modelo.kid.pm10.1, modelo.kid.pm10.2, modelo.kid.pm10.3,
          modelo.kid.pm10.4, modelo.kid.pm10.5, modelo.kid.pm10.6,
          modelo.kid.pm10.7, modelo.kid.pm10.8, modelo.kid.pm10.9,
          modelo.kid.pm10.10, modelo.kid.pm10.11, modelo.kid.pm10.12)

anova(modelo.kid.pm10.1, modelo.kid.pm10.2, modelo.kid.pm10.3,
      modelo.kid.pm10.4, modelo.kid.pm10.5, modelo.kid.pm10.6,
      modelo.kid.pm10.7, modelo.kid.pm10.8, modelo.kid.pm10.9,
      modelo.kid.pm10.10, modelo.kid.pm10.11, modelo.kid.pm10.12)


################################ CID I ############################################
###################### MODELO UNI-POLUENTE COM PM10 COM IDOSOS ###################
###################################### Lag 0 ########################################
modelo.old.I.I.pm10.1 <- mgcv::gam(SIH_GV_I_Old ~ pm10.gv + Temp.Ar + 
                                     Umidade.Ar,
                                   data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.I.pm10.1) 
residuo <- residuals(modelo.old.I.I.pm10.1)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.pm10.2 <- mgcv::gam(SIH_GV_I_Old ~ pm10.gv + Temp.Ar + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm10.2)
residuo <- residuals(modelo.old.I.pm10.2)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.pm10.3 <- mgcv::gam(SIH_GV_I_Old ~ pm10.gv + s(Temp.Ar) + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm10.3)
residuo <- residuals(modelo.old.I.pm10.3)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.pm10.4 <- mgcv::gam(SIH_GV_I_Old ~ pm10.gv + s(Temp.Ar) + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm10.4)
residuo <- residuals(modelo.old.I.pm10.4)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


################################ CID I ############################################
###################### MODELO UNI-POLUENTE COM PM10 COM IDOSOS ###################
###################################### Lag 6 ########################################
modelo.old.I.pm10.5 <- mgcv::gam(SIH_GV_I_Old ~ pm10.gv.d.6 + Temp.Ar + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm10.5) 
residuo <- residuals(modelo.old.I.pm10.5)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

modelo.old.I.pm10.6 <- mgcv::gam(SIH_GV_I_Old ~ pm10.gv.d.6 + Temp.Ar + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm10.6)
residuo <- residuals(modelo.old.I.pm10.6)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.pm10.7 <- mgcv::gam(SIH_GV_I_Old ~ pm10.gv.d.6 + s(Temp.Ar) + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm10.7)
residuo <- residuals(modelo.old.I.pm10.7)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.pm10.8 <- mgcv::gam(SIH_GV_I_Old ~ pm10.gv.d.6 + s(Temp.Ar) + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm10.8)
residuo <- residuals(modelo.old.I.pm10.8)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM10 COM IDOSOS ###################
###################################### Lag 7 ########################################
modelo.old.I.pm10.9 <- mgcv::gam(SIH_GV_I_Old ~ pm10.gv.d.7 + Temp.Ar + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm10.9) 
residuo <- residuals(modelo.old.I.pm10.9)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)



modelo.old.I.pm10.10 <- mgcv::gam(SIH_GV_I_Old ~ pm10.gv.d.7 + Temp.Ar + 
                                    s(Umidade.Ar),
                                  data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm10.10)
residuo <- residuals(modelo.old.I.pm10.10)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.pm10.11 <- mgcv::gam(SIH_GV_I_Old ~ pm10.gv.d.7 + s(Temp.Ar) + 
                                    s(Umidade.Ar),
                                  data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm10.11)
residuo <- residuals(modelo.old.I.pm10.11)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.pm10.12 <- mgcv::gam(SIH_GV_I_Old ~ pm10.gv.d.7 + s(Temp.Ar) + 
                                    Umidade.Ar,
                                  data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm10.12)
residuo <- residuals(modelo.old.I.pm10.12)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)



library(bbmle)
AIC.mod.pm10.old <- AICctab(modelo.old.I.pm10.1, modelo.old.I.pm10.2, modelo.old.I.pm10.3,
                            modelo.old.I.pm10.4, modelo.old.I.pm10.5, modelo.old.I.pm10.6,
                            modelo.old.I.pm10.7, modelo.old.I.pm10.8, modelo.old.I.pm10.9,
                            modelo.old.I.pm10.10, modelo.old.I.pm10.11, modelo.old.I.pm10.12,
                            base = T, weights=T)
AIC.mod.pm10.old

anova.gam(modelo.old.I.pm10.1, modelo.old.I.pm10.2, modelo.old.I.pm10.3,
          modelo.old.I.pm10.4, modelo.old.I.pm10.5, modelo.old.I.pm10.6,
          modelo.old.I.pm10.7, modelo.old.I.pm10.8, modelo.old.I.pm10.9,
          modelo.old.I.pm10.10, modelo.old.I.pm10.11, modelo.old.I.pm10.12)

anova(modelo.old.I.pm10.1, modelo.old.I.pm10.2, modelo.old.I.pm10.3,
      modelo.old.I.pm10.4, modelo.old.I.pm10.5, modelo.old.I.pm10.6,
      modelo.old.I.pm10.7, modelo.old.I.pm10.8, modelo.old.I.pm10.9,
      modelo.old.I.pm10.10, modelo.old.I.pm10.11, modelo.old.I.pm10.12)
