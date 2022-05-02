##############################################################################################
############################# IMPORTANDO OS DADOS ############################################

library(readxl)
library(dplyr)

setwd('C:/Users/pedro/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/SIH_filtrado/')

list.files()

dados_pol <- read_excel("SIH_unificado_poluentesmedio.xlsx", sheet = "Defasagem-completa") #dados de saude e polui

setwd('C:/Users/pedro/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/Meteorologicos/')

meteo <- read_excel("dados.meteo.aeroporto.15a19.xlsx")


dados <- data.frame(dados_pol, meteo[1:1462, 5:12])

# dados.co <- data.frame(SIH_GV_I_Old, SIH_GV_J_Kid, SIH_GV_J_Old,
#                          co.gv, co.gv.d.1, co.gv.d.2, co.gv.d.3,
#                          co.gv.d.4, co.gv.d.5, co.gv.d.6, co.gv.d.7,
#                          Temp.Ar, Temp.maxima, Temp.minima, Umidade.Ar,
#                          Precip.acumulada)

#####################################################################################
############################# MODELOS COM O GAM ####################################

library("gam")
library("mgcv")

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM co COM IDOSOS ###################
###################################### Lag 0 ########################################

modelo.old.co.1 <- mgcv::gam(SIH_GV_J_Old ~ co.gv + Temp.Ar + 
                                 Umidade.Ar,
                              data = dados, family = poisson, na.action = na.gam.replace)


summary(modelo.old.co.1)
residuo <- residuals(modelo.old.co.1)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.co.2 <- mgcv::gam(SIH_GV_J_Old ~ co.gv + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.2)
residuo <- residuals(modelo.old.co.2)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.co.3 <- mgcv::gam(SIH_GV_J_Old ~ co.gv + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.3)
residuo <- residuals(modelo.old.co.3)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.co.4 <- mgcv::gam(SIH_GV_J_Old ~ co.gv + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.4)
residuo <- residuals(modelo.old.co.4)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM co COM IDOSOS ###################
###################################### Lag 1 ########################################
modelo.old.co.5 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.1 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.5) 
residuo <- residuals(modelo.old.co.5)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.co.6 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.1 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.6)
residuo <- residuals(modelo.old.co.6)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.co.7 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.1 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.7)
residuo <- residuals(modelo.old.co.7)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.co.8 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.1 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.8)
residuo <- residuals(modelo.old.co.8)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM co COM IDOSOS ###################
###################################### Lag 4 ########################################
modelo.old.co.9 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.4 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.9)
residuo <- residuals(modelo.old.co.9)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.co.10 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.4 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.10)
residuo <- residuals(modelo.old.co.10)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.co.11 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.4 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.11)
residuo <- residuals(modelo.old.co.11)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.co.12 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.4 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.12)
residuo <- residuals(modelo.old.co.12)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM co COM IDOSOS ###################
###################################### Lag 5 ########################################
modelo.old.co.13 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.5 + Temp.Ar + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.13) #umidade não significativo
residuo <- residuals(modelo.old.co.13)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

modelo.old.co.14 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.5 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.14)
residuo <- residuals(modelo.old.co.14)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.co.15 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.5 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.15)
residuo <- residuals(modelo.old.co.15)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.co.16 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.4 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.16)
residuo <- residuals(modelo.old.co.16)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM co COM IDOSOS ###################
###################################### Lag 6 ########################################
modelo.old.co.17 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.6 + Temp.Ar + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.17) #umidade não significativo
residuo <- residuals(modelo.old.co.17)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.co.18 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.6 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.18)
residuo <- residuals(modelo.old.co.18)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.co.19 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.6 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.19)
residuo <- residuals(modelo.old.co.19)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.co.20 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.6 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.20)
residuo <- residuals(modelo.old.co.20)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM co COM IDOSOS ###################
###################################### Lag 7 ########################################
modelo.old.co.21 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.7 + Temp.Ar + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.21) #umidade não significativo
residuo <- residuals(modelo.old.co.21)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.co.22 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.7 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.22)
residuo <- residuals(modelo.old.co.22)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.co.23 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.7 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.23)
residuo <- residuals(modelo.old.co.23)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.co.24 <- mgcv::gam(SIH_GV_J_Old ~ co.gv.d.7 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.co.24)
residuo <- residuals(modelo.old.co.24)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)



library(bbmle)
AIC.mod.co.old <- AICctab(modelo.old.co.1, modelo.old.co.2, modelo.old.co.3,
                            modelo.old.co.4, modelo.old.co.5, modelo.old.co.6,
                            modelo.old.co.7, modelo.old.co.8, modelo.old.co.9,
                            modelo.old.co.10, modelo.old.co.11, modelo.old.co.12,
                            modelo.old.co.13, modelo.old.co.14, modelo.old.co.15,
                            modelo.old.co.16, modelo.old.co.17, modelo.old.co.18,
                            modelo.old.co.19, modelo.old.co.20, modelo.old.co.21,
                            modelo.old.co.22, modelo.old.co.23, modelo.old.co.24,
                            base = T, weights=T)
AIC.mod.co.old

anova.gam(modelo.old.co.1, modelo.old.co.2, modelo.old.co.3,
          modelo.old.co.4, modelo.old.co.5, modelo.old.co.6,
          modelo.old.co.7, modelo.old.co.8, modelo.old.co.9,
          modelo.old.co.10, modelo.old.co.11, modelo.old.co.12,
          modelo.old.co.13, modelo.old.co.14, modelo.old.co.15,
          modelo.old.co.16, modelo.old.co.17, modelo.old.co.18,
          modelo.old.co.19, modelo.old.co.20, modelo.old.co.21,
          modelo.old.co.22, modelo.old.co.23, modelo.old.co.24)

anova(modeloo.ld.co.1, modelo.old.co.2, modelo.old.co.3,
      modelo.old.co.4, modelo.old.co.5, modelo.old.co.6,
      modelo.old.co.7, modelo.old.co.8, modelo.old.co.9,
      modelo.old.co.10, modelo.old.co.11, modelo.old.co.12,
      modelo.old.co.13, modelo.old.co.14, modelo.old.co.15,
      modelo.old.co.16, modelo.old.co.17, modelo.old.co.18,
      modelo.old.co.19, modelo.old.co.20, modelo.old.co.21,
      modelo.old.co.22, modelo.old.co.23, modelo.old.co.24)


################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM co COM CRIANCA ###################
###################################### Lag 0 ########################################
modelo.kid.co.1 <- mgcv::gam(SIH_GV_J_Kid ~ co.gv + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.co.1) 
residuo <- residuals(modelo.kid.co.1)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

modelo.kid.co.2 <- mgcv::gam(SIH_GV_J_Kid ~ co.gv + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.co.2)
residuo <- residuals(modelo.kid.co.2)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.co.3 <- mgcv::gam(SIH_GV_J_Kid ~ co.gv + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.co.3)
residuo <- residuals(modelo.kid.co.3)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.co.4 <- mgcv::gam(SIH_GV_J_Kid ~ co.gv + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.co.4)
residuo <- residuals(modelo.kid.co.4)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM co COM CRIANCA ###################
###################################### Lag 6 ########################################
modelo.kid.co.5 <- mgcv::gam(SIH_GV_J_Kid ~ co.gv.d.6 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.co.5) 
residuo <- residuals(modelo.kid.co.5)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.co.6 <- mgcv::gam(SIH_GV_J_Kid ~ co.gv.d.6 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.co.6)
residuo <- residuals(modelo.kid.co.6)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.co.7 <- mgcv::gam(SIH_GV_J_Kid ~ co.gv.d.6 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.co.7)
residuo <- residuals(modelo.kid.co.7)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.co.8 <- mgcv::gam(SIH_GV_J_Kid ~ co.gv.d.6 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.co.8)
residuo <- residuals(modelo.kid.co.8)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM co COM CRIANCA ###################
###################################### Lag 7 ########################################
modelo.kid.co.9 <- mgcv::gam(SIH_GV_J_Kid ~ co.gv.d.7 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.co.9)
residuo <- residuals(modelo.kid.co.9)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.co.10 <- mgcv::gam(SIH_GV_J_Kid ~ co.gv.d.7 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.co.10)
residuo <- residuals(modelo.kid.co.10)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.co.11 <- mgcv::gam(SIH_GV_J_Kid ~ co.gv.d.7 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.co.11)
residuo <- residuals(modelo.kid.co.11)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.co.12 <- mgcv::gam(SIH_GV_J_Kid ~ co.gv.d.7 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.co.12)
residuo <- residuals(modelo.kid.co.12)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)
library(bbmle)

AIC.mod.co.kid <- AICctab(modelo.kid.co.1, modelo.kid.co.2, modelo.kid.co.3,
                            modelo.kid.co.4, modelo.kid.co.5, modelo.kid.co.6,
                            modelo.kid.co.7, modelo.kid.co.8, modelo.kid.co.9,
                            modelo.kid.co.10, modelo.kid.co.11, modelo.kid.co.12,
                            base = T, weights=T)
AIC.mod.co.kid

anova.gam(modelo.kid.co.1, modelo.kid.co.2, modelo.kid.co.3,
          modelo.kid.co.4, modelo.kid.co.5, modelo.kid.co.6,
          modelo.kid.co.7, modelo.kid.co.8, modelo.kid.co.9,
          modelo.kid.co.10, modelo.kid.co.11, modelo.kid.co.12)

anova(modelo.kid.co.1, modelo.kid.co.2, modelo.kid.co.3,
      modelo.kid.co.4, modelo.kid.co.5, modelo.kid.co.6,
      modelo.kid.co.7, modelo.kid.co.8, modelo.kid.co.9,
      modelo.kid.co.10, modelo.kid.co.11, modelo.kid.co.12)


################################ CID I ############################################
###################### MODELO UNI-POLUENTE COM co COM IDOSOS ###################
###################################### Lag 0 ########################################
modelo.old.I.I.co.1 <- mgcv::gam(SIH_GV_I_Old ~ co.gv + Temp.Ar + 
                                     Umidade.Ar,
                                   data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.I.co.1) 
residuo <- residuals(modelo.old.I.I.co.1)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.co.2 <- mgcv::gam(SIH_GV_I_Old ~ co.gv + Temp.Ar + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.co.2)
residuo <- residuals(modelo.old.I.co.2)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.co.3 <- mgcv::gam(SIH_GV_I_Old ~ co.gv + s(Temp.Ar) + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.co.3)
residuo <- residuals(modelo.old.I.co.3)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.co.4 <- mgcv::gam(SIH_GV_I_Old ~ co.gv + s(Temp.Ar) + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.co.4)
residuo <- residuals(modelo.old.I.co.4)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


################################ CID I ############################################
###################### MODELO UNI-POLUENTE COM co COM IDOSOS ###################
###################################### Lag 6 ########################################
modelo.old.I.co.5 <- mgcv::gam(SIH_GV_I_Old ~ co.gv.d.6 + Temp.Ar + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.co.5) 
residuo <- residuals(modelo.old.I.co.5)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

modelo.old.I.co.6 <- mgcv::gam(SIH_GV_I_Old ~ co.gv.d.6 + Temp.Ar + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.co.6)
residuo <- residuals(modelo.old.I.co.6)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.co.7 <- mgcv::gam(SIH_GV_I_Old ~ co.gv.d.6 + s(Temp.Ar) + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.co.7)
residuo <- residuals(modelo.old.I.co.7)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.co.8 <- mgcv::gam(SIH_GV_I_Old ~ co.gv.d.6 + s(Temp.Ar) + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.co.8)
residuo <- residuals(modelo.old.I.co.8)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM co COM IDOSOS ###################
###################################### Lag 7 ########################################
modelo.old.I.co.9 <- mgcv::gam(SIH_GV_I_Old ~ co.gv.d.7 + Temp.Ar + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.co.9) 
residuo <- residuals(modelo.old.I.co.9)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)



modelo.old.I.co.10 <- mgcv::gam(SIH_GV_I_Old ~ co.gv.d.7 + Temp.Ar + 
                                    s(Umidade.Ar),
                                  data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.co.10)
residuo <- residuals(modelo.old.I.co.10)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.co.11 <- mgcv::gam(SIH_GV_I_Old ~ co.gv.d.7 + s(Temp.Ar) + 
                                    s(Umidade.Ar),
                                  data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.co.11)
residuo <- residuals(modelo.old.I.co.11)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.co.12 <- mgcv::gam(SIH_GV_I_Old ~ co.gv.d.7 + s(Temp.Ar) + 
                                    Umidade.Ar,
                                  data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.co.12)
residuo <- residuals(modelo.old.I.co.12)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)



library(bbmle)
AIC.mod.co.old <- AICctab(modelo.old.I.co.1, modelo.old.I.co.2, modelo.old.I.co.3,
                            modelo.old.I.co.4, modelo.old.I.co.5, modelo.old.I.co.6,
                            modelo.old.I.co.7, modelo.old.I.co.8, modelo.old.I.co.9,
                            modelo.old.I.co.10, modelo.old.I.co.11, modelo.old.I.co.12,
                            base = T, weights=T)
AIC.mod.co.old

anova.gam(modelo.old.I.co.1, modelo.old.I.co.2, modelo.old.I.co.3,
          modelo.old.I.co.4, modelo.old.I.co.5, modelo.old.I.co.6,
          modelo.old.I.co.7, modelo.old.I.co.8, modelo.old.I.co.9,
          modelo.old.I.co.10, modelo.old.I.co.11, modelo.old.I.co.12)

anova(modelo.old.I.co.1, modelo.old.I.co.2, modelo.old.I.co.3,
      modelo.old.I.co.4, modelo.old.I.co.5, modelo.old.I.co.6,
      modelo.old.I.co.7, modelo.old.I.co.8, modelo.old.I.co.9,
      modelo.old.I.co.10, modelo.old.I.co.11, modelo.old.I.co.12)
