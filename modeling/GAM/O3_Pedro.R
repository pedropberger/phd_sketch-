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

# dados.o3 <- data.frame(SIH_GV_I_Old, SIH_GV_J_Kid, SIH_GV_J_Old,
#                          o3.gv, o3.gv.d.1, o3.gv.d.2, o3.gv.d.3,
#                          o3.gv.d.4, o3.gv.d.5, o3.gv.d.6, o3.gv.d.7,
#                          Temp.Ar, Temp.maxima, Temp.minima, Umidade.Ar,
#                          Precip.acumulada)

#####################################################################################
############################# MODELOS COM O GAM ####################################

library("gam")
library("mgcv")

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM o3 COM IDOSOS ###################
###################################### Lag 0 ########################################

modelo.old.o3.1 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv + Temp.Ar + 
                                 Umidade.Ar,
                              data = dados, family = poisson, na.action = na.gam.replace)


summary(modelo.old.o3.1)
residuo <- residuals(modelo.old.o3.1)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.o3.2 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.2)
residuo <- residuals(modelo.old.o3.2)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.o3.3 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.3)
residuo <- residuals(modelo.old.o3.3)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.o3.4 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.4)
residuo <- residuals(modelo.old.o3.4)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM o3 COM IDOSOS ###################
###################################### Lag 1 ########################################
modelo.old.o3.5 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.1 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.5) 
residuo <- residuals(modelo.old.o3.5)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.o3.6 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.1 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.6)
residuo <- residuals(modelo.old.o3.6)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.o3.7 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.1 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.7)
residuo <- residuals(modelo.old.o3.7)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.o3.8 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.1 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.8)
residuo <- residuals(modelo.old.o3.8)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM o3 COM IDOSOS ###################
###################################### Lag 4 ########################################
modelo.old.o3.9 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.4 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.9)
residuo <- residuals(modelo.old.o3.9)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.o3.10 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.4 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.10)
residuo <- residuals(modelo.old.o3.10)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.o3.11 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.4 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.11)
residuo <- residuals(modelo.old.o3.11)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.o3.12 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.4 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.12)
residuo <- residuals(modelo.old.o3.12)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM o3 COM IDOSOS ###################
###################################### Lag 5 ########################################
modelo.old.o3.13 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.5 + Temp.Ar + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.13) #umidade não significativo
residuo <- residuals(modelo.old.o3.13)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

modelo.old.o3.14 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.5 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.14)
residuo <- residuals(modelo.old.o3.14)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.o3.15 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.5 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.15)
residuo <- residuals(modelo.old.o3.15)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.o3.16 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.4 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.16)
residuo <- residuals(modelo.old.o3.16)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM o3 COM IDOSOS ###################
###################################### Lag 6 ########################################
modelo.old.o3.17 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.6 + Temp.Ar + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.17) #umidade não significativo
residuo <- residuals(modelo.old.o3.17)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.o3.18 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.6 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.18)
residuo <- residuals(modelo.old.o3.18)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.o3.19 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.6 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.19)
residuo <- residuals(modelo.old.o3.19)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.o3.20 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.6 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.20)
residuo <- residuals(modelo.old.o3.20)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM o3 COM IDOSOS ###################
###################################### Lag 7 ########################################
modelo.old.o3.21 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.7 + Temp.Ar + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.21) #umidade não significativo
residuo <- residuals(modelo.old.o3.21)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.o3.22 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.7 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.22)
residuo <- residuals(modelo.old.o3.22)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.o3.23 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.7 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.23)
residuo <- residuals(modelo.old.o3.23)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.o3.24 <- mgcv::gam(SIH_GV_J_Old ~ o3.gv.d.7 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.o3.24)
residuo <- residuals(modelo.old.o3.24)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)



library(bbmle)
AIC.mod.o3.old <- AICctab(modelo.old.o3.1, modelo.old.o3.2, modelo.old.o3.3,
                            modelo.old.o3.4, modelo.old.o3.5, modelo.old.o3.6,
                            modelo.old.o3.7, modelo.old.o3.8, modelo.old.o3.9,
                            modelo.old.o3.10, modelo.old.o3.11, modelo.old.o3.12,
                            modelo.old.o3.13, modelo.old.o3.14, modelo.old.o3.15,
                            modelo.old.o3.16, modelo.old.o3.17, modelo.old.o3.18,
                            modelo.old.o3.19, modelo.old.o3.20, modelo.old.o3.21,
                            modelo.old.o3.22, modelo.old.o3.23, modelo.old.o3.24,
                            base = T, weights=T)
AIC.mod.o3.old

anova.gam(modelo.old.o3.1, modelo.old.o3.2, modelo.old.o3.3,
          modelo.old.o3.4, modelo.old.o3.5, modelo.old.o3.6,
          modelo.old.o3.7, modelo.old.o3.8, modelo.old.o3.9,
          modelo.old.o3.10, modelo.old.o3.11, modelo.old.o3.12,
          modelo.old.o3.13, modelo.old.o3.14, modelo.old.o3.15,
          modelo.old.o3.16, modelo.old.o3.17, modelo.old.o3.18,
          modelo.old.o3.19, modelo.old.o3.20, modelo.old.o3.21,
          modelo.old.o3.22, modelo.old.o3.23, modelo.old.o3.24)

anova(modeloo.ld.o3.1, modelo.old.o3.2, modelo.old.o3.3,
      modelo.old.o3.4, modelo.old.o3.5, modelo.old.o3.6,
      modelo.old.o3.7, modelo.old.o3.8, modelo.old.o3.9,
      modelo.old.o3.10, modelo.old.o3.11, modelo.old.o3.12,
      modelo.old.o3.13, modelo.old.o3.14, modelo.old.o3.15,
      modelo.old.o3.16, modelo.old.o3.17, modelo.old.o3.18,
      modelo.old.o3.19, modelo.old.o3.20, modelo.old.o3.21,
      modelo.old.o3.22, modelo.old.o3.23, modelo.old.o3.24)


################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM o3 COM CRIANCA ###################
###################################### Lag 0 ########################################
modelo.kid.o3.1 <- mgcv::gam(SIH_GV_J_Kid ~ o3.gv + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.o3.1) 
residuo <- residuals(modelo.kid.o3.1)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

modelo.kid.o3.2 <- mgcv::gam(SIH_GV_J_Kid ~ o3.gv + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.o3.2)
residuo <- residuals(modelo.kid.o3.2)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.o3.3 <- mgcv::gam(SIH_GV_J_Kid ~ o3.gv + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.o3.3)
residuo <- residuals(modelo.kid.o3.3)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.o3.4 <- mgcv::gam(SIH_GV_J_Kid ~ o3.gv + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.o3.4)
residuo <- residuals(modelo.kid.o3.4)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM o3 COM CRIANCA ###################
###################################### Lag 6 ########################################
modelo.kid.o3.5 <- mgcv::gam(SIH_GV_J_Kid ~ o3.gv.d.6 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.o3.5) 
residuo <- residuals(modelo.kid.o3.5)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.o3.6 <- mgcv::gam(SIH_GV_J_Kid ~ o3.gv.d.6 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.o3.6)
residuo <- residuals(modelo.kid.o3.6)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.o3.7 <- mgcv::gam(SIH_GV_J_Kid ~ o3.gv.d.6 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.o3.7)
residuo <- residuals(modelo.kid.o3.7)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.o3.8 <- mgcv::gam(SIH_GV_J_Kid ~ o3.gv.d.6 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.o3.8)
residuo <- residuals(modelo.kid.o3.8)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM o3 COM CRIANCA ###################
###################################### Lag 7 ########################################
modelo.kid.o3.9 <- mgcv::gam(SIH_GV_J_Kid ~ o3.gv.d.7 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.o3.9)
residuo <- residuals(modelo.kid.o3.9)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.o3.10 <- mgcv::gam(SIH_GV_J_Kid ~ o3.gv.d.7 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.o3.10)
residuo <- residuals(modelo.kid.o3.10)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.o3.11 <- mgcv::gam(SIH_GV_J_Kid ~ o3.gv.d.7 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.o3.11)
residuo <- residuals(modelo.kid.o3.11)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.o3.12 <- mgcv::gam(SIH_GV_J_Kid ~ o3.gv.d.7 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.o3.12)
residuo <- residuals(modelo.kid.o3.12)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)
library(bbmle)

AIC.mod.o3.kid <- AICctab(modelo.kid.o3.1, modelo.kid.o3.2, modelo.kid.o3.3,
                            modelo.kid.o3.4, modelo.kid.o3.5, modelo.kid.o3.6,
                            modelo.kid.o3.7, modelo.kid.o3.8, modelo.kid.o3.9,
                            modelo.kid.o3.10, modelo.kid.o3.11, modelo.kid.o3.12,
                            base = T, weights=T)
AIC.mod.o3.kid

anova.gam(modelo.kid.o3.1, modelo.kid.o3.2, modelo.kid.o3.3,
          modelo.kid.o3.4, modelo.kid.o3.5, modelo.kid.o3.6,
          modelo.kid.o3.7, modelo.kid.o3.8, modelo.kid.o3.9,
          modelo.kid.o3.10, modelo.kid.o3.11, modelo.kid.o3.12)

anova(modelo.kid.o3.1, modelo.kid.o3.2, modelo.kid.o3.3,
      modelo.kid.o3.4, modelo.kid.o3.5, modelo.kid.o3.6,
      modelo.kid.o3.7, modelo.kid.o3.8, modelo.kid.o3.9,
      modelo.kid.o3.10, modelo.kid.o3.11, modelo.kid.o3.12)


################################ CID I ############################################
###################### MODELO UNI-POLUENTE COM o3 COM IDOSOS ###################
###################################### Lag 0 ########################################
modelo.old.I.I.o3.1 <- mgcv::gam(SIH_GV_I_Old ~ o3.gv + Temp.Ar + 
                                     Umidade.Ar,
                                   data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.I.o3.1) 
residuo <- residuals(modelo.old.I.I.o3.1)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.o3.2 <- mgcv::gam(SIH_GV_I_Old ~ o3.gv + Temp.Ar + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.o3.2)
residuo <- residuals(modelo.old.I.o3.2)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.o3.3 <- mgcv::gam(SIH_GV_I_Old ~ o3.gv + s(Temp.Ar) + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.o3.3)
residuo <- residuals(modelo.old.I.o3.3)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.o3.4 <- mgcv::gam(SIH_GV_I_Old ~ o3.gv + s(Temp.Ar) + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.o3.4)
residuo <- residuals(modelo.old.I.o3.4)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


################################ CID I ############################################
###################### MODELO UNI-POLUENTE COM o3 COM IDOSOS ###################
###################################### Lag 6 ########################################
modelo.old.I.o3.5 <- mgcv::gam(SIH_GV_I_Old ~ o3.gv.d.6 + Temp.Ar + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.o3.5) 
residuo <- residuals(modelo.old.I.o3.5)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

modelo.old.I.o3.6 <- mgcv::gam(SIH_GV_I_Old ~ o3.gv.d.6 + Temp.Ar + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.o3.6)
residuo <- residuals(modelo.old.I.o3.6)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.o3.7 <- mgcv::gam(SIH_GV_I_Old ~ o3.gv.d.6 + s(Temp.Ar) + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.o3.7)
residuo <- residuals(modelo.old.I.o3.7)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.o3.8 <- mgcv::gam(SIH_GV_I_Old ~ o3.gv.d.6 + s(Temp.Ar) + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.o3.8)
residuo <- residuals(modelo.old.I.o3.8)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM o3 COM IDOSOS ###################
###################################### Lag 7 ########################################
modelo.old.I.o3.9 <- mgcv::gam(SIH_GV_I_Old ~ o3.gv.d.7 + Temp.Ar + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.o3.9) 
residuo <- residuals(modelo.old.I.o3.9)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)



modelo.old.I.o3.10 <- mgcv::gam(SIH_GV_I_Old ~ o3.gv.d.7 + Temp.Ar + 
                                    s(Umidade.Ar),
                                  data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.o3.10)
residuo <- residuals(modelo.old.I.o3.10)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.o3.11 <- mgcv::gam(SIH_GV_I_Old ~ o3.gv.d.7 + s(Temp.Ar) + 
                                    s(Umidade.Ar),
                                  data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.o3.11)
residuo <- residuals(modelo.old.I.o3.11)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.o3.12 <- mgcv::gam(SIH_GV_I_Old ~ o3.gv.d.7 + s(Temp.Ar) + 
                                    Umidade.Ar,
                                  data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.o3.12)
residuo <- residuals(modelo.old.I.o3.12)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)



library(bbmle)
AIC.mod.o3.old <- AICctab(modelo.old.I.o3.1, modelo.old.I.o3.2, modelo.old.I.o3.3,
                            modelo.old.I.o3.4, modelo.old.I.o3.5, modelo.old.I.o3.6,
                            modelo.old.I.o3.7, modelo.old.I.o3.8, modelo.old.I.o3.9,
                            modelo.old.I.o3.10, modelo.old.I.o3.11, modelo.old.I.o3.12,
                            base = T, weights=T)
AIC.mod.o3.old

anova.gam(modelo.old.I.o3.1, modelo.old.I.o3.2, modelo.old.I.o3.3,
          modelo.old.I.o3.4, modelo.old.I.o3.5, modelo.old.I.o3.6,
          modelo.old.I.o3.7, modelo.old.I.o3.8, modelo.old.I.o3.9,
          modelo.old.I.o3.10, modelo.old.I.o3.11, modelo.old.I.o3.12)

anova(modelo.old.I.o3.1, modelo.old.I.o3.2, modelo.old.I.o3.3,
      modelo.old.I.o3.4, modelo.old.I.o3.5, modelo.old.I.o3.6,
      modelo.old.I.o3.7, modelo.old.I.o3.8, modelo.old.I.o3.9,
      modelo.old.I.o3.10, modelo.old.I.o3.11, modelo.old.I.o3.12)
