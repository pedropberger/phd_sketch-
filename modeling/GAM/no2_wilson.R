##############################################################################################
############################# IMPORTANDO OS DADOS ############################################

library(readxl)
library(dplyr)

setwd('C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/SIH_filtrado/')

list.files()

dados_pol <- read_excel("SIH_unificado_poluentesmedio.xlsx", sheet = "Defasagem-completa") #dados de saude e polui

setwd('C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/Meteorologicos/')

meteo <- read_excel("dados.meteo.aeroporto.15a19.xlsx")


dados <- data.frame(dados_pol, meteo[1:1462, 5:12])

attach(dados)

#####################################################################################
############################# MODELOS COM O GAM ####################################

library("gam")
library("mgcv")

################################ CID J ############################################
###################### MODELOs UNI-POLUENTE COM no2 COM IDOSOS ###################
###################################### Lag 0 ########################################
modelo.old.no2_1 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv + Temp.Ar + 
                                Umidade.Ar,
                              data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_1) 
residuo.no2_1 <- residuals(modelo.old.no2_1)
plot(residuo.no2_1)
acf(residuo.no2_1)
qqnorm(residuo.no2_1)
qqline(residuo.no2_1)
hist(residuo.no2_1)


modelo.old.no2_2 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv + Temp.Ar + 
                                s(Umidade.Ar),
                              data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_2)
residuo.no2_2 <- residuals(modelo.old.no2_2)
plot(residuo.no2_2)
acf(residuo.no2_2)
qqnorm(residuo.no2_2)
qqline(residuo.no2_2)
hist(residuo.no2_2)
shapiro.test(residuo.no2_2)


modelo.old.no2_3 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv + s(Temp.Ar) + 
                                s(Umidade.Ar),
                              data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_3)
residuo.no2_3 <- residuals(modelo.old.no2_3)
plot(residuo.no2_3)
acf(residuo.no2_3)
qqnorm(residuo.no2_3)
qqline(residuo.no2_3)
hist(residuo.no2_3)
shapiro.test(residuo.no2_3)


modelo.old.no2_4 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv + s(Temp.Ar) + 
                                Umidade.Ar,
                              data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_4)
residuo.no2_4 <- residuals(modelo.old.no2_4)
plot(residuo.no2_4)
acf(residuo.no2_4)
qqnorm(residuo.no2_4)
qqline(residuo.no2_4)
hist(residuo.no2_4)
shapiro.test(residuo.no2_4)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM no2 COM IDOSOS ###################
###################################### Lag 1 ########################################
modelo.old.no2_5 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.1 + Temp.Ar + 
                                Umidade.Ar,
                              data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_5) 
residuo.no2_5 <- residuals(modelo.old.no2_5)
plot(residuo.no2_5)
acf(residuo.no2_5)
qqnorm(residuo.no2_5)
qqline(residuo.no2_5)
hist(residuo.no2_5)


modelo.old.no2_6 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.1 + Temp.Ar + 
                                s(Umidade.Ar),
                              data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_6)
residuo.no2_6 <- residuals(modelo.old.no2_6)
plot(residuo.no2_6)
acf(residuo.no2_6)
qqnorm(residuo.no2_6)
qqline(residuo.no2_6)
hist(residuo.no2_6)
shapiro.test(residuo.no2_6)


modelo.old.no2_7 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.1 + s(Temp.Ar) + 
                                s(Umidade.Ar),
                              data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_7)
residuo.no2_7 <- residuals(modelo.old.no2_7)
plot(residuo.no2_7)
acf(residuo.no2_7)
qqnorm(residuo.no2_7)
qqline(residuo.no2_7)
hist(residuo.no2_7)
shapiro.test(residuo.no2_7)


modelo.old.no2_8 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.1 + s(Temp.Ar) + 
                                Umidade.Ar,
                              data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_8)
residuo.no2_8 <- residuals(modelo.old.no2_8)
plot(residuo.no2_8)
acf(residuo.no2_8)
qqnorm(residuo.no2_8)
qqline(residuo.no2_8)
hist(residuo.no2_8)
shapiro.test(residuo.no2_8)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM no2 COM IDOSOS ###################
###################################### Lag 4 ########################################
modelo.old.no2_9 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.4 + Temp.Ar + 
                                Umidade.Ar,
                              data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_9)
residuo.no2_9 <- residuals(modelo.old.no2_9)
plot(residuo.no2_9)
acf(residuo.no2_9)
qqnorm(residuo.no2_9)
qqline(residuo.no2_9)
hist(residuo.no2_9)


modelo.old.no2_10 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.4 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_10)
residuo.no2_10 <- residuals(modelo.old.no2_10)
plot(residuo.no2_10)
acf(residuo.no2_10)
qqnorm(residuo.no2_10)
qqline(residuo.no2_10)
hist(residuo.no2_10)
shapiro.test(residuo.no2_10)


modelo.old.no2_11 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.4 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_11)
residuo.no2_11 <- residuals(modelo.old.no2_11)
plot(residuo.no2_11)
acf(residuo.no2_11)
qqnorm(residuo.no2_11)
qqline(residuo.no2_11)
hist(residuo.no2_11)
shapiro.test(residuo.no2_11)


modelo.old.no2_12 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.4 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_12)
residuo.no2_12 <- residuals(modelo.old.no2_12)
plot(residuo.no2_12)
acf(residuo.no2_12)
qqnorm(residuo.no2_12)
qqline(residuo.no2_12)
hist(residuo.no2_12)
shapiro.test(residuo.no2_12)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM no2 COM IDOSOS ###################
###################################### Lag 5 ########################################
modelo.old.no2_13 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.5 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_13) 
residuo.no2_13 <- residuals(modelo.old.no2_13)
plot(residuo.no2_13)
acf(residuo.no2_13)
qqnorm(residuo.no2_13)
qqline(residuo.no2_13)
hist(residuo.no2_13)


modelo.old.no2_14 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.5 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_14)
residuo.no2_14 <- residuals(modelo.old.no2_14)
plot(residuo.no2_14)
acf(residuo.no2_14)
qqnorm(residuo.no2_14)
qqline(residuo.no2_14)
hist(residuo.no2_14)
shapiro.test(residuo.no2_14)


modelo.old.no2_15 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.5 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_15)
residuo.no2_15 <- residuals(modelo.old.no2_15)
plot(residuo.no2_15)
acf(residuo.no2_15)
qqnorm(residuo.no2_15)
qqline(residuo.no2_15)
hist(residuo.no2_15)
shapiro.test(residuo.no2_15)


modelo.old.no2_16 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.5 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_16)
residuo.no2_16 <- residuals(modelo.old.no2_16)
plot(residuo.no2_16)
acf(residuo.no2_16)
qqnorm(residuo.no2_16)
qqline(residuo.no2_16)
hist(residuo.no2_16)
shapiro.test(residuo.no2_16)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM no2 COM IDOSOS ###################
###################################### Lag 6 ########################################
modelo.old.no2_17 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.6 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_17)  
residuo.no2_17 <- residuals(modelo.old.no2_17)
plot(residuo.no2_17)
acf(residuo.no2_17)
qqnorm(residuo.no2_17)
qqline(residuo.no2_17)
hist(residuo.no2_17)


modelo.old.no2_18 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.6 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_18)
residuo.no2_18 <- residuals(modelo.old.no2_18)
plot(residuo.no2_18)
acf(residuo.no2_18)
qqnorm(residuo.no2_18)
qqline(residuo.no2_18)
hist(residuo.no2_18)
shapiro.test(residuo.no2_18)


modelo.old.no2_19 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.6 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_19)
residuo.no2_19 <- residuals(modelo.old.no2_19)
plot(residuo.no2_19)
acf(residuo.no2_19)
qqnorm(residuo.no2_19)
qqline(residuo.no2_19)
hist(residuo.no2_19)
shapiro.test(residuo.no2_19)


modelo.old.no2_20 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.6 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_20)
residuo.no2_20 <- residuals(modelo.old.no2_20)
plot(residuo.no2_20)
acf(residuo.no2_20)
qqnorm(residuo.no2_20)
qqline(residuo.no2_20)
hist(residuo.no2_20)
shapiro.test(residuo.no2_20)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM no2 COM IDOSOS ###################
###################################### Lag 7 ########################################
modelo.old.no2_21 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.7 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_21)  
residuo.no2_21 <- residuals(modelo.old.no2_21)
plot(residuo.no2_21)
acf(residuo.no2_21)
qqnorm(residuo.no2_21)
qqline(residuo.no2_21)
hist(residuo.no2_21)


modelo.old.no2_22 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.7 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_22)
residuo.no2_22 <- residuals(modelo.old.no2_22)
plot(residuo.no2_22)
acf(residuo.no2_22)
qqnorm(residuo.no2_22)
qqline(residuo.no2_22)
hist(residuo.no2_22)
shapiro.test(residuo.no2_22)


modelo.old.no2_23 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.7 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_23)
residuo.no2_23 <- residuals(modelo.old.no2_23)
plot(residuo.no2_23)
acf(residuo.no2_23)
qqnorm(residuo.no2_23)
qqline(residuo.no2_23)
hist(residuo.no2_23)
shapiro.test(residuo.no2_23)


modelo.old.no2_24 <- mgcv::gam(SIH_GV_J_Old ~ no2.gv.d.7 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.no2_24)
residuo.no2_24 <- residuals(modelo.old.no2_24)
plot(residuo.no2_24)
acf(residuo.no2_24)
qqnorm(residuo.no2_24)
qqline(residuo.no2_24)
hist(residuo.no2_24)
shapiro.test(residuo.no2_24)



library(bbmle)
AIC.mod.no2.old <- AICctab(modelo.old.no2_1, modelo.old.no2_2, modelo.old.no2_3,
                           modelo.old.no2_4, modelo.old.no2_5, modelo.old.no2_6,
                           modelo.old.no2_7, modelo.old.no2_8, modelo.old.no2_9,
                           modelo.old.no2_10, modelo.old.no2_11, modelo.old.no2_12,
                           modelo.old.no2_13, modelo.old.no2_14, modelo.old.no2_15,
                           modelo.old.no2_16, modelo.old.no2_17, modelo.old.no2_18,
                           modelo.old.no2_19, modelo.old.no2_20, modelo.old.no2_21,
                           modelo.old.no2_22, modelo.old.no2_23, modelo.old.no2_24,
                           base = T, weights=T)
AIC.mod.no2.old

anova.gam.no2.old <- anova.gam(modelo.old.no2_1, modelo.old.no2_2, modelo.old.no2_3,
                               modelo.old.no2_4, modelo.old.no2_5, modelo.old.no2_6,
                               modelo.old.no2_7, modelo.old.no2_8, modelo.old.no2_9,
                               modelo.old.no2_10, modelo.old.no2_11, modelo.old.no2_12,
                               modelo.old.no2_13, modelo.old.no2_14, modelo.old.no2_15,
                               modelo.old.no2_16, modelo.old.no2_17, modelo.old.no2_18,
                               modelo.old.no2_19, modelo.old.no2_20, modelo.old.no2_21,
                               modelo.old.no2_22, modelo.old.no2_23, modelo.old.no2_24)

anova.gam.no2.old

anova.no2.old <- anova(modelo.old.no2_1, modelo.old.no2_2, modelo.old.no2_3,
                       modelo.old.no2_4, modelo.old.no2_5, modelo.old.no2_6,
                       modelo.old.no2_7, modelo.old.no2_8, modelo.old.no2_9,
                       modelo.old.no2_10, modelo.old.no2_11, modelo.old.no2_12,
                       modelo.old.no2_13, modelo.old.no2_14, modelo.old.no2_15,
                       modelo.old.no2_16, modelo.old.no2_17, modelo.old.no2_18,
                       modelo.old.no2_19, modelo.old.no2_20, modelo.old.no2_21,
                       modelo.old.no2_22, modelo.old.no2_23, modelo.old.no2_24)

anova.no2.old 

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM no2 COM CRIANCA ###################
###################################### Lag 0 ########################################
modelo.kid.no2_1 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv + Temp.Ar + 
                                Umidade.Ar,
                              data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_1)  
residuo.kid.no2_1 <- residuals(modelo.kid.no2_1)
plot(residuo.kid.no2_1)
acf(residuo.kid.no2_1)
qqnorm(residuo.kid.no2_1)
qqline(residuo.kid.no2_1)
hist(residuo.kid.no2_1)


modelo.kid.no2_2 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv + Temp.Ar + 
                                s(Umidade.Ar),
                              data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_2)
residuo.kid.no2_2 <- residuals(modelo.kid.no2_2)
plot(residuo.kid.no2_2)
acf(residuo.kid.no2_2)
qqnorm(residuo.kid.no2_2)
qqline(residuo.kid.no2_2)
hist(residuo.kid.no2_2)
shapiro.test(residuo.kid.no2_2)


modelo.kid.no2_3 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv + s(Temp.Ar) + 
                                s(Umidade.Ar),
                              data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_3)
residuo.kid.no2_3 <- residuals(modelo.kid.no2_3)
plot(residuo.kid.no2_3)
acf(residuo.kid.no2_3)
qqnorm(residuo.kid.no2_3)
qqline(residuo.kid.no2_3)
hist(residuo.kid.no2_3)
shapiro.test(residuo.kid.no2_3)


modelo.kid.no2_4 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv + s(Temp.Ar) + 
                                Umidade.Ar,
                              data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_4)
residuo.kid.no2_4 <- residuals(modelo.kid.no2_4)
plot(residuo.kid.no2_4)
acf(residuo.kid.no2_4)
qqnorm(residuo.kid.no2_4)
qqline(residuo.kid.no2_4)
hist(residuo.kid.no2_4)
shapiro.test(residuo.kid.no2_4)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM no2 COM CRIANCA ###################
###################################### Lag 1 ########################################
modelo.kid.no2_5 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.1 + Temp.Ar + 
                                Umidade.Ar,
                              data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_5)
residuo.kid.no2_5 <- residuals(modelo.kid.no2_5)
plot(residuo.kid.no2_5)
acf(residuo.kid.no2_5)
qqnorm(residuo.kid.no2_5)
qqline(residuo.kid.no2_5)
hist(residuo.kid.no2_5)


modelo.kid.no2_6 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.1 + Temp.Ar + 
                                s(Umidade.Ar),
                              data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_6)
residuo.kid.no2_6 <- residuals(modelo.kid.no2_6)
plot(residuo.kid.no2_6)
acf(residuo.kid.no2_6)
qqnorm(residuo.kid.no2_6)
qqline(residuo.kid.no2_6)
hist(residuo.kid.no2_6)
shapiro.test(residuo.kid.no2_6)


modelo.kid.no2_7 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.1 + s(Temp.Ar) + 
                                s(Umidade.Ar),
                              data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_7)
residuo.kid.no2_7 <- residuals(modelo.kid.no2_7)
plot(residuo.kid.no2_7)
acf(residuo.kid.no2_7)
qqnorm(residuo.kid.no2_7)
qqline(residuo.kid.no2_7)
hist(residuo.kid.no2_7)
shapiro.test(residuo.kid.no2_7)


modelo.kid.no2_8 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.1 + s(Temp.Ar) + 
                                Umidade.Ar,
                              data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_8)
residuo.kid.no2_8 <- residuals(modelo.kid.no2_8)
plot(residuo.kid.no2_8)
acf(residuo.kid.no2_8)
qqnorm(residuo.kid.no2_8)
qqline(residuo.kid.no2_8)
hist(residuo.kid.no2_8)
shapiro.test(residuo.kid.no2_8)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM no2  COM CRIANCA ###################
###################################### Lag 4 ########################################
modelo.kid.no2_9 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.4 + Temp.Ar + 
                                Umidade.Ar,
                              data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_9) 
residuo.kid.no2_9 <- residuals(modelo.kid.no2_9)
plot(residuo.kid.no2_9)
acf(residuo.kid.no2_9)
qqnorm(residuo.kid.no2_9)
qqline(residuo.kid.no2_9)
hist(residuo.kid.no2_9)


modelo.kid.no2_10 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.4 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_10)
residuo.kid.no2_10 <- residuals(modelo.kid.no2_10)
plot(residuo.kid.no2_10)
acf(residuo.kid.no2_10)
qqnorm(residuo.kid.no2_10)
qqline(residuo.kid.no2_10)
hist(residuo.kid.no2_10)
shapiro.test(residuo.kid.no2_10)


modelo.kid.no2_11 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.4 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_11)
residuo.kid.no2_11 <- residuals(modelo.kid.no2_11)
plot(residuo.kid.no2_11)
acf(residuo.kid.no2_11)
qqnorm(residuo.kid.no2_11)
qqline(residuo.kid.no2_11)
hist(residuo.kid.no2_11)
shapiro.test(residuo.kid.no2_11)


modelo.kid.no2_12 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.4 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_12)
residuo.kid.no2_12 <- residuals(modelo.kid.no2_12)
plot(residuo.kid.no2_12)
acf(residuo.kid.no2_12)
qqnorm(residuo.kid.no2_12)
qqline(residuo.kid.no2_12)
hist(residuo.kid.no2_12)
shapiro.test(residuo.kid.no2_12)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM no2 COM CRIANCA ###################
###################################### Lag 5 ########################################
modelo.kid.no2_13 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.5 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_13) 
residuo.kid.no2_13 <- residuals(modelo.kid.no2_13)
plot(residuo.kid.no2_13)
acf(residuo.kid.no2_13)
qqnorm(residuo.kid.no2_13)
qqline(residuo.kid.no2_13)
hist(residuo.kid.no2_13)


modelo.kid.no2_14 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.5 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_14)
residuo.kid.no2_14 <- residuals(modelo.kid.no2_14)
plot(residuo.kid.no2_14)
acf(residuo.kid.no2_14)
qqnorm(residuo.kid.no2_14)
qqline(residuo.kid.no2_14)
hist(residuo.kid.no2_14)
shapiro.test(residuo.kid.no2_14)


modelo.kid.no2_15 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.5 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_15)
residuo.kid.no2_15 <- residuals(modelo.kid.no2_15)
plot(residuo.kid.no2_15)
acf(residuo.kid.no2_15)
qqnorm(residuo.kid.no2_15)
qqline(residuo.kid.no2_15)
hist(residuo.kid.no2_15)
shapiro.test(residuo.kid.no2_15)


modelo.kid.no2_16 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.5 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_16)
residuo.kid.no2_16 <- residuals(modelo.kid.no2_16)
plot(residuo.kid.no2_16)
acf(residuo.kid.no2_16)
qqnorm(residuo.kid.no2_16)
qqline(residuo.kid.no2_16)
hist(residuo.kid.no2_16)
shapiro.test(residuo.kid.no2_16)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM no2 COM CRIANCA ###################
###################################### Lag 6 ########################################
modelo.kid.no2_17 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.6 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_17) 
residuo.kid.no2_17 <- residuals(modelo.kid.no2_17)
plot(residuo.kid.no2_17)
acf(residuo.kid.no2_17)
qqnorm(residuo.kid.no2_17)
qqline(residuo.kid.no2_17)
hist(residuo.kid.no2_17)


modelo.kid.no2_18 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.6 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_18)
residuo.kid.no2_18 <- residuals(modelo.kid.no2_18)
plot(residuo.kid.no2_18)
acf(residuo.kid.no2_18)
qqnorm(residuo.kid.no2_18)
qqline(residuo.kid.no2_18)
hist(residuo.kid.no2_18)
shapiro.test(residuo.kid.no2_18)


modelo.kid.no2_19 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.6 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_19)
residuo.kid.no2_19 <- residuals(modelo.kid.no2_19)
plot(residuo.kid.no2_19)
acf(residuo.kid.no2_19)
qqnorm(residuo.kid.no2_19)
qqline(residuo.kid.no2_19)
hist(residuo.kid.no2_19)
shapiro.test(residuo.kid.no2_19)


modelo.kid.no2_20 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.6 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_20)
residuo.kid.no2_20 <- residuals(modelo.kid.no2_20)
plot(residuo.kid.no2_20)
acf(residuo.kid.no2_20)
qqnorm(residuo.kid.no2_20)
qqline(residuo.kid.no2_20)
hist(residuo.kid.no2_20)
shapiro.test(residuo)


################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM no2 COM CRIANCA ###################
###################################### Lag 7 ########################################
modelo.kid.no2_21 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.7 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_21)  
residuo.kid.no2_21 <- residuals(modelo.kid.no2_21)
plot(residuo.kid.no2_21)
acf(residuo.kid.no2_21)
qqnorm(residuo.kid.no2_21)
qqline(residuo.kid.no2_21)
hist(residuo.kid.no2_21)


modelo.kid.no2_22 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.7 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_22)
residuo.kid.no2_22 <- residuals(modelo.kid.no2_22)
plot(residuo.kid.no2_22)
acf(residuo.kid.no2_22)
qqnorm(residuo.kid.no2_22)
qqline(residuo.kid.no2_22)
hist(residuo.kid.no2_22)
shapiro.test(residuo.kid.no2_22)


modelo.kid.no2_23 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.7 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_23)
residuo.kid.no2_23 <- residuals(modelo.kid.no2_23)
plot(residuo.kid.no2_23)
acf(residuo.kid.no2_23)
qqnorm(residuo.kid.no2_23)
qqline(residuo.kid.no2_23)
hist(residuo.kid.no2_23)
shapiro.test(residuo.kid.no2_23)


modelo.kid.no2_24 <- mgcv::gam(SIH_GV_J_Kid ~ no2.gv.d.7 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.no2_24)
residuo.kid.no2_24 <- residuals(modelo.kid.no2_24)
plot(residuo.kid.no2_24)
acf(residuo.kid.no2_24)
qqnorm(residuo.kid.no2_24)
qqline(residuo.kid.no2_24)
hist(residuo.kid.no2_24)
shapiro.test(residuo.kid.no2_24)


library(bbmle)
AIC.mod.no2_kid <- AICctab(modelo.kid.no2_1, modelo.kid.no2_2, modelo.kid.no2_3,
                           modelo.kid.no2_4, modelo.kid.no2_5, modelo.kid.no2_6,
                           modelo.kid.no2_7, modelo.kid.no2_8, modelo.kid.no2_9,
                           modelo.kid.no2_10, modelo.kid.no2_11, modelo.kid.no2_12,
                           modelo.kid.no2_13, modelo.kid.no2_14, modelo.kid.no2_15,
                           modelo.kid.no2_16, modelo.kid.no2_17, modelo.kid.no2_18,
                           modelo.kid.no2_19, modelo.kid.no2_20, modelo.kid.no2_21,
                           modelo.kid.no2_22, modelo.kid.no2_23, modelo.kid.no2_24,
                           base = T, weights=T)
AIC.mod.no2_kid

anova.gam.no2_kid <- anova.gam(modelo.kid.no2_1, modelo.kid.no2_2, modelo.kid.no2_3,
                               modelo.kid.no2_4, modelo.kid.no2_5, modelo.kid.no2_6,
                               modelo.kid.no2_7, modelo.kid.no2_8, modelo.kid.no2_9,
                               modelo.kid.no2_10, modelo.kid.no2_11, modelo.kid.no2_12,
                               modelo.kid.no2_13, modelo.kid.no2_14, modelo.kid.no2_15,
                               modelo.kid.no2_16, modelo.kid.no2_17, modelo.kid.no2_18,
                               modelo.kid.no2_19, modelo.kid.no2_20, modelo.kid.no2_21,
                               modelo.kid.no2_22, modelo.kid.no2_23, modelo.kid.no2_24)

anova.gam.no2_kid

anova.no2_kid <-anova(modelo.kid.no2_1, modelo.kid.no2_2, modelo.kid.no2_3,
                      modelo.kid.no2_4, modelo.kid.no2_5, modelo.kid.no2_6,
                      modelo.kid.no2_7, modelo.kid.no2_8, modelo.kid.no2_9,
                      modelo.kid.no2_10, modelo.kid.no2_11, modelo.kid.no2_12,
                      modelo.kid.no2_13, modelo.kid.no2_14, modelo.kid.no2_15,
                      modelo.kid.no2_16, modelo.kid.no2_17, modelo.kid.no2_18,
                      modelo.kid.no2_19, modelo.kid.no2_20, modelo.kid.no2_21,
                      modelo.kid.no2_22, modelo.kid.no2_23, modelo.kid.no2_24)


anova.no2_kid

################################ CID I ############################################
###################### MODELO UNI-POLUENTE COM no2COM IDOSOS ###################
###################################### Lag 0 ########################################
modelo.old.I.no2_1 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv + Temp.Ar + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_1) 
residuo.old.I.no2_1 <- residuals(modelo.old.I.no2_1)
plot(residuo.old.I.no2_1)
acf(residuo.old.I.no2_1)
qqnorm(residuo.old.I.no2_1)
qqline(residuo.old.I.no2_1)
hist(residuo.old.I.no2_1)


modelo.old.I.no2_2 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_2)
residuo.old.I.no2_2 <- residuals(modelo.old.I.no2_2)
plot(residuo.old.I.no2_2)
acf(residuo.old.I.no2_2)
qqnorm(residuo.old.I.no2_2)
qqline(residuo.old.I.no2_2)
hist(residuo.old.I.no2_2)
shapiro.test(residuo.old.I.no2_2)


modelo.old.I.no2_3 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_3)
residuo.old.I.no2_3 <- residuals(modelo.old.I.no2_3)
plot(residuo.old.I.no2_3)
acf(residuo.old.I.no2_3)
qqnorm(residuo.old.I.no2_3)
qqline(residuo.old.I.no2_3)
hist(residuo.old.I.no2_3)
shapiro.test(residuo.old.I.no2_3)


modelo.old.I.no2_4 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_4)
residuo.old.I.no2_4 <- residuals(modelo.old.I.no2_4)
plot(residuo.old.I.no2_4)
acf(residuo.old.I.no2_4)
qqnorm(residuo.old.I.no2_4)
qqline(residuo.old.I.no2_4)
hist(residuo.old.I.no2_4)
shapiro.test(residuo.old.I.no2_4)


################################ CID I ############################################
###################### MODELO UNI-POLUENTE COM no2 COM IDOSOS ###################
###################################### Lag 1 ########################################
modelo.old.I.no2_5 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.1 + Temp.Ar + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_5) 
residuo.old.I.no2_5 <- residuals(modelo.old.I.no2_5)
plot(residuo.old.I.no2_5)
acf(residuo.old.I.no2_5)
qqnorm(residuo.old.I.no2_5)
qqline(residuo.old.I.no2_5)
hist(residuo.old.I.no2_5)


modelo.old.I.no2_6 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.1 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_6)
residuo.old.I.no2_6 <- residuals(modelo.old.I.no2_6)
plot(residuo.old.I.no2_6)
acf(residuo.old.I.no2_6)
qqnorm(residuo.old.I.no2_6)
qqline(residuo.old.I.no2_6)
hist(residuo.old.I.no2_6)
shapiro.test(residuo.old.I.no2_6)


modelo.old.I.no2_7 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.1 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_7)
residuo.old.I.no2_7 <- residuals(modelo.old.I.no2_7)
plot(residuo.old.I.no2_7)
acf(residuo.old.I.no2_7)
qqnorm(residuo.old.I.no2_7)
qqline(residuo.old.I.no2_7)
hist(residuo.old.I.no2_7)
shapiro.test(residuo.old.I.no2_7)


modelo.old.I.no2_8 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.1 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_8)
residuo.old.I.no2_8 <- residuals(modelo.old.I.no2_8)
plot(residuo.old.I.no2_8)
acf(residuo.old.I.no2_8)
qqnorm(residuo.old.I.no2_8)
qqline(residuo.old.I.no2_8)
hist(residuo.old.I.no2_8)
shapiro.test(residuo.old.I.no2_8)

################################ CID I ############################################
###################### MODELO UNI-POLUENTE COM no2 COM IDOSOS ###################
###################################### Lag 4 ########################################
modelo.old.I.no2_9 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.4 + Temp.Ar + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_9) 
residuo.old.I.no2_9 <- residuals(modelo.old.I.no2_9)
plot(residuo.old.I.no2_9)
acf(residuo.old.I.no2_9)
qqnorm(residuo.old.I.no2_9)
qqline(residuo.old.I.no2_9)
hist(residuo.old.I.no2_9)


modelo.old.I.no2_10 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.4 + Temp.Ar + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_10)
residuo.old.I.no2_10 <- residuals(modelo.old.I.no2_10)
plot(residuo.old.I.no2_10)
acf(residuo.old.I.no2_10)
qqnorm(residuo.old.I.no2_10)
qqline(residuo.old.I.no2_10)
hist(residuo.old.I.no2_10)
shapiro.test(residuo.old.I.no2_10)


modelo.old.I.no2_11 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.4 + s(Temp.Ar) + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_11)
residuo.old.I.no2_11 <- residuals(modelo.old.I.no2_11)
plot(residuo.old.I.no2_11)
acf(residuo.old.I.no2_11)
qqnorm(residuo.old.I.no2_11)
qqline(residuo.old.I.no2_11)
hist(residuo.old.I.no2_11)
shapiro.test(residuo.old.I.no2_11)


modelo.old.I.no2_12 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.4 + s(Temp.Ar) + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_12)
residuo.old.I.no2_12 <- residuals(modelo.old.I.no2_12)
plot(residuo.old.I.no2_12)
acf(residuo.old.I.no2_12)
qqnorm(residuo.old.I.no2_12)
qqline(residuo.old.I.no2_12)
hist(residuo.old.I.no2_12)
shapiro.test(residuo.old.I.no2_12)



################################ CID I ############################################
###################### MODELO UNI-POLUENTE COM no2 COM IDOSOS ###################
###################################### Lag 5 ########################################
modelo.old.I.no2_13 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.5 + Temp.Ar + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_13) 
residuo.old.I.no2_13 <- residuals(modelo.old.I.no2_13)
plot(residuo.old.I.no2_13)
acf(residuo.old.I.no2_13)
qqnorm(residuo.old.I.no2_13)
qqline(residuo.old.I.no2_13)
hist(residuo.old.I.no2_13)


modelo.old.I.no2_14 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.5 + Temp.Ar + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_14)
residuo.old.I.no2_14 <- residuals(modelo.old.I.no2_14)
plot(residuo.old.I.no2_14)
acf(residuo.old.I.no2_14)
qqnorm(residuo.old.I.no2_14)
qqline(residuo.old.I.no2_14)
hist(residuo.old.I.no2_14)
shapiro.test(residuo.old.I.no2_14)


modelo.old.I.no2_15 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.5 + s(Temp.Ar) + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_15)
residuo.old.I.no2_15 <- residuals(modelo.old.I.no2_15)
plot(residuo.old.I.no2_15)
acf(residuo.old.I.no2_15)
qqnorm(residuo.old.I.no2_15)
qqline(residuo.old.I.no2_15)
hist(residuo.old.I.no2_15)
shapiro.test(residuo.old.I.no2_15)


modelo.old.I.no2_16 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.5 + s(Temp.Ar) + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_16)
residuo.old.I.no2_16 <- residuals(modelo.old.I.no2_16)
plot(residuo.old.I.no2_16)
acf(residuo.old.I.no2_16)
qqnorm(residuo.old.I.no2_16)
qqline(residuo.old.I.no2_16)
hist(residuo.old.I.no2_16)
shapiro.test(residuo.old.I.no2_16)

################################ CID I ############################################
###################### MODELO UNI-POLUENTE COM no2 COM IDOSOS ###################
###################################### Lag 6 ########################################
modelo.old.I.no2_17 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.6 + Temp.Ar + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_17) 
residuo.old.I.no2_17 <- residuals(modelo.old.I.no2_17)
plot(residuo.old.I.no2_17)
acf(residuo.old.I.no2_17)
qqnorm(residuo.old.I.no2_17)
qqline(residuo.old.I.no2_17)
hist(residuo.old.I.no2_17)


modelo.old.I.no2_18 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.6 + Temp.Ar + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_18)
residuo.old.I.no2_18 <- residuals(modelo.old.I.no2_18)
plot(residuo.old.I.no2_18)
acf(residuo.old.I.no2_18)
qqnorm(residuo.old.I.no2_18)
qqline(residuo.old.I.no2_18)
hist(residuo.old.I.no2_18)
shapiro.test(residuo.old.I.no2_18)


modelo.old.I.no2_19 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.6 + s(Temp.Ar) + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_19)
residuo.old.I.no2_19 <- residuals(modelo.old.I.no2_19)
plot(residuo.old.I.no2_19)
acf(residuo.old.I.no2_19)
qqnorm(residuo.old.I.no2_19)
qqline(residuo.old.I.no2_19)
hist(residuo.old.I.no2_19)
shapiro.test(residuo.old.I.no2_19)


modelo.old.I.no2_20 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.6 + s(Temp.Ar) + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_20)
residuo.old.I.no2_20 <- residuals(modelo.old.I.no2_20)
plot(residuo.old.I.no2_20)
acf(residuo.old.I.no2_20)
qqnorm(residuo.old.I.no2_20)
qqline(residuo.old.I.no2_20)
hist(residuo.old.I.no2_20)
shapiro.test(residuo.old.I.no2_20)


################################ CID I ############################################
###################### MODELO UNI-POLUENTE COM no2 COM IDOSOS ###################
###################################### Lag 7 ########################################
modelo.old.I.no2_21<- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.7 + Temp.Ar + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_21) 
residuo.old.I.no2_21<- residuals(modelo.old.I.no2_21)
plot(residuo.old.I.no2_21)
acf(residuo.old.I.no2_21)
qqnorm(residuo.old.I.no2_21)
qqline(residuo.old.I.no2_21)
hist(residuo.old.I.no2_21)


modelo.old.I.no2_22 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.7 + Temp.Ar + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_22)
residuo.old.I.no2_22 <- residuals(modelo.old.I.no2_22)
plot(residuo.old.I.no2_22)
acf(residuo.old.I.no2_22)
qqnorm(residuo.old.I.no2_22)
qqline(residuo.old.I.no2_22)
hist(residuo.old.I.no2_22)
shapiro.test(residuo.old.I.no2_22)


modelo.old.I.no2_23 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.7 + s(Temp.Ar) + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_23)
residuo.old.I.no2_23 <- residuals(modelo.old.I.no2_23)
plot(residuo.old.I.no2_23)
acf(residuo.old.I.no2_23)
qqnorm(residuo.old.I.no2_23)
qqline(residuo.old.I.no2_23)
hist(residuo.old.I.no2_23)
shapiro.test(residuo.old.I.no2_23)


modelo.old.I.no2_24 <- mgcv::gam(SIH_GV_I_Old ~ no2.gv.d.7 + s(Temp.Ar) + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.no2_24)
residuo.old.I.no2_24 <- residuals(modelo.old.I.no2_24)
plot(residuo.old.I.no2_24)
acf(residuo.old.I.no2_24)
qqnorm(residuo.old.I.no2_24)
qqline(residuo.old.I.no2_24)
hist(residuo.old.I.no2_24)
shapiro.test(residuo.old.I.no2_24)



library(bbmle)
AIC.mod.no2_old.I <- AICctab(modelo.old.I.no2_1, modelo.old.I.no2_2, modelo.old.I.no2_3,
                             modelo.old.I.no2_4, modelo.old.I.no2_5, modelo.old.I.no2_6,
                             modelo.old.I.no2_7, modelo.old.I.no2_8, modelo.old.I.no2_9,
                             modelo.old.I.no2_10, modelo.old.I.no2_11, modelo.old.I.no2_12,
                             modelo.old.I.no2_13, modelo.old.I.no2_14, modelo.old.I.no2_15,
                             modelo.old.I.no2_16, modelo.old.I.no2_17, modelo.old.I.no2_18,
                             modelo.old.I.no2_19, modelo.old.I.no2_20, modelo.old.I.no2_21,
                             modelo.old.I.no2_22, modelo.old.I.no2_23, modelo.old.I.no2_24,
                             base = T, weights=T)
AIC.mod.no2_old.I

anova.gam.no2_old.I <- anova.gam(modelo.old.I.no2_1, modelo.old.I.no2_2, modelo.old.I.no2_3,
                                 modelo.old.I.no2_4, modelo.old.I.no2_5, modelo.old.I.no2_6,
                                 modelo.old.I.no2_7, modelo.old.I.no2_8, modelo.old.I.no2_9,
                                 modelo.old.I.no2_10, modelo.old.I.no2_11, modelo.old.I.no2_12,
                                 modelo.old.I.no2_13, modelo.old.I.no2_14, modelo.old.I.no2_15,
                                 modelo.old.I.no2_16, modelo.old.I.no2_17, modelo.old.I.no2_18,
                                 modelo.old.I.no2_19, modelo.old.I.no2_20, modelo.old.I.no2_21,
                                 modelo.old.I.no2_22, modelo.old.I.no2_23, modelo.old.I.no2_24)

anova.no2_old.I   <- anova(modelo.old.I.no2_1, modelo.old.I.no2_2, modelo.old.I.no2_3,
                           modelo.old.I.no2_4, modelo.old.I.no2_5, modelo.old.I.no2_6,
                           modelo.old.I.no2_7, modelo.old.I.no2_8, modelo.old.I.no2_9,
                           modelo.old.I.no2_10, modelo.old.I.no2_11, modelo.old.I.no2_12, 
                           modelo.old.I.no2_13, modelo.old.I.no2_14, modelo.old.I.no2_15,
                           modelo.old.I.no2_16, modelo.old.I.no2_17, modelo.old.I.no2_18,
                           modelo.old.I.no2_19, modelo.old.I.no2_20, modelo.old.I.no2_21,
                           modelo.old.I.no2_22, modelo.old.I.no2_23, modelo.old.I.no2_24)

