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

dados.pm25 <- data.frame(SIH_GV_I_Old, SIH_GV_J_Kid, SIH_GV_J_Old,
                         pm25.gv, pm25.gv.d.1, pm25.gv.d.2, pm25.gv.d.3,
                         pm25.gv.d.4, pm25.gv.d.5, pm25.gv.d.6, pm25.gv.d.7,
                         Temp.Ar, Temp.maxima, Temp.minima, Umidade.Ar,
                         Precip.acumulada)

library(ggplot2)
library(ggcorrplot)

corre.pm25 <- cor(dados.pm25, y = NULL,  use = "na.or.complete")


ggcorrplot::ggcorrplot(corre.pm25, type = "lower", 
                       #hc.order = TRUE,
                       #method = "circle", lab = TRUE,
                       lab_size = 3,
                       title = "MATRIZ DE CORRELAÇÃO PM25 - CID - METEOROLOGICA",
                       ggtheme = theme_bw(),
                       p.mat = p.mat,
                       sig.level = TRUE)



attach(dados)

#####################################################################################
############################# MODELOS COM O GAM ####################################

library("gam")
library("mgcv")

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM2,5 COM IDOSOS ###################
###################################### Lag 0 ########################################
modelo.old.pm25.1 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv + Temp.Ar + 
                                Umidade.Ar,
                           data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.1) #umidade não significativo
residuo <- residuals(modelo.old.pm25.1)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.pm25.2 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.2)
residuo <- residuals(modelo.old.pm25.2)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm25.3 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.3)
residuo <- residuals(modelo.old.pm25.3)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm25.4 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.4)
residuo <- residuals(modelo.old.pm25.4)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM2,5 COM IDOSOS ###################
###################################### Lag 1 ########################################
modelo.old.pm25.5 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.1 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.5) 
residuo <- residuals(modelo.old.pm25.5)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.pm25.6 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.1 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.6)
residuo <- residuals(modelo.old.pm25.6)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm25.7 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.1 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.7)
residuo <- residuals(modelo.old.pm25.7)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm25.8 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.1 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.8)
residuo <- residuals(modelo.old.pm25.8)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM2,5 COM IDOSOS ###################
###################################### Lag 4 ########################################
modelo.old.pm25.9 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.4 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.9)
residuo <- residuals(modelo.old.pm25.9)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.pm25.10 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.4 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.10)
residuo <- residuals(modelo.old.pm25.10)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm25.11 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.4 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.11)
residuo <- residuals(modelo.old.pm25.11)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm25.12 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.4 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.12)
residuo <- residuals(modelo.old.pm25.12)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM2,5 COM IDOSOS ###################
###################################### Lag 5 ########################################
modelo.old.pm25.13 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.5 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.13) #umidade não significativo
residuo <- residuals(modelo.old.pm25.13)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

modelo.old.pm25.14 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.5 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.14)
residuo <- residuals(modelo.old.pm25.14)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm25.15 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.5 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.15)
residuo <- residuals(modelo.old.pm25.15)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm25.16 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.4 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.16)
residuo <- residuals(modelo.old.pm25.16)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM2,5 COM IDOSOS ###################
###################################### Lag 6 ########################################
modelo.old.pm25.17 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.6 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.17) #umidade não significativo
residuo <- residuals(modelo.old.pm25.17)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.pm25.18 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.6 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.18)
residuo <- residuals(modelo.old.pm25.18)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm25.19 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.6 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.19)
residuo <- residuals(modelo.old.pm25.19)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm25.20 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.6 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.20)
residuo <- residuals(modelo.old.pm25.20)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM2,5 COM IDOSOS ###################
###################################### Lag 7 ########################################
modelo.old.pm25.21 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.7 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.21) #umidade não significativo
residuo <- residuals(modelo.old.pm25.21)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.old.pm25.22 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.7 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.22)
residuo <- residuals(modelo.old.pm25.22)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm25.23 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.7 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.23)
residuo <- residuals(modelo.old.pm25.23)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.pm25.24 <- mgcv::gam(SIH_GV_J_Old ~ pm25.gv.d.7 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.pm25.24)
residuo <- residuals(modelo.old.pm25.24)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)



library(bbmle)
AIC.mod.pm25.old <- AICctab(modelo.old.pm25.1, modelo.old.pm25.2, modelo.old.pm25.3,
                            modelo.old.pm25.4, modelo.old.pm25.5, modelo.old.pm25.6,
                            modelo.old.pm25.7, modelo.old.pm25.8, modelo.old.pm25.9,
                            modelo.old.pm25.10, modelo.old.pm25.11, modelo.old.pm25.12,
                            modelo.old.pm25.13, modelo.old.pm25.14, modelo.old.pm25.15,
                            modelo.old.pm25.16, modelo.old.pm25.17, modelo.old.pm25.18,
                            modelo.old.pm25.19, modelo.old.pm25.20, modelo.old.pm25.21,
                            modelo.old.pm25.22, modelo.old.pm25.23, modelo.old.pm25.24,
                            base = T, weights=T)
AIC.mod.pm25.old

anova.gam(modelo.old.pm25.1, modelo.old.pm25.2, modelo.old.pm25.3,
          modelo.old.pm25.4, modelo.old.pm25.5, modelo.old.pm25.6,
          modelo.old.pm25.7, modelo.old.pm25.8, modelo.old.pm25.9,
          modelo.old.pm25.10, modelo.old.pm25.11, modelo.old.pm25.12,
          modelo.old.pm25.13, modelo.old.pm25.14, modelo.old.pm25.15,
          modelo.old.pm25.16, modelo.old.pm25.17, modelo.old.pm25.18,
          modelo.old.pm25.19, modelo.old.pm25.20, modelo.old.pm25.21,
          modelo.old.pm25.22, modelo.old.pm25.23, modelo.old.pm25.24)

anova(modeloo.ld.pm25.1, modelo.old.pm25.2, modelo.old.pm25.3,
      modelo.old.pm25.4, modelo.old.pm25.5, modelo.old.pm25.6,
      modelo.old.pm25.7, modelo.old.pm25.8, modelo.old.pm25.9,
      modelo.old.pm25.10, modelo.old.pm25.11, modelo.old.pm25.12,
      modelo.old.pm25.13, modelo.old.pm25.14, modelo.old.pm25.15,
      modelo.old.pm25.16, modelo.old.pm25.17, modelo.old.pm25.18,
      modelo.old.pm25.19, modelo.old.pm25.20, modelo.old.pm25.21,
      modelo.old.pm25.22, modelo.old.pm25.23, modelo.old.pm25.24)


################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM2,5 COM CRIANCA ###################
###################################### Lag 0 ########################################
modelo.kid.pm25.1 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.1) 
residuo <- residuals(modelo.kid.pm25.1)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

modelo.kid.pm25.2 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.2)
residuo <- residuals(modelo.kid.pm25.2)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.3 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.3)
residuo <- residuals(modelo.kid.pm25.3)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.4 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.4)
residuo <- residuals(modelo.kid.pm25.4)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM2,5 COM CRIANCA ###################
###################################### Lag 1 ########################################
modelo.kid.pm25.5 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.1 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.5) 
residuo <- residuals(modelo.kid.pm25.5)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.6 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.1 + Temp.Ar + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.6)
residuo <- residuals(modelo.kid.pm25.6)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.7 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.1 + s(Temp.Ar) + 
                                 s(Umidade.Ar),
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.7)
residuo <- residuals(modelo.kid.pm25.7)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.8 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.1 + s(Temp.Ar) + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.8)
residuo <- residuals(modelo.kid.pm25.8)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM2,5 COM CRIANCA ###################
###################################### Lag 4 ########################################
modelo.kid.pm25.9 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.4 + Temp.Ar + 
                                 Umidade.Ar,
                               data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.9) 
residuo <- residuals(modelo.kid.pm25.9)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.10 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.4 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.10)
residuo <- residuals(modelo.kid.pm25.10)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.11 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.4 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.11)
residuo <- residuals(modelo.kid.pm25.11)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.12 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.4 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.12)
residuo <- residuals(modelo.kid.pm25.12)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM2,5 COM CRIANCA ###################
###################################### Lag 5 ########################################
modelo.kid.pm25.13 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.5 + Temp.Ar + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.13) 
residuo <- residuals(modelo.kid.pm25.13)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.14 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.5 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.14)
residuo <- residuals(modelo.kid.pm25.14)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.15 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.5 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.15)
residuo <- residuals(modelo.kid.pm25.15)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.16 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.4 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.16)
residuo <- residuals(modelo.kid.pm25.16)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM2,5 COM CRIANCA ###################
###################################### Lag 6 ########################################
modelo.kid.pm25.17 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.6 + Temp.Ar + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.17) 
residuo <- residuals(modelo.kid.pm25.17)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.18 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.6 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.18)
residuo <- residuals(modelo.kid.pm25.18)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.19 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.6 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.19)
residuo <- residuals(modelo.kid.pm25.19)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.20 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.6 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.20)
residuo <- residuals(modelo.kid.pm25.20)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM2,5 COM CRIANCA ###################
###################################### Lag 7 ########################################
modelo.kid.pm25.21 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.7 + Temp.Ar + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.21)
residuo <- residuals(modelo.kid.pm25.21)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.22 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.7 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.22)
residuo <- residuals(modelo.kid.pm25.22)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.23 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.7 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.23)
residuo <- residuals(modelo.kid.pm25.23)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.24 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.7 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.24)
residuo <- residuals(modelo.kid.pm25.24)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM2,5 COM CRIANCA ###################
###################################### Lag 3 ########################################
modelo.kid.pm25.25 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.3 + Temp.Ar + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.25)
residuo <- residuals(modelo.kid.pm25.25)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)


modelo.kid.pm25.26 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.3 + Temp.Ar + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.26)
residuo <- residuals(modelo.kid.pm25.26)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.27 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.3 + s(Temp.Ar) + 
                                  s(Umidade.Ar),
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.27)
residuo <- residuals(modelo.kid.pm25.27)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.kid.pm25.28 <- mgcv::gam(SIH_GV_J_Kid ~ pm25.gv.d.3 + s(Temp.Ar) + 
                                  Umidade.Ar,
                                data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.kid.pm25.28)
residuo <- residuals(modelo.kid.pm25.28)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

library(bbmle)
AIC.mod.pm25.kid <- AICctab(modelo.kid.pm25.1, modelo.kid.pm25.2, modelo.kid.pm25.3,
                            modelo.kid.pm25.4, modelo.kid.pm25.5, modelo.kid.pm25.6,
                            modelo.kid.pm25.7, modelo.kid.pm25.8, modelo.kid.pm25.9,
                            modelo.kid.pm25.10, modelo.kid.pm25.11, modelo.kid.pm25.12,
                            modelo.kid.pm25.13, modelo.kid.pm25.14, modelo.kid.pm25.15,
                            modelo.kid.pm25.16, modelo.kid.pm25.17, modelo.kid.pm25.18,
                            modelo.kid.pm25.19, modelo.kid.pm25.20, modelo.kid.pm25.21,
                            modelo.kid.pm25.22, modelo.kid.pm25.23, modelo.kid.pm25.24,
                            base = T, weights=T)
AIC.mod.pm25.kid

anova.gam(modelo.kid.pm25.1, modelo.kid.pm25.2, modelo.kid.pm25.3,
          modelo.kid.pm25.4, modelo.kid.pm25.5, modelo.kid.pm25.6,
          modelo.kid.pm25.7, modelo.kid.pm25.8, modelo.kid.pm25.9,
          modelo.kid.pm25.10, modelo.kid.pm25.11, modelo.kid.pm25.12,
          modelo.kid.pm25.13, modelo.kid.pm25.14, modelo.kid.pm25.15,
          modelo.kid.pm25.16, modelo.kid.pm25.17, modelo.kid.pm25.18,
          modelo.kid.pm25.19, modelo.kid.pm25.20, modelo.kid.pm25.21,
          modelo.kid.pm25.22, modelo.kid.pm25.23, modelo.kid.pm25.24)

anova(modelo.kid.pm25.1, modelo.kid.pm25.2, modelo.kid.pm25.3,
      modelo.kid.pm25.4, modelo.kid.pm25.5, modelo.kid.pm25.6,
      modelo.kid.pm25.7, modelo.kid.pm25.8, modelo.kid.pm25.9,
      modelo.kid.pm25.10, modelo.kid.pm25.11, modelo.kid.pm25.12,
      modelo.kid.pm25.13, modelo.kid.pm25.14, modelo.kid.pm25.15,
      modelo.kid.pm25.16, modelo.kid.pm25.17, modelo.kid.pm25.18,
      modelo.kid.pm25.19, modelo.kid.pm25.20, modelo.kid.pm25.21,
      modelo.kid.pm25.22, modelo.kid.pm25.23, modelo.kid.pm25.24)


################################ CID I ############################################
###################### MODELO UNI-POLUENTE COM PM2,5 COM IDOSOS ###################
###################################### Lag 0 ########################################
modelo.old.I.I.pm25.1 <- mgcv::gam(SIH_GV_I_Old ~ pm25.gv + Temp.Ar + 
                                     Umidade.Ar,
                                   data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.I.pm25.1) 
residuo <- residuals(modelo.old.I.I.pm25.1)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.pm25.2 <- mgcv::gam(SIH_GV_I_Old ~ pm25.gv + Temp.Ar + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm25.2)
residuo <- residuals(modelo.old.I.pm25.2)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.pm25.3 <- mgcv::gam(SIH_GV_I_Old ~ pm25.gv + s(Temp.Ar) + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm25.3)
residuo <- residuals(modelo.old.I.pm25.3)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.pm25.4 <- mgcv::gam(SIH_GV_I_Old ~ pm25.gv + s(Temp.Ar) + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm25.4)
residuo <- residuals(modelo.old.I.pm25.4)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


################################ CID I ############################################
###################### MODELO UNI-POLUENTE COM PM2,5 COM IDOSOS ###################
###################################### Lag 6 ########################################
modelo.old.I.pm25.5 <- mgcv::gam(SIH_GV_I_Old ~ pm25.gv.d.6 + Temp.Ar + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm25.5) 
residuo <- residuals(modelo.old.I.pm25.5)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

modelo.old.I.pm25.6 <- mgcv::gam(SIH_GV_I_Old ~ pm25.gv.d.6 + Temp.Ar + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm25.6)
residuo <- residuals(modelo.old.I.pm25.6)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.pm25.7 <- mgcv::gam(SIH_GV_I_Old ~ pm25.gv.d.6 + s(Temp.Ar) + 
                                   s(Umidade.Ar),
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm25.7)
residuo <- residuals(modelo.old.I.pm25.7)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.pm25.8 <- mgcv::gam(SIH_GV_I_Old ~ pm25.gv.d.6 + s(Temp.Ar) + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm25.8)
residuo <- residuals(modelo.old.I.pm25.8)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)

################################ CID J ############################################
###################### MODELO UNI-POLUENTE COM PM2,5 COM IDOSOS ###################
###################################### Lag 7 ########################################
modelo.old.I.pm25.9 <- mgcv::gam(SIH_GV_I_Old ~ pm25.gv.d.7 + Temp.Ar + 
                                   Umidade.Ar,
                                 data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm25.9) 
residuo <- residuals(modelo.old.I.pm25.9)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)



modelo.old.I.pm25.10 <- mgcv::gam(SIH_GV_I_Old ~ pm25.gv.d.7 + Temp.Ar + 
                                    s(Umidade.Ar),
                                  data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm25.10)
residuo <- residuals(modelo.old.I.pm25.10)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.pm25.11 <- mgcv::gam(SIH_GV_I_Old ~ pm25.gv.d.7 + s(Temp.Ar) + 
                                    s(Umidade.Ar),
                                  data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm25.11)
residuo <- residuals(modelo.old.I.pm25.11)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)


modelo.old.I.pm25.12 <- mgcv::gam(SIH_GV_I_Old ~ pm25.gv.d.7 + s(Temp.Ar) + 
                                    Umidade.Ar,
                                  data = dados, family = poisson, na.action = na.gam.replace)

summary(modelo.old.I.pm25.12)
residuo <- residuals(modelo.old.I.pm25.12)
plot(residuo)
acf(residuo)
qqnorm(residuo)
qqline(residuo)
hist(residuo)
shapiro.test(residuo)



library(bbmle)
AIC.mod.pm25.old <- AICctab(modelo.old.I.pm25.1, modelo.old.I.pm25.2, modelo.old.I.pm25.3,
                            modelo.old.I.pm25.4, modelo.old.I.pm25.5, modelo.old.I.pm25.6,
                            modelo.old.I.pm25.7, modelo.old.I.pm25.8, modelo.old.I.pm25.9,
                            modelo.old.I.pm25.10, modelo.old.I.pm25.11, modelo.old.I.pm25.12,
                            base = T, weights=T)
AIC.mod.pm25.old

anova.gam(modelo.old.I.pm25.1, modelo.old.I.pm25.2, modelo.old.I.pm25.3,
          modelo.old.I.pm25.4, modelo.old.I.pm25.5, modelo.old.I.pm25.6,
          modelo.old.I.pm25.7, modelo.old.I.pm25.8, modelo.old.I.pm25.9,
          modelo.old.I.pm25.10, modelo.old.I.pm25.11, modelo.old.I.pm25.12)

anova(modelo.old.I.pm25.1, modelo.old.I.pm25.2, modelo.old.I.pm25.3,
      modelo.old.I.pm25.4, modelo.old.I.pm25.5, modelo.old.I.pm25.6,
      modelo.old.I.pm25.7, modelo.old.I.pm25.8, modelo.old.I.pm25.9,
      modelo.old.I.pm25.10, modelo.old.I.pm25.11, modelo.old.I.pm25.12)
