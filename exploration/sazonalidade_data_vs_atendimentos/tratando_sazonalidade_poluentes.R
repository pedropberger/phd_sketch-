
###########################carregando os pacotes###################################
library(readxl)
library(data.table)
library(mgcv)
library(car)
library(ggplot2)
library(grid)
library(animation)
##########################carregando as planilhas###################################
setwd('C:/Users/fisic/Desktop/PPGEA/CODIGOS_GAM')

dados_pol <- read_excel("SIH_unificado_poluentesmedio.xlsx", sheet = "Defasagem-completa") #dados de saude e poluicao

meteo <- read_excel("dados.meteo.aeroporto.15a19.xlsx")

dados_gv_kid_j <- as.data.table(data.frame(dados_pol[,c(1,9:15,22:27,34:39,46:51,58:63,70:75,82:87,94:99)], meteo[1:1462, c(2:4,9,12)]))
####################################################################################
###############funcao para colocar as estacoes  do ano##############################
####################################################################################
library(data.table)
library(zoo)
library(dplyr)

get.seasons <- function(dates, hemisphere = "N"){
  years <- unique(year(dates))
  years <- c(min(years - 1), max(years + 1), years) %>% sort
  
  if(hemisphere == "N"){
    seasons <- c("inverno", "primavera", "verao", "outono")}else{
      seasons <- c("verao", "outono", "inverno", "primavera")}
  
  dt.dates <- bind_rows(
    data.table(date = as.Date(paste0(years, "-12-21")), init = seasons[1], type = "B"),# Summer in south hemisphere
    data.table(date = as.Date(paste0(years, "-3-21")), init = seasons[2], type = "B"), # Fall in south hemisphere
    data.table(date = as.Date(paste0(years, "-6-21")), init = seasons[3], type = "B"), # Winter in south hemisphere
    data.table(date = as.Date(paste0(years, "-9-23")), init = seasons[4], type = "B"), # Winter in south hemisphere
    data.table(date = dates, i = 1:(length(dates)), type = "A") # dates to compute
  )[order(date)] 
  
  dt.dates[, init := zoo::na.locf(init)] 
  
  return(dt.dates[type == "A"][order(i)]$init)
}
###################################################################################
#########################fim da funcao ############################################
###################################################################################

##################acrescentando estacoes e dias da semana #########################

dados_gv_kid_j$season <- get.seasons(dados_gv_kid_j$Data,hemisphere = "S")

dados_gv_kid_j$week_day <- weekdays(as.Date(dados_gv_kid_j$Data))

dados_gv_kid_j[, season_number := as.integer(car::recode(season,
                                                         "'primavera'='1';'verao'='2';'outono'='3';'inverno'='4'"))]

dados_gv_kid_j[, week_number := as.integer(car::recode(week_day,
                                                       "'domingo'='1';'segunda-feira'='2';'terça-feira'='3';'quarta-feira'='4';
    'quinta-feira'='5';'sexta-feira'='6';'sábado'='7'"))]

##################armazenando informacoes das datas, meses e estacoes##############
n_Data <- unique(dados_gv_kid_j[, Data])
n_weekdays <- unique(dados_gv_kid_j[, week_day])
n_months <- unique(dados_gv_kid_j[,mês])
n_seasons <- unique(dados_gv_kid_j[,season])
period <- 7

##################plotando internacoes criancas CID j X datas #####################
ggplot(dados_gv_kid_j, aes(Data, SIH_GV_J_Kid)) +
  geom_line() +
  labs(x = "Data", y = "Atendimentos")

#################criando o objeto matrix_gam com as frequencias semanais, mensais e das estacoes do ano#############
N <- nrow(dados_gv_kid_j) 

window <- N / period 

matrix_gam <- data.table(atendimentos = dados_gv_kid_j[, SIH_GV_J_Kid], weekly = rep(1:period, window),monthly = dados_gv_kid_j[, mês])

matrix_gam <- cbind(matrix_gam,dados_gv_kid_j[,c(1,3:59)])

#################criando o modelo_1_com_poluentes_temperatura_umidade####################################################
modelo_1_polu <- gam(atendimentos ~ s(weekly, bs = "cr", k = period) +
                  s(monthly, bs = "ps", k = 12) + s(pm25.gv,pm10.gv, so2.gv, no2.gv, co.gv, o3.gv) + s(Temp.Ar,Umidade.Ar),
                data = matrix_gam,
                family = gaussian)

summary(modelo_1_polu)

###########plotando os valores ajustados pelo modelo_1_com_poluentes_temperatura_umidade e os valores reais ##############
comparacao_1_polu <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                               data.table(SIH_GV_J_Kid = modelo_1_polu$fitted.values,
                                          Data = dados_gv_kid_j[, Data])))
comparacao_1_polu[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_1_polu, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Datas", y = "Atendimentos",
       title = "Ajustado por GAM modelo_1_polu")

###################################################################################################################

######################criando o modelo_1_com_poluentes_temperatura_umidade_lag_1###################################

modelo_1_polu_lag_1 <- gam(atendimentos ~ s(weekly, bs = "cr", k = period) +
                       s(monthly, bs = "ps", k = 12) + s(pm25.gv.d.1,pm10.gv.d.1, so2.gv.d.1, no2.gv.d.1, co.gv.d.1, o3.gv.d.1) + s(Temp.Ar,Umidade.Ar),
                     data = matrix_gam,
                     family = gaussian)

summary(modelo_1_polu_lag_1)

###########plotando os valores ajustados pelo modelo_1_com_poluentes_temperatura_umidade_lag_1_temperatura_umidade e os valores reais ##############
comparacao_1_polu_lag_1 <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                                    data.table(SIH_GV_J_Kid = modelo_1_polu_lag_1$fitted.values,
                                               Data = dados_gv_kid_j[, Data])))
comparacao_1_polu_lag_1[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_1_polu_lag_1, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Datas", y = "Atendimentos",
       title = "Ajustado por GAM modelo_1_polu_lag_1")

#############################################################################################################################3
#################criando o modelo_1_com_poluentes_temperatura_umidade_lag_2####################################################

modelo_1_polu_lag_2 <- gam(atendimentos ~ s(weekly, bs = "cr", k = period) +
                             s(monthly, bs = "ps", k = 12) + s(pm25.gv.d.2,pm10.gv.d.2, so2.gv.d.2, no2.gv.d.2, co.gv.d.2, o3.gv.d.2) + s(Temp.Ar,Umidade.Ar),
                           data = matrix_gam,
                           family = gaussian)

summary(modelo_1_polu_lag_2)

###########plotando os valores ajustados pelo modelo_1_com_poluentes_temperatura_umidade_lag_2_temperatura_umidade e os valores reais ##############
comparacao_1_polu_lag_2 <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                                          data.table(SIH_GV_J_Kid = modelo_1_polu_lag_2$fitted.values,
                                                     Data = dados_gv_kid_j[, Data])))
comparacao_1_polu_lag_2[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_1_polu_lag_2, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Datas", y = "Atendimentos",
       title = "Ajustado por GAM modelo_1_polu_lag_2")

#############################################################################################################################3
#################criando o modelo_1_com_poluentes_temperatura_umidade_lag_3####################################################

modelo_1_polu_lag_3 <- gam(atendimentos ~ s(weekly, bs = "cr", k = period) +
                             s(monthly, bs = "ps", k = 12) + s(pm25.gv.d.3,pm10.gv.d.3, so2.gv.d.3, no2.gv.d.3, co.gv.d.3, o3.gv.d.3) + s(Temp.Ar,Umidade.Ar),
                           data = matrix_gam,
                           family = gaussian)

summary(modelo_1_polu_lag_3)

###########plotando os valores ajustados pelo modelo_1_com_poluentes_temperatura_umidade_lag_3_temperatura_umidade e os valores reais ##############
comparacao_1_polu_lag_3 <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                                          data.table(SIH_GV_J_Kid = modelo_1_polu_lag_3$fitted.values,
                                                     Data = dados_gv_kid_j[, Data])))
comparacao_1_polu_lag_3[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_1_polu_lag_3, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Datas", y = "Atendimentos",
       title = "Ajustado por GAM modelo_1_polu_lag_3")

#############################################################################################################################3
#################criando o modelo_1_com_poluentes_temperatura_umidade_lag_4####################################################

modelo_1_polu_lag_4 <- gam(atendimentos ~ s(weekly, bs = "cr", k = period) +
                             s(monthly, bs = "ps", k = 12) + s(pm25.gv.d.4,pm10.gv.d.4, so2.gv.d.4, no2.gv.d.4, co.gv.d.4, o3.gv.d.4) + s(Temp.Ar,Umidade.Ar),
                           data = matrix_gam,
                           family = gaussian)

summary(modelo_1_polu_lag_4)

###########plotando os valores ajustados pelo modelo_1_com_poluentes_temperatura_umidade_lag_4_temperatura_umidade e os valores reais ##############
comparacao_1_polu_lag_4 <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                                          data.table(SIH_GV_J_Kid = modelo_1_polu_lag_4$fitted.values,
                                                     Data = dados_gv_kid_j[, Data])))
comparacao_1_polu_lag_4[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_1_polu_lag_4, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Datas", y = "Atendimentos",
       title = "Ajustado por GAM modelo_1_polu_lag_4")

#############################################################################################################################3
#################criando o modelo_1_com_poluentes_temperatura_umidade_lag_5####################################################

modelo_1_polu_lag_5 <- gam(atendimentos ~ s(weekly, bs = "cr", k = period) +
                             s(monthly, bs = "ps", k = 12) + s(pm25.gv.d.5,pm10.gv.d.5, so2.gv.d.5, no2.gv.d.5, co.gv.d.5, o3.gv.d.5) + s(Temp.Ar,Umidade.Ar),
                           data = matrix_gam,
                           family = gaussian)

summary(modelo_1_polu_lag_5)

###########plotando os valores ajustados pelo modelo_1_com_poluentes_temperatura_umidade_lag_5_temperatura_umidade e os valores reais ##############
comparacao_1_polu_lag_5 <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                                          data.table(SIH_GV_J_Kid = modelo_1_polu_lag_5$fitted.values,
                                                     Data = dados_gv_kid_j[, Data])))
comparacao_1_polu_lag_5[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_1_polu_lag_5, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Datas", y = "Atendimentos",
       title = "Ajustado por GAM modelo_1_polu_lag_5")

#############################################################################################################################
#################criando o modelo_1_com_poluentes_temperatura_umidade_lag_6####################################################

modelo_1_polu_lag_6 <- gam(atendimentos ~ s(weekly, bs = "cr", k = period) +
                             s(monthly, bs = "ps", k = 12) + s(pm25.gv.d.6,pm10.gv.d.6, so2.gv.d.6, no2.gv.d.6, co.gv.d.6, o3.gv.d.6) + s(Temp.Ar,Umidade.Ar),
                           data = matrix_gam,
                           family = gaussian)

summary(modelo_1_polu_lag_6)

###########plotando os valores ajustados pelo modelo_1_com_poluentes_temperatura_umidade_lag_6_temperatura_umidade e os valores reais ##############
comparacao_1_polu_lag_6 <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                                          data.table(SIH_GV_J_Kid = modelo_1_polu_lag_6$fitted.values,
                                                     Data = dados_gv_kid_j[, Data])))
comparacao_1_polu_lag_6[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_1_polu_lag_6, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Datas", y = "Atendimentos",
       title = "Ajustado por GAM modelo_1_polu_lag_6")

AIC(modelo_1_polu, modelo_1_polu_lag_1, modelo_1_polu_lag_2, modelo_1_polu_lag_3,modelo_1_polu_lag_4, modelo_1_polu_lag_5, modelo_1_polu_lag_6)




#________________________________________________________________

modelo_TESTE <- gam(atendimentos ~ s(weekly, bs = "cr", k = period) +
                             s(monthly, bs = "ps", k = 12),
                           data = matrix_gam,
                           family = gaussian)

summary(modelo_TESTE)

###########plotando os valores ajustados pelo modelo_1_com_poluentes_temperatura_umidade_lag_6_temperatura_umidade e os valores reais ##############
comparacao_TESTE<- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                                          data.table(SIH_GV_J_Kid = modelo_TESTE$fitted.values,
                                                     Data = dados_gv_kid_j[, Data])))
comparacao_TESTE[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_TESTE, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Datas", y = "Atendimentos",
       title = "Ajustado por GAM modelo_TESTE")

#________________________________________________________________

modelo_TESTE2 <- gam(atendimentos ~+ s(pm10.gv, so2.gv, no2.gv, co.gv, o3.gv) ,
                    data = matrix_gam,
                    family = gaussian)

summary(modelo_TESTE2)

###########plotando os valores ajustados pelo modelo_1_com_poluentes_temperatura_umidade_lag_6_temperatura_umidade e os valores reais ##############
comparacao_TESTE2<- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                                  data.table(SIH_GV_J_Kid = modelo_TESTE2$fitted.values,
                                             Data = dados_gv_kid_j[, Data])))
comparacao_TESTE2[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_TESTE2, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Datas", y = "Atendimentos",
       title = "Ajustado por GAM modelo_TESTE2")




















