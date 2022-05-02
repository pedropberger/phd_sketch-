###########################carregando os pacotes###################################
library(readxl)
library(data.table)
library(mgcv)
library(car)
library(ggplot2)
library(grid)
library(animation)
##########################carregando as planilhas###################################
setwd('C:/Users/fisic/Desktop/CODIGOS_GAM')

dados_pol <- read_excel("SIH_unificado_poluentesmedio.xlsx", sheet = "Defasagem-completa") #dados de saude e poluicao

meteo <- read_excel("dados.meteo.aeroporto.15a19.xlsx")

dados_gv_kid_j <- as.data.table(data.frame(dados_pol[,c(1,9:15)], meteo[1:1462, c(2:4,9,12)]))
####################################################################################
###############funcao para colocar a estação  do ano################################
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
  labs(x = "Data", y = "Internacoes")
#################criando o objeto matrix_gam com as frequencias semanais, mensais e das estacoes do ano#############
N <- nrow(dados_gv_kid_j)  
window <- N / period  

matrix_gam <- data.table(atendimentos = dados_gv_kid_j[, SIH_GV_J_Kid], weekly = rep(1:period, window),monthly = dados_gv_kid_j[, mês],seasonaly = dados_gv_kid_j[, season_number])

matrix_gam <- cbind(matrix_gam,dados_gv_kid_j[,c(3:8)])
#################criando o modelo_1_s ################################################################################
################ regressao cubica para sazonalidade semanal e p-splines para mensalmente e cr para estacoes do ano
###################################################################################################################
modelo_1_s <- gam(atendimentos ~ s(weekly, bs = "cr", k = period) +
               s(monthly, bs = "ps", k = 12) + s(seasonaly, bs="cr", k = 4),
             data = matrix_gam,
             family = gaussian)

layout(matrix(1:3, nrow = 1))
plot(modelo_1_s, shade = TRUE)

layout(matrix(1:1, nrow = 1))

summary(modelo_1_s)

comparacao_1_s <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                        data.table(SIH_GV_J_Kid = modelo_1_s$fitted.values,
                                   Data = dados_gv_kid_j[, Data])))
comparacao_1_s[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_1_s, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Datas", y = "Atendimentos",
       title = "Ajustado por GAM, modelo_1_s")
#################criando o modelo_2_s ################################################################################
##########interacao entre sazonalidade semanal e mensal e das estacoes do ano (o default eh ts =thinsplines
###################################################################################################################
modelo_2_s <- gam(atendimentos ~ s(weekly, monthly, seasonaly),
             data = matrix_gam,
             family = gaussian)

summary(modelo_2_s)

comparacao_2_s <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                        data.table(value = modelo_2_s$fitted.values,
                                   Data = dados_gv_kid_j[, Data])))
comparacao_2_s[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_2_s, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Data", y = "Atendimentos",
       title = "Ajustado por GAM, modelo_2_s")
#################criando o modelo_3 ##########################################################
########## cr = regressao cubica para semanal e p splines para mensal, k escolhido pelo pacote
##############################################################################################
modelo_3_s <- gam(atendimentos ~ te(weekly, monthly, seasonaly,
                       bs = c("cr", "ps", "ps")),
             data = matrix_gam,
             family = gaussian)

summary(modelo_3_s)

comparacao_3_s <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                        data.table(SIH_GV_J_Kid = modelo_3_s$fitted.values,
                                   Data = dados_gv_kid_j[, Data])))
comparacao_3_s[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_3_s, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Data", y = "Atendimentos",
       title = "Ajustado por GAM, modelo_3_s")
#################criando o modelo_4 ####################################################
########## cr = regressao cubica para semanal (k=period=7) e p splines (k=12) para mensal
#########  e k=4 para estacoes do ano 
######### te() produz um produto tensorial completo entre weekly e monthly
########################################################################################
modelo_4_s <- gam(atendimentos ~ te(weekly, monthly, seasonaly,
                       k = c(period, 12, 4),
                       bs = c("cr", "ps", "cr")),
             data = matrix_gam,
             family = gaussian)

summary(modelo_4_s)

comparacao_4_s <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                        data.table(value = modelo_4_s$fitted.values,
                                   Data = dados_gv_kid_j[, Data])))
comparacao_4_s[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_4_s, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Data", y = "Atendimentos",
       title = "Ajustado por GAM, modelo_4_s")
#################criando o modelo_4_s_fx ####################################################
########## cr = regressao cubica para semanal (k=period=7) e p splines (k=12) para mensal
#########  e k=4 para estacoes do ano 
######### te() produz um produto tensorial completo entre weekly e monthly
########################################################################################
modelo_4_s_fx <- gam(atendimentos ~ te(weekly, monthly, seasonaly,
                          k = c(period, 12, 4),
                          bs = c("cr", "ps", "cr"),
                          fx = TRUE),
                data = matrix_gam,
                family = gaussian)

summary(modelo_4_s_fx)

comparacao_4_s_fx <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                        data.table(value = modelo_4_s_fx$fitted.values,
                                   Data = dados_gv_kid_j[, Data])))
comparacao_4_s_fx[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_4_s_fx, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Data", y = "Atendimentos",
       title = "Ajustado por GAM, modelo_4_s_fx")
#################criando o modelo_5_S #################################################################
#  cr = regressao cubica para semanal (k=7) e p splines (k=12) para mensal, cr para estacoes com k = 4
# ti() produz um produto tensorial apropriada quando estao presentes interacoes menores 
#efeitos principais.
######### fx= é usado para indicar se os termos têm ou não um número fixo de graus de liberdade
######################################################################################################

modelo_5_s <- gam(atendimentos ~ s(weekly, bs = "cr", k = period) +
                  s(monthly, bs = "ps", k = 12) + s(seasonaly, bs = "cr", k = 4) +
                  ti(weekly, monthly, seasonaly,
                     k = c(period, 12, 4),
                     bs = c("cr", "ps", "cr")),
                data = matrix_gam,
                family = gaussian)

summary(modelo_5_s)

comparacao_5_s <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                        data.table(value = modelo_5_s$fitted.values,
                                   Data = dados_gv_kid_j[, Data])))
comparacao_5_s[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_5_s, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Data", y = "Atendimentos",
       title = "Ajustado por GAM, modelo_5_s")

#################criando o modelo_6 ###########################################################################
#  cr = regressao cubica para semanal (k=7) e p splines (k=12) para mensal, com k =4 e cr para estacoes do ano
# t2() t2 é uma função alternativa a te e usa um método de penalização diferente.
# full = TRU, porque assim temos invariância estrita com penalidades.
##############################################################################################################

modelo_6_s <- gam(atendimentos ~ t2(weekly, monthly,seasonaly,
                       k = c(period, 12, 4),
                       bs = c("cr", "ps", "cr"),
                       full = TRUE),
             data = matrix_gam,
             family = gaussian)

summary(modelo_6_s)

comparacao_6_s <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                        data.table(value = modelo_6_s$fitted.values,
                                   Data = dados_gv_kid_j[, Data])))
comparacao_6_s[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_6_s, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Data", y = "Atendimentos",
       title = "Ajustado por GAM, modelo_6_s")

AIC(modelo_1_s, modelo_2_s, modelo_3_s, modelo_4_s,modelo_4_s_fx, modelo_5_s, modelo_6_s)
