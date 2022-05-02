#
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


#dados_GV <- data.frame(dados_pol[,c(6:9,10:15,22:27,34:39,46:51,58:63,70:75,82:87,94:99,106:113)], meteo[1:1462, c(9,12)])



meteo <- read_excel("dados.meteo.aeroporto.15a19.xlsx")

dados_gv_kid_j <- as.data.table(data.frame(dados_pol[,c(1,9,10:15,22:27,34:39,46:51,58:63,70:75,82:87,94:99)], meteo[1:1462, c(2:4,9,12)]))
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
  labs(x = "Data", y = "Atendimentos")
#################criando o objeto matrix_gam com as frequencias semanais, mensais e das estacoes do ano#############
N <- nrow(dados_gv_kid_j) 

window <- N / period 

matrix_gam <- data.table(atendimentos = dados_gv_kid_j[, SIH_GV_J_Kid], weekly = rep(1:period, window),monthly = dados_gv_kid_j[, mês],pm25.gv = dados_gv_kid_j[, pm25.gv])

matrix_gam <- cbind(matrix_gam,dados_gv_kid_j[,c(3:8,12,13)])
#################criando o modelo_1 ####################################################
################ regressao cubica para sazonalidade semanal e p-splines para mensalmente
########################################################################################
modelo_1 <- gam(atendimentos ~ s(weekly, bs = "cr", k = period) +
               s(monthly, bs = "ps", k = 12),
             data = matrix_gam,
             family = gaussian)

summary(modelo_1)
################## plotando influencia semanal e mensal no modelo_1 ####################
layout(matrix(1:2, nrow = 1))

plot(modelo_1, shade = TRUE)

layout(matrix(1:1, nrow = 1))
########################################################################################
#observe que no grafico o pico de atendimentos entre 
# quinta e sexta feira e no grafico da direita que o pico
# de atendimentos é entre os meses de marco e abril
########################################################################################

###########plotando os valores ajustados pelo modelo_1 e os valores reais ##############
comparacao_1 <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                        data.table(SIH_GV_J_Kid = modelo_1$fitted.values,
                                   Data = dados_gv_kid_j[, Data])))
comparacao_1[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_1, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Datas", y = "Atendimentos",
       title = "Ajustado por GAM modelo_1")
#################criando o modelo_2 ####################################################
##########interacao entre sazonalidade semanal e mensal(o default eh ts =thinsplines
########################################################################################
modelo_2 <- gam(atendimentos ~ s(weekly, monthly),
             data = matrix_gam,
             family = gaussian)

summary(modelo_2)
################## plotando influencia semanal e mensal no modelo_2 ####################
plot(modelo_2, shade = TRUE)
########################################################################################
#observe que no grafico as "curvas de nivel" mais alta valor 8
# esta na, aproximadamente, na "abscissa" de sexta e na "ordenada" de abril
########################################################################################

###########plotando os valores ajustados pelo modelo_2 e os valores reais ##############
comparacao_2 <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                        data.table(value = modelo_2$fitted.values,
                                   Data = dados_gv_kid_j[, Data])))
comparacao_2[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_2, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Data", y = "Atendimentos",
       title = "Ajustado por GAM modelo_2")
#################criando o modelo_3 ##########################################################
########## cr = regressao cubica para semanal e p splines para mensal, k escolhido pelo pacote
##############################################################################################
modelo_3 <- gam(atendimentos ~ te(weekly, monthly,
                       bs = c("cr", "ps")),
             data = matrix_gam,
             family = gaussian)

summary(modelo_3)
################## plotando influencia semanal e mensal no modelo_3 ####################

plot(modelo_3, shade = TRUE)

###########plotando os valores ajustados pelo modelo_3 e os valores reais ##############

comparacao_3 <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                        data.table(SIH_GV_J_Kid = modelo_3$fitted.values,
                                   Data = dados_gv_kid_j[, Data])))
comparacao_3[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_3, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Data", y = "Atendimentos",
       title = "Ajustado por GAM modelo_3")
#################criando o modelo_4 ####################################################
########## cr = regressao cubica para semanal (k=period=7) e p splines (k=12) para mensal
######### te() produz um produto tensorial completo entre weekly e monthly
########################################################################################
modelo_4 <- gam(atendimentos ~ te(weekly, monthly,
                       k = c(period, 12),
                       bs = c("cr", "ps")),
             data = matrix_gam,
             family = gaussian)

summary(modelo_4)
################## plotando influencia semanal e mensal no modelo_4 ####################
layout(matrix(1:1, nrow = 1))

plot(modelo_4, shade = TRUE)
###########plotando os valores ajustados pelo modelo_4 e os valores reais ##############

comparacao_4 <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                        data.table(value = modelo_4$fitted.values,
                                   Data = dados_gv_kid_j[, Data])))
comparacao_4[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_4, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Data", y = "Atendimentos",
       title = "Ajustado por GAM modelo_4")
#################criando o modelo_4_fx ########################################################
########## cr = regressao cubica para semanal (k=7) e p splines (k=12) para mensal
######### te() produz um produto tensorial completo entre weekly e monthly
######### fx= é usado para indicar se os termos têm ou não um número fixo de graus de liberdade
###############################################################################################
modelo_4_fx <- gam(atendimentos ~ te(weekly, monthly,
                          k = c(period, 12),
                          bs = c("cr", "ps"),
                          fx = TRUE),
                data = matrix_gam,
                family = gaussian)

summary(modelo_4_fx)
################## plotando influencia semanal e mensal no modelo_4_fx ####################
layout(matrix(1:1, nrow = 1))

plot(modelo_4_fx, shade = TRUE)
###########plotando os valores ajustados pelo modelo_4_fx e os valores reais ##############
comparacao_4_fx <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                        data.table(value = modelo_4_fx$fitted.values,
                                   Data = dados_gv_kid_j[, Data])))
comparacao_4_fx[, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_4_fx, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Data", y = "Atendimentos",
       title = "Ajustado por GAM modelo_4_fx")
#################criando o modelo_5 ########################################################
#  cr = regressao cubica para semanal (k=7) e p splines (k=12) para mensal
# ti() produz um produto tensorial apropriada quando estao presentes interacoes menores 
#efeitos principais.
######### fx= é usado para indicar se os termos têm ou não um número fixo de graus de liberdade
###############################################################################################
modelo_5 <- gam(atendimentos ~ s(weekly, bs = "cr", k = period) +
               s(monthly, bs = "ps", k = 12) +
               ti(weekly, monthly,
                  k = c(period, 12),
                  bs = c("cr", "ps")),
             data = matrix_gam,
             family = gaussian)

summary(modelo_5) 
################## plotando influencia semanal e mensal no modelo_5 ####################
layout(matrix(1:3, nrow = 1))

plot(modelo_5, shade = TRUE)

layout(matrix(1:1, nrow = 1))
###########plotando os valores ajustados pelo modelo_5 e os valores reais ##############
comparacao_5 <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                        data.table(value = modelo_5$fitted.values,
                                   Data = dados_gv_kid_j[, Data])))
comparacao_5 [, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_5, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Data", y = "Atendimentos",
       title = "Ajustado por GAM modelo_5")
#################criando o modelo_6 ########################################################
#  cr = regressao cubica para semanal (k=7) e p splines (k=12) para mensal
# t2() t2 é uma função alternativa a te e usa um método de penalização diferente.
# full = TRU, porque assim temos invariância estrita com penalidades.
###############################################################################################
modelo_6 <- gam(atendimentos ~ t2(weekly, monthly,
                       k = c(period, 12),
                       bs = c("cr", "ps"),
                       full = TRUE),
             data = matrix_gam,
             family = gaussian)

summary(modelo_6)
################## plotando influencia semanal e mensal no modelo_6 ####################
layout(matrix(1:1, nrow = 1))

plot(modelo_6, shade = TRUE)

layout(matrix(1:1, nrow = 1))
###########plotando os valores ajustados pelo modelo_6 e os valores reais ##############
comparacao_6 <- rbindlist(list(dados_gv_kid_j[, .(SIH_GV_J_Kid, Data)],
                               data.table(value = modelo_6$fitted.values,
                                          Data = dados_gv_kid_j[, Data])))
comparacao_6 [, tipo := c(rep("Real", nrow(dados_gv_kid_j)), rep("Ajustado", nrow(dados_gv_kid_j)))]

ggplot(data = comparacao_6, aes(Data, SIH_GV_J_Kid, group = tipo, colour = tipo)) +
  geom_line(size = 0.8) +
  theme_bw() +
  labs(x = "Data", y = "Atendimentos",
       title = "Ajustado por GAM modelo_6")
################################################################################

AIC(modelo_1, modelo_2, modelo_3, modelo_4,modelo_4_fx, modelo_5, modelo_6)

#################################################################################
# pelos valores retornados pela funcao AIC vemos que os modelos 5 e 6 sao "bons"
#################################################################################
# vamos analisar qual sera a  melhore escolha entre os dois
################################################################################
summary(modelo_5)$edf
summary(modelo_6)$edf
summary(modelo_5)$s.table
summary(modelo_6)$s.table
summary(modelo_5)$r.sq
summary(modelo_6)$r.sq
summary(modelo_5)$sp.criterion
summary(modelo_6)$sp.criterion
################################################################################
# como interpretar esses resultados do summary do modelo:
#
# edf = explica quanto cada variavel foi suavizada(quanto maior 
# o valor de edf maiscomplexos são os splines).
#
# p-values =  significancia estatistica da variavel (quanto menor
# melhor).
#
# R-sq. (adj)= R quadrado  ajustado, quanto maior o valor melhor.
#
# GCV = Generalized Cross Validation (usado para estimar parametros 
# de suavizacao ideais e os graus de liberdade,  quanto menor melhor).
#########################################################################
par(mfrow=c(2,4))

gam.check(modelo_5)

gam.check(modelo_6)

par(mfrow=c(1,1))
#########################################################################


