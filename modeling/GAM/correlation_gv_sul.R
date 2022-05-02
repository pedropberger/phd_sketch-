#####carregando as planilhas
library(readxl)

library(dplyr)

setwd('C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/SIH_filtrado/')

dados_pol <- read_excel("SIH_unificado_poluentesmedio.xlsx", sheet = "Defasagem-completa") #dados de saude e polui

setwd('C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/Meteorologicos/')

meteo <- read_excel("dados.meteo.aeroporto.15a19.xlsx")

####separando os dados para a grande vitória

dados_cor_gv <- data.frame(dados_pol[,c(6:9,10:15,22:27,34:39,46:51,58:63,70:75,82:87,94:99)], meteo[1:1462, c(9,12)])


####separando os dados para o sul do estado

dados_cor_sul <- data.frame(dados_pol[,c(2:5,16:21,28:33,40:45,52:57,64:69,76:81,88:93,100:105)], meteo[1:1462, c(9,12)])

library(corrplot)

#####correlacao para a RGV######
correlacao_gv <-cor(dados_cor_gv[,1:4], dados_cor_gv[5:54], use="complete.obs")

correlacao_gv

corrplot(cor(dados_cor_gv[,1:4], dados_cor_gv[5:54], use="complete.obs"), method = "number", type = "full")

#####correlacao para a sul######

correlacao_sul <-cor(dados_cor_sul[,1:4], dados_cor_sul[5:54], use="complete.obs")

correlacao_sul

corrplot(cor(dados_cor_sul[,1:4], dados_cor_sul[5:54], use="complete.obs"), method = "number", type = "full")
