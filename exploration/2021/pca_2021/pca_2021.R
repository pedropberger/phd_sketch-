########carregando pacotes###############
library(stats) 
library(factoextra) 
library(readxl)
library(dplyr)

dados_pol <- read_excel("C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/SIH_filtrado/SIH_unificado_poluentesmedio.xlsx", sheet = "Defasagem-completa")
meteo <- read_excel("C:/Users/fisic/Dropbox/Projeto Dennys.Filipe.Pedro.Wilson/Base de Dados/Meteorologicos/dados.meteo.aeroporto.15a19.xlsx")
dados <- data.frame(dados_pol, meteo[1:1462, 5:12])

head(dados)

###################filtrando poluentes e variaveis climaticas da GV
##### dados de 01/01/2015 ate 31/12/2018##########################
###write.csv(x = dados_gv_kid_j, file = "C:/Users/fisic/dados_gv_kid_j.csv")

dados_gv_kid_j <- dados[1:1461,c(1,9,11:15,119:121)]

head(dados_gv_kid_j)

##############verificando medias e NA's######
sapply(dados_gv_kid_j, mean)
sapply(dados_gv_kid_j, mean, na.rm=TRUE)

####Substituindo NA's pela media########
dados_gv_kid_j$no2.gv[is.na(dados_gv_kid_j$no2.gv)]<- mean(dados_gv_kid_j$no2.gv, na.rm= TRUE)
dados_gv_kid_j$Temp.minima[is.na(dados_gv_kid_j$Temp.minima)]<- mean(dados_gv_kid_j$Temp.minima, na.rm= TRUE)
dados_gv_kid_j$Temp.maxima[is.na(dados_gv_kid_j$Temp.maxima)]<- mean(dados_gv_kid_j$Temp.maxima, na.rm= TRUE)
dados_gv_kid_j$Umidade.Ar[is.na(dados_gv_kid_j$Umidade.Ar)]<- mean(dados_gv_kid_j$Umidade.Ar, na.rm= TRUE)

#########conferindo se NA's foram substituidos pelas medias e um pouco de descritiva########
summary(dados_gv_kid_j)
sapply(dados_gv_kid_j, min)
sapply(dados_gv_kid_j, mean)
sapply(dados_gv_kid_j, max)
sapply(dados_gv_kid_j, sd)

##### PCA com a matriz de covariancias
pca_cov <- prcomp(dados_gv_kid_j[,-1])

summary(pca_cov)

##### PCA com a matriz de correlacao

pca_corr <- prcomp(dados_gv_kid_j[,-1], center = TRUE, scale = TRUE)

summary(pca_corr)

fviz_eig(pca_corr)

##### Coeficientes das componentes principais (autovetores da matriz de correlação)
summary(pca_corr)$rotation

######Cada variavel tem 1 escore para cada componente principal.

summary(pca_corr)$x


