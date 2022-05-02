#===============================================================================
# Universidade Federal do Espirito Santo - UFES - Brazil
# Programa de Pos-Graduacao em Engenharia Ambiental - PPGEA
# Nucleo de Modelagem Estocastica - NuMEs
# Authors: Dennis, Filipe, Pedro e Wilson
# Advisor: Valderio Anselmo Reisen 
#===============================================================================
#===============================================================================
# loading packages
#===============================================================================
library(gam)
library(vegan)
library(vars)
library(MTS)
library(fastDummies)

#===============================================================================
# loading Data
#===============================================================================
# ####Ensuring the correct type of each column
# nropollu <-rep("numeric",10)
# colunas <-c("Date", nropollu)
# 
# ####importing the spreadsheets(pollutants, temperature and air humidity)
# daily_concent_GV_09_13<- read.csv("C:/Users/fisic/artigo_2021/planilhas_resultados/daily_concent_GV_09_13.csv", header=TRUE, colClasses =colunas)
# daily_concent_GV_14_18<- read.csv("C:/Users/fisic/artigo_2021/planilhas_resultados/daily_concent_GV_14_18.csv", header=TRUE, colClasses =colunas)
# 
# ####convert the date type
# daily_concent_GV_09_13$date<-as.POSIXlt.Date(daily_concent_GV_09_13$date)
# daily_concent_GV_14_18$date<-as.POSIXlt.Date(daily_concent_GV_14_18$date)
# 
# ####importing the spreadsheets(pollutants, temperature and air humidity)
# colunas <-c("Date", "numeric")
# nro_adm_RGV_09_13_child_cid_j<- read.csv("C:/Users/fisic/artigo_2021/planilhas_resultados/nro_adm_RGV_09_13_child_cid_j.csv", header=TRUE, colClasses =colunas)
# nro_adm_RGV_14_18_child_cid_j<- read.csv("C:/Users/fisic/artigo_2021/planilhas_resultados/nro_adm_RGV_14_18_child_cid_j.csv", header=TRUE, colClasses =colunas)
# 
# ####combining data from two datasets (from 2009 to 2013 and from 2014 to 2018 )
# dados_09_13 <-cbind(daily_concent_GV_09_13, nro_adm_RGV_09_13_child_cid_j)
# dados_14_18 <-cbind(daily_concent_GV_14_18, nro_adm_RGV_14_18_child_cid_j)
# 
# dados_09_13 <-dados_09_13[,-12]
# dados_14_18 <-dados_14_18[,-12]

#===============================================================================
# GAM-PCA-VAR (from 2009 to 2013)
#===============================================================================
attach(dados_09_13)

names(dados_09_13)

n <-length(Admissions)

Pollutants<-dados_09_13[,2:6]


####
data1 <- dados_gv[, 1:2]                                       
data1$weekday <- weekdays(data1$date)                 
data1                        
data1<-dummy_cols(data1, select_columns = "weekday")


#### Descriptive statistics

summary(dados_09_13) 

### Plots of pollutants and response variable
par(mfrow=c(3,2))
  ts.plot(pm10.gv)
  ts.plot(so2.gv)
  ts.plot(no2.gv)
  ts.plot(co.gv)
  ts.plot(o3.gv)
  ts.plot(Admissions)
par(mfrow=c(1,1))
  
### ACF of pollutants and response variable
par(mfrow=c(3,2))
  acf(pm10.gv)
  acf(so2.gv)
  acf(no2.gv)
  acf(co.gv)
  acf(o3.gv)
  acf(Admissions)
par(mfrow=c(1,1))

#### Correlation matrix
Variables<-matrix(c(pm10.gv,so2.gv,no2.gv,co.gv,o3.gv,temp.gv,umid.gv,Admissions),nrow = n, ncol=8,dimnames = list(NULL, c("PM10","SO2","NO2","CO","O3","Temp","Umid","Admissions")))
cor(Variables,method='pearson')

#### GAM Model
M_GAM <- gam(Admissions~pm10.gv +so2.gv + no2.gv +co.gv + o3.gv, family=poisson(link=log))
summary(M_GAM)

#### MSE, AIC and BIC
MSE_GAM<-M_GAM$deviance/M_GAM$df.res
AIC_GAM<-M_GAM$aic

#### BIC
K=length(M_GAM$coef)
LIKE_GAM <- -(M_GAM$aic - 2*K)/2
BIC_GAM <- -2*LIKE_GAM + K*log(n)
cbind(MSE_GAM,AIC_GAM,BIC_GAM)

####Standardizing the pollutant
Stand_Pollutants<- decostand(Pollutants, "standardize")


#### Do the PCA 
pca <- prcomp(Stand_Pollutants,center=T,scale=T)

summary(pca)

eigen(cor(Stand_Pollutants))

#Retrieving the PCs
pcs <- pca$x

#Retaining only the first 3 PCs
PCs <- pcs[,1:3]
PCs <- as.data.frame(PCs)
attach(PCs)

#ACF of PCs
par(mfrow=c(2,2))
stats::acf(PC1,main="PC1")
stats::ccf(PC1, PC2, lag.max = NULL, type = c("correlation"), main="PC1 x PC2")
stats::ccf(PC1, PC3, lag.max = NULL, type = c("correlation"), main="PC1 x PC3")
stats::ccf(PC2, PC3, lag.max = NULL, type = c("correlation"), main="PC2 x PC3")

par(mfrow=c(1,1))

# GAM-PCA Model


M_GAM_PCA <- gam(Admissions~PC1+PC2+PC3, 
                 family=poisson(link=log))
summary(M_GAM_PCA)

# MSE, AIC and BIC
MSE_GAM_PCA<-M_GAM_PCA$deviance/M_GAM_PCA$df.res
AIC_GAM_PCA<-M_GAM_PCA$aic

# BIC
K_PCA=length(M_GAM_PCA$coef)
LIKE_GAM_PCA <- -(M_GAM_PCA$aic - 2*K_PCA)/2
BIC_GAM_PCA <- -2*LIKE_GAM_PCA + K_PCA*log(n)
cbind(MSE_GAM_PCA,AIC_GAM_PCA,BIC_GAM_PCA)

# SAR(1)(1)_7 model to the residuals of GAM_PCA
Res_GAM_PCA <- ts(M_GAM_PCA$res,start=1,frequency=1) 
M_Res<-arima(Res_GAM_PCA, order = c(7,1,0))

#ACF and PACF of residuals from GAM-PCA model
par(mfrow=c(2,1))
acf(M_Res$res)
pacf(M_Res$res)
par(mfrow=c(1,1))



#############################################################################
##########################################################################
################################################################

# GAM-PCA-VAR Model


############################################################################
# VAR and PRINCIPAL COMPONENT ANALYSIS
############################################################################

#VAR Filtering

fitSVAR=sVARMA(Pollutants,order=c(1,0,0),sorder=c(1,0,0),s=7)
stats::acf(fitSVAR)

######Standardizing the VAR filtered pollutants
StanPolutants_VAR <- decostand(fitSVAR$res, "standardize")

# Do the PCA 
pca_VAR <- prcomp(StanPolutants_VAR,center=T,scale=T)
summary(pca_VAR)
eigen(cor(StanPolutants_VAR))

#Retrieving the PCs
pcs_VAR <- pca_VAR$x
#Retaining only the first 3 PCs
PCs_VAR <- pcs_VAR[,1:3]
PCs_VAR <- as.data.frame(PCs_VAR)
attach(PCs_VAR)

#ACF of PCs
par(mfrow=c(2,2))
stats::acf(PC1,main="PC1")
stats::ccf(PC1, PC2, lag.max = NULL, type = c("correlation"), main="PC1 x PC2")
stats::ccf(PC1, PC3, lag.max = NULL, type = c("correlation"), main="PC1 x PC3")
stats::ccf(PC2, PC3, lag.max = NULL, type = c("correlation"), main="PC2 x PC3")

KVAR<-length(PC1)
M_GAM_PCA_VAR <- gam(Admissions[1:KVAR]~Tuesday[1:KVAR]+Wednesday[1:KVAR]+Thursday[1:KVAR]+Friday[1:KVAR]+Saturday[1:KVAR]
                     +Sunday[1:KVAR]+Holiday2[1:KVAR]+Holiday3[1:KVAR]+s(RHm12,12)[1:KVAR]+PC1+PC2+PC3, 
                     family=poisson(link=log))

# MSE, AIC and BIC
MSE_GAM_PCA_VAR<-M_GAM_PCA_VAR$deviance/M_GAM_PCA_VAR$df.res
AIC_GAM_PCA_VAR<-M_GAM_PCA_VAR$aic
# BIC
K_PCA_VAR=length(M_GAM_PCA_VAR$coef)
LIKE_GAM_PCA_VAR <- -(M_GAM_PCA_VAR$aic - 2*K_PCA_VAR)/2
BIC_GAM_PCA_VAR <- -2*LIKE_GAM_PCA_VAR + K_PCA_VAR*log(KVAR)
cbind(MSE_GAM_PCA_VAR,AIC_GAM_PCA_VAR,BIC_GAM_PCA_VAR)

#ACF and PACF of residuals from GAM-PCA-VAR model
par(mfrow=c(2,1))
acf(M_GAM_PCA_VAR$res)
pacf(M_GAM_PCA_VAR$res)


