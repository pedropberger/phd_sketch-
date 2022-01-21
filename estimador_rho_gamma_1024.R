#===============================================================================
# Universidade Federal do Espirito Santo - UFES - Brasil
# Programa de Pos-Graduacao em Engenharia Ambiental - PPGEA
# Nucleo de Modelagem Estocastica - NuMEs
# Autores: Filipe e Wilson
# Orientador: Valderio Anselmo Reisen 

# Referencia: Choice of Thresholds for Wavelet Shrinkage Estimate of the Spectrum

#===============================================================================
# carregando pacotes
#===============================================================================
library(MASS);library(waveslim)
library(TSA);library(TSSS);library(ggplot2)
library(tidyverse); library(wavethresh); library(Metrics)
#===============================================================================
# Gerando (simulando) dados
#===============================================================================
phi = 0.7
n = 1024
nb_repli=1000
lagmax=10
#baselog=10

# dados=matrix(NA,nrow=nb_repli,ncol=n)
# for (i in 1:nb_repli)
# {
#   cat(i,'\n')
#   dados[i,] <- arima.sim(model=list(ar=phi),n=n)
# #  out<-sample(1:n, 0.2*n)
# #  dados[i,out]<- dados[i,out] + rnorm(0.2*n, 10, 2)
# }
# 
# dados_1024_1000 <- dados
setwd("C:/Users/rcfil/OneDrive/Filipe e Wilson/Codigos_R/Estimador rho AR/")
#setwd("C:/Users/fisic/OneDrive/Filipe e Wilson/Codigos_R/Estimador rho AR/")
#save(dados_1024_1000, file = "dados_1024_1000")
# 
load("dados_1024_1000")
dados<-dados_1024_1000
#===============================================================================
#  Encontrando a matriz P ortogonal.
#===============================================================================
P_n=1/sqrt(n)*rep(1,n)
for (j in 1:floor(n/2))
{
  cat(j,'\n')
  omega_j=2*pi*j/n
  c_j=sqrt(2/n)*cos((0:(n-1))*omega_j)
  s_j=sqrt(2/n)*sin((0:(n-1))*omega_j)
  P_n=rbind(P_n,c_j,s_j)
}

#===============================================================================
# 1.1 - Calculando o periodograma.
#===============================================================================
f_est=matrix(NA,nrow=nb_repli,ncol=n)
for (k in 1:nb_repli)
{
  cat(k,'\n')
  
  period_dados <- abs(fft(dados[k,]))^2/(2*pi*n)
  
  TWg<-wavethresh::wd(period_dados)# 1.3 -  transformada ondaleta.
  
  thres_g <-wavethresh::threshold.wd(TWg, type= "soft",policy="universal")# 1.4 - Aplicando o limiares (thresholdings) na transformada ondaleta.
  
  f_aux_est <-wavethresh::wr(thres_g) # 1.5 - aplicando a transformada inversa ondaleta
  
  f_est[k,] <-f_aux_est
} 

#===============================================================================
# 1.6 - Encontrando o estimador de f.
#===============================================================================
# f_est <-10^(result_g_est)
# f_est
#===============================================================================
# 1.7 - Calculando a matriz Gama_hat_wt.
#===============================================================================
matriz_gamma_hat_wt<-matrix(NA,nrow=nb_repli,ncol=lagmax+1)
matriz_rho_hat_wt<-matrix(NA,nrow=nb_repli,ncol=lagmax+1)
for (l in 1:nb_repli)
{
  cat(l,'\n')
  D_n_hat_wt=diag(c(f_est[l,1],rep(f_est[l,2:floor(1+n/2)],each=2)))
  Gamma_hat_wt=2*pi*t(P_n)%*%D_n_hat_wt%*%P_n
  gamma_hat_wt=Gamma_hat_wt[1:(lagmax+1),1]
  matriz_gamma_hat_wt[l,]<-gamma_hat_wt
  matriz_rho_hat_wt[l,]<- gamma_hat_wt/gamma_hat_wt[1]
}

#===============================================================================
# 2.1 - Calculando estimador do Brockwell e Davis 
#===============================================================================
matriz_gamma_hat_brock<-matrix(NA,nrow=nb_repli,ncol=lagmax+1)
matriz_rho_hat_brock<-matrix(NA,nrow=nb_repli,ncol=lagmax+1)
for (a in 1:nb_repli)
{
  cat(a,'\n')
  period_dados_b <- abs(fft(dados[a,]))^2/(2*pi*n)
  D_n_hat_brock=diag(c(period_dados_b[1],rep(period_dados_b[2:floor(1+n/2)],each=2)))
  Gamma_hat_brock=2*pi*t(P_n)%*%D_n_hat_brock%*%P_n
  gamma_hat_brock=Gamma_hat_brock[1:(lagmax+1),1]
  matriz_gamma_hat_brock[a,]<-gamma_hat_brock
  matriz_rho_hat_brock[a,]<- gamma_hat_brock/gamma_hat_brock[1]
}

#===============================================================================
# 3.1 - Estimador classico
#===============================================================================
matriz_gamma_hat<-matrix(NA,nrow=nb_repli,ncol=lagmax+1)
matriz_rho_hat <- matrix(NA,nrow=nb_repli,ncol=lagmax+1)

for (b in 1:nb_repli)
{
  cat(b,'\n')
  gamma_hat=as.vector(acf(dados[b,], lag.max = lagmax,type = "covariance", plot = FALSE)[[1]])
  matriz_gamma_hat[b,]<-gamma_hat
  rho_hat=as.vector(acf(dados[b,], lag.max = lagmax,type = "correlation", plot = FALSE)[[1]])
  rho_hat <- c(1, rho_hat)
  matriz_rho_hat[b,]<-rho_hat
}

#===============================================================================
# Resultados - Covariancia
#===============================================================================
resultado_gamma_classico_1024_1000=rbind(apply(matriz_gamma_hat,2,mean))
resultado_gamma_brock_1024_1000=rbind(apply(matriz_gamma_hat_brock,2,mean))
resultado_gamma_wt_1024_1000=rbind(apply(matriz_gamma_hat_wt,2,mean))

#===============================================================================
# Resultados - Correlação
#===============================================================================
resultado_rho_classico_1024_1000=rbind(apply(matriz_rho_hat,2,mean))
resultado_rho_brock_1024_1000=rbind(apply(matriz_rho_hat_brock,2,mean))
resultado_rho_wt_1024_1000=rbind(apply(matriz_rho_hat_wt,2,mean))

setwd("C:/Users/rcfil/OneDrive/Filipe e Wilson/Codigos_R/Estimador rho AR/")
setwd("C:/Users/fisic/OneDrive/Filipe e Wilson/Codigos_R/Estimador rho AR/")
save(resultado_rho_classico_1024_1000, file = "resultado_rho_classico_1024_1000")
save(resultado_rho_brock_1024_1000, file = "resultado_rho_brock_1024_1000")
save(resultado_rho_wt_1024_1000, file = "resultado_rho_wt_1024_1000")

#===============================================================================
# Correlação real
#===============================================================================
vetor_auxiliar <- c(0:lagmax)
rho_1024_1000 <- phi^(vetor_auxiliar)

save(rho_1024_1000, file = "rho_1024_1000")
#===============================================================================
# Covariancia real
#===============================================================================



#===============================================================================
# MRSE
#===============================================================================

mrse_resultado_1024_1000<- rbind(rmse(rho_1024_1000, resultado_rho_classico_1024_1000), 
                                rmse(rho_1024_1000, resultado_rho_brock_1024_1000), 
                                rmse(rho_1024_1000, resultado_rho_wt_1024_1000))

bias_resultado_1024_1000<- rbind(bias(rho_1024_1000, resultado_rho_classico_1024_1000), 
                                bias(rho_1024_1000, resultado_rho_brock_1024_1000), 
                                bias(rho_1024_1000, resultado_rho_wt_1024_1000))
mrse_resultado_1024_1000
bias_resultado_1024_1000

save(mrse_resultado_1024_1000, file = "mrse_resultado_1024_1000")
save(bias_resultado_1024_1000, file = "bias_resultado_1024_1000")

################################################################################