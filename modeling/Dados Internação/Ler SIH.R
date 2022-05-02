#### Dados Internações ####


#Pacotes

install.packages("xlsx")
install.packages("foreign")


library(xlsx)
library(foreign)
library(readr)


###Leitura dos dados filtrados

SIH_GV_IJ_Kid_ncit <- read_csv("SIH_filtrado/SIH_GV_IJ_Kid_ncit.csv", locale = locale())


### Filtrando

# CID

gvik = subset(SIH_GV_IJ_Kid_ncit, SIH_GV_IJ_Kid_ncit$CID_focado == "I")

# Data

gvik = subset(gvik, gvik$Data_inter > "2015-01-01" & gvik$Data_inter < "2019-01-01" )

# Preenchendo datas vazias

hh<- data.frame(date=seq(as.Date("2015-01-01"), as.Date("2019-01-01"), by="days"))
res <- merge(gvik, hh, by.x="Data_inter", by.y="date", all.x=T,all.y=T)

res$`Contagem de Internação`[is.na(res$`Contagem de Internação`)] <- 0
res$CID_focado[is.na(res$CID_focado)] <- "I"
res$FaixaEtaria[is.na(res$FaixaEtaria)] <- "0|-12"

gvik = res
remove(res)

# Plotando série

plot(gvik$Data_inter, gvik$`Contagem de Internação`, type="l" )




