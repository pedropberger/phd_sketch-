install.packages("read.dbc")
install.packages("xlsx")
install.packages("foreign")


library(read.dbc)
library(xlsx)
library(foreign)


## Pegar nomes dos arquivos (lembrar de alterar local)
file.names=list.files("SIH/")
write.csv(file.names, file = "arquivos.csv")



## Lendo a turma toda

x2<- read.dbc("SIH/RDES1501.dbc")
x3<- read.dbc("SIH/RDES1502.dbc")
x4<- read.dbc("SIH/RDES1503.dbc")
x5<- read.dbc("SIH/RDES1504.dbc")
x6<- read.dbc("SIH/RDES1505.dbc")
x7<- read.dbc("SIH/RDES1506.dbc")
x8<- read.dbc("SIH/RDES1507.dbc")
x9<- read.dbc("SIH/RDES1508.dbc")
x10<- read.dbc("SIH/RDES1509.dbc")
x11<- read.dbc("SIH/RDES1510.dbc")
x12<- read.dbc("SIH/RDES1511.dbc")
x13<- read.dbc("SIH/RDES1512.dbc")
x14<- read.dbc("SIH/RDES1601.dbc")
x15<- read.dbc("SIH/RDES1602.dbc")
x16<- read.dbc("SIH/RDES1603.dbc")
x17<- read.dbc("SIH/RDES1604.dbc")
x18<- read.dbc("SIH/RDES1605.dbc")
x19<- read.dbc("SIH/RDES1606.dbc")
x20<- read.dbc("SIH/RDES1607.dbc")
x21<- read.dbc("SIH/RDES1608.dbc")
x22<- read.dbc("SIH/RDES1609.dbc")
x23<- read.dbc("SIH/RDES1610.dbc")
x24<- read.dbc("SIH/RDES1611.dbc")
x25<- read.dbc("SIH/RDES1612.dbc")
x26<- read.dbc("SIH/RDES1701.dbc")
x27<- read.dbc("SIH/RDES1702.dbc")
x28<- read.dbc("SIH/RDES1703.dbc")
x29<- read.dbc("SIH/RDES1704.dbc")
x30<- read.dbc("SIH/RDES1705.dbc")
x31<- read.dbc("SIH/RDES1706.dbc")
x32<- read.dbc("SIH/RDES1707.dbc")
x33<- read.dbc("SIH/RDES1708.dbc")
x34<- read.dbc("SIH/RDES1709.dbc")
x35<- read.dbc("SIH/RDES1710.dbc")
x36<- read.dbc("SIH/RDES1711.dbc")
x37<- read.dbc("SIH/RDES1712.dbc")
x38<- read.dbc("SIH/RDES1801.dbc")
x39<- read.dbc("SIH/RDES1802.dbc")
x40<- read.dbc("SIH/RDES1803.dbc")
x41<- read.dbc("SIH/RDES1804.dbc")
x42<- read.dbc("SIH/RDES1805.dbc")
x43<- read.dbc("SIH/RDES1806.dbc")
x44<- read.dbc("SIH/RDES1807.dbc")
x45<- read.dbc("SIH/RDES1808.dbc")
x46<- read.dbc("SIH/RDES1809.dbc")
x47<- read.dbc("SIH/RDES1810.dbc")
x48<- read.dbc("SIH/RDES1811.dbc")
x49<- read.dbc("SIH/RDES1812.dbc")
x50<- read.dbc("SIH/RDES1901.dbc")
x51<- read.dbc("SIH/RDES1902.dbc")
x52<- read.dbc("SIH/RDES1903.dbc")
x53<- read.dbc("SIH/RDES1904.dbc")
x54<- read.dbc("SIH/RDES1905.dbc")
x55<- read.dbc("SIH/RDES1906.dbc")
x56<- read.dbc("SIH/RDES1907.dbc")
#x57<- read.dbc("SIH/RDES1907.dbf")
#x57<- read.dbf("SIH/RDES1907.dbf")
x58<- read.dbc("SIH/RDES1908.dbc")
x59<- read.dbc("SIH/RDES1909.dbc")
x60<- read.dbc("SIH/RDES1910.dbc")
x61<- read.dbc("SIH/RDES1911.dbc")
x62<- read.dbc("SIH/RDES1912.dbc")
x63<- read.dbc("SIH/RDES2001.dbc")
x64<- read.dbc("SIH/RDES2002.dbc")
x65<- read.dbc("SIH/RDES2003.dbc")
x66<- read.dbc("SIH/RDES2004.dbc")
x67<- read.dbc("SIH/RDES2005.dbc")


##Juntando a turma toda

DJ = rbind(
  x2,
  x3,
  x4,
  x5,
  x6,
  x7,
  x8,
  x9,
  x10,
  x11,
  x12,
  x13,
  x14,
  x15,
  x16,
  x17,
  x18,
  x19,
  x20,
  x21,
  x22,
  x23,
  x24,
  x25,
  x26,
  x27,
  x28,
  x29,
  x30,
  x31,
  x32,
  x33,
  x34,
  x35,
  x36,
  x37,
  x38,
  x39,
  x40,
  x41,
  x42,
  x43,
  x44,
  x45,
  x46,
  x47,
  x48,
  x49,
  x50,
  x51,
  x52,
  x53,
  x54,
  x55,
  x56,
  x57,
  x58,
  x59,
  x60,
  x61,
  x62,
  x63,
  x64,
  x65,
  x66,
  x67)


### Subset

DJ

DJ.filter <-- subset(DJ, select=c(CEP,
                                  MUNIC_RES,
                                  NASC,
                                  SEXO,
                                  DT_INTER,
                                  DT_SAIDA,
                                  DIAG_PRINC,
                                  MUNIC_MOV,
                                  COD_IDADE,
                                  IDADE,
                                  DIAS_PERM,
                                  MORTE,
                                  CAR_INT,
                                  RACA_COR,
                                  ETNIA))


names(DJ)

DJzinho[, ] <-- DJ[ ,c("CEP", "MUNIC_RES", "NASC", "SEXO", "DT_INTER", "DT_SAIDA", "DIAG_PRINC", "MUNIC_MOV", "COD_IDADE", "IDADE", "DIAS_PERM", "MORTE", "CAR_INT", "RACA_COR", "ETNIA")]

DJzinho

DJ.filter

### Escrevendo
write.csv(DJ, file = "DadosDJ.csv")

write.xlsx(DJ, "dadosDJ.xlsx", sheetName="data")

### Exemplo de como ler direto da fonte - não testei ###

#url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/DOPR2013.dbc"
#download.file(url, destfile = "DOPR2013.dbc")
#dopr <- read.dbc("DOPR2013.dbc")
#head(dopr)
#str(dopr)

