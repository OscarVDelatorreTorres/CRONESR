# Cron de las notas de administraciond e inevrsiones y generar sus gráficas:

# Setup====

if (!require(eikonapir)) {install.packages('eikonapir')
  library(eikonapir)} else {library(eikonapir)}  
if (!require(tibble)) {install.packages('tibble')
  library(tibble)} else {library(tibble)}    
if (!require(stringr)) {install.packages('stringr')
  library(stringr)} else {library(stringr)}     
if (!require(openxlsx)) {install.packages('openxlsx')
  library(openxlsx)} else {library(openxlsx)}      
if (!require(lubridate)) {install.packages('lubridate')
  library(lubridate)} else {library(lubridate)}  
if (!require(quantmod)) {install.packages('quantmod')
  library(quantmod)} else {library(quantmod)}
if (!require(tseries)) {install.packages('tseries')
  library(tseries)} else {library(tseries)}
if (!require(RSQLite)) {install.packages('RSQLite')
  library(RSQLite)} else {library(RSQLite)}
if (!require(siebanxicor)) {install.packages('siebanxicor')
  library(siebanxicor)} else {library(siebanxicor)}

# Funciones de R desde Github:
source("https://raw.githubusercontent.com/OscarVDelatorreTorres/fechaTexto/main/fechaTexto.R")

# Se define el Token de consulta en Banxico:
banxico.token="b6b5b54077d88fce3eb64e2b92b0fc188923abe20cefc65a9f569b70f483128a"
setToken(banxico.token)

computadora="iMacUMSNH"

# Conecta a la base de datos de control:

switch(computadora,
       "iMacUMSNH"={rutaBD="/Users/oscardelatorretorres/Dropbox/01 TRABAJO/12 Software/CRONfiles/"},
       "MacProUMSNH"={rutaBD="/Users/oscardelatorretorres/Dropbox/01 TRABAJO/12 Software/CRONfiles/"}
)

conn <- dbConnect(RSQLite::SQLite(), paste0(rutaBD,"admonCRONES.db"))


crones=dbGetQuery(conn, "SELECT * FROM crones WHERE idCRON='CRONAssetAllocation'")

ruta=paste0(rutaBD,crones$folder[1],"/")

# Extrae las variaciones porcentuales del precio muestra====
# Ruta de BD:
rutaBDIndices="/Users/oscardelatorretorres/Dropbox/01 TRABAJO/12 Software/CRONfiles/01cronBDPreciosBase/01stockIndexes/equityIndexes.db"
connBD <- dbConnect(RSQLite::SQLite(), rutaBDIndices)

preciosIndices=dbGetQuery(connBD,"SELECT * FROM indexMXN WHERE RIC=='.MXX' AND Date>='2001-02-21'")

preciosIndices=data.frame(Date=as.Date(preciosIndices$Date),
                          IPC=preciosIndices$Close)

preciosIndices$worldStocks=NA

preciosIndices2=dbGetQuery(connBD,"SELECT * FROM indexMXN WHERE RIC=='.MIWO00000PUS' AND Date>='2001-02-21'")
preciosIndices2=data.frame(Date=as.Date(preciosIndices2$Date),
                           worldStocks=preciosIndices2$Close)

for (a in 1:nrow(preciosIndices)){
  idFila=which(preciosIndices2$Date==preciosIndices$Date[a])
  if (length(idFila)>0){
    preciosIndices$worldStocks[a]=preciosIndices2$worldStocks[idFila]    
  }

}

rendimientosAcciones=read.xlsx("https://www.dropbox.com/scl/fi/5lfhosv8o94qv55yozkn5/rendimientosMercadoCapitales.xlsx?rlkey=jobja1g8jg5du3mpnm7f6snpc&raw=1")
rendimientosAcciones=na.locf(rendimientosAcciones)

# Corrige VECTPAF
rendimientosAcciones$VECTPAF.MX=as.numeric(rendimientosAcciones$VECTPAF.MX)
rendimientosAcciones$VECTPAF.MX[which(is.na(rendimientosAcciones$VECTPAF.MX))]=0
rendimientosAcciones$VECTPAF.MX[which(is.infinite(rendimientosAcciones$VECTPAF.MX))]=0

rendimientosAcciones$NAFTRACISHRS.MX=as.numeric(rendimientosAcciones$NAFTRACISHRS.MX)

# Calcula los preciob B100 de los precios:
tablaPreciosB100=data.frame(Date=rendimientosAcciones$Date,
                       rendimientosAcciones[,2:ncol(rendimientosAcciones)]+1)

tablaPreciosB100[1,2:ncol(tablaPreciosB100)]=100


tablaPreciosB100[,2:ncol(tablaPreciosB100)]=cumprod(tablaPreciosB100[,2:ncol(tablaPreciosB100)])

valmerMD=read.csv("http://www.valmer.com.mx/VAL/Web_Benchmarks/SP/Historicos/Benchmarks_SP_Historico_MD.csv")
valmerNames=colnames(valmerMD)
valmerMD$FECHA=as.integer(valmerMD$FECHA)
valmerMD$FECHA=as.Date(ymd(valmerMD$FECHA))

# Objeto preciosFig1 para futuros cálculos:
preciosFig1=data.frame(
  Fecha=as.Date(tablaPreciosB100[,1]),
  bolsaMex=tablaPreciosB100$MXX
)

#cetesColId=which(valmerNames=="S.P.BMV.Sovereign.CETES.28.Day.Bond.Index")
#cetes28D=valmerMD[,cetesColId]

preciosFig1$CETES28D=NA

for (a in 1:nrow(preciosFig1)){
  fechaRowId=which(valmerMD$FECHA==preciosFig1$Fecha[a])
  if (length(fechaRowId)>0){
    preciosFig1$CETES28D[a]=valmerMD$`S.P.BMV.Sovereign.CETES.28.Day.Bond.Index`[fechaRowId]    
  }
  
}

if (is.na(preciosFig1$CETES28D[1])){
  preciosFig1$CETES28D[1]=preciosFig1$CETES28D[2]
}
preciosFig1$CETES28D=na.locf(preciosFig1$CETES28D)
preciosFig1$CETES28D=(preciosFig1$CETES28D/preciosFig1$CETES28D[1])*100

# Bonos guber
preciosFig1$bonosGuber=NA

for (a in 1:nrow(preciosFig1)){
  fechaRowId=which(valmerMD$FECHA==preciosFig1$Fecha[a])
  if (length(fechaRowId)>0){
    preciosFig1$bonosGuber[a]=valmerMD$`S.P.BMV.Mexico.Sovereign.Bond.Index`[fechaRowId]    
  }
  
}

if (is.na(preciosFig1$bonosGuber[1])){
  preciosFig1$bonosGuber[1]=preciosFig1$bonosGuber[2]
}
preciosFig1$bonosGuber=na.locf(preciosFig1$bonosGuber)
preciosFig1$bonosGuber=(preciosFig1$bonosGuber/preciosFig1$bonosGuber[1])*100

preciosFig1$bonosCorporativos=NA

for (a in 1:nrow(preciosFig1)){
  fechaRowId=which(valmerMD$FECHA==preciosFig1$Fecha[a])
  if (length(fechaRowId)>0){
    preciosFig1$bonosCorporativos[a]=valmerMD$`S.P.BMV.CORPOTRAC`[fechaRowId]    
  }
  
}

if (is.na(preciosFig1$bonosCorporativos[1])){
  preciosFig1$bonosCorporativos[1]=preciosFig1$bonosCorporativos[2]
}
preciosFig1$bonosCorporativos=na.locf(preciosFig1$bonosCorporativos)
preciosFig1$bonosCorporativos=(preciosFig1$bonosCorporativos/preciosFig1$bonosCorporativos[1])*100

# Bonos UMS
preciosFig1$UMS=NA

for (a in 1:nrow(preciosFig1)){
  fechaRowId=which(valmerMD$FECHA==preciosFig1$Fecha[a])
  if (length(fechaRowId)>0){
    preciosFig1$UMS[a]=valmerMD$`S.P.BMV.Sovereign.International.UMS.Bond.Index..USD.`[fechaRowId]    
  }
  
}

if (is.na(preciosFig1$UMS[1])){
  preciosFig1$UMS[1]=preciosFig1$UMS[2]
}
preciosFig1$UMS=na.locf(preciosFig1$UMS)
preciosFig1$UMS=(preciosFig1$UMS/preciosFig1$UMS[1])*100

# CETES 7 D
preciosFig1$CETES7D=NA

for (a in 1:nrow(preciosFig1)){
  fechaRowId=which(valmerMD$FECHA==preciosFig1$Fecha[a])
  if (length(fechaRowId)>0){
    preciosFig1$CETES7D[a]=valmerMD$`S.P.BMV.Sovereign.CETES.7.Day.Bond.Index`[fechaRowId]    
  }
  
}

if (is.na(preciosFig1$CETES7D[1])){
  preciosFig1$CETES7D[1]=preciosFig1$CETES7D[2]
}
preciosFig1$CETES7D=na.locf(preciosFig1$CETES7D)
preciosFig1$CETES7D=(preciosFig1$CETES7D/preciosFig1$CETES7D[1])*100

# Bonos M 1 a 5:
preciosFig1$BonosM1a5=NA

for (a in 1:nrow(preciosFig1)){
  fechaRowId=which(valmerMD$FECHA==preciosFig1$Fecha[a])
  if (length(fechaRowId)>0){
    preciosFig1$BonosM1a5[a]=valmerMD$`S.P.BMV.Government.MBONOS.1.5.Year.Bond.Index`[fechaRowId]    
  }
  
}

if (is.na(preciosFig1$BonosM1a5[1])){
  preciosFig1$BonosM1a5[1]=preciosFig1$BonosM1a5[2]
}
preciosFig1$BonosM1a5=na.locf(preciosFig1$BonosM1a5)
preciosFig1$BonosM1a5=(preciosFig1$BonosM1a5/preciosFig1$BonosM1a5[1])*100

# Bonos M 5 a 10:
preciosFig1$BonosM5a10=NA

for (a in 1:nrow(preciosFig1)){
  fechaRowId=which(valmerMD$FECHA==preciosFig1$Fecha[a])
  if (length(fechaRowId)>0){
    preciosFig1$BonosM5a10[a]=valmerMD$`S.P.BMV.Government.MBONOS.5.10.Year.Bond.Index`[fechaRowId]    
  }
  
}

if (is.na(preciosFig1$BonosM5a10[1])){
  preciosFig1$BonosM5a10[1]=preciosFig1$BonosM5a10[2]
}
preciosFig1$BonosM5a10=na.locf(preciosFig1$BonosM5a10)
preciosFig1$BonosM5a10=(preciosFig1$BonosM5a10/preciosFig1$BonosM5a10[1])*100

# Bonos M
preciosFig1$BonosM=NA

for (a in 1:nrow(preciosFig1)){
  fechaRowId=which(valmerMD$FECHA==preciosFig1$Fecha[a])
  if (length(fechaRowId)>0){
    preciosFig1$BonosM[a]=valmerMD$`S.P.BMV.Sovereign.MBONOS.Bond.Index`[fechaRowId]    
  }
  
}

if (is.na(preciosFig1$BonosM[1])){
  preciosFig1$BonosM[1]=preciosFig1$BonosM[2]
}
preciosFig1$BonosM=na.locf(preciosFig1$BonosM)
preciosFig1$BonosM=(preciosFig1$BonosM/preciosFig1$BonosM[1])*100


# NAFTRAC
preciosFig1$NAFTRAC=tablaPreciosB100$NAFTRACISHRS.MX


if (is.na(preciosFig1$NAFTRAC[1])){
  preciosFig1$NAFTRAC[1]=preciosFig1$NAFTRAC[2]
}
preciosFig1$NAFTRAC=na.locf(preciosFig1$NAFTRAC)
preciosFig1$NAFTRAC=(preciosFig1$NAFTRAC/preciosFig1$NAFTRAC[1])*100

# VECTPAF:
preciosFig1$VECTPAF=tablaPreciosB100$VECTPAF.MX


if (is.na(preciosFig1$VECTPAF[1])){
  preciosFig1$VECTPAF[1]=preciosFig1$VECTPAF[2]
}
preciosFig1$VECTPAF=na.locf(preciosFig1$VECTPAF)
preciosFig1$VECTPAF=(preciosFig1$VECTPAF/preciosFig1$VECTPAF[1])*100

# USDMXN:
preciosFig1$USDMXN=tablaPreciosB100$USDMXN


if (is.na(preciosFig1$USDMXN[1])){
  preciosFig1$USDMXN[1]=preciosFig1$USDMXN[2]
}
preciosFig1$USDMXN=na.locf(preciosFig1$USDMXN)
preciosFig1$USDMXN=(preciosFig1$USDMXN/preciosFig1$USDMXN[1])*100

# Oro:
preciosFig1$Oro=tablaPreciosB100$GLD


if (is.na(preciosFig1$Oro[1])){
  preciosFig1$Oro[1]=preciosFig1$Oro[2]
}
preciosFig1$Oro=na.locf(preciosFig1$Oro)
preciosFig1$Oro=(preciosFig1$Oro/preciosFig1$Oro[1])*100

# Petroleo:
preciosFig1$WTI=tablaPreciosB100$CLF


if (is.na(preciosFig1$WTI[1])){
  preciosFig1$WTI[1]=preciosFig1$WTI[2]
}
preciosFig1$WTI=na.locf(preciosFig1$WTI)
preciosFig1$WTI=(preciosFig1$WTI/preciosFig1$WTI[1])*100

# Escribe tablas de salida de valores históricos====

write.xlsx(preciosFig1,paste0(ruta,"historicosAssetAllocation.xlsx"),overwrite=TRUE)

# Fuente de VALMER: "http://www.valmer.com.mx/es/valmer/Indices_SP"
valmerMBonos=read.csv("http://www.valmer.com.mx/VAL/Web_Benchmarks/SP/ConstituentsUFF/20230825_SPVMBNS_CLS.csv")
write.xlsx(valmerMBonos,paste0(ruta,"benchmarkBonosM.xlsx"),overwrite=TRUE)

# Genera tabla de resumen estadístico 1 para notas (precio):

resumenEstadistico1P=data.frame(
  CETES28D=preciosFig1$CETES28D,
  bonosM=preciosFig1$BonosM,
  UMS=preciosFig1$UMS,
  IPC=preciosFig1$bolsaMex,
  oro=preciosFig1$Oro,
  WTI=preciosFig1$WTI
)

write.xlsx(resumenEstadistico1P,paste0(ruta,"resumenPortafolioP1.xlsx"),overwrite=TRUE)

resumenEstadistico2P=data.frame(
  IPC=preciosFig1$bolsaMex,
  ALFAA=tablaPreciosB100$ALFAA.MX,
  BIMBOA=tablaPreciosB100$BIMBOA.MX,
  CEMEXCPO=tablaPreciosB100$CEMEXCPO.MX,
  NAFTRAC=tablaPreciosB100$NAFTRACISHRS.MX,
  FUNO=tablaPreciosB100$FUNO11.MX,
  FIHO=tablaPreciosB100$FIHO12.MX,
  GOOGLMX=tablaPreciosB100$GOOGL.MX
)

write.xlsx(resumenEstadistico2P,paste0(ruta,"resumenPortafolioP2.xlsx"),overwrite=TRUE)

resumenEstadistico1R=(log(resumenEstadistico1P[2:nrow(resumenEstadistico1P),])-
  log(resumenEstadistico1P[1:(nrow(resumenEstadistico1P)-1),]))*100

write.xlsx(resumenEstadistico1R,paste0(ruta,"resumenPortafolioR1.xlsx"),overwrite=TRUE)

resumenEstadistico2R=(log(resumenEstadistico2P[2:nrow(resumenEstadistico2P),])-
  log(resumenEstadistico2P[1:(nrow(resumenEstadistico2P)-1),]))*100

write.xlsx(resumenEstadistico2R,paste0(ruta,"resumenPortafolioR2.xlsx"),overwrite=TRUE)

#valmerCETES28=read.csv("http://www.valmer.com.mx/VAL/Web_Benchmarks/SP/ConstituentsUFF/20230825_SPVM28A_CLS.csv")
#valmerUMS=read.csv("http://www.valmer.com.mx/VAL/Web_Benchmarks/SP/ConstituentsUFF/20230825_SPVIUMS_CLS.csv")
#valmerFondeoBancario=read.csv("http://www.valmer.com.mx/VAL/Web_Benchmarks/SP/ConstituentsUFF/20230825_SPVFONR_CLS.csv")
#valmerCorpoTrac=read.csv("http://www.valmer.com.mx/VAL/Web_Benchmarks/SP/ConstituentsUFF/20230825_SPVCORT_CLS.csv")
#valmerTIIE28=read.csv("http://www.valmer.com.mx/VAL/Web_Benchmarks/SP/ConstituentsUFF/20230825_SPVIIRB_CLS.csv")