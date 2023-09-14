if (!require(devtools)) {install.packages("devtools")
  library(devtools)} else {library(devtools)}  
if (!require(eikonapir)) {install_github("ahmedmohamedali/eikonapir")
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
if (!require(forecast)) {install.packages('forecast')
  library(forecast)} else {library(forecast)}
if (!require(RSQLite)) {install.packages('RSQLite')
  library(RSQLite)} else {library(RSQLite)}
if (!require(MSwM)) {install.packages('MSwM')
  library(MSwM)} else {library(MSwM)}

source("https://raw.githubusercontent.com/OscarVDelatorreTorres/CRONESR/main/02MSanalysis/fourRegimesIdentification.R?token=GHSAT0AAAAAACHRY45I66TUM4YKQM4AIWPCZIDRCJA")

set_app_id('617a3f4d2db44d8f859314f8b45307a9be2d33cd')

computadora="MacProUMSNH"

# Conecta a la base de datos de control:

switch(computadora,
       "MacProUMSNH"={rutaBD="/Users/oscardelatorretorres/Dropbox/01 TRABAJO/12 Software/CRONfiles/01cronBDPreciosBase/02CommodittiesFutures/"}
)

# Conexión con base de datos:

DBroute=paste0(rutaBD,"commodityFutures.db")
connIndex <- dbConnect(RSQLite::SQLite(), DBroute)  

RICS=dbGetQuery(connIndex,"SELECT * FROM RIC WHERE updateCondMean=='YES'")
a=1

tsData=dbGetQuery(connIndex,
                  paste0("SELECT * FROM priceNative WHERE RIC=='",RICS$RIC[a],"'")
                  )
tsDataD=data.frame(Date=as.Date(tsData$Date),
                  Last=as.numeric(tsData$Close),
                  Return=NA)

tsDataD$Return[2:nrow(tsDataD)]=diff(log(tsDataD$Last[2:nrow(tsDataD)]))*100
tsDataD=tsDataD[-1,]

#tsDataW=to.weekly(tsDataD,OHLC=FALSE)

# ARIMA forecast:
# arimaObjectD=auto.arima(tsDataD$Return,max.p=2,max.q=2,d=0,seasonal=TRUE,allowmean = TRUE,allowdrift = TRUE,trace=FALSE)

pruebaDataFrame=fourRegimesIdentificacion(tsDataD)


