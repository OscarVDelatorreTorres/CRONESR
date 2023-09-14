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

tsDataW=to.weekly(tsDataD,OHLC=FALSE)

# ARIMA forecast:
# arimaObjectD=auto.arima(tsDataD$Return,max.p=2,max.q=2,d=0,seasonal=TRUE,allowmean = TRUE,allowdrift = TRUE,trace=FALSE)

fourRegimesIdentificacion=function(Datos,regresoras=NA){

  if (is.na(regresoras)){
    ecuacion=paste0("Return~1")
  } else {
    ecuacion=paste0("Return~1",paste(regresoras,collapse="+"))    
  }
  
  # Return Scenario coding:
  if (tail(Datos$Return,1)<0){
    returnScenarioD=-1
  } else {
    returnScenarioD=1
  }
  
  # volatility scenario coding:
  
  
  modelo1d=lm(ecuacion,data=Datos)
  startTime=Sys.time()
  print("Estimating the two-regime MS model. Please, wait...")
  modD=msmFit(modelo1d,k=2,sw=c(TRUE,TRUE))
  endTime=Sys.time()
  
  # Identifies the column with the high volatility regime:
  reg2IdD=c(which(modD@std==min(modD@std)),which(modD@std==max(modD@std)))

  if (modD@Fit@smoProb[reg2IdD[1]]>0.5){
    volScenarioD=2
  } else {
    volScenarioD=1
  }
  
  
  # Determines the NN scenario code:
  # Daily:
  
  if (volScenarioD<2){
    if (returnScenarioD>0){
      regimeScenarioD=1
    } else {
      regimeScenarioD=2
    }
  } else {
    if (returnScenarioD>0){
      regimeScenarioD=3
    } else {
      regimeScenarioD=4
    }  
  }
  
  transMatrixFinal=matrix(rbind(modD@transMat[reg2IdD[1],],modD@transMat[reg2IdD[2],]),2,2)
  
  forecastProbs=as.data.frame(cbind(
        matrix(as.character(tail(Datos$Date,1)),5,1),
        c("ForecastSmothProbt1",
          "ForecastSmothProbt2",
          "ForecastSmothProbt3",
          "ForecastSmothProbt4",
          "ForecastSmothProbt5"),
        rbind(
    c(tail(modD@Fit@smoProb,1)[reg2IdD[1]],tail(modD@Fit@smoProb,1)[reg2IdD[2]])%*%(transMatrixFinal),
    c(tail(modD@Fit@smoProb,1)[reg2IdD[1]],tail(modD@Fit@smoProb,1)[reg2IdD[2]])%*%(transMatrixFinal^2),
    c(tail(modD@Fit@smoProb,1)[reg2IdD[1]],tail(modD@Fit@smoProb,1)[reg2IdD[2]])%*%(transMatrixFinal^3),
    c(tail(modD@Fit@smoProb,1)[reg2IdD[1]],tail(modD@Fit@smoProb,1)[reg2IdD[2]])%*%(transMatrixFinal^4),
    c(tail(modD@Fit@smoProb,1)[reg2IdD[1]],tail(modD@Fit@smoProb,1)[reg2IdD[2]])%*%(transMatrixFinal^5)
    )
  ))
  colnames(forecastProbs)=c("Date","Statistic")
  # output data table:
  
  regimeCols=data.frame(Date=tail(Datos$Date,1),
                        Statistic=c("returnScenarioD",
                                    "volatilityScenarioD",
                                    "regimeScenarioD",
                                    "expectedReturnR1D","expectedReturnR2D",
                                    "expectedVolR1D","expectedVolR2D",
                                    "observedReturnDT",
                                    "AICD",
                                    "LLF",
                                    "smoothProbR2TD",
                                    "transMatS1S1",
                                    "transMatS2S1",
                                    "transMatS1S2",
                                    "transMatS2S2",
                                    "timeEllapsed"),
                        Value=c(
                          returnScenarioD,
                          volScenarioD,
                          regimeScenarioD,
                          modD@Coef[reg2IdD[1],],modD@Coef[reg2IdD[2],],
                          modD@std[reg2IdD[1]],modD@std[reg2IdD[2]],
                          tail(Datos$Return,1),
                          AIC(modD),
                          modD@Fit@logLikel,
                          tail(modD@Fit@smoProb[reg2IdD[2]],1),
                          matrix(modD@transMat,4,1),
                          as.numeric(endTime-startTime)
                        ))
  objetoSalida=list(
    dbDataFrame=regimeCols,
    MSObject=modD,
    elapsTime=as.numeric(endTime-startTime)
  )
  
  print(paste0("Out put data frame ready for DB. Elapsed time: ",round(as.numeric(endTime-startTime),4)," minutes."))
  return(objetoSalida)
  
}

pruebaDataFrame=fourRegimesIdentificacion(tsDataD)


