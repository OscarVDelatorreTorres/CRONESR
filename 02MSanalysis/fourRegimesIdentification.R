

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