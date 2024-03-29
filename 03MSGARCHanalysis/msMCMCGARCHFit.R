
msMCMCGARCHFit=function(eq,Data,numberMCMC=10000,numberBurn=500,GARCHmodels=c("sGARCH","sGARCH"),pdfFunct=c("norm","norm"),experiment="",timeFixed=FALSE){
  
  #outputData=Data
  
  # Starts calculation time:  
  startCalculation=Sys.time()
  
  words <- strsplit(eq, "[^[:alnum:]]+") # Split the string using non-alphanumeric characters as separators
  words = words[[1]]
  words=words[-1]
  num_words <- length(words)
  
  variables=c("(Intercept)",words)
  variablesValues=rep(0,num_words)
  
  # Fit the full model 
  full.model <- lm(eq, data = Data)
  # Stepwise regression model
  step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
  
  coeficientesStepWise=summary(step.model)$coefficients
  
  num_VarsStep=nrow(coeficientesStepWise)
  name_VarsStep=rownames(coeficientesStepWise)
  
  
  DBTable=data.frame(
    Date=as.character(tail(Data$Date,1)),
    Value=rep(0,length(variables)),
    Ticker="factor model coefs",
    Experiment=experiment
  )
  
  for (numVar in 1:nrow(DBTable)){
    
    idCoefRow=which(name_VarsStep[numVar]==DBTable$Ticker)
    if (length(idCoefRow)>0){
      DBTable$Value[idCoefRow]=coeficientesStepWise[numVar] 
    }
    
  }  
  
  DBTable=rbind(DBTable,
                data.frame(
                  Date=as.character(tail(Data$Date,1)),
                  Value=tail(datos$Settle,1),
                  Ticker="Price at t",
                  Experiment=experiment
                ),
                data.frame(
                  Date=as.character(tail(Data$Date,1)),
                  Value=tail(datos$Return,1),
                  Ticker="Return at t",
                  Experiment=experiment
                ),
                data.frame(
                  Date=as.character(tail(Data$Date,1)),
                  Value=tail(step.model$fitted.values,1),
                  Ticker="Return Forecast at t",
                  Experiment=experiment
                )                   
  )
  # Extract the residuals from the model:
  residuals=step.model$residuals
  
  # Starts calculation time:  
  startCalculation=Sys.time()

# Determines if the equation is a multifactor or a single one:
    
  if (isTRUE(timeFixed)){
    
    MSspec=CreateSpec(variance.spec = list(model = c("sARCH","sARCH")), 
                      distribution.spec = list(distribution = pdfFunct),
                      switch.spec = list(do.mix = FALSE),
                      constraint.spec = list(fixed = list(alpha1_1 = 1e-06,
                                                          alpha1_2 = 1e-06)))    
  } else {

    MSspec=CreateSpec(variance.spec = list(model = GARCHmodels), 
                      distribution.spec = list(distribution = pdfFunct),
                      switch.spec = list(do.mix = FALSE))
    
  } 
 
# Estimates the MSGARH model:
  
print(paste0("Estimating MS-GARCH model for", experiment,", date: ",tail(Data$Date)))  

  fittedMSGARCHD = tryCatch(FitMCMC(spec = MSspec, data = residuals, 
                                    ctr=list(nburn=numberBurn,nmcmc=numberMCMC)) , 
                           error=function(e) NULL) 
  
  if (!is.null(fittedMSGARCHD)){

    meltedCoefsTable=melt(summary(fittedMSGARCHD)$summary,id=c("Mean","SE"))
    # Parameters coefficients:
    DBTable=rbind(DBTable,
                  data.frame(Date=as.character(tail(Data$Date,1)),
                       Value=meltedCoefsTable$value,
                       Ticker=paste0(meltedCoefsTable$Var1,"-",meltedCoefsTable$Var2),
                       Experiment=experiment
                              )
    )
    # DIC values:
    DBTable=rbind(DBTable,
                  data.frame(Date=as.character(tail(Data$Date,1)),
                             Value=summary(fittedMSGARCHD)$DIC,
                             Ticker="DIC",
                             Experiment=experiment
                  )
    )    
    
  }
  
# Generating the Smoothed, transtition and forecasted probabilities for model 1:

cat("\f")  

print(paste0("Estimating regime-specific smoothed probs. ","norm","-","norm"," (model 1 of 6). (",experiment,"-",GARCHmodels,")")) 

# Smoothed probabilites:
Smooth.probs1 = State(fittedMSGARCHD)$SmoothProb[,1, 1:MSspec$K, drop = TRUE]

# Transition probability matrix:
transprob1=summary(fittedMSGARCHD)$post.trans.mat

# Forecasted prpbabilities:

Predprobs1 = rbind(Smooth.probs1[nrow(Smooth.probs1),]%*%transprob1,
                  Smooth.probs1[nrow(Smooth.probs1),]%*%transprob1^2,
                  Smooth.probs1[nrow(Smooth.probs1),]%*%transprob1^3,
                  Smooth.probs1[nrow(Smooth.probs1),]%*%transprob1^4,
                  Smooth.probs1[nrow(Smooth.probs1),]%*%transprob1^5)

# smooth probs Data Base table:
DBTable=rbind(DBTable,
            data.frame(Date=as.character(tail(Data$Date,1)),
           Value=c(Predprobs1[,1],Predprobs1[,2]),
           Ticker=c(paste0("Calm prob. forecast t+",seq(1,5)),
                    paste0("Crisis prob. forecast t+",seq(1,5))),
           Experiment=experiment
           )
)

# Estimates forecasted volatility and VaR at t:

cat("\f")
print(paste0("Estimating expected volatility at t. (",experiment,"-",GARCHmodels,")"))

pred1 <- predict(fittedMSGARCHD, nahead = 5, do.return.draws = FALSE)


DBTable=rbind(DBTable,
              data.frame(
                Date=as.character(tail(Data$Date,1)),
                Value=pred1$vol,
                Ticker=paste0(paste0("Volatility at t+",seq(1:5))," model 1"),
                Experiment=experiment
              )           
)

# Estimates CVaR

RiskPred=Risk(fittedMSGARCHD,alpha=c(0.02,0.05),nahead=5)

# Risk forecast table:

DBTable=rbind(DBTable,
              data.frame(Date=as.character(tail(Data$Date,1)),
                         Value=c(RiskPred$VaR[,1],
                                 RiskPred$VaR[,2]),
                         Ticker=c(paste0("VaR 98% t+",seq(1,5)),
                                  paste0("VaR 95% t+",seq(1,5))),
                         Experiment=experiment
                          ),
              data.frame(Date=as.character(tail(Data$Date,1)),
                         Value=c(RiskPred$ES[,1],
                                 RiskPred$ES[,2]),
                         Ticker=c(paste0("CVaR 98% t+",seq(1,5)),
                                  paste0("CVaR 95% t+",seq(1,5))),
                         Experiment=experiment
              )              
)
              
# Closes the code:  
  tiempoPasado=as.numeric(Sys.time()-startCalculation)
  
  elapsTimeMsg=paste0("Estimation elapsed time: ",
                      round(tiempoPasado/360,0),
                      ":",
                      round(tiempoPasado/60,0),":",
                      round(tiempoPasado,3))
  DBTable=rbind(DBTable,
               data.frame(Date=as.character(tail(Data$Date,1)),
                         Value=c(tiempoPasado,
                                 elapsTimeMsg),
                         Ticker=c("elapsed time",
                                  "elapses time message"),
                         Experiment=experiment)
               )
  
  MSGARCHResults=list(
    outputData=outputData,
    probsDBtable=DBTable,
    elapsTimeMsg=elapsTimeMsg,
    elapsTime=tiempoPasado
  )
  
  cat("\f")
  print(elapsTimeMsg)
  
  return(MSGARCHResults)

}
