#' @title Función msMCMCGARCHFit
#' @description
#' This function uses the Ardia et al. (2009) MSGARCH library to estimate Markov-switching GARCH models. This specific funciont generates a data frame that you can store in a data base for simulation purposes.
#' @param eq You can determine the type of conditional mean equation to be used, given a least squares lm() function estimation. If you have no regressors in your mean equation, you can use eq=[the variable name in data frame]~1.
#' @param Data Is the data.frame object with the input X and Y time series.
#' @param numberMCMC Is the number of Markov-chain Monte Carlo paths for the simulations. The default value is 10000.
#' @param numberBurn Is the number of burned paths for MCMC calibration purposes. The default value os 500.
#' @param GARCHmodels Is a vector object that determines the number of regimes (the length of the vector determines the number of regimes) and the GARCH model in each regime. The possible values are "sARCH" for a symmetric ARCH model, "SGARCH" (symmetric GARCH), "eGARCH" (Nelson's assymetric EGARCH model), "gjrGARCH" (the GJR-GARCH assymetric model), "tGARCH" (the assymetric t-GARCH one).
#' @param pdfFunct Is a vector with the marginal pdf of each regime. It must have the same length as object GARCHmodels. The options are "norm", "std", "ged" for the gaussian, Student's t and GED pdfs and their skewed versions ("snorm","sstd", "sged")
#' @param experiment Is a tittle with which your are goin to mark your experiment or estimations in your data base.
#' @param timeFixed Is a logical input that determines if the Markov-Switching model must estimate time-fixed variances or ARCH/GARCH models. The default is FALSE. If you write TRUE the GARCHmodels object is ignored.
#' @return A list object that includes the MSGARCH object, the data frame (dbData object) and a table with the X and Y data, dates and the regime-specific smoothed probabilities.
#' @examples
#' MCMCobject=msMCMCGARCHFit(eq="Y~X1+X2",Data=df,experiment="My tests")
#' MCMCobject=msMCMCGARCHFit(eq="Y~1",Data=df,GARCHmodels=c("sGARCH","gjrGARCH","gjrGARCH"),pdfFunct=c("norm","sstd","sged"),experiment="My tests")
#' MCMCobject=msMCMCGARCHFit(eq="Y~1",Data=df,pdfFunct=c("sstd","sged"),experiment="My tests",timeFixed=TRUE)

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
 
# Estimates the MSCARH model:
  
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
  
aaa=
# Generating the Smoothed, transtition and forecasted probabilities for model 1:

cat("\f")  

print(paste0("Estimating regime-specific smoothed probs. ","norm","-","norm"," (model 1 of 6). (",experiment,"-",estDate,"-",GARCHmodels,")")) 

# Smoothed probabilites:
Smooth.probs1 = State(fitedMSGARCHD1)$SmoothProb[,1, 1:MSspec1$K, drop = TRUE]

# Transition probability matrix:
transprob1=summary(fitedMSGARCHD1)$post.trans.mat

# Forecasted prpbabilities:

Predprobs1 = rbind(Smooth.probs1[nrow(Smooth.probs1),]%*%transprob1,
                  Smooth.probs1[nrow(Smooth.probs1),]%*%transprob1^2,
                  Smooth.probs1[nrow(Smooth.probs1),]%*%transprob1^3,
                  Smooth.probs1[nrow(Smooth.probs1),]%*%transprob1^4,
                  Smooth.probs1[nrow(Smooth.probs1),]%*%transprob1^5)

# smooth probs Data Base table:
DBTable=rbind(DBTable,
            data.frame(Date=as.character(tail(outputData$Date,1)),
           Value=c(Predprobs1[,1],Predprobs1[,2]),
           Ticker=c(paste0("Calm prob. forecast t+",seq(1,5)),
                    paste0("Crisis prob. forecast t+",seq(1,5))),
           ModelID=dicTable$Model[1],
           GARCHSpec=dicTable$GARCHSpec[1],
           Experiment=experiment
           )
)

# Adding smooth probs to output data:

outputData$NormNormSmoothProbR1=Smooth.probs1[2:nrow(Smooth.probs1),1]
outputData$NormNormSmoothProbR2=Smooth.probs1[2:nrow(Smooth.probs1),2]

# Creating the regime-specific chart data:

#regime1ChartDataPrice=data.frame(Date=Data$Date,
          #                       Value=Data$Settle,
           #                      Ticker="Historical price")

regime1ChartDataPrice=data.frame(Date=Data$Date,
                                 Value=Smooth.probs1[2:nrow(Smooth.probs1),1],
                                 Ticker=paste0(dicTable$Model[1]," LLFs"))

regime2ChartDataPrice=data.frame(Date=Data$Date,
                                       Value=Smooth.probs1[2:nrow(Smooth.probs1),2],
                                       Ticker=paste0(dicTable$Model[1]," LLFs"))


cat("\f")  

print(paste0("Estimating regime-specific smoothed probs. ","tStud","-","tSud"," (model 2 of 6). (",experiment,"-",estDate,"-",GARCHmodels,")")) 

# Smoothed, transtition and forecasted probabilities for model 2:

# Smoothed probabilites:
Smooth.probs2 = State(fitedMSGARCHD2)$SmoothProb[,1, 1:MSspec2$K, drop = TRUE]

# Transition probability matrix:
transprob2 = summary(fitedMSGARCHD2)$post.trans.mat

# Forecasted prpbabilities:

Predprobs2 = rbind(Smooth.probs2[nrow(Smooth.probs2),]%*%transprob2,
                   Smooth.probs2[nrow(Smooth.probs2),]%*%transprob2^2,
                   Smooth.probs2[nrow(Smooth.probs2),]%*%transprob2^3,
                   Smooth.probs2[nrow(Smooth.probs2),]%*%transprob2^4,
                   Smooth.probs2[nrow(Smooth.probs2),]%*%transprob2^5)

DBTable=rbind(DBTable,
              data.frame(Date=as.character(tail(outputData$Date,1)),
                   Value=c(Predprobs2[,1],Predprobs2[,2]),
                   Ticker=c(paste0("Calm prob. forecast t+",seq(1,5)),
                            paste0("Crisis prob. forecast t+",seq(1,5))),
                   ModelID=dicTable$Model[2],
                   GARCHSpec=dicTable$GARCHSpec[2],
                   Experiment=experiment
)
)
# Adding smooth probs to output data:

outputData$tStudtStudSmoothProbR1=Smooth.probs2[2:nrow(Smooth.probs2),1]
outputData$tStudtStudSmoothProbR2=Smooth.probs2[2:nrow(Smooth.probs2),2]

# Creating the regime-specific chart data:

regime1ChartDataPrice=rbind(regime1ChartDataPrice,
                            data.frame(Date=Data$Date,
                                       Value=Smooth.probs2[2:nrow(Smooth.probs2),1],
                                       Ticker=paste0(dicTable$Model[2]," LLFs"))
)

regime2ChartDataPrice=rbind(regime2ChartDataPrice,
                            data.frame(Date=Data$Date,
                                       Value=Smooth.probs2[2:nrow(Smooth.probs2),2],
                                       Ticker=paste0(dicTable$Model[2]," LLFs"))
)

cat("\f")  

print(paste0("Estimating regime-specific smoothed probs. ","GED","-","GED"," (model 3 of 6). (",experiment,"-",estDate,"-",GARCHmodels,")")) 

# Smoothed, transtition and forecasted probabilities for model 3:

# Smoothed probabilites:
Smooth.probs3 = State(fitedMSGARCHD3)$SmoothProb[,1, 1:MSspec3$K, drop = TRUE]

# Transition probability matrix:
transprob3 = summary(fitedMSGARCHD3)$post.trans.mat

# Forecasted prpbabilities:

Predprobs3 = rbind(Smooth.probs3[nrow(Smooth.probs3),]%*%transprob3,
                   Smooth.probs3[nrow(Smooth.probs3),]%*%transprob3^2,
                   Smooth.probs3[nrow(Smooth.probs3),]%*%transprob3^3,
                   Smooth.probs3[nrow(Smooth.probs3),]%*%transprob3^4,
                   Smooth.probs3[nrow(Smooth.probs3),]%*%transprob3^5)

DBTable=rbind(DBTable,
              data.frame(Date=as.character(tail(outputData$Date,1)),
                         Value=c(Predprobs3[,1],Predprobs3[,2]),
                         Ticker=c(paste0("Calm prob. forecast t+",seq(1,5)),
                                  paste0("Crisis prob. forecast t+",seq(1,5))),
                         ModelID=dicTable$Model[3],
                         GARCHSpec=dicTable$GARCHSpec[3],
                         Experiment=experiment
              )
)

# Adding smooth probs to output data:

outputData$GEDGEDSmoothProbR1=Smooth.probs3[2:nrow(Smooth.probs3),1]
outputData$GEDGEDSmoothProbR2=Smooth.probs3[2:nrow(Smooth.probs3),2]

# chart data tables update:
regime1ChartDataPrice=rbind(regime1ChartDataPrice,
                            data.frame(Date=Data$Date,
                                       Value=Smooth.probs3[2:nrow(Smooth.probs3),1],
                                       Ticker=paste0(dicTable$Model[3]," LLFs"))
)

regime2ChartDataPrice=rbind(regime2ChartDataPrice,
                            data.frame(Date=Data$Date,
                                       Value=Smooth.probs3[2:nrow(Smooth.probs3),2],
                                       Ticker=paste0(dicTable$Model[3]," LLFs"))
)

cat("\f")  

print(paste0("Estimating regime-specific smoothed probs. ","norm","-","tStud"," (model 4 of 6). (",experiment,"-",estDate,"-",GARCHmodels,")")) 


# Smoothed, transtition and forecasted probabilities for model 4:

# Smoothed probabilites:
Smooth.probs4 = State(fitedMSGARCHD4)$SmoothProb[,1, 1:MSspec4$K, drop = TRUE]

# Transition probability matrix:
transprob4 = summary(fitedMSGARCHD4)$post.trans.mat

# Forecasted prpbabilities:

Predprobs4 = rbind(Smooth.probs4[nrow(Smooth.probs4),]%*%transprob4,
                   Smooth.probs4[nrow(Smooth.probs4),]%*%transprob4^2,
                   Smooth.probs4[nrow(Smooth.probs4),]%*%transprob4^3,
                   Smooth.probs4[nrow(Smooth.probs4),]%*%transprob4^4,
                   Smooth.probs4[nrow(Smooth.probs4),]%*%transprob4^5)

DBTable=rbind(DBTable,
              data.frame(Date=as.character(tail(outputData$Date,1)),
                         Value=c(Predprobs4[,1],Predprobs4[,2]),
                         Ticker=c(paste0("Calm prob. forecast t+",seq(1,5)),
                                  paste0("Crisis prob. forecast t+",seq(1,5))),
                         ModelID=dicTable$Model[4],
                         GARCHSpec=dicTable$GARCHSpec[4],
                         Experiment=experiment
              )
)
# Adding smooth probs to output data:

outputData$NormtStudSmoothProbR1=Smooth.probs4[2:nrow(Smooth.probs4),1]
outputData$NormtStudSmoothProbR2=Smooth.probs4[2:nrow(Smooth.probs4),2]

# chart data tables update:
regime1ChartDataPrice=rbind(regime1ChartDataPrice,
                            data.frame(Date=Data$Date,
                                       Value=Smooth.probs4[2:nrow(Smooth.probs4),1],
                                       Ticker=paste0(dicTable$Model[4]," LLFs"))
)

regime2ChartDataPrice=rbind(regime2ChartDataPrice,
                            data.frame(Date=Data$Date,
                                       Value=Smooth.probs4[2:nrow(Smooth.probs4),2],
                                       Ticker=paste0(dicTable$Model[4]," LLFs"))
)

cat("\f")  

print(paste0("Estimating regime-specific smoothed probs. ","norm","-","GED"," (model 5 of 6). (",experiment,"-",estDate,"-",GARCHmodels,")")) 


# Smoothed, transtition and forecasted probabilities for model 5:

# Smoothed probabilites:
Smooth.probs5 = State(fitedMSGARCHD5)$SmoothProb[,1, 1:MSspec5$K, drop = TRUE]

# Transition probability matrix:
transprob5 = summary(fitedMSGARCHD5)$post.trans.mat

# Forecasted prpbabilities:

Predprobs5 = rbind(Smooth.probs5[nrow(Smooth.probs5),]%*%transprob5,
                   Smooth.probs5[nrow(Smooth.probs5),]%*%transprob5^2,
                   Smooth.probs5[nrow(Smooth.probs5),]%*%transprob5^3,
                   Smooth.probs5[nrow(Smooth.probs5),]%*%transprob5^4,
                   Smooth.probs5[nrow(Smooth.probs5),]%*%transprob5^5)

DBTable=rbind(DBTable,
              data.frame(Date=as.character(tail(outputData$Date,1)),
                         Value=c(Predprobs5[,1],Predprobs5[,2]),
                         Ticker=c(paste0("Calm prob. forecast t+",seq(1,5)),
                                  paste0("Crisis prob. forecast t+",seq(1,5))),
                         ModelID=dicTable$Model[5],
                         GARCHSpec=dicTable$GARCHSpec[5],
                         Experiment=experiment
              )
)

# Adding smooth probs to output data:

outputData$NormGEDSmoothProbR1=Smooth.probs5[2:nrow(Smooth.probs5),1]
outputData$NormGEDSmoothProbR2=Smooth.probs5[2:nrow(Smooth.probs5),2]

# chart data tables update:
regime1ChartDataPrice=rbind(regime1ChartDataPrice,
                            data.frame(Date=Data$Date,
                                       Value=Smooth.probs5[2:nrow(Smooth.probs5),1],
                                       Ticker=paste0(dicTable$Model[5]," LLFs"))
)

regime2ChartDataPrice=rbind(regime2ChartDataPrice,
                            data.frame(Date=Data$Date,
                                       Value=Smooth.probs5[2:nrow(Smooth.probs5),2],
                                       Ticker=paste0(dicTable$Model[5]," LLFs"))
)

cat("\f")  

print(paste0("Estimating regime-specific smoothed probs. ","tStud","-","GED"," (model 6 of 6). (",experiment,"-",estDate,"-",GARCHmodels,")")) 

# Smoothed, transtition and forecasted probabilities for model 6:

# Smoothed probabilites:
Smooth.probs6 = State(fitedMSGARCHD6)$SmoothProb[,1, 1:MSspec6$K, drop = TRUE]

# Transition probability matrix:
transprob6 = summary(fitedMSGARCHD6)$post.trans.mat

# Forecasted prpbabilities:

Predprobs6 = rbind(Smooth.probs6[nrow(Smooth.probs6),]%*%transprob6,
                   Smooth.probs6[nrow(Smooth.probs6),]%*%transprob6^2,
                   Smooth.probs6[nrow(Smooth.probs6),]%*%transprob6^3,
                   Smooth.probs6[nrow(Smooth.probs6),]%*%transprob6^4,
                   Smooth.probs6[nrow(Smooth.probs6),]%*%transprob6^5)

DBTable=rbind(DBTable,
              data.frame(Date=as.character(tail(outputData$Date,1)),
                         Value=c(Predprobs6[,1],Predprobs6[,2]),
                         Ticker=c(paste0("Calm prob. forecast t+",seq(1,5)),
                                  paste0("Crisis prob. forecast t+",seq(1,5))),
                         ModelID=dicTable$Model[6],
                         GARCHSpec=dicTable$GARCHSpec[6],
                         Experiment=experiment
              )
)

# Adding smooth probs to output data:

outputData$tStudGEDSmoothProbR1=Smooth.probs6[2:nrow(Smooth.probs6),1]
outputData$tStudGEDSmoothProbR2=Smooth.probs6[2:nrow(Smooth.probs6),2]

# chart data tables update:
regime1ChartDataPrice=rbind(regime1ChartDataPrice,
                            data.frame(Date=Data$Date,
                                       Value=Smooth.probs6[2:nrow(Smooth.probs6),1],
                                       Ticker=paste0(dicTable$Model[6]," LLFs"))
)

regime2ChartDataPrice=rbind(regime2ChartDataPrice,
                            data.frame(Date=Data$Date,
                                       Value=Smooth.probs6[2:nrow(Smooth.probs6),2],
                                       Ticker=paste0(dicTable$Model[6]," LLFs"))
)

# Smoothed, transtition and forecasted probabilities for model 7 (time-fixed):

# Smoothed probabilites:
Smooth.probs7 = State(fitedMSGARCHD7)$SmoothProb[,1, 1:MSspec7$K, drop = TRUE]

# Transition probability matrix:
transprob7 = summary(fitedMSGARCHD7)$post.trans.mat

# Forecasted prpbabilities:

Predprobs7 = rbind(Smooth.probs6[nrow(Smooth.probs7),]%*%transprob7,
                   Smooth.probs6[nrow(Smooth.probs7),]%*%transprob7^2,
                   Smooth.probs6[nrow(Smooth.probs7),]%*%transprob7^3,
                   Smooth.probs6[nrow(Smooth.probs7),]%*%transprob7^4,
                   Smooth.probs6[nrow(Smooth.probs7),]%*%transprob7^5)

DBTable=rbind(DBTable,
              data.frame(Date=as.character(tail(outputData$Date,1)),
                         Value=c(Predprobs7[,1],Predprobs7[,2]),
                         Ticker=c(paste0("Calm prob. forecast t+",seq(1,5)),
                                  paste0("Crisis prob. forecast t+",seq(1,5))),
                         ModelID=dicTable$Model[7],
                         GARCHSpec=dicTable$GARCHSpec[7],
                         Experiment=experiment
              )
)

# Adding smooth probs to output data:

outputData$tStudGEDSmoothProbR1=Smooth.probs6[2:nrow(Smooth.probs6),1]
outputData$tStudGEDSmoothProbR2=Smooth.probs6[2:nrow(Smooth.probs6),2]

# chart data tables update:
regime1ChartDataPrice=rbind(regime1ChartDataPrice,
                            data.frame(Date=Data$Date,
                                       Value=Smooth.probs6[2:nrow(Smooth.probs6),1],
                                       Ticker=paste0(dicTable$Model[6]," LLFs"))
)

regime2ChartDataPrice=rbind(regime2ChartDataPrice,
                            data.frame(Date=Data$Date,
                                       Value=Smooth.probs6[2:nrow(Smooth.probs6),2],
                                       Ticker=paste0(dicTable$Model[6]," LLFs"))
)

# Smoothed, transtition and forecasted probabilities for best model:

cat("\f")  

print(paste0("Estimating regime-specific smoothed probs. ","(best fiting model)")) 

msBestmodel=dicTable$msMod[8]



# Smoothed probabilites:
eval(parse(text=paste0("Smooth.probsBest = State(",msBestmodel,")$SmoothProb[,1, 1:MSspec5$K, drop = TRUE]")))

# Transition probability matrix:
eval(parse(text=paste0("transprobBest = summary(",msBestmodel,")$post.trans.mat")))

# Forecasted prpbabilities:

PredprobsBest = rbind(Smooth.probsBest[nrow(Smooth.probsBest),]%*%transprobBest,
                      Smooth.probsBest[nrow(Smooth.probsBest),]%*%transprobBest^2,
                      Smooth.probsBest[nrow(Smooth.probsBest),]%*%transprobBest^3,
                      Smooth.probsBest[nrow(Smooth.probsBest),]%*%transprobBest^4,
                      Smooth.probsBest[nrow(Smooth.probsBest),]%*%transprobBest^5)

# Adding smooth probs to output data:

outputData$BestModelSmoothProbR1=Smooth.probsBest[2:nrow(Smooth.probsBest),1]
outputData$BestModelSmoothProbR2=Smooth.probsBest[2:nrow(Smooth.probsBest),2]

# chart data tables update:
regime1ChartDataPrice=rbind(regime1ChartDataPrice,
                            data.frame(Date=Data$Date,
                                       Value=Smooth.probsBest[2:nrow(Smooth.probsBest),1],
                                       Ticker=paste0(dicTable$Model[7]," (",
                                                     dicTable$Model[bestModelRow],")"))
)

regime2ChartDataPrice=rbind(regime2ChartDataPrice,
                            data.frame(Date=Data$Date,
                                       Value=Smooth.probsBest[2:nrow(Smooth.probsBest),2],
                                       Ticker=paste0(dicTable$Model[7]," (",
                                                     dicTable$Model[bestModelRow],")"))
)

# Estimates forecasted volatility and VaR at t:

cat("\f")
print(paste0("Estimating expected volatility at t (model 1). (",experiment,"-",estDate,")"))

pred1 <- predict(fitedMSGARCHD1, nahead = 5, do.return.draws = FALSE)


DBTable=rbind(DBTable,
              data.frame(
                Date=as.character(tail(outputData$Date,1)),
                Value=pred1$vol,
                Ticker=paste0(paste0("Volatility at t+",seq(1:5))," model 1"),
                ModelID=dicTable$Model[1],
                GARCHSpec=dicTable$GARCHSpec[1],
                Experiment=experiment
              )           
)

print(paste0("Estimating expected volatility at t (model 2). (",experiment,"-",estDate,"-",GARCHmodels,")"))

pred2 <- predict(fitedMSGARCHD2, nahead = 5, do.return.draws = FALSE)


DBTable=rbind(DBTable,
              data.frame(
                Date=as.character(tail(outputData$Date,1)),
                Value=pred2$vol,
                Ticker=paste0(paste0("Volatility at t+",seq(1:5))," model 2"),
                ModelID=dicTable$Model[2],
                GARCHSpec=dicTable$GARCHSpec[2],
                Experiment=experiment
              )           
)

print(paste0("Estimating expected volatility at t (model 3). (",experiment,"-",estDate,"-",GARCHmodels,")"))

pred3 <- predict(fitedMSGARCHD3, nahead = 5, do.return.draws = FALSE)


DBTable=rbind(DBTable,
              data.frame(
                Date=as.character(tail(outputData$Date,1)),
                Value=pred3$vol,
                Ticker=paste0(paste0("Volatility at t+",seq(1:5))," model 3"),
                ModelID=dicTable$Model[3],
                GARCHSpec=dicTable$GARCHSpec[3],
                Experiment=experiment
              )           
)

print(paste0("Estimating expected volatility at t (model 4). (",experiment,"-",estDate,"-",GARCHmodels,")"))

pred4 <- predict(fitedMSGARCHD4, nahead = 5, do.return.draws = FALSE)


DBTable=rbind(DBTable,
              data.frame(
                Date=as.character(tail(outputData$Date,1)),
                Value=pred4$vol,
                Ticker=paste0(paste0("Volatility at t+",seq(1:5))," model 4"),
                ModelID=dicTable$Model[4],
                GARCHSpec=dicTable$GARCHSpec[4],
                Experiment=experiment
              )           
)

print(paste0("Estimating expected volatility at t (model 5). (",experiment,"-",estDate,"-",GARCHmodels,")"))

pred5 <- predict(fitedMSGARCHD5, nahead = 5, do.return.draws = FALSE)


DBTable=rbind(DBTable,
              data.frame(
                Date=as.character(tail(outputData$Date,1)),
                Value=pred5$vol,
                Ticker=paste0(paste0("Volatility at t+",seq(1:5))," model 5"),
                ModelID=dicTable$Model[5],
                GARCHSpec=dicTable$GARCHSpec[5],
                Experiment=experiment
              )           
)

print(paste0("Estimating expected volatility at t (model 6). (",experiment,"-",estDate,"-",GARCHmodels,")"))

pred6 <- predict(fitedMSGARCHD6, nahead = 5, do.return.draws = FALSE)


DBTable=rbind(DBTable,
              data.frame(
                Date=as.character(tail(outputData$Date,1)),
                Value=pred1$vol,
                Ticker=paste0(paste0("Volatility at t+",seq(1:5))," model 6"),
                ModelID=dicTable$Model[6],
                GARCHSpec=dicTable$GARCHSpec[6],
                Experiment=experiment
              )           
)

print(paste0("Estimating expected volatility at t (model 7, time-fixed). (",experiment,"-",estDate,"-",GARCHmodels,")"))

pred6 <- predict(fitedMSGARCHD7, nahead = 5, do.return.draws = FALSE)


DBTable=rbind(DBTable,
              data.frame(
                Date=as.character(tail(outputData$Date,1)),
                Value=pred1$vol,
                Ticker=paste0(paste0("Volatility at t+",seq(1:5))," model 7"),
                ModelID=dicTable$Model[7],
                GARCHSpec=dicTable$GARCHSpec[7],
                Experiment=experiment
              )           
)

print(paste0("Estimating expected volatility at t (Best model). (",experiment,"-",estDate,"-",GARCHmodels,")"))

predBest <- predict(fitedMSGARCHDBest, nahead = 5, do.return.draws = FALSE)


DBTable=rbind(DBTable,
              data.frame(
                Date=as.character(tail(outputData$Date,1)),
                Value=predBest$vol,
                Ticker=paste0(paste0("Volatility at t+",seq(1:5))," Best model"),
                ModelID=dicTable$Model[8],
                GARCHSpec=dicTable$GARCHSpec[8],
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
  
  MSGARCHResults=list(
    outputData=outputData,
    dicDBTable=dicTable,
    dicTableDB=dicTableDB,
    regime1ChartSmoothProbs=regime1ChartDataPrice,
    regime2ChartSmoothProbs=regime2ChartDataPrice,
    probsDBtable=DBTable,
    elapsTimeMsg=elapsTimeMsg,
    elapsTime=tiempoPasado
  )
  
  cat("\f")
  print(elapsTimeMsg)
  
  return(MSGARCHResults)

}
