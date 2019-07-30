updateDateInputNEW <- function (session, inputId, label = NULL, value = NULL, min = NULL, 
          max = NULL, datesdisabled = NULL) 
{
  dropNulls <- function(x) {
    x[!vapply(x, is.null, FUN.VALUE=logical(1))]
  }
  formatDate <- function(x) {
    if (is.null(x)) 
      return(NULL)
    format(as.Date(x), "%Y-%m-%d")
  }
  value <- formatDate(value)
  min <- formatDate(min)
  max <- formatDate(max)
  datesdisabled <- formatDate(datesdisabled)
  message <- dropNulls(list(label = label, value = value, 
                            min = min, max = max,datesdisabled=datesdisabled))
  session$sendInputMessage(inputId, message)
}



LoadData <- function()
{
    ZMat <<- matrix(ncol = 11,byrow = TRUE, data = 
                  c(0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                   0, 0, 0, -1, -1, -1, -1, 0, 1, 0, 0, 
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                   -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, -1, 
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                   1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                   0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 
                   1, 1, 1, 1, 1, 1, 1, -1, -1, -1, 0))
    F1MMat <<- matrix(ncol = 11,byrow = TRUE, data = 
                      c(0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 
                     0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 
                     0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
                     0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 
                     0, -1, -1, -1, 0, -1, -1, 0, 0, 0, -1, 
                     0.4, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 
                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                     0.4, 1, 1, 1, 1, 1, 1, -1, 0, -1, 0, 
                     0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0)
                          )
    F1YMat <<- matrix(ncol = 11,byrow = TRUE, data = 
                    c(0.25, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 
                     0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 
                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 
                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                     0, 0, 0, -1, -1, -1, -1, 0, 0, 0, 0, 
                     0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 
                     0, 0.2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 
                     0.5, 0.4, 0, 0, 1, 1, 1, 0, 0, -1, 0, 
                     0.25, 0.4, 1, 1, 1, 1, 1, -1, -1, 0, 0)
                      )
    FbarMat <<- matrix(ncol = 11,byrow = TRUE, data = 
                        c(0, -1, -1, -1, 0, 0, -1, 1, 1, 1, 0, 
                          0, 0, 0, 0, -1, -1, 0, 0, 0, 0, -1, 
                          0, 0, 0, 0, 1, 1, 0, -1, -1, -1, 0, 
                          0.2, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 
                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                          0.4, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 
                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                          0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
                          0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0)
                        )
    EMat <<- matrix(ncol = 11,byrow = TRUE, data = 
                     c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                       0.666666666666667, 1, 0.666666666666667, 1, 1, 0.666666666666667, 1, -1, -1, 0, 0, 
                       0.333333333333333, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 
                       0, -1, 0, -1, 0, 0, 0, 1, 1, -1, 0, 
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                       0, 0, 0.333333333333333, 0, 0, 0.333333333333333, 0, 0, 0, 0, 0, 
                       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    #sampleb <<- sort(sample(1:dim(art3dat)[1],round(0.25*dim(art3dat)[1],0)))
    #art3datin <-     art3dat[-sampleb,chosenvars]
    Z_CAD <<- read.csv("data/Testing/CAD/Clean Zeroes.csv",row.names=1)  
    Z_EUR <<- read.csv("data/Testing/EUR/Clean Zeroes.csv",row.names=1)  
    Z_HKD <<- read.csv("data/Testing/HKD/Clean Zeroes.csv",row.names=1) 
    Z_KES <<- read.csv("data/Testing/KES/Clean Zeroes.csv",row.names=1) 
    Z_NZD <<- read.csv("data/Testing/NZD/Clean Zeroes.csv",row.names=1) 
    E_CAD <<- read.csv("data/Testing/CAD/Clean Econ.csv",row.names=1) 
    E_EUR <<- read.csv("data/Testing/EUR/Clean Econ.csv",row.names=1) 
    E_HKD <<- read.csv("data/Testing/HKD/Clean Econ.csv",row.names=1) 
    E_KES <<- read.csv("data/Testing/KES/Clean Econ.csv",row.names=1) 
    E_NZD <<- read.csv("data/Testing/NZD/Clean Econ.csv",row.names=1) 
    Z_AUD <<- read.csv("data/Training/AUD/Clean Zeroes.csv",row.names=1) 
    Z_GBP <<- read.csv("data/Training/GBP/Clean Zeroes.csv",row.names=1) 
    Z_JPY <<- read.csv("data/Training/JPY/Clean Zeroes.csv",row.names=1) 
    Z_USD <<- read.csv("data/Training/USD/Clean Zeroes.csv",row.names=1) 
    Z_ZAR <<- read.csv("data/Training/ZAR/Clean Zeroes.csv",row.names=1) 
    E_AUD <<- read.csv("data/Training/AUD/Clean Econ.csv",row.names=1) 
    E_GBP <<- read.csv("data/Training/GBP/Clean Econ.csv",row.names=1) 
    E_JPY <<- read.csv("data/Training/JPY/Clean Econ.csv",row.names=1) 
    E_USD <<- read.csv("data/Training/USD/Clean Econ.csv",row.names=1) 
    E_ZAR <<- read.csv("data/Training/ZAR/Clean Econ.csv",row.names=1) 
    Term <<- c(0,0.003,0.005,0.019,0.038,0.083,0.167,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.75,5,5.25,5.5,5.75,6,6.25,6.5,6.75,7,7.25,7.5,7.75,8,8.25,8.5,8.75,9,9.25,9.5,9.75,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50)
    cbind(Z_CAD[,1],Z_CAD)
    Z_CAD <<-     cbind(Z_CAD[,1],Z_CAD)
    Z_EUR <<-     cbind(Z_EUR[,1],Z_EUR) 
    Z_HKD <<-     cbind(Z_HKD[,1],Z_HKD)
    Z_KES <<-     cbind(Z_KES[,1],Z_KES)
    Z_NZD <<-     cbind(Z_NZD[,1],Z_NZD)
    Z_AUD <<-     cbind(Z_AUD[,1],Z_AUD) 
    Z_GBP <<-     cbind(Z_GBP[,1],Z_GBP) 
    Z_JPY <<-     cbind(Z_JPY[,1],Z_JPY) 
    Z_USD <<-     cbind(Z_USD[,1],Z_USD) 
    Z_ZAR <<-     cbind(Z_ZAR[,1],Z_ZAR)  
    
    }

SparseFunct <- 
  function (Trmsinraw,
            Rtsraw, 
            addterms, 
            addrates, 
            Trmsout,
            RtsCorr, 
            TrmsCorr,method = "NS") 
  {
    require(xts)
    require(YieldCurve)
    Nelson.SiegelA <- function (rate, maturity) 
    {
      rate <- try.xts(rate, error = as.matrix)
      if (ncol(rate) == 1) rate <- matrix(as.vector(rate), 1, nrow(rate))
      pillars.number <- length(maturity)
      lambdaValues <- seq(1/12, maturity[pillars.number], by = 0.25)
      FinalResults <- matrix(0, nrow(rate), 4)
      colnames(FinalResults) <- c("beta_0", "beta_1", "beta_2", "lambda")
      j <- 1
      while (j <= nrow(rate)) 
      {
        InterResults <- matrix(0, length(lambdaValues), 5)
        colnames(InterResults) <- c("beta0", "beta1", "beta2", "lambda", "SSR")
        for (i in 1:length(lambdaValues)) 
        {
          lambdaTemp <- optimize(.factorBeta2, interval = c(0.001, 12), 
                                 maturity = lambdaValues[i], maximum = TRUE)$maximum
          temprates <- as.numeric(rate[j,])[!is.na(rate[j,])]
          tempdates <- maturity[!is.na(rate[j,])]
          InterEstimation <- .NS.estimator(temprates, tempdates, lambdaTemp)
          BetaCoef <- InterEstimation$Par
          SSR <- sum(InterEstimation$Res^2)
          InterResults[i, ] <- c(BetaCoef, lambdaTemp, SSR)
          if (lambdaValues[i]>max(tempdates)) InterResults[i, ]<- c(BetaCoef, 
                                                                    lambdaValues[i], 1e+05)
        }
        BestRow <- which.min(InterResults[, 5])
        FinalResults[j, ] <- InterResults[BestRow, 1:4]
        j <- j + 1
      }
      reclass(FinalResults, rate)
    }
    SvenssonA <-    function (rate, maturity) 
    {
      rate <- try.xts(rate, error = as.matrix)
      if (ncol(rate) == 1) rate <- matrix(as.vector(rate), 1, nrow(rate))
      pillars.number <- length(maturity)
      Tau1Values <<- seq(maturity[1], median(maturity), by = 1)
      Tau2Values <<- seq(median(maturity), maturity[pillars.number], by = 1.5)
      FinalResults <- matrix(0, nrow(rate), 6)
      FinalResultsTau2 <- matrix(0, length(Tau1Values), 7)
      colnames(FinalResults) <- c("beta_0", "beta_1", "beta_2", "beta_3", "tau1", "tau2")
      j <- 1
      while (j <= nrow(rate)) 
      {
        temprates<- as.numeric(rate[j,])[!is.na(rate[j,])]
        tempdates<- maturity[!is.na(rate[j,])]
        InterResultsTau1 <- matrix(0, length(Tau1Values), 7)
        InterResultsTau2 <- matrix(0, length(Tau2Values), 7)
        for (i in 1:length(Tau1Values)) 
        {
          Tau1Temp <- optimize(.beta2Spot, interval = c(0.001, max(Tau1Values)), 
                               maturity = Tau1Values[i], maximum = TRUE)$maximum
          for (a in 1:length(Tau2Values)) 
          {
            Tau2Temp <- optimize(.beta2Spot, interval = c(0.001,
                                                          maturity[pillars.number]), maturity = Tau2Values[a],
                                 maximum = TRUE)$maximum
            InterEstimation <- .NSS.estimator(temprates, tempdates, Tau1Temp, Tau2Temp)
            BetaCoef <- InterEstimation$Par
            SSR <- sum(InterEstimation$Res^2)
            InterResultsTau2[a, ] <- c(BetaCoef, Tau1Temp,Tau2Temp, SSR)
            if (Tau2Values[a]>max(tempdates))
            {
              InterResults[i, ]<- c(BetaCoef, Tau1Temp, Tau2Temp, 1e+05)
            }
            if (Tau1Values[i]>max(tempdates))
            {
              InterResults[i, ]<- c(BetaCoef,Tau1Temp, Tau2Temp, 1e+05)
            }
          }
          BestRowTau2 <- which.min(InterResultsTau2[, 7])
          FinalResultsTau2[i, ] <- InterResultsTau2[BestRowTau2,]
        }
        BestRow <- which.min(FinalResultsTau2[, 7])
        FinalResults[j, ] <- FinalResultsTau2[BestRow, 1:6]
        j <- j + 1
      }
      reclass(FinalResults, rate)
    }
    

    #NSrates <- function (Coeff, maturity) 
    #{
    #  if(nrow(Coeff==1))Curve <- matrix(0, 1, length(maturity))
    # else Curve <- xts(matrix(0, nrow(Coeff), length(maturity)), order.by = time(Coeff))
    #  colnames(Curve) <- make.names(maturity)
    #  Coeff <- as.matrix(Coeff)
    #  for (i in 1:nrow(Curve)) {
    #    Curve[i, ] <- as.numeric(Coeff[i, 1]) * rep(1, length(maturity)) + 
    #      as.numeric(Coeff[i, 2]) * as.numeric(.factorBeta1(Coeff[i, 
    #       4], maturity)) + as.numeric(Coeff[i, 3]) * as.numeric(.factorBeta2(Coeff[i, 
    #                                                                                                                                       4], maturity))
    #  }
    #  return(Curve)
    #}
    Trmsin <- c(Trmsinraw,addterms)
    Rts <- merge(Rtsraw,addrates)
    if(method == "NS") NSParameters <- Nelson.SiegelA( rate= Rts, maturity=Trmsin )
    if(method == "SV") NSParameters <- SvenssonA( rate= Rts, maturity=Trmsin )
    #y <- NSrates(NSParameters, Trmsout)
    #yind <- Trmsout>max(Trmsinraw) & !is.na(RtsCorr)
    #yhat <- t(y)[yind]
    #ytrue <- t(RtsCorr)[yind]
    #RMSE <- sqrt(sum((yhat-ytrue)^2)/length(yhat))*100
    return(NSParameters);
  }


########################################################################################################################################
########################################################################################################################################
########################################################################################################################################

LoadData()
#width = validateCssUnit("50%")
ui <- tagList(
      navbarPage(
        theme = "yeti", 
        "Approximating risk-free curves in sparse data",
        tabPanel("",
                 sidebarPanel(
                   tabPanel("Data to show",
                   fluidRow(column(6,selectInput("currency",label="Currency",choices=c("AUD","CAD","EUR","GBP","HKD","JPY","KES","NZD","USD","ZAR"),selected="ZAR")),
                            column(6,dateInput(inputId = "date",label = "Date", value = "2014-03-12",daysofweekdisabled = c(0,6),min="2005-06-09",max="2016-10-31"))),
                   sliderInput("sparse",label = "Sparsity", min = 2, max = 9,value = 9,step = 1)),
                   tabPanel("Methods to use",
                   radioButtons("curve",label = "Model parameterization", choiceNames = c("Nelson Siegel", "Nelson Siegel Svensson"), 
                                  choiceValues = c("NS","SV"), selected = "NS",inline=TRUE),
                   radioButtons("cutoff",label = "Initial data discarded", choiceNames = c("Nothing", "0.5 years", "1.5 years"), choiceValues = c(0,0.5,1.5),inline=TRUE),
                   radioButtons("addterm", label = "Additional point's term in years", choices = c(10,20,30),selected = 30,inline=TRUE),
                   radioButtons("addmethod", label = "Additional point's method", choiceValues = c("Z","B","NS","NSL"),selected="Z",
                                 choiceNames = c("Zero coupon term structure", "Bullet forward rates", "NS (without level proxy)", "NS (with level proxy)")),
                   radioButtons("addcovariate", label = "Additional point's covariate set", choiceValues = c("Z","F1M","F1Y","Fbar","E"),selected = "F1Y",
                                 choiceNames = c("Zero Rates", "1 Month forward rates", "1 Year forward rates", "Average forward rates", "Economic variables"))
                  )),
                 mainPanel(
                   "The graph shows the original zero curve (grey), the sparse curve (black), the additional data point (blue), and the approximated curve (red).",
                   plotOutput(outputId = "main",height=700,width=1000)
                 )
        )
    )
)


datesreturn <- function(Datain)
{
  startenddates <- c(rownames(Datain)[1],rownames(Datain)[length(rownames(Datain))])
  alldates <- seq(as.Date(startenddates[1]), as.Date(startenddates[2]), by="days")
  excludedates <- alldates[is.na(match(as.Date(alldates),as.Date(rownames(Datain))))]
  return(list(excludedates = excludedates, datemin = startenddates[1], startenddates[2]))
}


server <- function(input,output,session) 
{
  LoadData()
    datesupdate <<- datesreturn(Z_ZAR)
    updateDateInputNEW(session, "date", min = datesupdate$datemin,max = datesupdate$datemax, datesdisabled = datesupdate$excludedates, value = datesupdate$datemax)
    rv <<- reactiveValues()
    output$main <- renderPlot({
    rv$sparse <- input$sparse
    rv$currency <- input$currency
    rv$date <- input$date
    rv$curve <- input$curve
    rv$cutoff <- as.numeric(input$cutoff)
    rv$addterm <- as.numeric(input$addterm)
    rv$addmethod <- input$addmethod
    rv$addcovariate <- input$addcovariate
    if(rv$currency == "AUD") plotdata <<- Z_AUD
    if(rv$currency == "CAD") plotdata <<- Z_CAD
    if(rv$currency == "EUR") plotdata <<- Z_EUR
    if(rv$currency == "GBP") plotdata <<- Z_GBP
    if(rv$currency == "HKD") plotdata <<- Z_HKD
    if(rv$currency == "JPY") plotdata <<- Z_JPY
    if(rv$currency == "KES") plotdata <<- Z_KES
    if(rv$currency == "NZD") plotdata <<- Z_NZD
    if(rv$currency == "USD") plotdata <<- Z_USD
    if(rv$currency == "ZAR") plotdata <<- Z_ZAR
    
    datesupdate <<- datesreturn(plotdata)
    updateDateInputNEW(session, "date", min = datesupdate$datemin,max = datesupdate$datemax, datesdisabled = datesupdate$excludedates, value = datesupdate$datemax)
    curvefullwithNA <<-  plotdata[match(rv$date,as.Date(rownames(plotdata))),]
    curvefull <<- matrix(curvefullwithNA[!is.na(curvefullwithNA)],ncol=1)
    curvefull <<- cbind(Term[1:length(curvefull)],curvefull)
    colnames(curvefull) <<- c("Term","Rate")
    curvesparse <<- curvefull[curvefull[,1] <= rv$sparse,]

    #COVARIATES:
    
    #ECONOMIC:
    
    if(rv$currency == "AUD") ecodata <<- E_AUD
    if(rv$currency == "CAD") ecodata <<- E_CAD
    if(rv$currency == "EUR") ecodata <<- E_EUR
    if(rv$currency == "GBP") ecodata <<- E_GBP
    if(rv$currency == "HKD") ecodata <<- E_HKD
    if(rv$currency == "JPY") ecodata <<- E_JPY
    if(rv$currency == "KES") ecodata <<- E_KES
    if(rv$currency == "NZD") ecodata <<- E_NZD
    if(rv$currency == "USD") ecodata <<- E_USD
    if(rv$currency == "ZAR") ecodata <<- E_ZAR
    ecoin <<- ecodata[match(rv$date,as.Date(rownames(ecodata))),]
    EcoVEC <<- as.numeric(ecoin) %*% EMat

    #ZERO:
    CovTerms <<- seq(from = curvesparse[1,1], to = curvesparse[dim(curvesparse)[1],1],length.out = 11)
    Zin <<- cbind(CovTerms, approx(curvesparse[,1],curvesparse[,2],CovTerms)$y)
    ZVEC <<- as.numeric(Zin[,2]) %*% ZMat
    
    #1YForward:
    F1YTau1 <<- seq(from = curvesparse[1,1], to = curvesparse[dim(curvesparse)[1],1]-1,length.out = 11)
    F1YTau2 <<- seq(from = curvesparse[1,1]+1, to = curvesparse[dim(curvesparse)[1],1],length.out = 11)
    ZF1Ytau1 <<- cbind(F1YTau1, approx(curvesparse[,1],curvesparse[,2],F1YTau1)$y)
    ZF1Ytau2 <<- cbind(F1YTau2, approx(curvesparse[,1],curvesparse[,2],F1YTau2)$y)
    F1Yin <<- (ZF1Ytau2*F1YTau2 - ZF1Ytau1*F1YTau1)/1
    F1YVEC <<- as.numeric(F1Yin[,2]) %*% F1YMat
    
    #1MForward:
    F1MTau1 <<- seq(from = curvesparse[1,1], to = curvesparse[dim(curvesparse)[1],1]-1/12,length.out = 11)
    F1MTau2 <<- seq(from = curvesparse[1,1]+1/12, to = curvesparse[dim(curvesparse)[1],1],length.out = 11)
    ZF1Mtau1 <<- cbind(F1MTau1, approx(curvesparse[,1],curvesparse[,2],F1MTau1)$y)
    ZF1Mtau2 <<- cbind(F1MTau2, approx(curvesparse[,1],curvesparse[,2],F1MTau2)$y)
    F1Min <<- (ZF1Mtau2*F1MTau2 - ZF1Mtau1*F1MTau1)/(1/12)
    F1MVEC <<- as.numeric(F1Min[,2]) %*% F1MMat
    
    #FAvg

    FavgTauS <<- Term[Term<=min(3,rv$sparse)]
    if (rv$sparse <= 3) FavgTauM <<- rv$sparse
    if (rv$sparse > 3) FavgTauM <<- Term[Term<=min(6,rv$sparse) & Term>min(3,rv$sparse)]
    if (rv$sparse <= 6) FavgTauL <<- rv$sparse
    if (rv$sparse > 6) FavgTauL <<- Term[Term>6 & Term<=rv$sparse]
    
    #tau1m
    FavgTauS1M <- FavgTauS - 1/12
    FavgTauS1M <- FavgTauS1M[FavgTauS1M>=0]
    FavgTauM1M <- FavgTauM - 1/12
    FavgTauM1M <- FavgTauM1M[FavgTauM1M>=0]
    FavgTauL1M <- FavgTauL - 1/12
    FavgTauL1M <- FavgTauL1M[FavgTauL1M>=0]
    
    #tau6m
    FavgTauS6M <- FavgTauS - 6/12
    FavgTauS6M <- FavgTauS6M[FavgTauS6M>=0]
    FavgTauM6M <- FavgTauM - 6/12
    FavgTauM6M <- FavgTauM6M[FavgTauM6M>=0]
    FavgTauL6M <- FavgTauL - 6/12
    FavgTauL6M <- FavgTauL6M[FavgTauL6M>=0]
    
    #tau1Y
    FavgTauS1Y <- FavgTauS - 12/12
    FavgTauS1Y <- FavgTauS1Y[FavgTauS1Y>=0]
    FavgTauM1Y <- FavgTauM - 12/12
    FavgTauM1Y <- FavgTauM1Y[FavgTauM1Y>=0]
    FavgTauL1Y <- FavgTauL - 12/12
    FavgTauL1Y <- FavgTauL1Y[FavgTauL1Y>=0]

    
    Favg1MTauS <<- (approx(curvesparse[,1],curvesparse[,2],FavgTauS1M+1/12)$y*(FavgTauS1M+1/12) - approx(curvesparse[,1],curvesparse[,2],FavgTauS1M)$y*(FavgTauS1M))/(1/12)
    Favg6MTauS <<- (approx(curvesparse[,1],curvesparse[,2],FavgTauS6M+6/12)$y*(FavgTauS6M+6/12) - approx(curvesparse[,1],curvesparse[,2],FavgTauS6M)$y*(FavgTauS6M))/(6/12)
    Favg1YTauS <<- (approx(curvesparse[,1],curvesparse[,2],FavgTauS1Y+12/12)$y*(FavgTauS1Y+12/12) - approx(curvesparse[,1],curvesparse[,2],FavgTauS1Y)$y*(FavgTauS1Y))/(12/12)

    Favg1MTauM <<- (approx(curvesparse[,1],curvesparse[,2],FavgTauM1M+1/12)$y*(FavgTauM1M+1/12) - approx(curvesparse[,1],curvesparse[,2],FavgTauM1M)$y*(FavgTauM1M))/(1/12)
    Favg6MTauM <<- (approx(curvesparse[,1],curvesparse[,2],FavgTauM6M+6/12)$y*(FavgTauM6M+6/12) - approx(curvesparse[,1],curvesparse[,2],FavgTauM6M)$y*(FavgTauM6M))/(6/12)
    Favg1YTauM <<- (approx(curvesparse[,1],curvesparse[,2],FavgTauM1Y+12/12)$y*(FavgTauM1Y+12/12) - approx(curvesparse[,1],curvesparse[,2],FavgTauM1Y)$y*(FavgTauM1Y))/(12/12)
    
    Favg1MTauL <<- (approx(curvesparse[,1],curvesparse[,2],FavgTauL1M+1/12)$y*(FavgTauL1M+1/12) - approx(curvesparse[,1],curvesparse[,2],FavgTauL1M)$y*(FavgTauL1M))/(1/12)
    Favg6MTauL <<- (approx(curvesparse[,1],curvesparse[,2],FavgTauL6M+6/12)$y*(FavgTauL6M+6/12) - approx(curvesparse[,1],curvesparse[,2],FavgTauL6M)$y*(FavgTauL6M))/(6/12)
    Favg1YTauL <<- (approx(curvesparse[,1],curvesparse[,2],FavgTauL1Y+12/12)$y*(FavgTauL1Y+12/12) - approx(curvesparse[,1],curvesparse[,2],FavgTauL1Y)$y*(FavgTauL1Y))/(12/12)
    
    Favgin <<- c(mean(Favg1MTauS,na.rm= TRUE),mean(Favg1MTauM,na.rm= TRUE),mean(Favg1MTauL,na.rm= TRUE),
                 mean(Favg6MTauS,na.rm= TRUE),mean(Favg6MTauM,na.rm= TRUE),mean(Favg6MTauL,na.rm= TRUE),
                 mean(Favg1YTauS,na.rm= TRUE),mean(Favg1YTauM,na.rm= TRUE),mean(Favg1YTauL,na.rm= TRUE))

    FAvgVEC <<- as.numeric(Favgin) %*% FbarMat
    
    #additional point
    
    if(rv$addmethod=="Z")
    {
      if (rv$addcovariate == "Z")
      {if(rv$addterm == 10) addpoint <<- ZVEC[1]
      if(rv$addterm == 20) addpoint <<- ZVEC[2]
      if(rv$addterm == 30) addpoint <<- ZVEC[3]
      }
      if (rv$addcovariate == "F1M")
      {if(rv$addterm == 10) addpoint <<- F1MVEC[1]
      if(rv$addterm == 20) addpoint <<- F1MVEC[2]
      if(rv$addterm == 30) addpoint <<- F1MVEC[3]
      }
      if (rv$addcovariate == "F1Y")
      {if(rv$addterm == 10) addpoint <<- F1YVEC[1]
      if(rv$addterm == 20) addpoint <<- F1YVEC[2]
      if(rv$addterm == 30) addpoint <<- F1YVEC[3]
      }
      if (rv$addcovariate == "Fbar")
      {if(rv$addterm == 10) addpoint <<- FAvgVEC[1]
      if(rv$addterm == 20) addpoint <<- FAvgVEC[2]
      if(rv$addterm == 30) addpoint <<- FAvgVEC[3]
      }
      if (rv$addcovariate == "E")
      {if(rv$addterm == 10) addpoint <<- EcoVEC[1]
      if(rv$addterm == 20) addpoint <<- EcoVEC[2]
      if(rv$addterm == 30) addpoint <<- EcoVEC[3]
      }
    }
    
    if(rv$addmethod=="B")
    {
      lastpoint <- curvesparse[dim(curvesparse)[1],2]
      if (rv$addcovariate == "Z")
      {if(rv$addterm == 10) addpoint <<- (lastpoint*rv$sparse + (10 - rv$sparse)*ZVEC[4])/10
      if(rv$addterm == 20) addpoint <<- (lastpoint*rv$sparse + (20 - rv$sparse)*ZVEC[5])/20
      if(rv$addterm == 30) addpoint <<- (lastpoint*rv$sparse + (30 - rv$sparse)*ZVEC[6])/30
      }
      if (rv$addcovariate == "F1M")
      {if(rv$addterm == 10) addpoint <<- (lastpoint*rv$sparse + (10 - rv$sparse)*F1MVEC[4])/10
      if(rv$addterm == 20) addpoint <<- (lastpoint*rv$sparse + (20 - rv$sparse)*F1MVEC[5])/20
      if(rv$addterm == 30) addpoint <<- (lastpoint*rv$sparse + (30 - rv$sparse)*F1MVEC[6])/30
      }
      if (rv$addcovariate == "F1Y")
      {if(rv$addterm == 10) addpoint <<- (lastpoint*rv$sparse + (10 - rv$sparse)*F1YVEC[4])/10
      if(rv$addterm == 20) addpoint <<- (lastpoint*rv$sparse + (20 - rv$sparse)*F1YVEC[5])/20
      if(rv$addterm == 30) addpoint <<- (lastpoint*rv$sparse + (30 - rv$sparse)*F1YVEC[6])/30
      }
      if (rv$addcovariate == "Fbar")
      {if(rv$addterm == 10) addpoint <<- (lastpoint*rv$sparse + (10 - rv$sparse)*FAvgVEC[4])/10
      if(rv$addterm == 20) addpoint <<- (lastpoint*rv$sparse + (20 - rv$sparse)*FAvgVEC[5])/20
      if(rv$addterm == 30) addpoint <<- (lastpoint*rv$sparse + (30 - rv$sparse)*FAvgVEC[6])/30
      }
      if (rv$addcovariate == "E")
      {if(rv$addterm == 10) addpoint <<- (lastpoint*rv$sparse + (10 - rv$sparse)*EcoVEC[4])/10
      if(rv$addterm == 20) addpoint <<- (lastpoint*rv$sparse + (20 - rv$sparse)*EcoVEC[5])/20
      if(rv$addterm == 30) addpoint <<- (lastpoint*rv$sparse + (30 - rv$sparse)*EcoVEC[6])/30
      }
    }
    
    if(rv$addmethod=="NS")
    {
      lastpoint <- curvesparse[dim(curvesparse)[1],2]
      lambdain <- 0.7308
      if (rv$addcovariate == "Z") addpoint <<- ZVEC[7] + ZVEC[8]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm))+ ZVEC[9]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm)-exp(-lambdain*rv$addterm))
      if (rv$addcovariate == "F1M") addpoint <<- F1MVEC[7] + F1MVEC[8]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm))+ F1MVEC[9]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm)-exp(-lambdain*rv$addterm))
      if (rv$addcovariate == "F1Y") addpoint <<- F1YVEC[7] + F1YVEC[8]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm))+ F1YVEC[9]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm)-exp(-lambdain*rv$addterm))
      if (rv$addcovariate == "Fbar") addpoint <<- FAvgVEC[7] + FAvgVEC[8]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm))+ FAvgVEC[9]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm)-exp(-lambdain*rv$addterm))
      if (rv$addcovariate == "E") addpoint <<- EcoVEC[7] + EcoVEC[8]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm))+ EcoVEC[9]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm)-exp(-lambdain*rv$addterm))
    }
    
    if(rv$addmethod=="NSL")
    {
      lambdain <- 0.7308
      Lest <- mean(Favg1MTauM,na.rm= TRUE)
      if (rv$addcovariate == "Z") addpoint <-   Lest + ZVEC[8]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm))+ ZVEC[9]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm)-exp(-lambdain*rv$addterm))
      if (rv$addcovariate == "F1M") addpoint <-   Lest + F1MVEC[8]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm))+ F1MVEC[9]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm)-exp(-lambdain*rv$addterm))
      if (rv$addcovariate == "F1Y") addpoint <-   Lest + F1YVEC[8]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm))+ F1YVEC[9]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm)-exp(-lambdain*rv$addterm))
      if (rv$addcovariate == "Fbar") addpoint <-   Lest + FAvgVEC[8]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm))+ FAvgVEC[9]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm)-exp(-lambdain*rv$addterm))
      if (rv$addcovariate == "E") addpoint <-   Lest + EcoVEC[8]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm))+ EcoVEC[9]*((1-exp(-lambdain*rv$addterm))/(lambdain*rv$addterm)-exp(-lambdain*rv$addterm))
    }
    
    if(rv$curve == "NS")
    {
    Sparsefitout <<- SparseFunct(Trmsinraw = t(curvesparse[,1][curvesparse[,1]>=rv$cutoff]),
                                Rtsraw = t(curvesparse[,2][curvesparse[,1]>=rv$cutoff]),
                                addterms = rv$addterm,
                                addrates = addpoint,
                                Trmsout = t(curvefull[,1]),
                                RtsCorr = t(curvefull[,2]),
                                TrmsCorr = t(curvefull[,1]),
                                method = "NS"
                                )
    ApproxCurve <<- Sparsefitout[1] + Sparsefitout[2]*((1-exp(-Sparsefitout[4]*Term))/(Sparsefitout[4]*Term))+ Sparsefitout[3]*((1-exp(-Sparsefitout[4]*Term))/(Sparsefitout[4]*Term)-exp(-Sparsefitout[4]*Term))
    
    }
    
    if(rv$curve == "SV")
    {
      Sparsefitout <<- SparseFunct(Trmsinraw = t(curvesparse[,1][curvesparse[,1]>=rv$cutoff]),
                                  Rtsraw = t(curvesparse[,2][curvesparse[,1]>=rv$cutoff]),
                                  addterms = rv$addterm,
                                  addrates = addpoint,
                                  Trmsout = t(curvefull[,1]),
                                  RtsCorr = t(curvefull[,2]),
                                  TrmsCorr = t(curvefull[,1]),
                                  method = "SV"
      )
    ApproxCurve <<- Sparsefitout[1] + Sparsefitout[2] * .beta1Spot(Term, 
                   Sparsefitout[5]) + Sparsefitout[3] * .beta2Spot(Term, 
                   Sparsefitout[5]) + Sparsefitout[4] * .beta2Spot(Term, 
                   Sparsefitout[6])
    
    
    }
    plot(c(0,50),c(0,max(curvefull[,2])+0.02),col="white",xlab="Term in years",ylab = "Rate")
    lines(curvefull,lwd=2,col="grey")
    lines(curvesparse[curvesparse[,1]>rv$cutoff,],lwd=3)
    lines(Term[Term>=rv$sparse],ApproxCurve[Term>=rv$sparse],col="red")
    points(x=rv$addterm, y=addpoint,type="p",col="blue")
    if(is.nan(addpoint)==TRUE) text("Not enough data for additional point.", x = 0, y = 0)
    })
 }

shinyApp(ui=ui, server = server)

