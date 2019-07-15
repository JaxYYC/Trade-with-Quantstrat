#################^^ Run this part once at startup unless you change assets ^^^^^^^^##############################################
library(quantstrat)
library(blotter)
library(quantmod)
library(PerformanceAnalytics)
library(PeerPerformance)

osFixedDollar <- function(timestamp,orderqty, portfolio, symbol, ruletype, ...)
{
  ClosePrice <- as.numeric(Cl(mktdata[timestamp,]))
  orderqty <- round(tradeSize/ClosePrice,-2)
  return(orderqty)
}


osInvestAll <- function (data, timestamp, orderqty, ordertype, 
                         orderside, equity, portfolio, symbol, ruletype, ..., initEq) {
  datePos <- format(timestamp,"%Y-%m-%d %H:%M:%OS")
  
  datePos <- strptime(c(datePos), format = "%Y-%m-%d %H:%M:%OS", tz = 
                        "UTC") + 86400 #for daily data
  
  updatePortf(Portfolio=portfolio,Symbol=symbol,Dates=paste0(start(data), 
                                                             "/", datePos))
  # After updating portfolio profit, we can extract the Net.Trading.PL 
  
  trading_pl <- sum(.getPortfolio(portfolio)$summary$Net.Trading.PL)
  
  equity <- initEq + trading_pl
  ClosePrice <- getPrice(data, prefer = "Close")[datePos]
  UnitSize <- as.numeric((equity / ClosePrice))
  UnitSize1 <- round(UnitSize, digits = 8)
  ifelse(is.na(UnitSize1),0,UnitSize1)
}


osInvestAllShort <- function(...) { -osInvestAll(...)}


#######  add or remove assets here #################################
symbolsCTA <- c('DBC','GLD','IAU','SLV','USO')

# For example: 
# symbolsCTA <- c("GLD", "IAU", "SLV", "DBC", "USO", 
#              "PPLT", "DBE", "BNO", "UGAZ", "GLL",
#              "PALL", "VXX", "VIXM", "SPY", "QQQ",
#              "EWJ", "EWG", "INDA", "MCHI", "EWZ",
#              "EWQ", "EWH", "FXI", "CORN", "WEAT", "SOYB")
###################################################################

## Set up investment strategy params
init_date <- "2014-01-01"
start_date <- "2014-01-01"
end_date <- "2019-04-11"


getSymbols(Symbols = symbolsCTA, 
           src = "yahoo", 
           index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = TRUE)

# chart_Series(Cl(GLD))
# chart_Series(Cl(SLV))





### Initialize Portfolio
# Here we set up some accounting to keep track of how well or badly we are doing with our strategy.


# everyone get $10MM to start
init_equity <- 10000000 # $10,000,000

currency("USD")
stock(symbolsCTA, 
      currency = "USD", 
      multiplier = 1)


portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"
#################^^ Run once at startup (unless you change assets in symbolsCTA) ^^^^^^^#################
#################^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#################
#########################################################################################################





#########################################################################################################
##### to re-run start here   ############################################################################
rm.strat(portfolio.st)
rm.strat(account.st)
rm.strat(strategy.st)



initPortf(name = portfolio.st,
          symbols = symbolsCTA,
          initDate = init_date)


initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)


initOrders(portfolio = portfolio.st,
           symbols = symbolsCTA,
           initDate = init_date)

strategy(strategy.st, store = TRUE)


### Set up Inidcator
# We use a fast and a slow Simple Moving Average **(SMA)** in the underlying asset we are looking at as two indicators to compare against each other to decide entry for long and short positions. The speed of the fast/slow averages are kept the same across assets  assets in the portfolio, but there is no reason this needs to be the case. The speed could be asset specific with a bit of programming ((ie ***SMA*** windows are the same for every asset ) )It may be something you might look to optimize).

################################################
# SMA speeds can be modified here
fastSMA=28
slowSMA=45
################################################
stopLimLong=.02
stopLimShort=.07


# indicators are set here -> do not touch! ######
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), 
                               n = fastSMA),
              label = "nFast")

add.indicator(strategy = strategy.st, 
              name = "SMA", 
              arguments = list(x = quote(Cl(mktdata)), 
                               n = slowSMA), 
              label = "nSlow")
######################################################



### Set up signal -Do not touch! ##############################################################
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")

add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")
### Set up signal -Do not touch! ^^^^^^^^^^######################################




### Set up Rules
# Once we have indicators we can add rules we can set up a trading strategy. We need to size the trade so we use the amount of equity we have divided by the number of assets we are working with from our data pull. In this exercise we use the same trade size whether going long or short. We do not update the trade size for changes in our trading account equity.


#### Change trade rules here for all assets ###########

# Trade sizes for long/short

tradeSize=init_equity/length(symbolsCTA)  # allocate capital equally to trade strategies
.orderqty=tradeSize
.txnfees=.threshold=0

#######################################################


######  Add rules here - Do not touch! ###########################################




add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='short',
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2LONG'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='long' ,
                        ordertype='market',
                        orderqty='all',
                        TxnFees=.txnfees,
                        replace=TRUE
         ),
         type='exit',
         label='Exit2SHORT'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='long' , sigval=TRUE,
                        orderside='long' ,
                        ordertype='stoplimit', prefer='High', threshold=.threshold,
                        orderqty= .orderqty,
                        tradeSize=tradeSize,
                        replace=FALSE
         ),
         type='enter',
         label='EnterLONG'
)

add.rule(strategy.st, name='ruleSignal',
         arguments=list(sigcol='short', sigval=TRUE,
                        orderside='short',
                        ordertype='stoplimit', prefer='Low', threshold=-.threshold,
                        orderqty= -.orderqty,
                        replace=FALSE
         ),
         type='enter',
         label='EnterSHORT'
)

add.rule(strategy.st,name='ruleSignal',
         arguments = list(sigcol="long", sigval=TRUE,
                          replace=FALSE,
                          orderside='long',
                          ordertype='stoplimit',
                          tmult=TRUE,     #tmult if TRUE, threshold is a percent multiplier
                          threshold=quote( stopLimLong ),   # stopLimLong=0.03
                          orderqty='all',
                          orderset='ocolong'   # OCO: One Cancel the Other Order
         ),
         type='chain', parent="EnterLONG",   # chain: rules executed upon fill of an order corresponding to the label of the parent rule identified by the parent arg.
         label='StopLossLong',
         enabled=TRUE
)
#StopLossPrice=fill price −(stopLimLong ∗fill price). If market price moves below $StopLossPrice, 
#the StopLossLONG order becomes a market order and the Exit2SHORT order is cancelled (OCO).

add.rule(strategy.st,name='ruleSignal',
         arguments = list(sigcol="short", sigval=TRUE,
                          replace=FALSE,
                          orderside='short',
                          ordertype='stoplimit',
                          tmult=TRUE,
                          threshold=quote( stopLimShort ), # stopLimShort=0.02
                          orderqty='all',
                          orderset='ocolong'
         ),
         type='chain', parent="EnterSHORT",
         label='StopLossShort',
         enabled=TRUE
)

######  Add rules here - Do not touch! ##########################################
######^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##########################################


### Get the Results - run to end w/o changing unless you want different output
# Now run the rules/signal/strategies against the data and get the results 
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
  file.remove(results_file)}

results <- applyStrategy(strategy.st, portfolios = portfolio.st,verbose=FALSE)
updatePortf(portfolio.st,verbose=F)
updateAcct(account.st)
updateEndEq(account.st)

#### chart results by symbol ##############
for(sym in symbolsCTA) {
  chart.Posn(portfolio.st, Symbol = sym,
             TA = "add_SMA(n = fastSMA, col = 4); add_SMA(n = slowSMA, col = 2)")
  Sys.sleep(0)
}

### Tabulate Result Stats by Individual Asset
tstats <- tradeStats(portfolio.st)
knitr::kable(t(tstats))

sum(tstats$End.Equity)
p <- getPortfolio(portfolio.st)
sum(p$summary$Long.Value)

### Now get Total Portfolio Results   ####
final_acct <- getAccount(account.st)
end_eq <- final_acct$summary$End.Eq
returns <- Return.calculate(end_eq, method="discrete")
chart_title <- paste('Strategy Performance',fastSMA,slowSMA,stopLimLong,stopLimShort,sep = '_')
charts.PerformanceSummary(returns, colorset = bluefocus, main = chart_title)

# returns_2 <- PortfReturns(account.st)
# colnames(returns_2) <- symbolsCTA
# returns_2 <- na.omit(cbind(returns_2,Return.calculate(end_eq)))
# names(returns_2)[length(names(returns_2))] <- 'Total'
# chart.CumReturns(R = returns_2$GLD, legend.loc = 'topleft',colorset = rich6equal,main = chart_title)
 

knitr::kable(end_eq[dim(end_eq)[1],])
knitr::kable(SharpeRatio(returns,annualize=T))
knitr::kable(SortinoRatio(returns))
knitr::kable(CalmarRatio(returns))

