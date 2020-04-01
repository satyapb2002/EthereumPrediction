set.seed(123)
#loading in prive/volume data
ethereum = read.csv("Coinbase_ETHUSD_daily.csv")
ethereum = ethereum[seq(dim(ethereum)[1],1),]
#Creation of Response Variables
ethereum$closeDiff = append(0,diff(ethereum$Close, differences = 1))
ethereum$Y = ethereum$closeDiff
ethereum$Yscaled = scale(ethereum$closeDiff)
#OHLCV lag
ethereum$openLag = Lag(ethereum$Open,1)
ethereum$highLag = Lag(ethereum$High,1)
ethereum$lowLag = Lag(ethereum$Low,1)
ethereum$closeLag = Lag(ethereum$Close,1)
ethereum$volume.ethLag = Lag(ethereum$Volume.ETH,1)
ethereum$Date = as.Date(mdy(as.character(ethereum$Date)))
ethereum$closeDiffLagged = Lag(ethereum$closeDiff, 1)
ethereum$emaLagged = EMA(ethereum$closeLag, 1)
#loading in and cutting to appropriate window
eth_chain = read.csv('chain_data_ETH.csv')
eth_chain$Date = as.Date(as.POSIXct(eth_chain$time, origin="1970-01-01"))
eth_chain = eth_chain[eth_chain$Date >= as.POSIXct("2016-05-27"), ]
eth_chain = eth_chain[eth_chain$Date <= as.POSIXct("2020-03-05"), ]
#lag columns of eth_chain
z = colnames(eth_chain)
z = z[!z %in% c('id', 'symbol', 'time', 'Date')]
for (i in 1:length(z)) {
eth_chain[, z[i]] = Lag(eth_chain[, z[i]], 1)
}
eth_chain = eth_chain[-c(1), ]
data = merge(ethereum, eth_chain, by="Date", all = T)
#remove missing dates
data = data[-c(1028:1039), ]
data
# creation of technical indicators, using ttr library
eth_hlc = data[, c('highLag', 'lowLag', 'closeLag')]
eth_hl = data[, c('highLag', 'lowLag')]
eth_close = data[, c('closeLag')]
data$snr = SNR(eth_hlc, 5)
data$obv = OBV(data[, c('closeLag')], data[, c('volume.ethLag')])
data$runsum = runSum(data[, c('closeLag')], n=5)
data$vwma = VWMA(data[, c('closeLag')], 5, volume=data[, c('volume.ethLag')])
data$rsi = RSI(eth_close, n=5)
data$roc = ROC(eth_close)
13data$percentrank = runPercentRank(eth_close, n =5)
data$mfi = MFI(eth_hlc, data[, c('volume.ethLag')], n=5)
data$cci = CCI(eth_hlc,n=7)
data$clv = CLV(eth_hlc)
data$sma = SMA(eth_close, n=5)
#cbinding the multiple-column technical indicators
aroon = as.data.frame(aroon(eth_hl, 4))
data = cbind(data, aroon)
bbands = BBands(eth_hlc, 5)
data = cbind(data, bbands)
trix = TRIX(eth_close, n=2, nSig=9)
data = cbind(data, trix)
tdi = TDI(eth_close, n=3)
data = cbind(data, tdi)
gmma = GMMA(eth_close, short = c(3, 5, 8, 10, 12, 15), long = c(30, 35, 40, 45, 50, 60))
data = cbind(data, gmma)
pbands = PBands(eth_close, n=3)
data = cbind(data, pbands)
sar = SAR(eth_hl, accel = c(0.02, 0.2))
data = cbind(data, sar)
stoch = stoch(eth_hlc, nFastK = 5)
data = cbind(data, stoch)
macd = MACD(eth_close, nFast = 2, nSlow = 5, nSig = 5)
data = cbind(data, macd)
adx = ADX(eth_hlc, n = 5)
data = cbind(data, adx)
#data partitioning
N = nrow(data)
n = round(N * 0.7, digits = 0)
train_new = data[1:n,]
train_new = train_new[-c(1:7), ]
test_new = data[(n+1):N,]
mask = colnames(train_new)
#removing all variables that have NAs, or any variable which may confound predictions
mask = mask[!mask %in% c('Date', 'Symbol', 'id', 'symbol', 'time',
'Open', 'High', 'Low', 'Close', 'Volume.ETH','Volume.USD',
'short lag 10', 'short lag 12', 'short lag 15', 'short lag 30',
'short lag 35','short lag 8', 'long lag 30', 'long lag 35', 'long lag 40',
'long lag 45', 'long lag 50', 'long lag 60', 'slowD','ADX', 'short lag 3',
'short lag 5','signal','obv','closeDiff','Yscaled')]
#create master train/test dataframes, for usage in rf, boosting, and lstm
train_boost = train_new[, mask]
test_boost = test_new[, mask]
####Boosting Procedures
library(gbm)
set.seed(123)
boost_diff_fit = gbm(Y~., data=train_boost, n.trees = 1000)
diff_model_boost <- predict(boost_diff_fit, newdata = test_boost, n.trees = 1000)
boosting_df_diff <- data.frame(test_new$Date, test_new$Y, diff_model_boost)
colnames(boosting_df_diff) <- c('time', 'Y', 'model')
ggplot(boosting_df_diff, aes(time)) +
geom_line(aes(y = Y, colour = "actual")) +
geom_line(aes(y = diff_model_boost, colour = 'predicted'))
binaryPred.diff.boost = ifelse(diff_model_boost > 0,1,0)
binaryPredSig.diff.boost = ifelse(diff_model_boost > 0,1,-1)
binaryTest = ifelse(test_new$Y > 0,1,0)
imp =summary(boost_diff_fit)
boost_vars = imp[imp$rel.inf >= 0.8, ]
boost_vars = as.vector(boost_vars[, 1])
boost_vars = append('Y', boost_vars)
train_final = train_boost[, names(train_boost) %in% boost_vars]
test_final = test_boost[, names(train_boost) %in% boost_vars]
boost_final = gbm(Y~., data=train_final, n.trees = 5000, interaction.depth = 5)
boostPredFinal <- predict(boost_final, newdata = test_final, n.trees = 5000)
finalBoostDF <- data.frame(test_new$Date, test_new$Y, boostPredFinal)
colnames(finalBoostDF) <- c('time', 'Y', 'model')
ggplot(finalBoostDF, aes(time)) +
geom_line(aes(y = Y, colour = "actual")) +
geom_line(aes(y = boostPredFinal, colour = 'predicted'))
binPredBoostFinal = ifelse(boostPredFinal > 0,1,0)
binPredSigBoostFinal = ifelse(boostPredFinal > 0,1,-1)
binaryTest = ifelse(test_new$Y > 0,1,0)
confusionMatrix(as.factor(binPredBoostFinal),as.factor(binaryTest))
#ROC curve
predBoost = prediction(binPredBoostFinal,binaryTest)
roc.perfB = performance(predBoost,measure = 'tpr',x.measure = 'fpr')
plot(roc.perfB)
abline(a=0,b=1)
auc.perfB = performance(predBoost, measure = "auc")
auc.perfB@y.values
Random Forest Procedures
library(forecast)
library(randomForest)
set.seed(123)
initial_rf <-randomForest(Y~., data=train_boost, ntree=500,nodesize=15, importance=TRUE)
varImpPlot(initial_rf, type=1)
## naive accuracy results
set.seed(123)
rfPred = predict(initial_rf, test_boost)
accuracy(rfPred,test_boost$Y)
rf_df <- data.frame(test_new$Date, test_new$Y, rfPred)
colnames(rf_df) <- c('time', 'closeDiff', 'rfPred')
indexHighDelta = which(test_boost$Y>5,arr.ind = TRUE)
binaryPred.rf = ifelse(rfPred > 0,1,0)
binaryPredSig.rf = ifelse(rfPred > 0 , 1, -1)
binaryTest.rf = ifelse(test_boost$Y > 0,1,0)
confusionMatrix(as.factor(binaryPred.rf),as.factor(binaryTest.rf))
#testing high delta predivtive accuracy
dfHiDelta = rf_df[which(abs(rf_df$closeDiff) > 5),]
dfHiDelta$predBin = ifelse(dfHiDelta$rfPred > 0,1,0)
dfHiDelta$testBin = ifelse(dfHiDelta$closeDiff > 0,1,0)
confusionMatrix(as.factor(dfHiDelta$predBin), as.factor(dfHiDelta$testBin))
#Simlulating Returns:
#Simulates long-only strat
binaryPred.rf[is.na(binaryPred.rf)] = 0
binaryPredSig.rf[is.na(binaryPredSig.rf)] = 0
returnLong = binaryPred.rf * test_boost$Y
# long/short/hold strat
magPredSig.rf = ifelse(abs(rfPred) < 0.5,0,ifelse(rfPred > 0.5,1,-1))
magPredReturns = magPredSig.rf * test_boost$Y
agReturnsMagStrat = cumsum(magPredReturns)
sum(magPredReturns)
#control returns
#Vector which simulates a long/short strategy consideration
binaryPredSig = ifelse(rfPred > 0,1,-1)
#Simulates buying or selling (long/short strat)
returnStrat = binaryPredSig.rf * test_boost$Y
agReturnsStrat = cumsum(returnStrat)
agReturnsLong = cumsum(returnLong)
Returns = cumsum(test_boost$Y)
agDF = data.frame(Returns,agReturnsStrat,agReturnsLong,agReturnsMagStrat)
DaysElapsed = 1:410
#plotting returns
plot_ly(agDF, x = DaysElapsed, y = ~Returns, type = "scatter", mode = "markers") %>%
add_trace(y = agReturnsStrat, x = 1:410, name = "Long/Short", mode = "lines")%>%
add_trace(y = agReturnsLong, x = 1:410, name = "Long Only", mode = "lines")%>%
add_trace(y = agReturnsMagStrat, x = 1:410, name = "Long/Short/Hold", mode = "lines")%>%
add_trace(y = ~Returns, x = 1:410, name = "Index", mode = "lines")
#variable importance analysis
varImpRF =initial_rf$importance
import = initial_rf$importanceSD
importVar = import[import > 2]
rf_vars = names(importVar)
rf_vars = append('Y', rf_vars)
rf_train_final = train_boost[, names(train_boost) %in% rf_vars]
rf_test_final = test_boost[, names(train_boost) %in% rf_vars]
final_rf <-randomForest(Y~., data=rf_train_final, ntree=500,nodesize=15, importance=TRUE)
rfPredFinal = predict(final_rf, rf_test_final)
accuracy(rfPredFinal,rf_test_final$Y)
rf_df <- data.frame(test_new$Date, test_new$Y, rfPredFinal)
colnames(rf_df) <- c('time', 'Y', 'rfPred')
binaryPred.rf.final = ifelse(rfPredFinal > 0,1,0)
binaryPredSig.rf.final = ifelse(rfPredFinal > 0 , 1, -1)
binaryTest.rf = ifelse(test_boost$Y > 0,1,0)
confusionMatrix(as.factor(binaryPred.rf.final),as.factor(binaryTest.rf))
#construction of ROC curve
pred = prediction(binaryPred.rf,binaryTest)
roc.perf = performance(pred,measure = 'tpr',x.measure = 'fpr')
plot(roc.perf)
abline(a=0,b=1)
auc.perf = performance(pred, measure = "auc")
auc.perf@y.values
###LSTM Procedures
#we define a lag of 1
dLag = 1
#define batch size, must be a factor of the lagged train/test length
batch.size = 1
#now basically want everything but the close diff, think a=
x.train = array(
data = as.matrix(scale(train_boost[,-1])),
dim = c(nrow(train_boost),1,51))
y.train = array(data = scale(train_boost$Y), dim = c(nrow(train_boost), 1))
x.test = array(
data = as.matrix(scale(test_boost[,-1])),
dim = c(nrow(test_boost),1,51))
y.test = array(data = scale(test_boost$Y), dim = c(nrow(test_boost), 1))
#keras_model_sequential construction
set.seed(123)
model <- keras_model_sequential()
model %>%
layer_lstm(units = 100,
input_shape = c(dLag, 51),
batch_size = batch.size,
return_sequences = TRUE,
stateful = TRUE, activation = 'tanh') %>%
layer_dropout(rate = 0.3) %>%
layer_lstm(units = 50,
return_sequences = FALSE,
stateful = TRUE, activation = 'tanh') %>%
layer_dropout(rate = 0.2) %>%
17layer_dense(units = 1, activation = 'linear')
model %>%
compile(loss = 'mae', optimizer = 'adam')
set.seed(123)
for(i in 1:5){
model %>% fit(x = x.train,
y = y.train,
batch_size = batch.size,
epochs = 1,
verbose = 0,
shuffle = FALSE)
model %>% reset_states()
}
set.seed(123)
pred_out <- model %>% predict(x.test, batch_size = batch.size)
model %>% evaluate(x.test, y.test,verbose = 0, batch_size = batch.size)
#discretizing predictions
scaleFactor = scale(test_boost$Y)
unscaled_pred = unscale(pred_out,scaleFactor)
binaryPred = ifelse(unscaled_pred > 0,1,0)
binaryPredSig = ifelse(unscaled_pred > 0,1,-1)
binaryTest = ifelse(y.test > 0,1,0)
diffPred = diffinv(unscaled_pred,1) + ethereum$Close[n]
L = length(unscaled_pred)
predictionsR = numeric(L)
for(i in 1:L){
# invert scaling
# invert differencing
yhat = unscaled_pred[i] + ethereum$Close[n-1+i]
# store
predictionsR[i] <- yhat
}
resid = unscaled_pred - y.test
confusionMatrix(as.factor(binaryPred),as.factor(binaryTest))
#Calculating Returns:
#simulates buying or selling (long/short strat)
returnStrat = binaryPredSig * test_boost$Y
#simulates long-only strat
returnLong = binaryPred * test_boost$Y
#control returns
agReturnsStrat = cumsum(returnStrat)
agReturnsLong = cumsum(returnLong)
Returns = cumsum(test_boost$Y)
agDF = data.frame(Returns,agReturnsStrat,agReturnsLong)
DaysElapsed = 1:410
#plotting returns
plot_ly(agDF, x = DaysElapsed, y = ~Returns, type = "scatter", mode = "markers") %>%
add_trace(y = agReturnsStrat, x = 1:410, name = "Long/Short", mode = "lines")%>%
add_trace(y = agReturnsLong, x = 1:410, name = "Long Only", mode = "lines")%>%
add_trace(y = ~Returns, x = 1:410, name = "Index", mode = "lines")
###ARIMA Procedures
eth_ts = ts(ethereum$Close, start = c(2016,5,27), frequency = 365)
eth_diff_ts = ts(ethereum$closeDiff, start = c(2016,5,27), frequency = 365)
plot(eth_ts)
plot(eth_diff_ts)
components.ts = decompose(eth_ts)
components.diff.ts = decompose(eth_diff_ts)
plot(components.ts)
plot(components.diff.ts)
#stationarize
library("fUnitRoots")
urkpssTest(eth_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
ndiffs(eth_ts)
ndiffs(eth_diff_ts)
#train/test split
arima_data = arima_data[, c('closeDiff', 'transaction_count', 'current_supply', 'Volume.ETH', 'Volume.USD')
N = nrow(arima_data)
n = round(N * 0.7, digits = 0)
train_arima = arima_data[1:n,]
test_arima = arima_data[(n+1):N,]
#create ts object
train_diff_ts = ts(train_arima$closeDiff, start = decimal_date(as.Date("2016-05-27")), frequency = 365)
train_arima = as.matrix(train_arima[, c('transaction_count', 'current_supply', 'Volume.ETH', 'Volume.USD')]
eth_arima = auto.arima(train_diff_ts, trace=TRUE, xreg= train_arima)
#predictions
test_arima = test_arima[, c('transaction_count', 'current_supply', 'Volume.ETH', 'Volume.USD')]
futurVal = forecast(eth_arima,h=410, level=c(99.5), xreg = as.matrix(test_arima))
plot(futurVal, main="Arima Price Difference Forecast")
#predictions and returns
binaryPred_arima = ifelse(futurVal$mean > 0,1,0)
binaryPredSig_arima = ifelse(futurVal$mean > 0,1,-1)
binaryTest = ifelse(test_new$closeDiff > 0,1,0)
confusionMatrix(as.factor(binaryPred_arima),as.factor(binaryTest))
returnStrat_arima = binaryPred_arima * test_new$closeDiff
returnStrat_short_arima = binaryPredSig_arima * test_new$closeDiff
sum(returnStrat_arima)
sum(returnStrat_short_arima)
sum(test_new$closeDiff)
agReturnsLong_arima = cumsum(returnStrat_arima)
agReturnsControl = cumsum(test_new$closeDiff)
agReturnsStrat_arima = cumsum(returnStrat_short_arima)
agDF = data.frame(agReturnsControl,agReturnsLong_arima, agReturnsStrat_arima)
head(agDF)
#plotting returns
plot(1:410,agReturnsControl)
plot(1:410,agReturnsLong_arima)
plot(1:410,agReturnsStrat_arima)
arima_returns = plot_ly(agDF, x = ~1:410, y = ~agReturnsControl, type = "scatter", mode = "markers", name="
add_trace(y = agReturnsStrat_arima, x = 1:410, name = "Long/Short", mode = "lines")%>%
add_trace(y = agReturnsLong_arima, x = 1:410, name = "Long Only", mode = "lines")
arima_returns <- arima_returns %>% layout(xaxis = list(title="Time"), yaxis = list(title="Returns"))
arima_returns
#roc curve
library(ROCR)
plot(c(0,1),c(0,1),xlab='FPR',ylab='TPR',main="ROC curve",cex.lab=1,type="n")
pred = prediction(as.vector(binaryPred_arima), as.vector(binaryTest))
perf = performance(pred, measure = "tpr", x.measure = "fpr") #calculate performance in lines(perf@x.valu
aucnow=performance(pred, measure = "auc")
lines(perf@x.values[[1]], perf@y.values[[1]])
abline(0,1,lty=2)