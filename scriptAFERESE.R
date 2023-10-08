install.packages("forecast")
install.packages("ggplot2")
install.packages("urca")
install.packages("lmtest")
install.packages("seasonal")
install.packages("seasonalview")
install.packages("readxl")
install.packages("DescTools")
install.packages("dplyr")
install.packages("tsibble")
install.packages("feasts")

library(readxl)
library(DescTools)
library(forecast)
library(ggplot2)
library(urca)
library(lmtest)
library(seasonal)
library(seasonalview)
library(dplyr)
library(tsibble)
library(feasts)

############################ INICIALIZAÇÃO DAS SÉRIES TEMPORAIS ################

dados_total = read_excel("C:\\projetoR\\dados_sangue.xlsx", sheet = "total",col_names = FALSE)
mytsTotal = ts(dados_total, start = c(2014,1), end = c(2021,12), frequency = 12)
mytsTotal
autoplot(mytsTotal, ylab = "Nº de bolsas", xlab = "Tempo")
boxplot(mytsTotal)
summary(mytsTotal)

dados_aferese = read_excel("C:\\projetoR\\dados_sangue.xlsx", sheet = "aferese",col_names = FALSE)
mytsaferese = ts(dados_aferese, start = c(2014,1), end = c(2021,12), frequency = 12)
mytsaferese
autoplot(mytsaferese, ylab = "Nº de bolsas", xlab = "Tempo")
boxplot(mytsaferese)

############################ FIM INICIALIZAÇÃO DAS SÉRIES TEMPORAIS ############
############################ ANÁLISE EXPLORATÓRIA ##############################

split.screen(figs=c(1,2))
screen(1)
plot(mytsTotal, main = "Bolsas sangue Total", xlab = "Tempo", ylab = "Nº de bolsas total")
screen(2)
plot(mytsaferese, main = "Bolsas sangue Aférese", xlab = "Tempo", ylab = "Nº de bolsas aférese")
close.screen(all=T)

autoplot(mstl(mytsTotal))
autoplot(mstl(mytsaferese))

plot(mstl(mytsTotal), main = "Decomposição série sangue total")
plot(mstl(mytsaferese), main = "Decomposição série sangue aférese")

summary(mytsTotal)
summary(mytsaferese)

############################ FIM ANÁLISE EXPLORATÓRIA ##########################
############################ DEFINIÇÃO TREINO TESTE ############################
TotalMeses = 96
TotalMesesTreino = ceiling(0.8*TotalMeses) 
TotalMesesTeste = TotalMeses - TotalMesesTreino

treinoSangueAferese = window(mytsaferese, start = c(2014,1),end=c(2020,5))
treinoSangueAferese
testeSangueAferese = window(mytsaferese, start = c(2020,6), end = c(2021,12))
testeSangueAferese

############################ FIM DEFINIÇÃO TREINO TESTE ########################
############################ GERAÇÃO DOS MODELOS ###############################

mdlTreinoSangueAfereseRL = tslm(treinoSangueAferese ~ season + trend, data=treinoSangueAferese)
print(mdlTreinoSangueAfereseRL)
accuracy(treinoSangueAferese, mdlTreinoSangueAfereseRL$fitted.values)

mdlTreinoSangueAfereseArima = auto.arima(treinoSangueAferese, trace=T,stepwise = F, approximation = F)
print(mdlTreinoSangueAfereseArima)
accuracy(treinoSangueAferese, mdlTreinoSangueAfereseArima$fitted)

prevTreinoSangueAfereseSTFL = stlf(treinoSangueAferese, h=TotalMesesTeste)
print(prevTreinoSangueAfereseSTFL$model)
accuracy(treinoSangueAferese, prevTreinoSangueAfereseSTFL$model$fitted)
#autoplot(prevTreinoSangueAfereseSTFL$model)

prevTreinoSangueAferesehwAdditive = hw(treinoSangueAferese, seasonal = "additive", h=TotalMesesTeste)         
print(prevTreinoSangueAferesehwAdditive$model)
accuracy(treinoSangueAferese, prevTreinoSangueAferesehwAdditive$model$fitted)

prevTreinoSangueAferesehwMultiplicative = hw(treinoSangueAferese, seasonal = "multiplicative", h=TotalMesesTeste)         
print(prevTreinoSangueAferesehwMultiplicative$model)
accuracy(treinoSangueAferese, prevTreinoSangueAferesehwMultiplicative$model$fitted)

mdlTreinoSangueAfereseNNETAR = nnetar(treinoSangueAferese)
accuracy(treinoSangueAferese, mdlTreinoSangueAfereseNNETAR$fitted)



############################ FIM GERAÇÃO DOS MODELOS ###########################
########################### TESTES NORMALIDADE #################################

shapiro.test(mdlTreinoSangueAfereseArima$residuals)
shapiro.test(mdlTreinoSangueAfereselRL$residuals)
shapiro.test(prevTreinoSangueAfereseSTFL$model$residuals)

ks.test(mdlTreinoSangueAfereselRL$residuals,"pnorm")
ks.test(prevTreinoSangueAfereseSTFL$model$residuals,"pnorm")
qqnorm(prevTreinoSangueAfereseSTFL$residuals)
qqline(prevTreinoSangueAfereseSTFL$residuals)

autoplot(prevTreinoSangueAfereseSTFL$model$residuals)
ks.test(residuals(prevTreinoSangueAfereseSTFL),"pnorm")
shapiro.test(residuals(prevTreinoSangueAfereseSTFL$model, type = "innovation"))
########################### FIM TESTES NORMALIDADE #############################
########################### TESTES AUTOCORRELAÇÃO RESIDUOS #####################

checkresiduals(mdlTreinoSangueAfereseArima)
checkresiduals(mdlTreinoSangueAfereselRL)
checkresiduals(prevTreinoSangueAfereseSTFL$model)

########################### FIM TESTES AUTOCORRELAÇÃO RESIDUOS #################

# print(prevTreinoSangueAfereseSTFL$model)
# var(prevTreinoSangueAfereseSTFL$residuals, na.rm = T)
# mean(as.vector(prevTreinoSangueAfereseSTFL$residuals), na.rm=T)
# mean(as.vector(prevTreinoSangueAfereseArima$residuals), na.rm=T)
# var(prevTreinoSangueAfereseArima$residuals, na.rm = T)
# CV(mdlTreinoSangueAfereseRL)
# CV(mdlTreinoSangueAfereseArima)



############################ TESTES ############################################

prevTreinoSangueAfereseRL = forecast(mdlTreinoSangueAfereseRL, h=TotalMesesTeste)
prevTreinoSangueAfereseArima = forecast(mdlTreinoSangueAfereseArima, h=TotalMesesTeste)
prevTreinoSangueAfereseNNETAR = forecast(mdlTreinoSangueAfereseNNETAR, h=TotalMesesTeste)

accuracy(testeSangueAferese, prevTreinoAfereseRL$mean)
accuracy(testeSangueAferese, prevTreinoSangueAfereseArima$mean)
accuracy(testeSangueAferese, prevTreinoSangueAfereseSTFL$mean)
accuracy(testeSangueAferese, prevTreinoSangueAferesehwAdditive$mean)
accuracy(testeSangueAferese, prevTreinoSangueAferesehwMultiplicative$mean)
accuracy(testeSangueAferese, prevTreinoSangueAfereseNNETAR$mean)

prevTreinoSangueAfereseArima$residuals

############################ FIM TESTES ########################################
############################ PLOT GRAFICOS #####################################

### FITS #######

autoplot(treinoSangueAferese, main = "Modelos ajustados") +
  autolayer(mdlTreinoSangueAfereseRL$fitted.values, series="TSLM") +
  autolayer(mdlTreinoSangueAfereseArima$fitted, series="ARIMA") +
  autolayer(prevTreinoSangueAfereseSTFL$model$fitted, series="ETS(M,N,N)") +
  ggtitle("PREVISÕES COM AUTOPLOT E AUTOLAYER") +
  xlab("Tempo") + ylab("Nº de bolsas")

plot(treinoSangueAferese, xlab = "Tempo", ylab = "Nº de bolsas",col = "black")
lines(mdlTreinoSangueAfereseRL$fitted.values, col="red")
lines(mdlTreinoSangueAfereseArima$fitted, col="blue")
lines(prevTreinoSangueAfereseSTFL$model$fitted, col="green")
legend("bottomright", legend = c("Real", "TSLM","ARIMA(0,1,2)","ETS(M,N,N)"), col = c("black","red","blue","green"), lty = 1:2,cex=0.8)
close.screen(all=T)

plot(treinoSangueAferese, xlab = "Tempo", ylab = "Nº de bolsas",col = "black")
lines(mdlTreinoSangueAfereseRL$fitted.values, col="red")
legend("bottomright", legend = c("Real", "TSLM"), col = c("black","red"), lty = 1:2,cex=0.8)
close.screen(all=T)

plot(treinoSangueAferese, xlab = "Tempo", ylab = "Nº de bolsas",col = "black")
lines(mdlTreinoSangueAfereseArima$fitted, col="blue")
legend("bottomright", legend = c("Real","ARIMA(1,0,0)"), col = c("black","blue"), lty = 1:2,cex=0.8)
close.screen(all=T)

plot(treinoSangueAferese, xlab = "Tempo", ylab = "Nº de bolsas",col = "black")
lines(prevTreinoSangueAfereseSTFL$model$fitted, col="green")
legend("bottomright", legend = c("Real","ETS(A,N,N)"), col = c("black","green"), lty = 1:2,cex=0.8)
close.screen(all=T)

### FIM FITS ###
### PLOT TESTE #

plot(testeSangueAferese, xlab = "Tempo", ylab = "Nº de bolsas", col = "black")
lines(prevTreinoSangueAfereseRL$mean, col="red")
lines(prevTreinoSangueAfereseArima$mean, col="blue")
lines(prevTreinoSangueAfereseSTFL$mean, col="green")
legend("topright", legend = c("Real", "TSLM","ARIMA(1,0,0)","ETS(A,N,N)"), col = c("black","red","blue","green"), lty = 1:2,cex=0.8)
close.screen(all=T)

plot(testeSangueAferese, xlab = "Tempo", ylab = "Nº de bolsas", col = "black")
lines(prevTreinoSangueAfereseRL$mean, col="red")
legend("topright", legend = c("Real", "TSLM"), col =  c("black","red"), lty = 1:2,cex=0.8)
close.screen(all=T)

plot(testeSangueAferese, xlab = "Tempo", ylab = "Nº de bolsas", col = "black")
lines(prevTreinoSangueAfereseArima$mean, col="blue")
legend("topright", legend = c("Real","ARIMA(1,0,0)"), col = c("black","blue"), lty = 1:2,cex=0.8)
close.screen(all=T)

plot(testeSangueAferese, xlab = "Tempo", ylab = "Nº de bolsas", col = "black")
lines(prevTreinoSangueAfereseSTFL$mean, col="blue")
legend("topright", legend = c("Real","ETS(A,N,N)"), col = c("black","blue"), lty = 1:1,cex=0.8)
close.screen(all=T)

############################ FIM PLOT GRAFICOS #################################

############################ RASCUNHO ##########################################

split.screen(figs=c(1,2))
screen(1)
plot(prevTreinoSangueTotalSTFL, main = "Suavização exponencial - ETS(M,N,N)",
     xlab = "Tempo",
     ylab = "Nº de bolsas sangue total",
     las = 1)
screen(2)
plot(prevTreinoSangueAfereseSTFL, main = "Suavização exponencial - ETS(A,N,N)",
     xlab = "Tempo",
     ylab = "Nº de bolsas sangue aférese",
     las = 1)
close.screen(all=T)


split.screen(figs=c(1,2))
screen(1)
plot(treinoSangueTotal, main = "Suavização exponencial - ETS(M,N,N)", xlab = "Tempo", ylab = "Nº de bolsas sangue total",col = "black",las = 1)
lines(prevTreinoSangueTotalSTFL$model$fitted, col="blue")
legend("topright", legend = c("Real","ETS(M,N,N)"), col = c("black","blue"), lty = 1:1, cex = 0.6)
screen(2)
plot(treinoSangueAferese, main = "Suavização exponencial - ETS(A,N,N)", xlab = "Tempo", ylab = "Nº de bolsas sangue aférese",col = "black",las = 1)
lines(prevTreinoSangueAfereseSTFL$model$fitted, col="blue")
legend("bottomright", legend = c("Real","ETS(A,N,N)"), col = c("black","blue"), lty = 1:1, cex = 0.6)
close.screen(all=T)

split.screen(figs=c(1,2))
screen(1)
plot(mytsTotal, main = "Suavização exponencial - ETS(M,N,N)", xlab = "Tempo", ylab = "Nº de bolsas sangue total",col = "black",las = 1)
lines(prevTreinoSangueTotalSTFL$fitted, col="blue")
lines(prevTreinoSangueTotalSTFL$mean, col="blue")
lines(testeSangueTotal, col = "red")
legend("topright", legend = c("TREINO","PREVISÃO","TESTE"), col =  c("black","blue","red"), lty = 1:1,cex = 0.6)
screen(2)
plot(mytsaferese, main = "Suavização exponencial - ETS(A,N,N)", xlab = "Tempo", ylab = "Nº de bolsas sangue aférese",col = "black",las = 1)
lines(prevTreinoSangueAfereseSTFL$fitted, col="blue")
lines(prevTreinoSangueAfereseSTFL$mean, col="blue")
lines(testeSangueAferese, col = "red")
legend("bottomright", legend = c("TREINO","PREVISÃO","TESTE"), col =  c("black","blue","red"), lty = 1:1,cex = 0.6)
close.screen(all=T)

colunasX <- c("2020-06","2020-07","2020-08","2020-09","2020-10","2020-11",
              "2020-12","2021-01","2021-02","2021-03","2021-04","2021-05",
              "2021-06","2021-07","2021-08","2021-09","2021-10","2021-11",
              "2021-12")
colunasXDate <- as.yearmon(colunasX, )

as.yearmon("2007-12")

split.screen(figs=c(1,2))
screen(1)
plot(testeSangueTotal, main = "Suavização exponencial - ETS(M,N,N)", xlab = "Tempo", ylab = "Nº de bolsas sangue total",col = "black",las = 1, lwd=2,xaxt = "n")
axis(1,colunasXDate,format(colunasXDate,"%b %Y"))
lines(prevTreinoSangueTotalSTFL$mean, col="blue", lwd=2)
legend("topleft", legend = c("Real","ETS(M,N,N)"), col = c("black","blue"), lty = 1:1,cex=0.8, lwd=2)
screen(2)
plot(testeSangueAferese, main = "ARIMA(1,0,0)", xlab = "Tempo", ylab = "Nº de bolsas sangue aférese",col = "black",las = 1, lwd=2,xaxt = "n")
axis(1,colunasXDate,format(colunasXDate,"%b %Y"))
lines(prevTreinoSangueAfereseArima$mean, col="blue", lwd=2)
legend("topright", legend = c("Real","ARIMA(1,0,0)"), col = c("black","blue"), lty = 1:1,cex=0.8,lwd=2)
close.screen(all=T)

testeSangueTotal

autoplot(treinoSangueTotal) + 
  autolayer(testeSangueTotal, series = "TESTE") +
  autolayer(prevTreinoSangueTotalSTFL, series = "ETS(M,N,N)", PI=FALSE) +
  ggtitle("PREVISÕES COM AUTOPLOT E AUTOLAYER") +
  xlab("Tempo") + ylab("Nº de bolsas")

autoplot(testeSangueTotal) +
  autolayer(testeSangueTotal, series = "TESTE") +
  autolayer(prevTreinoSangueTotalSTFL, series = "ETS(M,N,N)", PI=FALSE) +
  ggtitle("PREVISÕES COM AUTOPLOT E AUTOLAYER") +
  xlab("Tempo") + ylab("Nº de bolsas")