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
#adicionei o framework shiny
install.packages("shiny")

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
library(shiny)
############################ DASHBOARD WEB SHINY ###############################
#EXEMPLOS#
runExample("05_sliders")
##########
library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      h1("First level title"),
      h2("Second level title"),
      h3("Third level title"),
      h4("Fourth level title"),
      h5("Fifth level title")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

############################ FIM DASHBOARD WEB SHINY ###########################

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

treinoSangueTotal = window(mytsTotal, start = c(2014,1),end=c(2020,5))
treinoSangueTotal
testeSangueTotal = window(mytsTotal, start = c(2020,6), end = c(2021,12))
testeSangueTotal

############################ FIM DEFINIÇÃO TREINO TESTE ########################
############################ GERAÇÃO DOS MODELOS ###############################

mdlTreinoSangueTotalRL = tslm(treinoSangueTotal ~ season + trend, data=treinoSangueTotal)
print(mdlTreinoSangueTotalRL)


mdlTreinoSangueTotalArima = auto.arima(treinoSangueTotal, trace=T,stepwise = F, approximation = F)
print(mdlTreinoSangueTotalArima)


prevTreinoSangueTotalSTFL = stlf(treinoSangueTotal, h=19)
print(prevTreinoSangueTotalSTFL$model)
#autoplot(prevTreinoSangueTotalSTFL$model)

prevTreinoSangueTotalhwAdditive = hw(treinoSangueTotal, seasonal = "additive", h=TotalMesesTeste)         
print(prevTreinoSangueTotalhwAdditive$model)


prevTreinoSangueTotalhwMultiplicative = hw(treinoSangueTotal, seasonal = "multiplicative", h=TotalMesesTeste)         
print(prevTreinoSangueTotalhwMultiplicative$model)


mdlTreinoSangueTotalNNETAR = nnetar(treinoSangueTotal)




############################ FIM GERAÇÃO DOS MODELOS ###########################
############################ VERIFICAÇÃO MODELO AJUSTADO #######################
accuracy(treinoSangueTotal, mdlTreinoSangueTotalRL$fitted.values)
accuracy(treinoSangueTotal, mdlTreinoSangueTotalArima$fitted)
accuracy(treinoSangueTotal, prevTreinoSangueTotalSTFL$model$fitted)
accuracy(treinoSangueTotal, prevTreinoSangueTotalhwAdditive$model$fitted)
accuracy(treinoSangueTotal, prevTreinoSangueTotalhwMultiplicative$model$fitted)
accuracy(treinoSangueTotal, mdlTreinoSangueTotalNNETAR$fitted)



############################ FIM VERIFICAÇÃO MODELO AJUSTADO ###################
########################### TESTES AUTOCORRELACAO RESIDUOS #####################

#checkresiduals(mdlTreinoSangueTotalRL) # Calcula utilizando Breusch-Godfrey
#checkresiduals(prevTreinoSangueTotalSTFL)
#Box.test(mdlTreinoSangueTotalArima$residuals, type="Ljung-Box")
#Box.test(prevTreinoSangueTotalSTFL$residuals, type="Ljung-Box")
#acf(mdlTreinoSangueTotalRL$residuals, na.action = na.pass)
#pacf(mdlTreinoSangueTotalRL$residuals, na.action = na.pass)

########################### FIM TESTES AUTOCORRELACAO RESIDUOS #################
########################### TESTES NORMALIDADE #################################

shapiro.test(mdlTreinoSangueTotalArima$residuals)
shapiro.test(mdlTreinoSangueTotalRL$residuals)
shapiro.test(prevTreinoSangueTotalSTFL$residuals)

########################### FIM TESTES NORMALIDADE #############################
########################### TESTES AUTOCORRELAÇÃO RESIDUOS #####################

checkresiduals(mdlTreinoSangueTotalArima)
checkresiduals(mdlTreinoSangueTotalRL)
checkresiduals(prevTreinoSangueTotalSTFL$model)

########################### FIM TESTES AUTOCORRELAÇÃO RESIDUOS #################

# print(prevTreinoSangueTotalSTFL$model)
# var(prevTreinoSangueTotalSTFL$residuals, na.rm = T)
# mean(as.vector(prevTreinoSangueTotalSTFL$residuals), na.rm=T)
# mean(as.vector(prevTreinoSangueTotalArima$residuals), na.rm=T)
# CV(mdlTreinoSangueTotalRL)

# BprevTreinoSangueTotalSTFL <- prevTreinoSangueTotalSTFL
# prevTreinoSangueTotalSTFL$mean
# BprevTreinoSangueTotalSTFL$mean <- BprevTreinoSangueTotalSTFL$mean + 45
# BprevTreinoSangueTotalSTFL$upper <- BprevTreinoSangueTotalSTFL$upper + 45
# BprevTreinoSangueTotalSTFL$lower <- BprevTreinoSangueTotalSTFL$lower + 45
# mean(as.vector(BprevTreinoSangueTotalSTFL$residuals), na.rm=T)
# var(as.vector(BprevTreinoSangueTotalSTFL$residuals), na.rm=T)
# accuracy(testeSangueTotal,BprevTreinoSangueTotalSTFL$mean)
# checkresiduals(BprevTreinoSangueTotalSTFL)
# shapiro.test(BprevTreinoSangueTotalSTFL$residuals)

############################ TESTES ############################################

prevTreinoSangueTotalRL = forecast(mdlTreinoSangueTotalRL, h=TotalMesesTeste)
prevTreinoSangueTotalArima = forecast(mdlTreinoSangueTotalArima, h=TotalMesesTeste)
prevTreinoSangueTotalNNETAR = forecast(mdlTreinoSangueTotalNNETAR, h=TotalMesesTeste)


accuracy(testeSangueTotal, prevTreinoTotalRL$mean)
accuracy(testeSangueTotal, prevTreinoSangueTotalArima$mean)
accuracy(testeSangueTotal, prevTreinoSangueTotalSTFL$mean)
accuracy(testeSangueTotal, prevTreinoSangueTotalhwAdditive$mean)
accuracy(testeSangueTotal, prevTreinoSangueTotalhwMultiplicative$mean)
accuracy(testeSangueTotal, prevTreinoSangueTotalNNETAR$mean)

prevTreinoSangueTotalSTFL$model

autoplot(treinoTotal) + 
  autolayer(testeTotal, series = "TESTE")

############################ FIM TESTES ########################################
############################ PLOT GRAFICOS #####################################

### FITS #######

autoplot(treinoSangueTotal, main = "Modelos ajustados") +
  autolayer(mdlTreinoSangueTotalRL$fitted.values, series="TSLM") +
  autolayer(mdlTreinoSangueTotalArima$fitted, series="ARIMA") +
  autolayer(prevTreinoSangueTotalSTFL$model$fitted, series="ETS(M,N,N)") +
  ggtitle("PREVISÕES COM AUTOPLOT E AUTOLAYER") +
  xlab("Tempo") + ylab("Nº de bolsas")

plot(treinoSangueTotal, xlab = "Tempo", ylab = "Nº de bolsas",col = "black")
lines(mdlTreinoSangueTotalRL$fitted.values, col="red")
lines(mdlTreinoSangueTotalArima$fitted, col="blue")
lines(prevTreinoSangueTotalSTFL$model$fitted, col="green")
legend("topright", legend = c("Real", "TSLM","ARIMA(0,1,2)","ETS(M,N,N)"), col = c("black","red","blue","green"), lty = 1:2,cex=0.8)
close.screen(all=T)

plot(treinoSangueTotal, xlab = "Tempo", ylab = "Nº de bolsas",col = "black")
lines(mdlTreinoSangueTotalRL$fitted.values, col="red")
legend("topright", legend = c("Real", "TSLM"), col = c("black","red"), lty = 1:2,cex=0.8)
close.screen(all=T)

plot(treinoSangueTotal, xlab = "Tempo", ylab = "Nº de bolsas",col = "black")
lines(mdlTreinoSangueTotalArima$fitted, col="blue")
legend("topright", legend = c("Real","ARIMA(0,1,2)"), col = c("black","blue"), lty = 1:2,cex=0.8)
close.screen(all=T)

plot(treinoSangueTotal, xlab = "Tempo", ylab = "Nº de bolsas",col = "black")
lines(prevTreinoSangueTotalSTFL$model$fitted, col="green")
legend("topright", legend = c("Real","ETS(M,N,N)"), col = c("black","green"), lty = 1:2,cex=0.8)
close.screen(all=T)

### FIM FITS ###
### PLOT TESTE #

plot(testeSangueTotal, xlab = "Tempo", ylab = "Nº de bolsas", col = "black")
lines(prevTreinoSangueTotalRL$mean, col="red")
lines(prevTreinoSangueTotalArima$mean, col="blue")
lines(prevTreinoSangueTotalSTFL$mean, col="green")
legend("topright", legend = c("Real", "TSLM","ARIMA(0,1,2)","ETS(M,N,N)"), col = c("black","red","blue","green"), lty = 1:2,cex=0.8)
close.screen(all=T)

plot(testeSangueTotal, xlab = "Tempo", ylab = "Nº de bolsas", col = "black")
lines(prevTreinoSangueTotalRL$mean, col="red")
legend("topright", legend = c("Real", "TSLM"), col =  c("black","red"), lty = 1:2,cex=0.8)
close.screen(all=T)

plot(testeSangueTotal, xlab = "Tempo", ylab = "Nº de bolsas", col = "black")
lines(prevTreinoSangueTotalArima$mean, col="blue")
legend("topright", legend = c("Real","ARIMA(0,1,2)"), col = c("black","blue"), lty = 1:2,cex=0.8)
close.screen(all=T)

plot(testeSangueTotal, xlab = "Tempo", ylab = "Nº de bolsas", col = "black")
lines(prevTreinoSangueTotalSTFL$mean, col="blue")
legend("topright", legend = c("Real","ETS(M,N,N)"), col = c("black","blue"), lty = 1:1,cex=0.8)
close.screen(all=T)

############################ FIM PLOT GRAFICOS #################################


############################ RASCUNHO ##########################################

cores <- c("green","red")
autoplot(prevTreinoSangueTotalSTFL)
plot(prevTreinoSangueTotalSTFL, main = "Suavização exponencial - ETS(M,N,N)",
     xlab = "Tempo",
     ylab = "Nº de bolsas sangue total",
     las = 1)

summary(prevTreinoSangueTotalRL$model)$adj.r.squared
summary(prevTreinoSangueTotalSTFL$model)$adj.r.squared
summary(prevTreinoSangueTotalArima$model)$adj.r.squared

stlTreinoSangueTotal = stl(treinoSangueTotal[,1], s.window = "periodic")
plot(stlTreinoSangueTotal)
stlTreinoSangueTotal
