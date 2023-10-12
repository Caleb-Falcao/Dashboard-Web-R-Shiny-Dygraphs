#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
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

##################Serie temporal Total####################################
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



# Define UI for application that draws a histogram
ui <- fluidPage(
  htmlTemplate("www/index.html")
)


# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
