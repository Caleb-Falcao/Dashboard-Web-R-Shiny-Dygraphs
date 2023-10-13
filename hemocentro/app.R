#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("shiny")
# install.packages("htmltools")
# install.packages("bslib")
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
####

library(shiny)
library(htmltools)
library(bslib)
############################ INICIALIZAÇÃO DAS SÉRIES TEMPORAIS ################

dados_total = read_excel("C:\\projetoR\\dados_sangue.xlsx", sheet = "total",col_names = FALSE)
mytsTotal = ts(dados_total, start = c(2014,1), end = c(2021,12), frequency = 12)
mytsTotal
autoplot = autoplot(mytsTotal, ylab = "Nº de bolsas", xlab = "Tempo")
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


# bs_theme_preview
theme = bs_theme(version = 5.0, font_scale = 1.2, spacer = "2rem",
                 bootswatch = "materia")

# Define UI for application
ui <- bootstrapPage(
  #bootstrap 5
  theme = theme,
  #Tag head
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$link(rel="stylesheet", type="text/css", href="www/style.css"),
    tags$link(href="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
              rel="stylesheet",
              integrity="sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
              crossorigin="anonymous"),
    tags$title("Hemocentro Dashboard")
  ),
  
  htmlTemplate(
    #renderizar a pagina html
    "www/index.html",
    #widget de inputs
    inputgrafico = tagList(
      sliderInput(inputId = "bins", label = "Number of bins:", min = 1, max = 50, value = 30),
      submitButton(text = "Apply Changes", icon = NULL, width = NULL)
    ),
    #render grafico de barra
    grafico_barra = plotOutput(outputId = "distPlot"
    ),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #criando o grafico de barra
  output$distPlot <- renderPlot({
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
