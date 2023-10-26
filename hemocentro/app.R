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
# install.packages("readxl")
# install.packages("readxl")

#library(DescTools)
#library(forecast)
#library(ggplot2)
#library(urca)
#library(lmtest)
#library(seasonal)
#library(seasonalview)
#library(dplyr)
#library(tsibble)
#library(feasts)
####
###exemplo zoo####
#install.packages("zoo")
library(zoo)
################
library(readxl)
library(shiny)
library(htmltools)
library(bslib)
############################ INICIALIZAÇÃO DAS SÉRIES TEMPORAIS ################

dados_total = read_excel("dados_sangue.xlsx", sheet = "total",col_names = FALSE)
mytsTotal = ts(dados_total, start = c(2014,1), end = c(2021,12), frequency = 12)
print(mytsTotal)


# autoplot = autoplot(mytsTotal, ylab = "Nº de bolsas", xlab = "Tempo")
# boxplot(mytsTotal)
# summary(mytsTotal)
# 
# dados_aferese = read_excel("C:\\projetoR\\dados_sangue.xlsx", sheet = "aferese",col_names = FALSE)
# mytsaferese = ts(dados_aferese, start = c(2014,1), end = c(2021,12), frequency = 12)
# mytsaferese
# autoplot(mytsaferese, ylab = "Nº de bolsas", xlab = "Tempo")
# boxplot(mytsaferese)
# 
# ############################ FIM INICIALIZAÇÃO DAS SÉRIES TEMPORAIS ############
# 
# ############################ ANÁLISE EXPLORATÓRIA ##############################
# 
# split.screen(figs=c(1,2))
# screen(1)
# plot(mytsTotal, main = "Bolsas sangue Total", xlab = "Tempo", ylab = "Nº de bolsas total")
# screen(2)
# plot(mytsaferese, main = "Bolsas sangue Aférese", xlab = "Tempo", ylab = "Nº de bolsas aférese")
# close.screen(all=T)
# 
# autoplot(mstl(mytsTotal))
# autoplot(mstl(mytsaferese))
# 
# plot(mstl(mytsTotal), main = "Decomposição série sangue total")
# plot(mstl(mytsaferese), main = "Decomposição série sangue aférese")
# 
# summary(mytsTotal)
# summary(mytsaferese)

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
    tags$link(rel="stylesheet", type="text/css", href="style.css"),
    tags$link(href="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
              rel="stylesheet",
              integrity="sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
              crossorigin="anonymous"),
    tags$title("Hemocentro Dashboard")
  ),
  
  htmlTemplate(
    #renderizar a pagina html
    "www/index.html",
    #criar periodo do dados
    intervalo_tempo = dateRangeInput("dates", "Selecione o periodo:",
                   start = "2014-01-01",
                   end   = "2021-12-31"),
    #Mostrar total doação periodo(serie_temporal)
    card = uiOutput("total_output"),
    #media = uiOutput("total_output"),
    #render grafico de barra
    grafico_barra = plotOutput(outputId = "distPlot"
    ),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$total_output <- renderUI({
    # Converte a série temporal para um objeto 'zoo' para facilitar a manipulação de datas
    z <- as.zoo(mytsTotal)
    
    # Obtém as datas de início e fim selecionadas pelo usuário
    start_date <- as.yearmon(input$dates[1])
    end_date <- as.yearmon(input$dates[2])
    
    # Filtra os dados para o intervalo de datas selecionado
    z_filtered <- window(z, start=start_date, end=end_date)
    
    # Calcula o total dos dados no intervalo de datas selecionado
    total <- sum(z_filtered)
    #media
    media <- mean(z_filtered)
    #mediana
    mediana
    #Total plaquetas aferese
    
    #Minimo
    
    #maximo
    
    # Retorna o total como código HTML
    HTML(paste("<h1>Total: ", total, "</h1>"))
    
    
  })
  #criar o grafico de barra
  
}

# Run the application 
shinyApp(ui = ui, server = server)
