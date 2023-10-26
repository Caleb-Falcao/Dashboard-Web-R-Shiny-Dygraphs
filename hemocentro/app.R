################
# install.packages("zoo")
# install.packages("ggplot2")
# install.packages("shiny")
# install.packages("htmltools")
# install.packages("bslib")
# install.packages("readxl")
# install.packages("forecast")
################
library(zoo)
library(readxl)
library(shiny)
library(htmltools)
library(bslib)
library(ggplot2)
library(forecast)
############################ INICIALIZAÇÃO DAS SÉRIES TEMPORAIS ################

dados_total = read_excel("dados_sangue.xlsx", sheet = "total",col_names = FALSE)
mytsTotal = ts(dados_total, start = c(2014,1), end = c(2021,12), frequency = 12)
print(mytsTotal)


 autoplot = autoplot(mytsTotal, ylab = "Nº de bolsas", xlab = "Tempo")
# boxplot(mytsTotal)
# summary(mytsTotal)
# 
dados_aferese = read_excel("dados_sangue.xlsx", sheet = "aferese",col_names = FALSE)
mytsaferese = ts(dados_aferese, start = c(2014,1), end = c(2021,12), frequency = 12)
# mytsaferese
# autoplot(mytsaferese, ylab = "Nº de bolsas", xlab = "Tempo")
# boxplot(mytsaferese)
# 
# ############################ FIM INICIALIZAÇÃO DAS SÉRIES TEMPORAIS ############
# 
# ############################ ANÁLISE EXPLORATÓRIA ##############################
# 
 split.screen(figs=c(1,2))
 screen(1)
 plot(mytsTotal, main = "Bolsas sangue Total", xlab = "Tempo", ylab = "Nº de bolsas total")
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

# UI da aplicação
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
                   start = "01-01-2014",
                   end   = "31-12-2021",min = "2013-01-01", max ="2022-12-31",format = "dd/mm/yyyy", startview = "month",language = "pt-BR"),
    #Mostrar cards com as variaveis
    card = uiOutput("total_output"),
    #render grafico de barra
    graficogg = plotOutput("timeSeriesPlot"),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #############################Cards#################################
  output$total_output <- renderUI({
    # Converte a série temporal para um objeto 'zoo' para facilitar a manipulação de datas
    z <- as.zoo(mytsTotal)
    aferese <- as.zoo(mytsaferese)
    
    # Obtém as datas de início e fim selecionadas pelo usuário
    start_date <- as.yearmon(input$dates[1])
    end_date <- as.yearmon(input$dates[2])
    
    # Filtra os dados para o intervalo de datas selecionado
    z_filtered <- window(z, start=start_date, end=end_date)
    aferese_filtered <- window(aferese, start=start_date,end=end_date)
    
    # Calcula o total dos dados no intervalo de datas selecionado
    total <- sum(z_filtered)
    #media
    media <- as.integer(mean(z_filtered))
    #mediana
    mediana <- median(z_filtered)
    #Total plaquetas aferese
    total_aferese <- sum(aferese_filtered)
    #Minimo
    minimo <- min(z_filtered)
    #maximo
    maximo <- max(z_filtered)
    
    # Retorna o total como código HTML
    HTML(paste('<div class="row g-5 my-5">
        <!--coluna 1-->
        <div class="col-4">
          <div class="row my-2">
            <div class="card">
              <div class="card-body">
                <h5 class="card-title">Total Bolsas</h5>
                <span class="material-icons"> bloodtype </span>
                <p class="card-text">',total,' bolsas</p>
              </div>
            </div>
          </div>
          <div class="row my-2">
            <div class="card">
              <div class="card-body">
                <h5 class="card-title">Mediana</h5>
                <span class="material-icons"> medication_liquid </span>
                <p class="card-text">',mediana,' bolsas</p>
              </div>
            </div>
          </div>
        </div>
        <!--coluna 2-->
        <div class="col-4">
          <div class="row my-2">
            <div class="card">
              <div class="card-body">
                <h5 class="card-title">Total plaquetas aferese</h5>
                <span class="material-icons"> bloodtype </span>
                <p class="card-text">',total_aferese,' bolsas</p>
              </div>
            </div>
          </div>
          <div class="row my-2">
            <div class="card">
              <div class="card-body">
                <h5 class="card-title">Minimo</h5>
                <span class="material-icons"> bloodtype </span>
                <p class="card-text">',minimo,' bolsas</p>
              </div>
            </div>
          </div>
        </div>
        <!--coluna 3-->
        <div class="col-4">
          <div class="row my-2">
            <div class="card">
              <div class="card-body">
                <h5 class="card-title">Maximo</h5>
                <span class="material-icons"> bloodtype </span>

                <p class="card-text">',maximo,' bolsas</p>
              </div>
            </div>
          </div>
          <div class="row my-2">
            <div class="card">
              <div class="card-body">
                <h5 class="card-title">Média doação</h5>
                <span class="material-icons"> date_range </span>
                <p class="card-text">',media,'</p>
              </div>
            </div>
          </div>
        </div>
      </div>'))
    
  })
#####################################################################
  #criar o grafico de barra
  output$timeSeriesPlot <- renderPlot({
    # Converter a série temporal para um objeto 'zoo'
    z <- as.zoo(mytsTotal)
    print(z)
    
    # Criar um gráfico de barras
    barplot(coredata(z), main="Gráfico de Barras", xlab="Tempo", ylab="Valor")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
