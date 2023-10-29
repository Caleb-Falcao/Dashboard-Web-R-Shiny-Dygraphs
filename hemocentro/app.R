# install.packages("zoo")
# install.packages("ggplot2")
# install.packages("shiny")
# install.packages("htmltools")
# install.packages("bslib")
# install.packages("readxl")
# install.packages("forecast")
# install.packages("dygraphs")
################
library(zoo)
library(readxl)
library(shiny)
library(htmltools)
library(bslib)
library(ggplot2)
library(forecast)
library(dygraphs)
############################ INICIALIZAÇÃO DAS SÉRIES TEMPORAIS ################
dados_total <- read_excel("dados_sangue.xlsx", sheet = "total", col_names = FALSE)
mytsTotal <- ts(dados_total, start = c(2014, 1), end = c(2022, 12), frequency = 12)
dados_aferese <- read_excel("dados_sangue.xlsx", sheet = "aferese", col_names = FALSE)
mytsaferese <- ts(dados_aferese, start = c(2014, 1), end = c(2022, 12), frequency = 12)
############################# FIM INICIALIZAÇÃO DAS SÉRIES TEMPORAIS ############
############################ DEFINIÇÃO TREINO TESTE ############################
#TotalMeses = 96
#TotalMesesTreino = ceiling(0.8*TotalMeses) 
#TotalMesesTeste = TotalMeses - TotalMesesTreino
treinoSangueTotal = window(mytsTotal, start = c(2014,1),end=c(2022, 12))
treinoSangueTotal
#testeSangueTotal = window(mytsTotal, start = c(2020,6), end = c(2021,12))
#testeSangueTotal
############################ FIM DEFINIÇÃO TREINO TESTE ########################
#############GERACAO MODELO ETS, ARIMA##########################################
#print(prevTreinoSangueTotalSTFL$model)
#mdlTreinoSangueTotalArima = auto.arima(treinoSangueTotal, trace=T,stepwise = F, approximation = F)
#print(mdlTreinoSangueTotalArima)
######### PLOT MODELO ETS
print(prevTreinoSangueTotalSTFL)
autoplot(prevTreinoSangueTotalSTFL)
plot(prevTreinoSangueTotalSTFL, main = "Suavização exponencial - ETS(M,N,N)",
     xlab = "Tempo",
     ylab = "Nº de bolsas sangue total",
     las = 1)
# bootstrap parametros
theme <- bs_theme(
  version = 5.0, font_scale = 1.2, spacer = "2rem",
  bootswatch = "materia"
)
# UI da aplicação
ui <- bootstrapPage(
  # bootstrap 5
  theme = theme,
  #head
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(
      href = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
      rel = "stylesheet",
      integrity = "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
      crossorigin = "anonymous"
    ),
    tags$title("Hemocentro Dashboard")
  ),
  htmlTemplate(
    # renderizar a pagina html
    "www/index.html",
    # criar periodo do dados
    intervalo_tempo = dateRangeInput("dates", "Selecione o periodo:",
      start = "2014-01-01",
      end = "2024-07-31", min = "2013-01-01", max = "2030-12-31", format = "dd/mm/yyyy", startview = "month", language = "pt-BR"
    ),
    # Mostrar cards
    card = uiOutput("total_output"),
    #grafico linha
    grafico_linha2 = dygraphOutput("graficoAferese"),
    grafico_previsao = dygraphOutput("graficoTotalPrevisao"),
    #grafico barra
    grafico_barra_total = dygraphOutput("graficoBarraTotal"),
    grafico_barra_aferese = dygraphOutput("grafico_barra_aferese")
  )
)
#################################### Servidor####################################
server <- function(input, output) {
  ############################# RenderizarUI#################################
  output$total_output <- renderUI({
    # Obtém as datas de início e fim selecionadas pelo usuário
    start_date <- as.yearmon(input$dates[1])
    end_date <- as.yearmon(input$dates[2])
    # Converte a série temporal para um objeto 'zoo' para facilitar a manipulação de datas
    sangue_total <- as.zoo(mytsTotal)
    aferese <- as.zoo(mytsaferese)
    # Filtra os dados para o intervalo de datas selecionado
    sangue_t_filtered <- window(sangue_total, start = start_date, end = end_date)
    # Filtra os dados para o intervalo de datas selecionado aferese
    aferese_filtered <- window(aferese, start = start_date, end = end_date)
    ########### MODELO ETS
    prevTreinoSangueTotalSTFL = stlf(treinoSangueTotal, h= 19)
    #graficos dados e predicao
    previsao <- as.zoo(prevTreinoSangueTotalSTFL$mean)
    dados_e_previsao <- cbind(sangue_t_filtered, previsao)
    dados_e_previsao_filtered <- window(dados_e_previsao, start = start_date, end = end_date)
    # sangue total
    total <- sum(sangue_t_filtered)
    media <- as.integer(mean(sangue_t_filtered))
    mediana <- median(sangue_t_filtered)
    minimo <- min(sangue_t_filtered)
    maximo <- max(sangue_t_filtered)
    #aferese
    total_aferese <- sum(aferese_filtered)
    ##################grafico sangue total #############################
    output$graficoTotalPrevisao <- renderDygraph({
      dygraph(dados_e_previsao_filtered) %>% 
        dyAxis("y", label = "Nº de bolsas total") %>%
        dyAxis("x", label = "Tempo") %>%
        dySeries(color = "#b60000", label="Bolsas") %>%
        dyLegend(show = "follow") %>%
        dyRangeSelector()
    })
    output$graficoBarraTotal <- renderDygraph({
      dygraph(dados_e_previsao_filtered) %>% 
        dyAxis("y", label = "Nº de bolsas total") %>%
        dyAxis("x", label = "Tempo") %>%
        dySeries(color = "#b60000", label="Bolsas")%>%
      dyRangeSelector() %>%
      dyBarChart()
    })
    ################### grafico de linha sangue aferese ################
    output$graficoAferese <- renderDygraph({
      dygraph(aferese_filtered)%>%
        dyAxis("y", label = "Nº de bolsas aférese") %>%
        dyAxis("x", label = "Tempo") %>%
        dySeries(color = "#b60000", label="Bolsas") %>%
        dyLegend(show = "follow") %>%
        dyRangeSelector()
    })
    output$grafico_barra_aferese <- renderDygraph({
      dygraph(aferese_filtered) %>% 
        dyAxis("y", label = "Nº de bolsas aférese") %>%
        dyAxis("x", label = "Tempo") %>%
        dySeries(color = "#b60000", label="Bolsas")%>%
        dyRangeSelector() %>%
        dyBarChart()
    })
    # Retorna os cards
    HTML(paste('
<div class="row g-5 my-5">
  <!--coluna 1-->
  <div class="col-4">
    <div class="row">
      <div class="card">
        <h5 class="card-title">Maximo</h5>
        <span class="material-icons"> bloodtype </span>
        <p class="card-text">', maximo, '</p>
      </div>
    </div>
    <div class="row my-3">
      <div class="card">
        <h5 class="card-title">Minimo</h5>
        <span class="material-icons"> bloodtype </span>
        <p class="card-text">', minimo, '</p>
      </div>
    </div>
  </div>
  <!--coluna 2-->
  <div class="col-4">
    <div class="row">
      <div class="card">
        <h5 class="card-title">Média doação</h5>
        <span class="material-icons"> date_range </span>
        <p class="card-text">', media, '</p>
      </div>
    </div>
    <div class="row my-3">
      <div class="card">
        <h5 class="card-title">Mediana</h5>
        <span class="material-icons"> medication_liquid </span>
        <p class="card-text">', mediana, '</p>
      </div>
    </div>
  </div>
  <!--coluna 3-->
  <div class="col-4">
    <div class="row">
      <div class="card">
        <h5 class="card-title">Total Bolsas</h5>
        <span class="material-icons"> bloodtype </span>
        <p class="card-text">', total, '</p>
      </div>
    </div>
    <div class="row my-3">
      <div class="card">
        <h5 class="card-title">Total Aferese</h5>
        <span class="material-icons"> bloodtype </span>
        <p class="card-text">', total_aferese, "</p>
      </div>
    </div>
  </div>
</div>
"))
  })
}
#####################################################################
# Rodar aplicação
shinyApp(ui = ui, server = server)
