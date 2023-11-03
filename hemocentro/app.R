# install.packages("zoo")
# install.packages("shiny")
# install.packages("htmltools")
# install.packages("bslib")
# install.packages("readxl")
# install.packages("forecast")
# install.packages("dygraphs")
################bibliotecas#####################################################
#zoo: Reestruturar serie temporal irregular, a partir de modelos genericos
library(zoo)
library(readxl)
library(shiny)
library(htmltools)
library(bslib)
library(forecast)
library(dygraphs)
############################ INICIALIZAÇÃO DAS SÉRIES TEMPORAIS ################
dados_total <-
  read_excel("dados_sangue.xlsx", sheet = "total", col_names = FALSE)
mytsTotal <-
  ts(
    dados_total,
    start = c(2014, 1),
    end = c(2022, 12),
    frequency = 12
  )
dados_aferese <-
  read_excel("dados_sangue.xlsx", sheet = "aferese", col_names = FALSE)
mytsaferese <-
  ts(
    dados_aferese,
    start = c(2014, 1),
    end = c(2022, 12),
    frequency = 12
  )

################## GERACAO MODELO ARIMA ########################################
#print(prevTreinoSangueTotalSTFL$model)
#mdlTreinoSangueTotalArima = auto.arima(treinoSangueTotal, trace=T,stepwise = F, approximation = F)
#print(mdlTreinoSangueTotalArima)
#################################### Aplicação Web ##########################
# parametros do boostrap
theme <- bs_theme(
  version = 5.0,
  font_scale = 1.2,
  spacer = "2rem",
  bootswatch = "materia"
)

################# Processar pagina UI #######################################
ui <- bootstrapPage(
  # bootstrap referenciado
  theme = theme,
  #Tag head
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
  ## Processar o modelo HTML
  htmlTemplate(
    #renderizar pagina
    "www/index.html",
    #variavel com intervalo de tempo
    intervalo_tempo = dateRangeInput(
      "dates",
      "Selecione o período",
      start = "2014-01-01",
      end = "2023-12-31",
      min = "2014-01-01",
      max = "2025-12-31",
      format = "dd/mm/yyyy",
      startview = "month",
      language = "pt-BR"
    ),
    #variavel que mostra os cards
    card = uiOutput("total_output"),
    #Graficos sangue total
    graficoLinhaTotal = dygraphOutput("graficoLinhaTotal"),
    grafico_barra_total = dygraphOutput("graficoBarraTotal"),
    #Graficos sangue aférese
    graficoLinhaAferese = dygraphOutput("graficoLinhaAferese"),
    grafico_barra_aferese = dygraphOutput("graficoBarraAferese")
  )
)
#################################### Servidor#################################
server <- function(input, output) {
  output$total_output <- renderUI({
    ############################ DEFINIÇÃO TREINO TESTE ########################
    #total de meses de doação de sangue real
    TotalMeses = 108
    TotalMesesTreino = ceiling(0.8 * TotalMeses)
    #total de meses para previsão
    TotalMesesTeste = TotalMeses - TotalMesesTreino
    treinoSangueTotal = window(mytsTotal, start = c(2014, 1), end = c(2022, 12))
    ############################################################################
    #Obtém as datas de início e fim selecionadas pelo usuário
    start_date <- as.yearmon(input$dates[1])
    end_date <- as.yearmon(input$dates[2])
    #Converter a série temporal para um objeto 'zoo' para corrigir erros
    sangue_total <- as.zoo(mytsTotal)
    aferese <- as.zoo(mytsaferese)
    #Filtra os dados para o intervalo de datas selecionado
    sangueTotalFiltro <-
      window(sangue_total, start = start_date, end = end_date)
    #Filtra os dados para o intervalo de datas selecionado aferese
    aféreseFiltro <-
      window(aferese, start = start_date, end = end_date)
    ########### MODELO ETS
    prevTreinoSangueTotalSTFL = stlf(treinoSangueTotal, h = TotalMesesTeste)
    #Ajustando o modelo com a função predict
    
    #graficos dados e predicao
    previsao <- prevTreinoSangueTotalSTFL$mean
    dados_e_previsao <- cbind(sangueTotalFiltro, previsao)
    dados_e_previsao_filtered <-
      window(dados_e_previsao, start = start_date, end = end_date)
    # sangue total
    total <- sum(sangueTotalFiltro)
    media <- as.integer(mean(sangueTotalFiltro))
    mediana <- median(sangueTotalFiltro)
    minimo <- min(sangueTotalFiltro)
    maximo <- max(sangueTotalFiltro)
    #aferese
    total_aferese <- sum(aféreseFiltro)
    ##################graficos sangue total #############################
    output$graficoLinhaTotal <- renderDygraph({
      dygraph(dados_e_previsao_filtered) %>%
        dyAxis("y", label = "Nº de bolsas total") %>%
        dyAxis("x", label = "Tempo") %>%
        dySeries(color = "#9f0000", label = "Bolsas") %>%
        dyLegend(show = "follow") %>%
        dyOptions(fillGraph = TRUE) %>%
        dyRangeSelector(
          height = 35,
          strokeColor = "#910000",
          fillColor = "#9f0000"
        )
    })
    output$graficoBarraTotal <- renderDygraph({
      dygraph(dados_e_previsao_filtered) %>%
        dyAxis("y", label = "Nº de bolsas total") %>%
        dyAxis("x", label = "Tempo") %>%
        dyLegend(show = "follow") %>%
        dySeries(color = "#9f0000", label = "Bolsas") %>%
        dyRangeSelector() %>%
        dyBarChart()
    })
    ################### graficos de sangue aferese ################
    output$graficoLinhaAferese <- renderDygraph({
      dygraph(aféreseFiltro) %>%
        dyAxis("y", label = "Nº de bolsas aférese") %>%
        dyAxis("x", label = "Tempo") %>%
        dySeries(color = "#9f0000", label = "Bolsas") %>%
        dyLegend(show = "follow") %>%
        dyOptions(stackedGraph = TRUE) %>%
        dyRangeSelector()
    })
    output$graficoBarraAferese <- renderDygraph({
      dygraph(aféreseFiltro) %>%
        dyAxis("y", label = "Nº de bolsas aférese") %>%
        dyAxis("x", label = "Tempo") %>%
        dySeries(color = "#9f0000", label = "Bolsas") %>%
        dyLegend(show = "follow") %>%
        dyRangeSelector() %>%
        dyBarChart()
    })
    # Retorna os cards
    HTML(
      paste('
    <div class="container-fluid">
      <div class="row justify-content-center align-items-center p-3 g-5">
        <div class="col">
          <!--coluna 1-->
          <div class="col">
            <div class="row">
              <div class="card">
                <h5 class="card-title">Maximo doado</h5>
                <span class="material-icons"> bloodtype </span>
                <p class="card-text">', maximo, '</p>
              </div>
            </div>
            <div class="row my-2">
              <div class="card">
                <h5 class="card-title">Minimo doado</h5>
                <span class="material-icons"> bloodtype </span>
                <p class="card-text">', minimo, '</p>
              </div>
            </div>
          </div>
        </div>
        <div class="col">
          <!--coluna 2-->
          <div class="col">
            <div class="row">
              <div class="card">
                <h5 class="card-title">Média doação</h5>
                <span class="material-icons"> date_range </span>
                <p class="card-text">', media, '</p>
              </div>
            </div>
            <div class="row my-2">
              <div class="card">
                <h5 class="card-title">Mediana doação</h5>
                <span class="material-icons"> medication_liquid </span>
                <p class="card-text">', mediana, '</p>
              </div>
            </div>
          </div>
        </div>
        <div class="col me-5">
          <!--coluna 3-->
          <div class="col">
            <div class="row">
              <div class="card">
                <h5 class="card-title">Total Sangue</h5>
                <span class="material-icons"> bloodtype </span>
                <p class="card-text">', total, '</p>
              </div>
            </div>
          </div>
        </div>
        <div class="col ms-3">
          <!--coluna 3-->
          <div class="col">
            <div class="row">
              <div class="card">
                <h5 class="card-title">Total Aférese</h5>
                <span class="material-icons"> bloodtype </span>
                <p class="card-text">', total_aferese, '</p>
              </div>
            </div>
          </div>
        </div>
        <div class="col">
          <!--coluna 5-->
          <div class="col">
            <div class="row">
              <div class="card">
                <h5 class="card-title">Média doação</h5>
                <span class="material-icons"> date_range </span>
                <p class="card-text">', media, '</p>
              </div>
            </div>
            <div class="row my-2">
              <div class="card">
                <h5 class="card-title">Mediana doação</h5>
                <span class="material-icons"> medication_liquid </span>
                <p class="card-text">', mediana, '</p>
              </div>
            </div>
          </div>
        </div>
        <div class="col">
          <!--coluna 6-->
          <div class="col">
            <div class="row">
              <div class="card">
                <h5 class="card-title">Maximo doado</h5>
                <span class="material-icons"> bloodtype </span>
                <p class="card-text">', maximo, '</p>
              </div>
            </div>
          </div>
          <div class="row my-2">
            <div class="card">
              <h5 class="card-title">Minimo doado</h5>
              <span class="material-icons"> bloodtype </span>
              <p class="card-text">', minimo, '</p>
            </div>
          </div>
        </div>
      </div>
    </div>
'

      )
    )
  })
}
#####################################################################
# Rodar aplicação
shinyApp(ui = ui, server = server)
