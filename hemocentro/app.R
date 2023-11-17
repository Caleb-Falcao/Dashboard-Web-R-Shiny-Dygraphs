# install.packages("zoo")
# install.packages("shiny")
# install.packages("htmltools")
# install.packages("bslib")
# install.packages("readxl")
# install.packages("forecast")
# install.packages("dygraphs")
# install.packages("shinyWidgets")
################bibliotecas#####################################################
#zoo: Reestruturar serie temporal irregular, a partir de modelos genericos
library(zoo)
library(readxl)
library(shiny)
library(shinyWidgets)
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
    #variavel que mostra os cards
    card = uiOutput("total_output"),
    #variavel com intervalo de tempo
    intervalo_tempo = airDatepickerInput(
      "datesSangueTotal",
      label = "Período Sangue:",
      separator = " - ",
      value = c("2014-01-01", "2023-12-31"),
      minDate = "2014-01-01",
      maxDate = "2024-12-31",
      startView = "2014-01-01",
      view = "months",
      #editing what the popup calendar shows when it opens
      minView = "months",
      dateFormat = "MM/yyyy",
      range = TRUE,
      autoClose = TRUE,
      toggleSelected = TRUE,
      addon = "none",
      language = "pt-BR",
      position = "bottom right"
    ),
    intervalo_tempo_aferese = airDatepickerInput(
      "dates_aferese",
      label = "Período Aférese:",
      separator = " - ",
      value = c("2014-01-01", "2023-12-31"),
      minDate = "2014-01-01",
      maxDate = "2024-12-31",
      startView = "2014-01-01",
      view = "months",
      #editing what the popup calendar shows when it opens
      minView = "months",
      dateFormat = "MM/yyyy",
      range = TRUE,
      autoClose = TRUE,
      toggleSelected = TRUE,
      addon = "none",
      language = "pt-BR",
      position = "bottom right"
    )
    #dateRangeInput(
    #   "dates",
    #   "Selecione o período",
    #   start = "2014-01-01",
    #   end = "2023-12-31",
    #   min = "2014-01-01",
    #   max = "2024-12-31",
    #   format = "dd/mm/yyyy",
    #   startview = "year",
    #   language = "pt-BR",separator = "até"
    # )
    ,
    
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
    treinoSangueTotal = window(mytsTotal,
                               start = c(2014, 1),
                               end = c(2022, 12))
    treinoAfereseTotal = window(mytsaferese,
                                start = c(2014, 1),
                                end = c(2022, 12))
    #teste para saber o mape
    treinoTesteSangue = window(mytsTotal, start = c(2014,1),end=c(2021,3))
    #21 meses
    testeTotalSangue = window(mytsTotal, start = c(2021,4), end = c(2022,12))
    
    ############################### MODELO ETS#################################
    #sangue ets
    prevSTLFSangueTotal = stlf(treinoTesteSangue, h = TotalMesesTeste)
    mape = accuracy(treinoTesteSangue, prevSTLFSangueTotal$model$fitted)["Test set", "MAPE"]
    
    ################################MODELO ARIMA################################
    
    #######Obtém as datas de início e fim selecionadas pelo usuário###################
    start_date <- as.yearmon(input$datesSangueTotal[1])
    end_date <- as.yearmon(input$datesSangueTotal[2])
    start_date_aferese <- as.yearmon(input$dates_aferese[1])
    end_date_aferese <- as.yearmon(input$dates_aferese[2])
    #Converter a série temporal para um objeto 'zoo' para corrigir erros
    sangue_total <- as.zoo(mytsTotal)
    aferese <- as.zoo(mytsaferese)
    #Filtra os dados para o intervalo de datas selecionado
    sangueTotalFiltro <-
      window(sangue_total, start = start_date, end = end_date)
    #Filtra os dados para o intervalo de datas selecionado aferese
    afereseFiltro <-
      window(aferese, start = start_date_aferese, end = end_date_aferese)
    ########### MODELO ETS
    #sangue ets
    prevTreinoSangueTotalSTFL = stlf(treinoSangueTotal, h = TotalMesesTeste)
    
    #aferese ETS
    prevTreinoSangueAfereseSTFL = stlf(treinoAfereseTotal, h = TotalMesesTeste)
    
    #Ajustando o modelo com a função predict
    
    #graficos dados e predicao
    previsaoTotal <- prevTreinoSangueTotalSTFL$mean
    dados_e_previsao <- cbind(sangueTotalFiltro, previsaoTotal)
    dados_e_previsao_filtered <-
      window(dados_e_previsao, start = start_date, end = end_date)
    
    previsao_aferese <- prevTreinoSangueAfereseSTFL$mean
    dados_e_previsao_aferese <-
      cbind(afereseFiltro, previsao_aferese)
    dados_e_previsao_aferese_filtered <-
      window(dados_e_previsao_aferese,
             start = start_date_aferese,
             end = end_date_aferese)
    
    # sangue total
    total <- sum(sangueTotalFiltro)
    media <- as.integer(mean(sangueTotalFiltro))
    mediana <- median(sangueTotalFiltro)
    minimo <- min(sangueTotalFiltro)
    maximo <- max(sangueTotalFiltro)
    #aferese
    totalAferese <- sum(afereseFiltro)
    mediaAferese <- as.integer(mean(afereseFiltro))
    medianaAferese <- median(afereseFiltro)
    minimoAferese <- min(afereseFiltro)
    maximoAferese <- max(afereseFiltro)
    ##################graficos sangue total #############################
    output$graficoLinhaTotal <- renderDygraph({
      dygraph(dados_e_previsao_filtered) %>%
        dyAxis("y", label = "Nº de bolsas total") %>%
        dyAxis("x", label = "Tempo") %>%
        dySeries(color = "#9f0000", label = "Bolsas") %>%
        dySeries("previsaoTotal", label = "Previsão") %>%
        dyLegend(show = "follow", width = "0.8em") %>%
        dyOptions(fillGraph = TRUE, fillAlpha = 0.3) %>%
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
        dySeries("previsaoTotal", label = "Previsão") %>%
        dyLegend(show = "follow", width = "0.8em") %>%
        dyRangeSelector(
          height = 35,
          strokeColor = "#910000",
          fillColor = "#9f0000"
        ) %>%
        dyBarChart()
    })
    ################### graficos de sangue aferese ################
    output$graficoLinhaAferese <- renderDygraph({
      dygraph(dados_e_previsao_aferese_filtered) %>%
        dyAxis("y", label = "Nº de bolsas aférese") %>%
        dyAxis("x", label = "Tempo") %>%
        dySeries(color = "#9f0000", label = "Bolsas") %>%
        dySeries("previsao_aferese", label = "Previsão") %>%
        dyLegend(show = "follow", width = "0.8em") %>%
        dyOptions(fillGraph = TRUE, fillAlpha = 0.3) %>%
        dyRangeSelector(
          height = 35,
          strokeColor = "#910000",
          fillColor = "#9f0000"
        )
    })
    output$graficoBarraAferese <- renderDygraph({
      dygraph(dados_e_previsao_aferese_filtered) %>%
        dyAxis("y", label = "Nº de bolsas aférese") %>%
        dyAxis("x", label = "Tempo") %>%
        dySeries(color = "#9f0000", label = "Bolsas") %>%
        dySeries("previsao_aferese", label = "Previsão") %>%
        dyLegend(show = "follow", width = "0.8em") %>%
        dyRangeSelector(
          height = 35,
          strokeColor = "#910000",
          fillColor = "#9f0000"
        ) %>%
        dyBarChart()
    })
    # Retorna os cards
    HTML(
      paste(
        '
            <div
        class="row justify-content-center align-items-center p-3 g-5 gradient-cards"
      >
        <div class="col">
          <!--coluna 1-->
          <div class="col">
            <div class="row">
              <div class="card d-flex flex-row-reverse">
                <img src="img/gota_sangue.png" alt="gota de sangue card" />
                <div>
                  <h5 class="card-title">Maximo doado</h5>
                  <p class="card-text">',
        maximo,
        '</p>
                </div>
              </div>
            </div>
            <div class="row my-2">
              <div class="card d-flex flex-row-reverse">
                <img src="img/gota_sangue.png" alt="gota de sangue card" />
                <div>
                  <h5 class="card-title">Minimo doado</h5>
                  <p class="card-text">',
        minimo,
        '</p>
                </div>
              </div>
            </div>
          </div>
        </div>
        <div class="col">
          <!--coluna 2-->
          <div class="col">
            <div class="row">
              <div class="card d-flex flex-row-reverse">
                <img src="img/gota_sangue.png" alt="gota de sangue card" />
                <div>
                  <h5 class="card-title">Média doação</h5>
                  <p class="card-text">',
        media,
        '</p>
                </div>
              </div>
            </div>
            <div class="row my-2">
              <div class="card d-flex flex-row-reverse">
                <img src="img/gota_sangue.png" alt="gota de sangue card" />
                <div>
                  <h5 class="card-title">Mediana doação</h5>
                  <p class="card-text">',
        mediana,
        '</p>
                </div>
              </div>
            </div>
          </div>
        </div>
        <div class="col me-5">
          <!--coluna 3-->
          <div class="col">
            <div class="row">
              <div class="card d-flex flex-row-reverse">
                <img src="img/gota_sangue.png" alt="gota de sangue card" />
                <div>
                  <h5 class="card-title">Bolsa Sangue</h5>
                  <p class="card-text">',
        total,
        '</p>
                </div>
              </div>
            </div>
          </div>
        </div>
        <div class="col ms-3">
          <!--coluna 3-->
          <div class="col">
            <div class="row">
              <div class="card d-flex flex-row-reverse">
                <img src="img/gota_sangue.png" alt="gota de sangue card" />
                <div>
                  <h5 class="card-title">Bolsa Aférese</h5>
                  <p class="card-text">',
        totalAferese,
        '</p>
                </div>
              </div>
            </div>
          </div>
        </div>
        <div class="col">
          <!--coluna 5-->
          <div class="col">
            <div class="row">
              <div class="card d-flex flex-row-reverse">
                <img src="img/gota_sangue.png" alt="gota de sangue card" />
                <div>
                  <h5 class="card-title">Média doação</h5>
                  <p class="card-text">',
        mediaAferese,
        '</p>
                </div>
              </div>
            </div>
            <div class="row my-2">
              <div class="card d-flex flex-row-reverse">
                <img src="img/gota_sangue.png" alt="gota de sangue card" />
                <div>
                  <h5 class="card-title">Mediana doação</h5>
                  <p class="card-text">',
        medianaAferese,
        '</p>
                </div>
              </div>
            </div>
          </div>
        </div>
        <div class="col">
          <!--coluna 6-->
          <div class="col">
            <div class="row">
              <div class="card d-flex flex-row-reverse">
                <img src="img/gota_sangue.png" alt="gota de sangue card" />
                <div>
                  <h5 class="card-title">Maximo doado</h5>
                  <p class="card-text">',
        maximoAferese,
        '</p>
                </div>
              </div>
            </div>
          </div>
          <div class="row my-2">
            <div class="card d-flex flex-row-reverse">
              <img src="img/gota_sangue.png" alt="gota de sangue card" />
              <div>
                <h5 class="card-title">Minimo doado</h5>
                <p class="card-text">',
        minimoAferese,
        '</p>
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
