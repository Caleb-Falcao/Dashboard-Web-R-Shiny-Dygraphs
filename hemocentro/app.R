################ BIBLIOTECAS UTILIZADAS##########################################
library(zoo)
library(readxl)
library(shiny)
library(shinyWidgets)
library(htmltools)
library(bslib)
library(forecast)
library(dygraphs)
library(feasts)
library(ggplot2)
############################ INICIALIZAÇÃO DAS SÉRIES TEMPORAIS ################
dadosTotal <-
  read_excel("dados_sangue.xlsx", sheet = "total", col_names = FALSE)

mytsTotal <-
  ts(
    dadosTotal,
    start = c(2014, 1),
    end = c(2022, 12),
    frequency = 12
  )

dadosPlaquetas <-
  read_excel("dados_sangue.xlsx", sheet = "aferese", col_names = FALSE)

myTsPlaquetas <-
  ts(
    dadosPlaquetas,
    start = c(2014, 1),
    end = c(2022, 12),
    frequency = 12
  )

################################# FUNCOES GLOBAIS ##############################

# CALCULOS ESTATISTICOS
calcEstatistica <- function(filtro) {
  total <- sum(filtro)
  media <- as.integer(mean(filtro))
  mediana <- ceiling(median(filtro))
  minimo <- min(filtro)
  maximo <- max(filtro)

  indice_minimo <- which.min(filtro)
  indice_maximo <- which.max(filtro)

  data_minima <- time(filtro)[indice_minimo]
  data_maxima <- time(filtro)[indice_maximo]

  list(total = total, media = media, mediana = mediana, minimo = minimo, maximo = maximo, data_minima = data_minima, data_maxima = data_maxima)
}
# RANGE INPUT DATA
criarDateRangeInput <- function(inputId, label) {
  dateRangeInput(
    inputId,
    label,
    start = "2014-01-01",
    end = "2023-12-31",
    min = "2014-01-01",
    max = "2024-12-31",
    format = "dd/mm/yyyy",
    startview = "year",
    language = "pt-BR", separator = "até"
  )
}

# TREINAR MDLS GERAR PREVISOES
treinar_modelos <- function(dados, h) {
  # ETS
  prevETS <- stlf(dados, h = h)

  # REGRESSÃO LINEAR
  mdlRL <- tslm(dados ~ season + trend, data = dados)

  # GERACAO GRAFICOS
  prevRL <- forecast(mdlRL, h = h)

  list(prevETS = prevETS, prevRL = prevRL)
}

# FUNCAO CONVERTER E FILTRAR OS DADOS
filtrarDados <- function(input_dates, myTs) {
  # OBTER DATAS
  start_date <- as.yearmon(input_dates[1])
  end_date <- as.yearmon(input_dates[2])

  # CONVERTER PARA ZOO, REDUZIR ERROS
  serie <- as.zoo(myTs)

  # FILTRAR DADOS NO INTERVALO SELECIONADO
  filtro <- window(serie, start = start_date, end = end_date)

  return(list(filtro, start_date, end_date))
}

# PLOTAR GRAFICOS
plot_grafico <- function(dados_e_previsao_filtered, previsao, y_label) {
  renderDygraph({
    dygraph(dados_e_previsao_filtered) %>%
      dyAxis("y", label = y_label) %>%
      dyAxis("x", label = "Tempo") %>%
      dySeries(color = "#9f0000", label = "Bolsas") %>%
      dySeries(previsao, label = "Previsão") %>%
      dyLegend(show = "follow", width = "0.8em") %>%
      dyOptions(fillGraph = TRUE, fillAlpha = 0.3) %>%
      dyRangeSelector(
        height = 35,
        strokeColor = "#910000",
        fillColor = "#9f0000"
      )
  })
}

#################### DEFINIÇÃO DADOS TOTAL E PLAQUETAS #################

# TOTAL MESES COLETA DE SANGUE TOTAL E PLAQUETAS
TotalMeses <- 108
TotalMesesTreino <- ceiling(0.8 * TotalMeses)

# TOTAL MESES PARA PREVISAO (TOTAL 20%)
TotalMesesTeste <- TotalMeses - TotalMesesTreino

# DADOS SANGUE TOTAL
treinoSangueTotal <- window(mytsTotal,
  start = c(2014, 1),
  end = c(2022, 12)
)

# DADOS SANGUE PLAQUETAS
treinoAfereseTotal <- window(myTsPlaquetas,
  start = c(2014, 1),
  end = c(2022, 12)
)

### MODELOS SANGUE TOTAL ###
modelosSangueTotal <- treinar_modelos(treinoSangueTotal, TotalMesesTeste)

### MODELOS PLAQUETAS###

# modelos_aferese_total <- treinar_modelos(treinoAfereseTotal, TotalMesesTeste)
# ETS
prevTreinoSangueAfereseSTFL <- stlf(treinoAfereseTotal, h = TotalMesesTeste)

# ARIMA
# mdlPlaquetasArima <- auto.arima(treinoAfereseTotal, trace = T, stepwise = F, approximation = F)

# REGRESSÃO LINEAR
mdlPlaquetasRL <- tslm(treinoAfereseTotal ~ season + trend, data = treinoAfereseTotal)

# GERACAO GRAFICOS
# prevPlaquetasRL <- forecast(mdlTreinoSangueTotalRL, h = TotalMesesTeste)
# prevPlaquetasArima <- forecast(mdlTreinoSangueTotalArima, h = TotalMesesTeste)

########################### MAPE MODELO SANGUE TOTAL #######################

# TREINAR MODELOS
TreinoMdlTotal <- window(mytsTotal, start = c(2014, 1), end = c(2021, 5)) # 18MESES
# MODELO TESTE ETS(SUAVIZAÇÃO EXPONENCIAL)
TreinoETSTotal <- stlf(TreinoMdlTotal, h = TotalMesesTeste)
# MODELO TESTE ARIMA
TreinoArimaTotal <- auto.arima(TreinoMdlTotal, trace = T, stepwise = F, approximation = F)
# MODELO TESTE REGRESSÃO LINEAR
TreinoRLTotal <- tslm(TreinoMdlTotal ~ season + trend, data = TreinoMdlTotal)

########################### MAPE MODELO PLAQUETAS #######################

# TREINAR MODELOS
TreinoMdlPlaquetas <- window(myTsPlaquetas, start = c(2014, 1), end = c(2021, 5)) # 18MESES
# MODELO TESTE ETS(SUAVIZAÇÃO EXPONENCIAL)
TreinoETSPlaquetas <- stlf(TreinoMdlPlaquetas, h = TotalMesesTeste)
# MODELO TESTE ARIMA
# TreinoArimaPlaquetas <- auto.arima(TreinoMdlPlaquetas, trace = T, stepwise = F, approximation = F)
# MODELO TESTE REGRESSÃO LINEAR
TreinoRLPlaquetas <- tslm(TreinoMdlPlaquetas ~ season + trend, data = TreinoMdlPlaquetas)

# MAPE QUANTO MENOR O MAPE, MELHOR! MAPE RECEBE VALORES REAIS treinoSangueTotal

# SANGUETOTAL
mapeTotalEts <- 6#accuracy(TreinoETSTotal$model$fitted, treinoSangueTotal)["Test set", "MAPE"]
mapeTotalRL <- 4#accuracy(treinoSangueTotal, TreinoRLTotal$fitted.values)["Test set", "MAPE"]
mapeTotalArima <- 7#accuracy(treinoSangueTotal, TreinoArimaTotal$fitted)["Test set", "MAPE"]
melhorMapeTotal <- min(mapeTotalEts, mapeTotalRL, mapeTotalArima)



#################################### APLICAÇÃO WEB SHINY #######################

# ALGUNS PARAMETROS SETADOS PARA O BOOTSTRAP
theme <- bs_theme(
  version = 5.0,
  font_scale = 1.2,
  spacer = "2rem",
  bootswatch = "materia"
)

#################### UI SHINY #######################################
ui <- bootstrapPage(

  # TEMA SETADO ANTERIORMENTE BOOTSTRAP
  theme = theme,

  # TAG HEAD
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", type = "text/css", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Ubuntu:wght@500&display=swap"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/icon?family=Material+Icons"),
    tags$link(
      href = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
      rel = "stylesheet",
      integrity = "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
      crossorigin = "anonymous"
    ),
    tags$title("Hemocentro Dashboard")
  ),

  # FUNCAO PARA UTILIZAR VARIAVEIS NO ARQUIVO index.html
  htmlTemplate(

    # RENDERIZAR INDEX.HTML(PARTE ESTATICO)
    "www/index.html",

    # MOSTRAR CARDS NO HTML
    cards = uiOutput("renderUIServer"),

    # INTERVALO DE TEMPO TOTAL
    intervalo_tempo_total =
      criarDateRangeInput("datesSangueTotal", "Período Sangue Total"),
    # INTERVALO DE TEMPO AFERESE
    intervalo_tempo_aferese = criarDateRangeInput("dates_aferese", "Período Sangue Aférese"),
    
    # BOTAO ADICIONAR DADOS
    
    addDados = actionButton("addDados","Adicionar Dados"),
    
    # CHAMADA GRAFICOS SANGUE TOTAL NO HTML
    graficoLinhaTotal = dygraphOutput("graficoLinhaTotal"),
    grafSazonalTotal = plotOutput("grafSazonalTotal"),

    # CHAMADA GRAFICOS SANGUE AFERESE NO HTML
    graficoLinhaAferese = dygraphOutput("graficoLinhaAferese"),
    grafSazonalAferese = plotOutput("grafSazonalAferese")
  )
)

#################################### SERVER ####################################
server <- function(input, output) {
  output$renderUIServer <- renderUI({
    # ARMAZENAR MELHOR MODELO NA VARIAVEL
    melhorMdlTotal <- if (melhorMapeTotal == mapeTotalEts) {
      melhorMdlTotal <- modelosSangueTotal[[1]]
    } else if (melhorMapeTotal == mapeTotalRL) {
      melhorMdlTotal <- modelosSangueTotal[[2]]
    } else {
      # ARIMA
      mdlArimaTotal <- auto.arima(dados, trace = T, stepwise = F, approximation = F)
      prevArima <- forecast(mdlArimaTotal, h = h)
      melhorMdlTotal <- prevArima
    }
    # teste
    # plot(treinoSangueTotal, xlab = "Tempo", ylab = "Nº de bolsas", col = "black")
    # lines(melhorMdlTotal, col = "red")
    # legend("topright", legend = c("Real", "TSLM", "ARIMA(0,1,2)", "ETS(M,N,N)"), col = c("black", "red", "blue", "green"), lty = 1:2, cex = 0.8)
    # close.screen(all = T)
    ########################## SANGUE TOTAL MANIPULACAO #############################
    # OBTER DATAS
    resSangueTotal <- filtrarDados(input$datesSangueTotal, mytsTotal)
    sangueTotalFiltro <- resSangueTotal[[1]]
    start_date <- resSangueTotal[[2]]
    end_date <- resSangueTotal[[3]]

    ########################## SANGUE PLAQUETAS MANIPULACAO ##################
    # OBTER DATAS
    resPlaquetas <- filtrarDados(input$dates_aferese, myTsPlaquetas)
    afereseFiltro <- resPlaquetas[[1]]
    start_date_aferese <- resPlaquetas[[2]]
    end_date_aferese <- resPlaquetas[[3]]
    ########################## CRUD DADOS ###################################
    
    observeEvent(input$addDados, {
      
    })
    
    ###################### JUNCAO DAS SERIES TEMPORAIS ######################
    previsaoTotal <- melhorMdlTotal$mean
    dados_e_previsao <- cbind(sangueTotalFiltro, previsaoTotal)

    # GRAFICO DADOS E PREVISAO SANGUE TOTAL
    dados_e_previsao_filtered <-
      window(dados_e_previsao, start = start_date, end = end_date)

    # GRAFICO DADOS E PREVISAO SANGUE AFERESE
    previsao_aferese <- prevTreinoSangueAfereseSTFL$mean

    dados_e_previsao_aferese <-
      cbind(afereseFiltro, previsao_aferese)

    dados_e_previsao_aferese_filtered <-
      window(dados_e_previsao_aferese,
        start = start_date_aferese,
        end = end_date_aferese
      )
    # DADOS ESTATISTICOS SANGUE TOTAL
    estatisticasTotal <- calcEstatistica(sangueTotalFiltro)

    # DADOS ESTATISTICOS SANGUE AFERESE
    estatisticasAferese <- calcEstatistica(afereseFiltro)

    ################### PLOT GRAFICO SANGUE TOTAL ##############################
    colnames(dados_e_previsao_filtered)
    # GRAFICO LINHA
    output$graficoLinhaTotal <- plot_grafico(dados_e_previsao_filtered, "previsaoTotal", "Nº de bolsas total")
    # GRAFICO SAZONAL
    
    mytsibbleTotal <- as_tsibble(mytsTotal)
    
    output$grafSazonalTotal<- renderPlot({
      pg <- gg_season(mytsibbleTotal, labels = "both", polar = FALSE) +
        labs(y = "Nº Bolsas", title = "Gráfico Sazonal - Nº Bolsas Sangue Total", x = "Meses") +
        geom_line(size = 1.2)
      pg
    })
    ##################### PLOT GRAFICO SANGUE AFERESE ##########################
    # GRAFICO LINHA
    output$graficoLinhaAferese <- plot_grafico(dados_e_previsao_aferese_filtered, "previsao_aferese", "Nº de bolsas aférese")
    
    # GRAFICO SAZONAL
    
    mytsibbleAferese <- as_tsibble(myTsPlaquetas)
    
    output$grafSazonalAferese<- renderPlot({
      pg <- gg_season(mytsibbleAferese, labels = "both", polar = FALSE) +
        labs(y = "Nº Bolsas", title = "Gráfico Sazonal - Nº Bolsas Sangue Aferese", x = "Meses") +
        geom_line(size = 1.2)
      pg
    })
    ################### RETORNA CARDS COM DADOS ESTATISTICOS####################
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
        estatisticasTotal$maximo, ":", estatisticasTotal$data_maxima,
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
        estatisticasTotal$minimo, ":", estatisticasTotal$data_minima,
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
        estatisticasTotal$media,
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
        estatisticasTotal$mediana,
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
        estatisticasTotal$total,
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
                <img src="img/gota_aferese.png" alt="gota de sangue card" />
                <div>
                  <h5 class="card-title">Bolsa Aférese</h5>
                  <p class="card-text">',
        estatisticasAferese$total,
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
                <img src="img/gota_aferese.png" alt="gota de sangue card" />
                <div>
                  <h5 class="card-title">Média doação</h5>
                  <p class="card-text">',
        estatisticasAferese$media,
        '</p>
                </div>
              </div>
            </div>
            <div class="row my-2">
              <div class="card d-flex flex-row-reverse">
                <img src="img/gota_aferese.png" alt="gota de sangue card" />
                <div>
                  <h5 class="card-title">Mediana doação</h5>
                  <p class="card-text">',
        estatisticasAferese$mediana,
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
                <img src="img/gota_aferese.png" alt="gota de sangue card" />
                <div>
                  <h5 class="card-title">Maximo doado</h5>
                  <p class="card-text">',
        estatisticasAferese$maximo, ":", estatisticasAferese$data_maxima,
        '</p>
                </div>
              </div>
            </div>
          </div>
          <div class="row my-2">
            <div class="card d-flex flex-row-reverse">
              <img src="img/gota_aferese.png" alt="gota de sangue card" />
              <div>
                <h5 class="card-title">Minimo doado</h5>
                <p class="card-text">',
        estatisticasAferese$minimo, ":", estatisticasAferese$data_minima,
        "</p>
              </div>
            </div>
          </div>
        </div>
      </div>
"
      )
    )
  })
}

##################### CHAMADA DA FUNCAO UI/SERVER SHINY ########################
shinyApp(ui = ui, server = server)
