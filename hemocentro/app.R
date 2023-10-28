################
# install.packages("zoo")
# install.packages("ggplot2")
# install.packages("shiny")
# install.packages("htmltools")
# install.packages("bslib")
# ler arquivo xl
# install.packages("readxl")
# modelos
# install.packages("forecast")
# graficos de linha
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
mytsTotal <- ts(dados_total, start = c(2014, 1), end = c(2021, 12), frequency = 12)
dados_aferese <- read_excel("dados_sangue.xlsx", sheet = "aferese", col_names = FALSE)
mytsaferese <- ts(dados_aferese, start = c(2014, 1), end = c(2021, 12), frequency = 12)
#print(mytsaferese)
#autoplot <- autoplot(mytsaferese, ylab = "Nº de bolsas", xlab = "Tempo")
# boxplot(mytsaferese)
#
# ############################ FIM INICIALIZAÇÃO DAS SÉRIES TEMPORAIS ############
#
# ############################ ANÁLISE EXPLORATÓRIA ##############################
#
#split.screen(figs = c(1, 2))
#screen(1)
#plot(mytsTotal, main = "Bolsas sangue Total", xlab = "Tempo", ylab = "Nº de bolsas total")
#screen(2)
#plot(mytsaferese, main = "Bolsas sangue Aférese", xlab = "Tempo", ylab = "Nº de bolsas aférese")
#close.screen(all=T)
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

############################ DEFINIÇÃO TREINO TESTE ############################
#TotalMeses = 96
#TotalMesesTreino = ceiling(0.8*TotalMeses) 
#TotalMesesTeste = TotalMeses - TotalMesesTreino

treinoSangueTotal = window(mytsTotal, start = c(2014,1),end=c(2021, 12))
treinoSangueTotal
#testeSangueTotal = window(mytsTotal, start = c(2020,6), end = c(2021,12))
#testeSangueTotal

############################ FIM DEFINIÇÃO TREINO TESTE ########################

#############GERACAO MODELO ETS, ARIMA##########################################
prevTreinoSangueTotalSTFL = stlf(treinoSangueTotal, h=19)
mytsPrevisao = prevTreinoSangueTotalSTFL$mean
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
##########GRAFICO MODELOS
#plot(testeSangueTotal, xlab = "Tempo", ylab = "Nº de bolsas", col = "black")
#lines(prevTreinoSangueTotalRL$mean, col="red")
#lines(prevTreinoSangueTotalArima$mean, col="blue")
#lines(prevTreinoSangueTotalSTFL$mean, col="green")
#legend("topright", legend = c("Real", "TSLM","ARIMA(0,1,2)","ETS(M,N,N)"), col = c("black","red","blue","green"), lty = 1:2,cex=0.8)
#close.screen(all=T)

########### MODELO ARIMA
#mdlTreinoSangueTotalArima = auto.arima(treinoSangueTotal, trace=T,stepwise = F, approximation = F)
#print(mdlTreinoSangueTotalArima)

########### MODELO ETS
#prevTreinoSangueTotalSTFL = stlf(treinoSangueTotal, h=19)
#print(prevTreinoSangueTotalSTFL$model)
#autoplot(prevTreinoSangueTotalSTFL$model)




# bs_theme_preview
theme <- bs_theme(
  version = 5.0, font_scale = 1.2, spacer = "2rem",
  bootswatch = "materia"
)

# UI da aplicação
ui <- bootstrapPage(
  # bootstrap 5
  theme = theme,
  # Tag head
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
      end = "2050-12-31", min = "2013-01-01", max = "2050-12-31", format = "dd/mm/yyyy", startview = "month", language = "pt-BR"
    ),
    # Mostrar cards com as variaveis
    card = uiOutput("total_output"),
    # renderizer grafico de linha sangue total
    grafico_linha = dygraphOutput("graficoTotal"),
    # renderizar grafico de linha sangue aferese
    grafico_linha2 = dygraphOutput("graficoAferese"),
    grafico_previsao = dygraphOutput("graficoTotalPrevisao")

  )
)

#################################### Servidor####################################
server <- function(input, output) {
  ############################# RenderizarUI#################################
  output$total_output <- renderUI({
    # predicao para zoo
    predicao <- as.zoo(mytsPrevisao)
    # Converte a série temporal para um objeto 'zoo' para facilitar a manipulação de datas
    sangue_total <- as.zoo(mytsTotal)
    aferese <- as.zoo(mytsaferese)
    
    # Obtém as datas de início e fim selecionadas pelo usuário
    start_date <- as.yearmon(input$dates[1])
    end_date <- as.yearmon(input$dates[2])

    # Filtra os dados para o intervalo de datas selecionado
    sangue_t_filtered <- window(sangue_total, start = start_date, end = end_date)
    aferese_filtered <- window(aferese, start = start_date, end = end_date)
    ##predicao
    predicao_filtered <- window(predicao, start = start_date, end = end_date)
    # Calcula o total dos dados no intervalo de datas selecionado
    total <- sum(sangue_t_filtered)
    # media
    media <- as.integer(mean(sangue_t_filtered))
    # mediana
    mediana <- median(sangue_t_filtered)
    # Total plaquetas aferese
    total_aferese <- sum(aferese_filtered)
    # Minimo
    minimo <- min(sangue_t_filtered)
    # maximo
    maximo <- max(sangue_t_filtered)
    
    #juntando os graficos
    dados_e_previsao <- rbind(sangue_t_filtered, as.zoo(prevTreinoSangueTotalSTFL$mean))
    
    ################### grafico de linha sangue total #################################
    output$graficoTotal <- renderDygraph({
      dygraph(sangue_t_filtered) %>%
        dyAxis("y", label = "Nº de bolsas total") %>%
        dyAxis("x", label = "Tempo") %>%
        dySeries(color = "#b60000", label="Bolsas") %>%
        dyLegend(show = "follow") %>%
        dyRangeSelector()
    })
    ################### grafico de linha sangue aferese #################################
    output$graficoAferese <- renderDygraph({
      dygraph(aferese_filtered)%>%
        dyAxis("y", label = "Nº de bolsas aférese") %>%
        dyAxis("x", label = "Tempo") %>%
        dySeries(color = "#b60000", label="Bolsas") %>%
        dyLegend(show = "follow") %>%
        dyRangeSelector()
    })
    ##################grafico com predicao sangue total #############################
    output$graficoTotalPrevisao <- renderDygraph({
      dygraph(dados_e_previsao) %>%
        dyAxis("y", label = "Nº de bolsas total") %>%
        dyAxis("x", label = "Tempo") %>%
        dySeries(color = "#b60000", label="Bolsas") %>%
        dyLegend(show = "follow") %>%
        dyRangeSelector()
    })
    
    
    # Retorna os cards
    HTML(paste('
<div class="row g-5 my-5">
  <!--coluna 1-->
  <div class="col-4">
    <div class="row my-2">
      <div class="card">
        <h5 class="card-title">Total Bolsas</h5>
        <span class="material-icons"> bloodtype </span>
        <p class="card-text">', total, ' bolsas</p>
      </div>
    </div>
    <div class="row my-2">
      <div class="card">
        <h5 class="card-title">Mediana</h5>
        <span class="material-icons"> medication_liquid </span>
        <p class="card-text">', mediana, ' bolsas</p>
      </div>
    </div>
  </div>
  <!--coluna 2-->
  <div class="col-4">
    <div class="row my-2">
      <div class="card">
        <h5 class="card-title">Total plaquetas aferese</h5>
        <span class="material-icons"> bloodtype </span>
        <p class="card-text">', total_aferese, ' bolsas</p>
      </div>
    </div>
    <div class="row my-2">
      <div class="card">
        <h5 class="card-title">Minimo</h5>
        <span class="material-icons"> bloodtype </span>
        <p class="card-text">', minimo, ' bolsas</p>
      </div>
    </div>
  </div>
  <!--coluna 3-->
  <div class="col-4">
    <div class="row my-2">
      <div class="card">
        <h5 class="card-title">Maximo</h5>
        <span class="material-icons"> bloodtype </span>
        <p class="card-text">', maximo, ' bolsas</p>
      </div>
    </div>
    <div class="row my-2">
      <div class="card">
        <h5 class="card-title">Média doação</h5>
        <span class="material-icons"> date_range </span>
        <p class="card-text">', media, " bolsas</p>
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
