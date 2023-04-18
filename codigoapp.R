library(readxl)
library(magrittr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(tidyverse)
library(tidyr)
library(shiny)
#install.packages("leaflet.extras")
#library(leaflet.extras) # para adicionar opções extras ao mapa


# Definindo o caminho para salvar o arquivo
caminho <- "C:/Users/julia/OneDrive/Documentos/Juliana/Projeto Estatistica 2VA/dados_de_caminhada_corrida.xlsx"


# Lendo o arquivo
dados <- read_excel(caminho)

# Separando as colunas de Latitude e Longitude
dados <- dados %>%
  separate(Coordenadas, c("Latitude", "Longitude"), sep = ",", convert = TRUE)
# Convertendo as colunas de velocidade, latitude e longitude para numéricas
dados$Velocidade <- as.numeric(gsub(" km/h", "", dados$Velocidade))
dados$Latitude <- as.numeric(dados$Latitude , digits = 6)
dados$Longitude <- as.numeric(dados$Longitude, digits = 6)

library(lubridate)

dados_filtrados <- dados %>% 
  filter(Hora >=  parse_date_time("2023-03-23 18:40:53", orders = c("ymd HMS", "y/m/d HMS")) & 
           Hora <=  parse_date_time("2023-03-23 18:45:12", orders = c("ymd HMS", "y/m/d HMS")))


dados_filtrados1 <- dados %>% 
  filter(Hora >=  parse_date_time("2023-03-23 18:45:18", orders = c("ymd HMS", "y/m/d HMS")) & 
           Hora <=  parse_date_time("2023-03-23 18:49:23", orders = c("ymd HMS", "y/m/d HMS")))

#------------------------------------------------------------------------------------------------
# Modificação do codigo para abas 
#-------------------------------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Análise de Dados de Caminhada/Corrida"),
  sidebarLayout(
    sidebarPanel(
      numericInput("variancia", label = "Variância:", value = 10),
      radioButtons("testeTipo", label = "Tipo de teste:", 
                   choices = list("Bilateral" = "two.sided",
                                  "Unilateral à Direita" = "greater",
                                  "Unilateral à Esquerda" = "less")),
      sliderInput("mu0", label = "Hipótese Nula (mu0):", min = 0, max = 15, value = 5),
      sliderInput("alpha", label = "Nível de Significância:", min = 0.01, max = 0.1, value = 0.05, step = 0.01),
      sliderInput("confianca", label = "Nível de Confiança:", min = 0.90, max = 0.99, value = 0.95, step = 0.01)
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Histograma",
                 h4("Histograma da Velocidade:"),
                 plotOutput("grafico")
        ),
        tabPanel("Teste de Hipótese",
                 h4("Resultados do Teste:"),
                 verbatimTextOutput("resultado"),
                 h4("Interpretação dos Resultados:"),
                 verbatimTextOutput("interpretacao")
        ),
        tabPanel("Intervalo de Confiança",
                 h4("Intervalo de Confiança:"),
                 verbatimTextOutput("intervaloConfianca")
                 
        ),
        tabPanel("Mapa",
                 h4("Mapa de Velocidade:"),
                 leafletOutput("mapa")
        ),
        tabPanel("Gráfico de Dispersão com Regressão",verbatimTextOutput("resultadoR"), 
                 verbatimTextOutput("resultadoR2"), plotOutput("reg"))
        
        
      )
    )
  )
)

#SERVER
server <- function(input, output) {
  
  #------------------------------------------------------------------
  # Gráfico de Densidade
  #----------------------------------------------------------------
  output$grafico <- renderPlot({
    ggplot(dados, aes(x = Velocidade)) +
      geom_histogram(fill = "#1F77B4", color = "#1F77B4", alpha = 0.7, bins=50) +
      scale_fill_manual(values = "#1F77B4") +
      scale_color_manual(values = "red") +
      geom_vline(xintercept = input$mu0, color = "red", linewidth = 1) +
      ggtitle("Histograma da Velocidade") +
      xlab("Velocidade (km/h)") +
      ylab("Frequência") +
      theme_minimal()
  })
  #------------------------------------------------------------------
  output$intervaloConfianca <- renderPrint({
    xbar <- mean(dados_filtrados1$Velocidade)
    s <- sqrt(input$variancia)
    n <- nrow(dados_filtrados1)
    alpha <- input$alpha
    #------------------------------------
    alpha <- 1 - input$confianca
    
    t_alpha <- qt(1 - alpha/2, df = n - 1)
    lower <- xbar - t_alpha * s / sqrt(n)
    upper <- xbar + t_alpha * s / sqrt(n)
    paste("Intervalo de Confiança de", 100*(1-alpha),"% para a média da velocidade: [", round(lower,2), ", ", round(upper,2), "]")
  })
  
  
  #------------------------------------------------------------------  
  # Teste de Hipótese
  #------------------------------------------------------------------
  output$resultado <- renderPrint({
    t.test(dados_filtrados$Velocidade, mu = input$mu0, alternative = input$testeTipo, var.equal = TRUE)$p.value
  })
  #----------------------------------------------------
  # Interpretação dos Resultados
  #------------------------------------------------------
  output$interpretacao <- renderPrint({
    p_valor <- t.test(dados_filtrados$Velocidade, mu = input$mu0, alternative = input$testeTipo, var.equal = TRUE)$p.value
    alpha <- input$alpha
    
    if(p_valor <= alpha) {
      paste("Rejeitamos a Hipótese Nula. Há evidências suficientes para concluir que a média da velocidade é diferente de", input$mu0, "km/h com nível de significância de", alpha)
    } else {
      paste("Não rejeitamos a Hipótese Nula. Não há evidências suficientes para concluir que a média da velocidade é diferente de", input$mu0, "km/h com nível de significância de", alpha)
    }
  })
  output$mapa <- renderLeaflet({
    leaflet(data = dados) %>%
      addTiles() %>%
      addMarkers(
        ~Longitude, ~Latitude,
        popup = ~paste("Velocidade: ", Velocidade, "km/h"),
        label = ~Velocidade,
        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "auto"),
        clusterOptions = markerClusterOptions(),
        group = "todos"
      ) %>%
      addLayersControl(
        overlayGroups = c("todos"),
        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
      ) %>%
      addControl(
        position = "topright",
        html = htmltools::HTML("<div class='leaflet-control leaflet-bar my-control-class'><button id='meu-botao'>Meu Controle</button></div>")
      )
  })
    
  output$reg <- renderPlot({
    x <- cars$dist
    y <- cars$speed
    reta <- lm(y ~ x)
    alfa <- reta$coefficients[1]
    beta <- reta$coefficients[2]
    valores <- sprintf('y = %.2f + %.2fx, r² = %.2f', alfa, beta, summary(reta)$r.squared)
    
    ggplot(mapping = aes(x= x, y= y))+ #retorna as variáveis atribuídas
      geom_point()+ #informa que é um gráfico de dispersão
      labs(title = "Gráfico de Dispersão",
           x="Distancia",
           y="Velocidade")+
      geom_smooth(method = "lm")+ #usa a fórmula y~x
      geom_text(aes(x=min(x), y=max(y), label=valores), hjust=0, vjust=1) #exibe a equação da reta e r²
  })
  
  output$resultadoR <- renderPrint({
    r <- cor(x, y)
    r2 <- r^2
    paste("Valor de R =", r)
  })
  
  output$resultadoR2 <- renderPrint({
    r2 <- r^2
    paste("Valor de R² = ", r2)
  })
  
}

# RUN APP
shinyApp(ui, server)

