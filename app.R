## Rodrigo Dornelles - Sat Jul 03 00:00:43 2021
##
## Objetivo: aplicativo shiny


# Pacotes -----------------------------------------------------------

library(magrittr)
library(tibble)
library(shiny)
library(shinydashboard)
library(htmlwidgets)


# Base --------------------------------------------------------------------

load("data/discursos_cpi.rda")

# ideia -------------------------------------------------------------------

# Visão geral
  # quantidade de sessões
  # total de horas
  # quantidade de pessoas que falou
  # perfil

# Analisar discurso
  # o que a pessoa mais falou
  # quantidade de horas que a pessoa falou
  # palavras mais faladas
  # sessão em que mais falou

# Analisar sessão
  # duração
  # quantas falaram
  #

# Analisar termo
  # ranking
  # quem falou
  # quando falou

# UI ----------------------------------------------------------------------


ui <- dashboardPage(
  title = "CPI Pandemia - Análises",
  skin = "black",
  dashboardHeader(title = "CPI da Pandemia"),

  # Sidebar -----------------------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      # Apresentação
      menuItem("Apresentação", tabName = "intro"),

      # Visão Geral
      menuItem("Visão Geral", tabName = "visao_geral"),
      # quantidade de sessões
      # total de horas
      # quantidade de pessoas que falou
      # perfil

      # Analisar discurso
      menuItem("Discursos", tabName = "analisar_discursos"),
      # o que a pessoa mais falou
      # quantidade de horas que a pessoa falou
      # palavras mais faladas
      # sessão em que mais falou

      # Analisar sessão
      menuItem("Sessões", tabName = "analisar_sessoes"),
      # duração
      # quantas falaram
      #

      # Analisar termo
      menuItem("Termos", tabName = "analisar_termos")
      # ranking
      # quem falou
      # quando falou
    )
  ),

  # body --------------------------------------------------------------------

  dashboardBody(
    tabItems(
      # Analisar discursos da CPI
      # 1. Apresentação
      tabItem(
        tabName = "intro",
        fluidRow(
          column(
            width = 12,
            ## colocar aqruivo .md que contenha o texto
            includeMarkdown("README.md")
          )
        )
      ),


      # Visão Geral
      tabItem(
        # começo - visão geral

        tabName = "visao_geral",
        fluidRow(
          column(
            width = 12,
            h1("Visão Geral da CPI"),
            br(),
            "Texto bla bla bla bla bla"
          )
        ),
        fluidRow(
          # fileira de boxes 1
          valueBoxOutput(
              outputId = "quantidade_de_sessoes",
              width = 3
            ),

          valueBoxOutput(
            outputId = "primeira_sessao",
            width = 3
          ),

          valueBoxOutput(
            outputId = "ultima_sessao",
            width = 3
          ),

          valueBoxOutput(
            outputId = "contagem_regressiva",
            width = 3
          )
          ),
      # segunda fileira
      fluidRow(
        infoBoxOutput(
          width = 6,
          outputId = "total_horas_faladas"
        ),
        infoBoxOutput(
          width = 6,
          outputId = "quantidade_total_falantes"
        )
      ),

      # terceira fileira
      fluidRow(
        column(
          width = 6,
          h2("Distribuição das sessões"),
          br(),
          selectInput(
            inputId = "seletor_grafico_distribuicao_sessoes",
            label = "Selecione o tipo de análise",
            choices = c("Por datas", "Por dias da semana")
          ),
          br(),
          plotOutput(
            outputId = "grafico_distribuicao_sessoes"
          )
        ),
        column(
          width = 6,
          h2("Tempo de fala"),
          br(),
          selectInput(
            inputId = "seletor_grafico_tempo_fala",
            label = "Selecione o recorte de análise",
            choices = c("Por papel", "Por gênero", "Por partido")
          ),
          br(),
          plotOutput(
            outputId = "grafico_tempo_fala"
          )
        )
      )

        # fim - visão geral
        ),


      # Analisar discurso
      tabItem(
        tabName = "analisar_discursos"),
      # o que a pessoa mais falou
      # quantidade de horas que a pessoa falou
      # palavras mais faladas
      # sessão em que mais falou

      # Analisar sessão
      tabItem(
        tabName = "analisar_sessoes"),
      # duração
      # quantas falaram


      # Analisar termo
      tabItem(
        tabName = "analisar_termos")
      # ranking
      # quem falou
      # quando falou
    )
  )
)

# Server ------------------------------------------------------------------


server <- function(input, output, session) {

##########  Visão geral
#### primeira fila - valueBox

  # quantidade de sessões
  output$quantidade_de_sessoes <- renderValueBox({


    valueBox(

    )

  })

  # primeira sessão

  output$primeira_sessao <-renderValueBox({

    valueBox(

    )

  })

  # última sessão

  output$ultima_sessao <- renderValueBox({

    valueBox(

    )

  })

  # contagem regressiva

  output$contagem_regressiva <- renderValueBox({

    valueBox(

    )

  })

##### segunda fila - infoBox

  # total_horas_faladas
  output$total_horas_faladas <- renderInfoBox({


    infoBox(


    )
  })

  # quantidade_total_falantes
  output$quantidade_total_falantes  <- renderInfoBox({


    infoBox(


    )
  })


##### terceira fila - gráficos

  # grafico_distribuicao_sessoes -
    #input: seletor_grafico_distribuicao_sessoes

  output$grafico_distribuicao_sessoes <- renderPlot({



  })

  # grafico_tempo_fala
    #input: seletor_grafico_tempo_fala

  output$grafico_tempo_fala  <- renderPlot({



  })

##########  Analisar discurso

##########  Analisar sessão

##########  Analisar termo




}


# Call --------------------------------------------------------------------


shinyApp(ui, server)

