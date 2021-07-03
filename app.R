## Rodrigo Dornelles - Sat Jul 03 00:00:43 2021
##
## Objetivo: aplicativo shiny


# Pacotes -----------------------------------------------------------

library(magrittr)
library(tibble)
library(shiny)
library(shinydashboard)


# Base --------------------------------------------------------------------

load("data/discursos_cpi.rda")

# ideia -------------------------------------------------------------------

# Analisar discursos da CPI
# 1. Apresentação
# 2. Quantas sessões
  # duração
  # periodicidade
# 3. Discursos
  # por bloco
  # por alinhamento
# 4. parlamentares
  # quantidade de sessões em que falou
  # quantidade de tempo
# 5. depoentes
# 6. social
  # partido
  # uf
  # gênero
  # alinhamento
# 7. nuvem de palavras
  # por

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

        tabName = "visao_geral"

        # quantidade de sessões

        # total de horas

        # quantidade de pessoas que falou

        # perfil


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


}


# Call --------------------------------------------------------------------


shinyApp(ui, server)

