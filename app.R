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

# UI ----------------------------------------------------------------------


ui <- dashboardPage(
  title = "CPI Pandemia - Análises",
  skin = "black",
  dashboardHeader(title = "CPI da Pandemia"),

# Sidebar -----------------------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      # Analisar discursos da CPI
      # 1. Apresentação
      menuItem("Apresentação", tabName = "intro"),
      # 2. Quantas sessões
      menuItem("Sessões", tabName = "sessoes"),
      # 3. Discursos
      menuItem("Discursos", tabName = "discursos"),
      # 4. parlamentares
      menuItem("Parlamentares", tabName = "parlamentares"),
      # 5. depoentes
      menuItem("Depoimentos", tabName = "depoimentos"),
      # 6. social
      menuItem("Análises", tabName = "analises"),
      # 7. nuvem de palavras
      menuItem("Insights", tabName = "insigths")
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
      # 2. Quantas sessões
      tabItem(
        tabName = "sessoes",

        ),
      # duração
      # periodicidade
      # 3. Discursos
      tabItem(
        tabName = "discursos"),
      # por bloco
      # por alinhamento
      # 4. parlamentares
      tabItem(
        tabName = "parlamentares"),
      # quantidade de sessões em que falou
      # quantidade de tempo
      # 5. depoentes
      tabItem(
        tabName = "depoimentos"),
      # 6. social
      tabItem(
        tabName = "analises"),
      # partido
      # uf
      # gênero
      # alinhamento
      # 7. nuvem de palavras
      tabItem(
        tabName = "insigths")
    )
  )
)


# Server ------------------------------------------------------------------


server <- function(input, output, session) {

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


}


# Call --------------------------------------------------------------------


shinyApp(ui, server)

