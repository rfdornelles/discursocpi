## Rodrigo Dornelles - Sat Jul 03 00:00:43 2021
##
## Objetivo: aplicativo shiny


# Pacotes -----------------------------------------------------------

library(magrittr)
library(tibble)
library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(ggplot2)


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
            "Texto bla bla bla bla bla",
            br(),
            br(),
            br()
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

    valor_quantidade <- discursos_cpi %>%
      dplyr::pull(numero_sessao) %>%
      as.numeric() %>%
      sort(decreasing = TRUE) %>%
      head(1)

    valueBox(
      value = valor_quantidade,
      subtitle = "Quantidade de sessões",
      color = "teal",
      icon = icon("calendar-alt")
    )

  })

  # primeira sessão

  output$primeira_sessao <-renderValueBox({

    data_inicio = discursos_cpi %>%
      dplyr::arrange(data_sessao) %>%
      head(1) %>%
      dplyr::pull(data_sessao) %>%
      format("%d/%m/%y")

    valueBox(
      value = data_inicio,
      subtitle = "Primeira sessão",
      icon = icon("play-circle"),
      color = "olive"
    )

  })

  # última sessão

  output$ultima_sessao <- renderValueBox({

    data_fim = discursos_cpi %>%
      dplyr::arrange(data_sessao) %>%
      tail(1) %>%
      dplyr::pull(data_sessao) %>%
      format("%d/%m/%y")

    valueBox(
      value = data_fim,
      subtitle = "Última sessão",
      icon = icon("bookmark"),
      color = "light-blue"
    )

  })

  # contagem regressiva

  output$contagem_regressiva <- renderValueBox({

    prazo_final <- lubridate::ymd_hms("2021-08-07 23:59:59",
                                      tz = "America/Sao_Paulo")

    tempo <- Sys.time()

    falta <- difftime(prazo_final, tempo, units = "hours") %>%
      as.numeric()

    dias <- falta %/% 24

    horas <- round(falta %% 24, digits = 0)

    valueBox(
      value = glue::glue("{dias} dias"),
      subtitle = "Para o fim da CPI",
      icon = icon("hourglass-start"),
      color = "orange"

    )

  })

##### segunda fila - infoBox

  # total_horas_faladas
  output$total_horas_faladas <- renderInfoBox({

    tempo <- discursos_cpi %>%
      dplyr::summarise(
        tempo = sum(horario_duracao, na.rm = TRUE)
      ) %>%
      dplyr::pull() %>%
      lubridate::seconds()

    # tempo %/% lubridate::hours(1)
    infoBox(
      title = "Tempo de discuros",
      value = glue::glue("{tempo / lubridate::hours(1)} horas de fala"),
      subtitle = glue::glue("equivalente a mais de {tempo %/% lubridate::days(1)} dias"),
      icon = icon("comments"),
      color = "maroon",
      fill = TRUE
    )
  })

  # quantidade_total_falantes
  output$quantidade_total_falantes  <- renderInfoBox({

    quantidade_pessoas <- discursos_cpi %>%
      dplyr::distinct(falante) %>%
      dplyr::count()

    infoBox(
      title = "Participações",
      value = glue::glue("{quantidade_pessoas} pessoas falaram na CPI"),
      subtitle = "entre depoentes e parlamentares",
      icon = icon("users"),
      color = "purple",
      fill = TRUE

    )
  })


##### terceira fila - gráficos

  # grafico_distribuicao_sessoes -
    #input: seletor_grafico_distribuicao_sessoes
 output$grafico_distribuicao_sessoes <- renderPlot({

   base <- discursos_cpi

   if (input$seletor_grafico_distribuicao_sessoes == "Por dias da semana") {

     base <- base %>%
       dplyr::mutate(
         data_sessao = lubridate::wday(
           data_sessao,
           label = TRUE,
           abbr = FALSE)
       )  %>%
       dplyr::group_by(data_sessao) %>%
       dplyr::summarise(
         tempo_fala = mean(horario_duracao, na.rm = TRUE)
       )

     grafico <- base %>%
       ggplot(aes(x = data_sessao, y = tempo_fala)) +
       geom_col()

   } else {

      base <- base %>%
       dplyr::group_by(data_sessao) %>%
       dplyr::summarise(
         tempo_fala = sum(horario_duracao, na.rm = TRUE)
       )


      grafico <- base %>%
        ggplot(aes(x = data_sessao, y = tempo_fala)) +
        geom_line() +
        theme(
          axis.text.x = element_text(angle = 45, vjust = 0.5)) +
        scale_x_date(date_breaks = "2 day", date_labels = "%d/%m")

   }

   ## gráfico em si

   theme_set(theme_classic())

   grafico +
     labs(
       title = "Tempo de fala na sessões",
       x = "Dia da sessão",
       y = "Tempo de fala"
     )



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

