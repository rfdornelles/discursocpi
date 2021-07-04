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
library(rmarkdown)
library(discursocpi)


# Base --------------------------------------------------------------------
#
# load("data/discursos_cpi.rda")
# load("data/tabela_fotos.rda")
# source("R/auxiliares.R", encoding = "UTF-8")

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

      # Analisar discurso
      menuItem("Discursos", tabName = "analisar_discursos"),

      # Analisar sessão
      menuItem("Sessões", tabName = "analisar_sessoes"),
      # duração
      # quantas falaram

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

# apresentacao ------------------------------------------------------------
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


# visão geral -------------------------------------------------------------

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
            choices = c("Por datas", "Por dias da semana"),
            width = "100%"
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
            choices = c("Por papel", "Por gênero", "Por gênero e por papel",
                         "Por partido", "Por partido e por gênero"),
            width = "100%"
          ),
          br(),
          plotOutput(
            outputId = "grafico_tempo_fala"
          )
        )
      )

        # fim - visão geral
        ),


# discursos ---------------------------------------------------------------

      # Analisar discurso
      tabItem(
        tabName = "analisar_discursos",
        fluidRow(
          column(
            width = 12,
            h1("Análisar discurso conforme ordador(a)"),
            br(),
            "Blab bla bla bla bla",
            br(),
            br(),
            br(),
          )
        ),
        fluidRow(
          box(
            width = 12,
            "Filtros",
            br()
            ),
    # selecionar papel da pessoa
      box(
        width = 3,
        checkboxGroupInput(
          inputId = "select_discurso_papel_pessoa",
          label = "Selecione o papel exercido",
          choiceNames = as.list(c("Senador/a", "Depoente/Convidado")),
          choiceValues = as.list(c("Senador/a", "Depoente/Convidado")),
          selected = as.list(c("Senador/a", "Depoente/Convidado")),
          width = "100%"
        )
      ),
    # selecionar gênero
      box(
        width = 3,
        checkboxGroupInput(
          inputId = "select_discurso_genero",
          label = "Selecione o gênero",
          choiceNames = as.list(unique(discursos_cpi$genero)),
          choiceValues = as.list(unique(discursos_cpi$genero)),
          selected = as.list(unique(discursos_cpi$genero)),
          width = "100%"
        )
          ),

      # selecionar partido
      box(
        width = 6,
        checkboxGroupInput(
          inputId = "select_discurso_partido",
          label = "Selecione um partido (se cabível)",
          choiceNames = as.list(unique(discursos_cpi$partido_sigla)),
          choiceValues = as.list(unique(discursos_cpi$partido_sigla)),
          selected = as.list(unique(discursos_cpi$partido_sigla)),
          width = "100%",
          inline = TRUE
        )
        )
        ),
      ## nova linha
      fluidRow(
        box(
          width = 12,
          column(
            width = 8,
              selectInput(
              inputId = "select_pessoa_selecionada",
              label = "Selecione alguém que participou da CPI",
              choices = "Carregando...",
              width = "90%"
            )
            ),
          column(width = 4,
          htmlOutput(
            outputId = "foto_pessoa_selecionada"
          )
      ),
      ),
      ),
      ## nova linha
      fluidRow(

        # % fala nas sessoes
        valueBoxOutput(
          width = 3,
          outputId = "pct_fala_sessoes"
        ),
        # % fala no total
        valueBoxOutput(
          width = 3,
          outputId = "pct_fala_total"
        ),
        # % fala no papel
        valueBoxOutput(
          width = 3,
          outputId = "pct_fala_papel"
        ),
        # % fala no genero
        valueBoxOutput(
          width = 3,
          outputId = "pct_fala_genero"
        )
      ),

      ## nova linha
      fluidRow(

        ## tabela
        column(
          width = 6,
          reactable::reactableOutput(
            outputId = "tabela_tempo_por_sessao"
          )
        ),

        ## grafico / nuvem palavras
        column(
          width = 6,
          plotOutput(
            outputId = "nuvem_de_palavras"
          )
        )
      ),

      ## nova linha
      fluidRow(

        ## tabela - tf_idf
        column(
          width = 12,
          reactable::reactableOutput(
            outputId = "tabela_tf_idf"
          )
        )

      )

      ),
##############

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

    seletor_grafico_tempo_fala <- input$seletor_grafico_tempo_fala

    # "Por papel"
  if (seletor_grafico_tempo_fala == "Por papel") {

  grafico_tempo_fala <- discursos_cpi %>%
      dplyr::group_by(papel) %>%
      dplyr::summarise(
        tempo_fala = sum(horario_duracao, na.rm = TRUE) / 60
      ) %>%
      ggplot(aes(x = papel, y = tempo_fala, fill = papel)) +
      geom_col(show.legend = FALSE) +
      theme_classic() +
      labs(
        title = "Pelo papel na Comissão",
        subtitle = "no momento da fala",
        x = "Papel exercido",
        y = "Tempo de fala"
      )

  }

    # "Por gênero"
  if (seletor_grafico_tempo_fala == "Por gênero") {

    grafico_tempo_fala <- discursos_cpi %>%
      dplyr::group_by(genero) %>%
      dplyr::summarise(
        tempo_fala = sum(horario_duracao, na.rm = TRUE) / 60
      ) %>%
      ggplot(aes(x = genero, y = tempo_fala, fill = genero)) +
      geom_col(show.legend = FALSE) +
      theme_classic() +
      labs(
        title = "Conforme o gênero",
       # subtitle = "no momento da fala",
        x = "Papel exercido",
        y = "Tempo de fala"
      )

  }

# "Por gênero e por papel"
if (seletor_grafico_tempo_fala == "Por gênero e por papel") {

  grafico_tempo_fala <- discursos_cpi %>%
  dplyr::group_by(papel, genero) %>%
  dplyr::summarise(
    tempo_fala = sum(horario_duracao, na.rm = TRUE) / 60
  ) %>%
  ggplot(aes(x = papel, y = tempo_fala, fill = genero)) +
  geom_col(position = "dodge") +
  theme_classic() +
  labs(
    title = "Pelo papel na Comissão e por gênero",
    subtitle = "no momento da fala",
    x = "Papel exercido (no momento da fala)",
    y = "Tempo de fala",
    fill = "Gênero"
  )

}

    # "Por partido"

if (seletor_grafico_tempo_fala == "Por partido") {

  grafico_tempo_fala <- discursos_cpi %>%
    dplyr::filter(partido_sigla != "Sem partido/Não se aplica") %>%
    dplyr::group_by(partido_sigla) %>%
    dplyr::summarise(
      tempo_fala = sum(horario_duracao, na.rm = TRUE) / 60
    ) %>%
    dplyr::mutate(
      partido_sigla = forcats::fct_reorder(partido_sigla, tempo_fala,
                                           .desc = TRUE)
    ) %>%
  ggplot(aes(x = partido_sigla, y = tempo_fala, fill = partido_sigla)) +
    geom_col(position = "dodge", show.legend = FALSE) +
    theme_classic() +
    labs(
      title = "Por partido",
      subtitle = "apenas Senadores/as",
      x = "Partido",
      y = "Tempo de fala",
      fill = "Gênero"
    )  +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.5))

}
    # "Por partido e por gênero

if (seletor_grafico_tempo_fala == "Por partido e por gênero") {

  grafico_tempo_fala <-  discursos_cpi %>%
    dplyr::filter(partido_sigla != "Sem partido/Não se aplica") %>%
    dplyr::group_by(partido_sigla, genero) %>%
    dplyr::summarise(
      tempo_fala = sum(horario_duracao, na.rm = TRUE) / 60
    ) %>%
    dplyr::mutate(
      partido_sigla = forcats::fct_reorder(partido_sigla, tempo_fala,
                                           .desc = TRUE)
    ) %>%
    ggplot(aes(x = partido_sigla, y = tempo_fala, fill = genero)) +
    geom_col(position = "dodge") +
    theme_classic() +
    labs(
      title = "Por partido e por gênero",
      subtitle = "apenas Senadores/as",
      x = "Partido",
      y = "Tempo de fala",
      fill = "Gênero"
    )  +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.5))

}

    grafico_tempo_fala
  })

##########  Analisar discurso

## select_discurso_papel_pessoa

## select_discurso_genero
observe({

  valores_genero <- discursos_cpi %>%
    dplyr::filter(
      papel %in% input$select_discurso_papel_pessoa) %>%
    dplyr::pull(genero) %>%
    unique() %>%
    sort()

  updateCheckboxGroupInput(
    session,
    inputId = "select_discurso_genero",
    selected = as.list(valores_genero)
  )

 })

## select_discurso_partido

observe({

  valores_partido <- discursos_cpi %>%
    dplyr::filter(
      papel %in% input$select_discurso_papel_pessoa,
      genero %in% input$select_discurso_genero) %>%
    dplyr::pull(partido_sigla) %>%
    unique() %>%
    sort()

  updateCheckboxGroupInput(
    session,
    inputId = "select_discurso_partido",
    selected = as.list(valores_partido)
)


})

valores_falante <- reactive({

  valores_falante <- discursos_cpi %>%
    dplyr::filter(
      papel %in% input$select_discurso_papel_pessoa,
      genero %in% input$select_discurso_genero,
      partido_sigla %in% input$select_discurso_partido) %>%
    dplyr::pull(falante) %>%
    unique()
})

## select_pessoa_selecionada

observe({

    updateSelectInput(
    session,
    inputId = "select_pessoa_selecionada",
    choices = valores_falante()
  )

})

## tabela_dados_pessoa

output$tabela_dados_pessoa <- renderTable({

  tibble::tribble(
    ~Nome, ~`Papel exercido`, ~Gênero,
    input$select_pessoa_selecionada, input$select_discurso_papel,
    input$select_discurso_partido
  ) %>%
    knitr::kable()

}, colnames = FALSE)

## foto_pessoa_selecionada

output$foto_pessoa_selecionada <- renderUI({

  req(input$select_pessoa_selecionada)

  img(src = retorna_foto(input$select_pessoa_selecionada),
      width = "200", align = "center")
})


## pct_fala_sessoes

output$pct_fala_sessoes <- renderValueBox({

  valueBox(
    value = "1",
    subtitle = "fala das sessões",
    color = "yellow",
    icon = icon("people-arrows")
  )

  })

## pct_fala_total

output$pct_fala_total <- renderValueBox({

  valueBox(
    value = "1",
    subtitle = "fala das sessões",
    color = "orange",
    icon = icon("user-clock")
  )

})

## pct_fala_papel

output$pct_fala_papel <- renderValueBox({

  valueBox(
    value = "1",
    subtitle = "fala das sessões",
    color = "red",
    icon = icon("id-badge")
  )

})

## pct_fala_genero

output$pct_fala_genero <- renderValueBox({

  valueBox(
    value = "1",
    subtitle = "fala das sessões",
    color = "maroon",
    icon = icon("restroom")
  )

})


## tabela_tempo_por_sessao

output$tabela_tempo_por_sessao <- reactable::renderReactable({


})

## nuvem_de_palavras

output$nuvem_de_palavras <- renderPlot({


})

## tabela_tf_idf

output$tabela_tf_idf <- reactable::renderReactable({


})


##########  Analisar sessão

##########  Analisar termo




}


# Call --------------------------------------------------------------------


shinyApp(ui, server)

