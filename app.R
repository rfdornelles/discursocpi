## Rodrigo Dornelles - Sat Jul 03 00:00:43 2021

# Pacotes -----------------------------------------------------------

library(magrittr)
library(tibble)
library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(ggplot2)
library(rmarkdown)
library(reactable)
library(shinycssloaders)
# library(fst)

# Base --------------------------------------------------------------------

# função para leitura de bases remotas
ler_base_remota <- function(arquivo, extensao = ".rda") {

  url_base <- "https://github.com/rfdornelles/discursocpi/raw/main/data/"

  url_arquivo <- paste0(url_base, arquivo, extensao)

  tmp_base_remota <- tempfile()

  download.file(
    url = url_arquivo,
    destfile = tmp_base_remota
  )

 load(tmp_base_remota, envir =  .GlobalEnv)

}

# listar as bases usadas
# data("discursos_cpi")
# data("tabela_fotos")
# data("base_tokenizada")
lista_nome_bases <- c("discursos_cpi",
                      "tabela_fotos",
                      "base_tokenizada")

# carregar tudo
lapply(lista_nome_bases, ler_base_remota)

# UI ----------------------------------------------------------------------


ui <- dashboardPage(
  title = "CPI Pandemia - Análises",
  skin = "black",
  dashboardHeader(title = "CPI da Pandemia"),

  # Sidebar -----------------------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      # Apresentação
      menuItem("Apresentação",
               tabName = "intro",
               icon = icon("envelope-open-text")),

      # Visão Geral
      menuItem("Visão Geral",
               tabName = "visao_geral",
               icon = icon("tachometer-alt")),

      # Analisar discurso
      menuItem("Participantes da CPI",
               tabName = "analisar_discursos",
               icon = icon("users")),

      # Analisar sessões
      menuItem("Sessões: o que aconteceu?",
               tabName = "analisar_sessoes",
               icon = icon("calendar-alt")),

     # Analisar termo
      menuItem("Pesquisa de termos",
               tabName = "analisar_termos",
               icon = icon("search")),
     br(),
     br(),
     br(),


     # github
     menuItem("Acesse os dados e scripts", newtab = TRUE,
              href = "https://github.com/rfdornelles/discursocpi",
              icon = icon("github")),
     menuItem("LinkedIn", newtab = TRUE,
              href = "https://www.linkedin.com/in/rodrigo-dornelles-576705a2/",
              icon = icon("linkedin"))
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
            includeMarkdown("doc/apresentacao.md")
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
            #"Abaixo temos uma visão geral",
            # br(),
            # br(),
            # br()
          )
        ),
        fluidRow(
          # fileira de boxes 1
          valueBoxOutput(
              outputId = "quantidade_de_sessoes",
              width = 3
            ) %>% withSpinner(),

          valueBoxOutput(
            outputId = "primeira_sessao",
            width = 3
          ) %>% withSpinner(),

          valueBoxOutput(
            outputId = "ultima_sessao",
            width = 3
          ),

          valueBoxOutput(
            outputId = "contagem_regressiva",
            width = 3
          ) %>% withSpinner()
          ),
      # segunda fileira
      fluidRow(

        infoBoxOutput(
          width = 6,
          outputId = "total_horas_faladas"
        ) %>% withSpinner(),

        infoBoxOutput(
          width = 6,
          outputId = "quantidade_total_falantes"
        ) %>% withSpinner()
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
          ) %>% withSpinner()
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
            width = "100%",
            selected = "Por partido"
          ),
          br(),

          plotOutput(
            outputId = "grafico_tempo_fala"
          ) %>% withSpinner()
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
            h1("Analisar a atuação de participantes"),
            br(),
            "Veja abaixo algumas informações sobre a participação de alguma pessoa específica. Filtre conforme as variáveis de interesse e abaixo visualize as palavras mais usadas em formato de tabela ou gráfico de nuvem.",
            br(),
            br()
          )
        ),
        fluidRow(

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
        ),
        br()
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
        ),
        br()
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
          # width = "100%",
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
          ) %>% withSpinner()
      ),
      ),
      ),
      ## nova linha
      fluidRow(

        # % fala nas sessoes
        valueBoxOutput(
          width = 3,
          outputId = "pct_fala_sessoes"
        ) %>% withSpinner(),


        # % fala no total
        valueBoxOutput(
          width = 3,
          outputId = "pct_fala_total"
        ) %>% withSpinner(),


        # % fala no papel
        valueBoxOutput(
          width = 3,
          outputId = "pct_fala_papel"
        ) %>% withSpinner(),


        # % fala no genero
        valueBoxOutput(
          width = 3,
          outputId = "pct_fala_genero"
        ) %>% withSpinner()
      ),

      ## nova linha
      fluidRow(

        ## tabela
        column(
          width = 12,
          hr(),
          h2("Palavras mais usadas")
        ),
        ),


        fluidRow(
          box(width = 12,
          sliderInput(
            width = "100%",
            inputId = "select_quantidade_palavras_discurso",
            label = "Selecione a quantidade de termos",
            min = 1,
            max = 100,
            value = 50)
          ),

          box(
            width = 6,
            reactable::reactableOutput(
              outputId = "tabela_tf_idf"
            ) %>% withSpinner()
          ),

          box(
            width = 6,
           plotOutput(
              outputId = "nuvem_de_palavras"
            ) %>% withSpinner()
          )
        )
      ),
##############
      # Analisar sessão

      tabItem(
        tabName = "analisar_sessoes",
        fluidRow(
          column(
            width = 12,
            h1("Analisar sessões"),
            br(),
            "Especifique um período de tempo para ver os dados relativos à ssessões que nele ocorreram.",
            br(),
            br(),
          )
        ),
        fluidRow(
          box(
            width = 6,
            dateRangeInput(
              inputId = "sessoes_periodo_datas",
              label = "Selecione o período de tempo:",
              min = min(discursos_cpi$data_sessao),
              start = min(discursos_cpi$data_sessao),
              max = max(discursos_cpi$data_sessao),
              end = max(discursos_cpi$data_sessao),
              format = "dd/mm/yy",
              language = "pt-BR",
              autoclose = TRUE,
              separator = "até"
            ),
          ),

          valueBoxOutput(
              width = 3,
              outputId = "quantidade_sessoes_periodo"
            ) %>% withSpinner(),

            valueBoxOutput(
              width = 3,
              outputId = "tempo_reuniao_periodo"
            ) %>% withSpinner()
        ),


        fluidRow(
          box(
            width = 12,

            reactableOutput(
              outputId = "tabela_presenca_sessoes"
            ) %>% withSpinner()
          )
        ),


        fluidRow(

          box(
            width = 6,
            plotOutput(
              outputId = "grafico_sessoes_nuvem_palavras"
            ) %>% withSpinner()

            ),
          box(
            width = 6,

            reactableOutput(
              outputId = "tabela_sessao_ranking_palavras"
            ) %>% withSpinner()
          )
        ),
),
      # # Analisar termo
      tabItem(
        tabName = "analisar_termos",

        # intro

        fluidRow(
          column(
            width = 12,
            h1("Analisar palavras específicas"),
            br(),
            "Selecione alguma das palavras para saber quem são as pessoas que mais as utilizaram e como.",
            #br(),
            br(),
            br(),
          )
        ),

        #
        fluidRow(

          # select termo
          box(
            width = 12, title = "Termo",
            selectInput(
              inputId = "select_termo_usado",
              label = "Selecione o termo desejado",
              choices = "Carregando...",
              multiple = FALSE,
              selected = "Carregando..."
            ),

          # select perspectiva
          column(
            width = 6,
            selectInput(
              inputId = "select_perspectiva_termos",
              label = "Selecione uma variável para analisar",
              choices = c("Participante", "Partido", "Papel exercido",
                          "Gênero")
            )
          ),


          # select tf_idf
          column(
           width = 6,

           # selectInput(
           #   inputId = "select_tf_idf",
           #   label = "Deseja observar",
           #   choices = c("Ranking",
           #               "Relevância do termo (tf_idf)")
           # )
          )
        )
      ),

        fluidRow(

          # tabela
          column(
            width = 12,

            reactableOutput(
              outputId = "tabela_quem_falou_termo"
            ) %>% withSpinner()
          ),
          br(),

          # gráfico
          # column(
          #   width = 12,
          #
          #   # plotOutput(
          #   #   outputId = "grafico_uso_termo"
          #   # ) #%>% withSpinner()
          # )
        )
    )
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

    prazo_final <- lubridate::ymd_hms("2021-11-05 23:59:59",
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
      title = "Tempo de discursos",
      value = glue::glue("{scales::number(tempo / lubridate::hours(1), accuracy = 2, big.mark = '.', decimal.mark = ',')} horas de fala"),
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
       ggplot(aes(x = data_sessao, y = tempo_fala, fill = data_sessao)) +
       geom_col(show.legend = FALSE) +
       scale_fill_brewer(palette = "Set1") +
       labs(caption = "(média de duração)")

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
        scale_x_date(date_breaks = "2 day", date_labels = "%d/%m") +
        labs(caption = "(soma da duração)")

   }

   ## gráfico em si

   theme_set(theme_classic())

   grafico +
     labs(
       title = "Duração das sessões*",
       x = "Dia da sessão",
       y = "Duração"
     )



  })

  # grafico_tempo_fala
    #input: seletor_grafico_tempo_fala

output$grafico_tempo_fala  <- renderPlot({

  graficos_tempo_de_fala(input$seletor_grafico_tempo_fala)

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
    choices = sort(valores_falante()),
    selected = sample(valores_falante(), 1)
  )

})

## foto_pessoa_selecionada

output$foto_pessoa_selecionada <- renderUI({

  req(input$select_pessoa_selecionada)

  img(src = retorna_foto(input$select_pessoa_selecionada),
      width = "200", align = "center")
})


## pct_fala_sessoes

output$pct_fala_sessoes <- renderValueBox({

  sessoes_participou <- discursos_cpi %>%
    dplyr::filter(falante == input$select_pessoa_selecionada) %>%
    dplyr::distinct(numero_sessao) %>%
    dplyr::pull()

  valor <- length(sessoes_participou)/
    length(unique(discursos_cpi$numero_sessao)) * 100

  valor <- round(valor, digits = 2)

  valueBox(
    value = glue::glue("{valor}%"),
    subtitle = "de participação nas sessões da CPI da Pandemia",
    color = "yellow",
    icon = icon("people-arrows")
  )

  })

## pct_fala_total

output$pct_fala_total <- renderValueBox({

  sessoes_participou <- discursos_cpi %>%
    dplyr::filter(falante == input$select_pessoa_selecionada) %>%
    dplyr::distinct(numero_sessao) %>%
    dplyr::pull()

  tempo_total_sessoes <- discursos_cpi %>%
    dplyr::filter(numero_sessao %in% sessoes_participou) %>%
    dplyr::summarise(
      sum(horario_duracao, na.rm = TRUE)
    ) %>%
    dplyr::pull() %>%
    as.numeric()

  tempo_pessoa_falou <- discursos_cpi %>%
    dplyr::filter(falante == input$select_pessoa_selecionada) %>%
    dplyr::summarise(
      sum(horario_duracao, na.rm = TRUE)
    ) %>%
    dplyr::pull() %>%
    as.numeric()

  valor <- round(tempo_pessoa_falou/tempo_total_sessoes * 100,
                 digits = 2)


  valueBox(
    value = glue::glue("{valor}%"),
    subtitle = "do tempo de fala (das sessões que participou)",
    color = "orange",
    icon = icon("user-clock")
  )

})

## pct_fala_papel

output$pct_fala_papel <- renderValueBox({

  papel_pessoa <- discursos_cpi %>%
    dplyr::filter(
      falante == input$select_pessoa_selecionada) %>%
    head(1) %>%
    dplyr::pull(papel)

  bloco_mesmo_papel_rank <- discursos_cpi %>%
    dplyr::filter(papel == papel_pessoa) %>%
    dplyr::group_by(falante) %>%
    dplyr::summarise(
      tempo = sum(horario_duracao, na.rm = TRUE)
    ) %>%
    dplyr::arrange(-tempo) %>%
    tibble::rowid_to_column("rank")

  valor <- bloco_mesmo_papel_rank %>%
    dplyr::filter(falante == input$select_pessoa_selecionada) %>%
    dplyr::pull(rank)

  valueBox(
    value = glue::glue("{valor}º"),
    subtitle = glue::glue("{papel_pessoa} a mais falar (tempo de fala)"),
    color = "red",
    icon = icon("id-badge")
  )

})

## pct_fala_genero

output$pct_fala_genero <- renderValueBox({

  req(input$select_pessoa_selecionada)

  genero_pessoa <- discursos_cpi %>%
    dplyr::filter(
      falante == input$select_pessoa_selecionada) %>%
    head(1) %>%
    dplyr::pull(genero)

  bloco_mesmo_genero_rank <- discursos_cpi %>%
    dplyr::filter(genero == genero_pessoa) %>%
    dplyr::group_by(falante) %>%
    dplyr::summarise(
      tempo = sum(horario_duracao, na.rm = TRUE)
    ) %>%
    dplyr::arrange(-tempo) %>%
    tibble::rowid_to_column("rank")

  valor <- bloco_mesmo_genero_rank %>%
    dplyr::filter(falante == input$select_pessoa_selecionada) %>%
    dplyr::pull(rank)


  valueBox(
    value = glue::glue("{valor}º"),
    subtitle = glue::glue("no gênero {genero_pessoa} que mais fala"),
    color = "maroon",
    icon = icon("restroom")
  )

})

## nuvem_de_palavras

output$nuvem_de_palavras <- renderPlot({

  req(input$select_pessoa_selecionada)

lista_palavras_usadas() %>%
    dplyr::select(termo) %>%
    dplyr::count(termo, sort = TRUE) %>%
    desenhar_nuvem(
      qnt = 2 * input$select_quantidade_palavras_discurso
      ) +
    labs(title =
  glue::glue("Nuvem de palavras - {stringr::str_to_title(input$select_pessoa_selecionada)}")
  ) +
    theme(plot.title = element_text(face = "bold",
                                    size = 14,
                                    hjust = 0.5))


})

## tabela_tf_idf

# tornar a lista de palavras reativa

lista_palavras_usadas <- reactive({

  base_tokenizada %>%
    dplyr::filter(falante == input$select_pessoa_selecionada)

})

output$tabela_tf_idf <- reactable::renderReactable({

  req(input$select_pessoa_selecionada)

# input <- list(
#  select_pessoa_selecionada = "OMAR AZIZ",
#  select_quantidade_palavras_discurso = 10
#  )

 lista_palavras_usadas() %>%
    ranking_palavras_discurso(
      ranking = input$select_quantidade_palavras_discurso) %>%
    dplyr::select(rank, termo, n) %>%
    reactable::reactable(
      sortable = TRUE,
      pagination = TRUE,
      showPagination = TRUE,
      searchable = FALSE,
      highlight = TRUE,
      compact = TRUE,
      minRows = 10,
      defaultColDef = colDef(align = "center"),
      columns = list(
        rank = colDef(name = "Ranking", maxWidth = 80),
        termo = colDef(name = "Palavra", filterable = TRUE),
        n = colDef(name = "Repetições", maxWidth = 100)
      )
    )


})


##########  Analisar sessão

### reativo sessoes_periodo_datas

filtrar_sessoes_periodo <- reactive({

 req(input$sessoes_periodo_datas)

  discursos_cpi %>%
    dplyr::filter(
      data_sessao >= as.Date(input$sessoes_periodo_datas[1]),
      data_sessao <= as.Date(input$sessoes_periodo_datas[2])
    )

})


### quantidade_sessoes_periodo
output$quantidade_sessoes_periodo <- renderValueBox({

  valor = filtrar_sessoes_periodo() %>%
    dplyr::distinct(numero_sessao) %>%
    dplyr::pull() %>%
    length()

  valueBox(
    value = valor,
    subtitle = dplyr::if_else(
      valor == 1,
      "sessão no período",
      "sessões no período"),
    icon = icon("calendar-check"),
    color = "light-blue"
  )

})


### tempo_reuniao_periodo

output$tempo_reuniao_periodo <- renderValueBox({

  valor = filtrar_sessoes_periodo() %>%
    dplyr::summarise(
      tempo = sum(horario_duracao, na.rm = TRUE)
    ) %>%
    dplyr::pull(tempo) %>%
    lubridate::seconds()

  valueBox(
    value = scales::number(
      x = valor / lubridate::hours(1),
      accuracy = 1,
      suffix = " h",
      big.mark = ".",
      decimal.mark = ","),
    subtitle = "de sessões no período",
    icon = icon("stopwatch"),
    color = "olive"
  )

})

### tabela_presenca_sessoes

output$tabela_presenca_sessoes <- renderReactable({

  total_sessoes <- filtrar_sessoes_periodo() %>%
    dplyr::distinct(numero_sessao) %>%
    dplyr::pull() %>%
    length()


  tabela_presenca <- filtrar_sessoes_periodo() %>%
  # discursos_cpi %>%
    dplyr::select(numero_sessao, falante, horario_duracao,
                  papel, partido_sigla, genero) %>%
    dplyr::group_by(falante, papel, partido_sigla, genero) %>%
    dplyr::summarise(
      .groups = "drop",
      tempo_fala = sum(horario_duracao, na.rm = T),
      sessoes_presente = length(unique(numero_sessao))) %>%
     dplyr::mutate(
       pct_presenca = sessoes_presente / total_sessoes,
       tempo_fala = lubridate::seconds(tempo_fala) / lubridate::minutes(1),
       foto = falante
     ) %>%
    dplyr::select(foto, falante, partido_sigla,tempo_fala, sessoes_presente,
                  pct_presenca, papel, genero)


  tabela_presenca %>%
    reactable(
      showSortIcon = TRUE,
      showSortable = TRUE,
      sortable = TRUE,
      pagination = TRUE,
      showPagination = TRUE,
      searchable = FALSE,
      highlight = TRUE,
      compact = TRUE,
      minRows = 5,
      defaultPageSize = 5,
      defaultSorted = "tempo_fala",
      defaultColDef = colDef(
        align = "center",
        na = "-",
        format = colFormat(digits = 2)
        ),
      columns = list(
        foto = colDef(
           name = "", sortable = FALSE,
           cell = function(value) {
             image <- img(
               src = retorna_foto(value),
               height = "24px", alt = value)

              tagList(div(
              style = list(
                display = "inline-block",
                width = "45px"), image))}),
        falante = colDef(name = "Participante",
                         filterable = TRUE),
        papel = colDef(name = "Atuação"),
        partido_sigla = colDef(name = "Partido",
                               filterable = TRUE),
        genero = colDef(name = "Gênero"),
        tempo_fala = colDef(name = "Tempo de fala",
                            defaultSortOrder = "desc",
                            format = colFormat(
                              suffix = " min",
                              digits = 0)),
        sessoes_presente = colDef(name = "Sessões presentes",
                                  format = colFormat(digits = 0)),
        pct_presenca = colDef(name = "% presença",
                              format = colFormat(digits = 1,
                                                 percent = TRUE))
      )
    )



})

## reativo

lista_palavras_usadas_sessoes <- reactive({

  req(input$sessoes_periodo_datas)

  base_tokenizada  %>%
    dplyr::filter(
      data_sessao >= as.Date(input$sessoes_periodo_datas[1]),
      data_sessao <= as.Date(input$sessoes_periodo_datas[2])
    ) %>%
    dplyr::select(termo) %>%
    dplyr::count(termo, sort = TRUE)


})
### grafico_sessoes_nuvem_palavras

output$grafico_sessoes_nuvem_palavras <- renderPlot({

  req(input$sessoes_periodo_datas[1])

  lista_palavras_usadas_sessoes() %>%
    desenhar_nuvem(
      qnt = 150, corMax = "#01044a", corMin = "#4dfff9") +
    labs(title = glue::glue(
"Nuvem de palavras usadas nas sessões \nentre {format(input$sessoes_periodo_datas[1], '%d/%m/%y')} e {format(input$sessoes_periodo_datas[2], '%d/%m/%y')}")
    ) +
    theme(plot.title = element_text(face = "bold",
                                    size = 14,
                                    hjust = 0.5))

})


### tabela_sessao_ranking_palavras

output$tabela_sessao_ranking_palavras <- renderReactable({

  req(input$select_termo_usado)

  lista_palavras_usadas_sessoes() %>%
    reactable(
      sortable = TRUE,
      pagination = TRUE,
      showPagination = TRUE,
      searchable = FALSE,
      highlight = TRUE,
      compact = TRUE,
      minRows = 10,
      defaultSortOrder = "desc",
      defaultSorted = "n",
      defaultColDef = colDef(align = "center"),
      columns = list(
        termo = colDef(name = "Palavra", filterable = TRUE),
        n = colDef(name = "Repetições",
                   format = colFormat(separators = TRUE))
      )
    )




})



##########  Analisar termo
## update select_termo_usado

observe({

  escolhas <- base_tokenizada %>%
    dplyr::count(termo, sort = TRUE) %>%
    dplyr::pull(termo)

  updateSelectInput(
    session,
    inputId = "select_termo_usado",
    choices = escolhas,
    selected = sample(
      c("vacina", "covid", "saude", "pandemia", "federal",
       "tratamento", "cloroquina"),
      size = 1)
  )

})

## tabela_quem_falou_termo

## reativo

# token_termos_usados_reativo <- reactive({
#
#   base_tokenizada %>%
#     dplyr::filter(termo %in% input$select_termo_usado)
#
#
# })

tabela_filtrada_reativa <- reactive({

  req(input$select_perspectiva_termos)

  resposta <- input$select_perspectiva_termos

  variavel_selecionada <- dplyr::case_when(
    resposta == "Partido" ~ "partido_sigla",
    resposta == "Papel exercido" ~ "papel",
    resposta == "Gênero" ~ "genero",
    TRUE ~ "falante"
  )

  termo_usado <- input$select_termo_usado

       ranking_palavras_discurso(
        ranking = 10000,
        # tf_idf = tipo_tf_idf,
        documento = variavel_selecionada
      ) %>%
         dplyr::filter(termo %in% termo_usado) %>%
         dplyr::relocate(
           termo, dplyr::matches(variavel_selecionada)
         )

})

output$tabela_quem_falou_termo <- renderReactable({

  req(input$select_termo_usado)

   # tipo_tf_idf <- dplyr::if_else(
  #   input$select_tf_idf == "Ranking",
  #   FALSE,
  #   TRUE,
  #   FALSE
  # )
#
#      ranking_palavras_discurso(
#       ranking = 3,
#       # tf_idf = tipo_tf_idf,
#       documento = variavel_selecionada
#     ) %>%
#        dplyr::filter(termo %in% input$select_termo_usado) %>%
#     dplyr::relocate(
#       termo, dplyr::matches(variavel_selecionada)
#     ) %>%
#
    tabela_filtrada_reativa() %>%
      dplyr::mutate(
        participacao = n / sum(n)
      ) %>%
    reactable(
      sortable = TRUE,
      pagination = TRUE,
      showPagination = TRUE,
      searchable = FALSE,
      highlight = TRUE,
      compact = TRUE,
      #minRows = 10,
      showPageSizeOptions = TRUE,
      #groupBy = "termo",
      defaultSorted = c("n"),
      defaultSortOrder = "desc",
      defaultColDef = colDef(align = "center"),
      columns = list(
        termo = colDef(name = "Palavra"),
        papel = colDef(name = "Atuação"),
        partido_sigla = colDef(name = "Partido",
                               filterable = TRUE),
        genero = colDef(name = "Gênero"),
        n = colDef(name = "Repetições",
                   format = colFormat(separators = TRUE)),
        total = colDef(show = FALSE),
        falante = colDef(name = "Participante",
                         filterable = TRUE),
        rank = colDef(name = "Rank de uso", show = FALSE),
        freq_termo = colDef(
          format = colFormat(percent = TRUE, digits = 2),
          header = with_tooltip(
          "Importância da palavra",
          "O quanto a palavra é frequente no vocabulário da pessoa. Quanto maior, mais essa palavra foi usada pela pessoa em seus discursos.")
                            ),
        participacao = colDef(
          name = "O quanto usou da palavra",
          format = colFormat(percent = TRUE, digits = 2),
          header = with_tooltip(
          "Frequência do uso",
          "O quanto a pessoa representa do uso total da palavra. Quanto maior, mais a pessoa fez uso da palavra em relação aos demais."
          )),
        tf = colDef(show = FALSE),
        idf = colDef(show = FALSE),
        tf_idf = colDef(show = FALSE)

      )

    )



})

## grafico_uso_termo





}


# Call --------------------------------------------------------------------


shinyApp(ui, server)

