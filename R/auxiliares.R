
# options -----------------------------------------------------------------
options(
  reactable.theme =
    reactable::reactableTheme(
      borderColor = "#dfe2e5",
      stripedColor = "#f6f8fa",
      highlightColor = "#f0f5f9",
      cellPadding = "8px 12px",
      style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
      searchInputStyle = list(width = "100%")
    )
)


# desenha nuvem palavras --------------------------------------------------

desenhar_nuvem <- function(texto, qnt = 40, corMin = "#abbf8f",
                           corMax = "#07e31d", angular = 0) {

  # receber o texto já "tratado" com a função
  set.seed(837)

  # angular = 0, não faz angulação
  # angular = 1, faz a angulação

  texto %>%
    dplyr::mutate(
      angle = angular * (90 * sample(c(0, 1), dplyr::n(), replace = TRUE,
                                     prob = c(80, 20)))) %>%
    head(qnt) %>%
    ggplot2::ggplot(ggplot2::aes(label = termo, size = n,
                                 color = n, angle = angle)) +
    ggwordcloud::geom_text_wordcloud(rm_outside = TRUE) +
    ggplot2::scale_size_area(max_size = 15,
                             trans = ggwordcloud::power_trans(1/.7)) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_gradient(low = corMin, high = corMax)

}

# foto --------------------------------------------------------------------


retorna_foto <- function(falante) {

  falante <- stringr::str_to_upper(falante)

  foto_desconhecido <- "https://queridojeito.com/wp-content/uploads/2016/09/Autor-Desconhecido.jpg"

  endereco_foto <- tabela_fotos %>%
  dplyr::filter(falante == {{falante}}) %>%
    dplyr::pull(url_foto)

  if (length(endereco_foto) != 1) {

    endereco_foto <- foto_desconhecido

  }

  return(endereco_foto)

}


# graficos_tempo_de_fala --------------------------------------------------

graficos_tempo_de_fala <- function (seletor_grafico_tempo_fala) {

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

}



# analisar discursos ------------------------------------------------------

# ranking -----------------------------------------------------------------

ranking_palavras_discurso <-function(
  base = base_tokenizada,
  documento = "falante",
  ranking = 5,
  tf_idf = FALSE) {

  # contar cada palavra em cada documento
  tabela_palavras <- base %>%
    dplyr::select({{documento}}, termo) %>%
    dplyr::count(.data[[documento]], termo, sort = TRUE)

  # contagem geral de palavras

  tabela_total_palavras <- tabela_palavras %>%
    dplyr::group_by(.data[[documento]]) %>%
    dplyr::summarise(total = dplyr::n())

  # juntar as bases

  tabela_palavras <- dplyr::left_join(
    x = tabela_palavras,
    y = tabela_total_palavras)

  if (tf_idf == FALSE) {

  # ranking frequência de palavras

  frequencia_rank <- tabela_palavras %>%
    dplyr::arrange(-n) %>%
    dplyr::group_by(.data[[documento]]) %>%
    dplyr::mutate(
      rank = dplyr::row_number(),
      freq_termo = n / total
    ) %>%
    dplyr::ungroup()


  frequencia_rank %>%
    dplyr::group_by(.data[[documento]]) %>%
    dplyr::top_n(wt = n, n = ranking) %>%
    dplyr::arrange(.data[[documento]], rank) %>%
    dplyr::ungroup()

  } else {

    tabela_tf_idf <- tabela_palavras %>%
      tidytext::bind_tf_idf(
        term = termo,
        document = .data[[documento]],
        n = n
      )

    # quanto maior o tf_idf maior a relevância do termo

    tabela_tf_idf %>%
      dplyr::select(-total) %>%
      dplyr::group_by(.data[[documento]]) %>%
      dplyr::arrange(.data[[documento]], -tf_idf) %>%
      dplyr::slice_max(order_by = tf_idf, n = ranking) %>%
      dplyr::ungroup()

  }



}

