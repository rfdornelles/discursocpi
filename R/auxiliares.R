
#' Foto da pessoa selecionada
#'
#' @param falante
#'

#' @export
#'

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
