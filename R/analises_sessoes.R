# quantidade de sessões
discursos_cpi %>%
  dplyr::distinct(numero_sessao) %>%
  nrow()

# datas das sessões
dias_sessoes <- discursos_cpi %>%
  dplyr::distinct(data_sessao) %>%
  dplyr::mutate(
    mes = lubridate::month(data_sessao),
    dia_semana = lubridate::wday(data_sessao, label = TRUE, abbr = FALSE)
  )

dias_sessoes %>%
  dplyr::count(dia_semana)

# duracao das sessões
discursos_cpi %>%
  dplyr::select(data_sessao, horario_duracao) %>%
  dplyr::group_by(data_sessao) %>%
  dplyr::summarise(total = sum(horario_duracao, na.rm = TRUE)) %>%
  dplyr::mutate(
    total = as.numeric(total)/60/60,
    dia_semana = lubridate::wday(data_sessao, label = TRUE, abbr = FALSE)
  ) %>%
  dplyr::group_by(dia_semana) %>%
  dplyr::summarise(
    minimo = min(total),
    maximo = max(total),
    media = mean(total)
  )

# quantidades de falas nas sessões
discursos_cpi %>%
  dplyr::mutate(
    dia_semana = lubridate::wday(data_sessao, label = TRUE, abbr = FALSE)
  ) %>%
