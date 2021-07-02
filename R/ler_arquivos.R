# raspar arquivo ----------------------------------------------------------

data_sessao <- xml2::read_html("data-raw/discursos_10068.html", encoding = "UTF-8") %>%
  xml2::xml_find_all(
    "//div[@class = 'escriba-jq']/*/div[@style = 'width:85%; float:left;']"
  ) %>%
  xml2::xml_text() %>%
  stringr::str_trim()

data <- data_sessao %>%
  stringr::str_extract("\\d{2,2}/\\d{2,2}/\\d{4,4}") %>%
  lubridate::dmy()

sessao <- data_sessao %>%
  stringr::str_extract("(?<= - )\\d{1,3}")


discursos <- xml2::read_html("data-raw/discursos_10068.html", encoding = "UTF-8") %>%
  xml2::xml_find_all("//table[@id = 'tabelaQuartos']") %>%
  rvest::html_table() %>%
  purrr::pluck(1) %>%
  janitor::clean_names() %>%
  dplyr::rename(texto = texto_com_revisao)

discursos_limpo <- discursos %>%
  dplyr::filter(texto != "") %>%
  dplyr::mutate(
    data = data,
    sessao = sessao,
    revisado = dplyr::if_else(stringr::str_detect(horario, "  R$"),
                              TRUE, FALSE,NA),
    horario = stringr::str_extract(horario, "\\d{2,2}\\:\\d{2,2}")
  )

discursos_limpo <- discursos_limpo %>%
  dplyr::mutate(
    orador = stringr::str_extract(texto, pattern = ".*(?<=\\))"),
  )

discursos_limpo <- discursos_limpo %>%
  dplyr::mutate(
    horario_inicio = lubridate::ymd_hm(paste(data, horario)),
    horario_fim = dplyr::lead(horario),
    horario_fim = lubridate::ymd_hm(paste(data, horario_fim)),
    horario_duracao =  horario_fim - horario_inicio
  )

discursos_limpo %>%
  dplyr::select(texto) %>%
  dplyr::mutate(
    conta = stringr::str_count(texto, "(O|A) (SR|SRA)\\. [A-Z]+")
  )


discursos_limpo %>%
  dplyr::select(horario, texto) %>%
  # (O|A) (SR|SRA)\\.
  tidyr::separate_rows(texto, sep = "(O|A) (SR|SRA)\\.") %>%
  tidyr::separate(col = texto, into = c("orador", "texto_2"),
                  sep = " - ", remove = FALSE) %>% View()
