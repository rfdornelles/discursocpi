## Rodrigo Dornelles - Fri Jul 02 17:18:57 2021
##
## Objetivo: Ler os arquivos baixados da CPI


# Pacotes -----------------------------------------------------------

library(magrittr)
library(tibble)


# lista de arquivos -------------------------------------------------------

lista_html <- fs::dir_info("data-raw", regex = "\\.html$") %>%
  dplyr::pull(path)


# função dados da sessões -------------------------------------------------

ler_infos_sessao <- function(arquivo) {

  # ler o html do arquivo, com encoding correto
  html <- xml2::read_html(arquivo, encoding = "UTF-8")

  # ler as informações do cabeçalho da página
  infos_data_sessao <- html %>%
    xml2::xml_find_all(
      "//div[@class = 'escriba-jq']/*/div[@style = 'width:85%; float:left;']"
    ) %>%
    xml2::xml_text() %>%
    stringr::str_trim()

  # data da sessão
  data <- infos_data_sessao %>%
    stringr::str_extract("\\d{2,2}/\\d{2,2}/\\d{4,4}")


  # número da sessão
  sessao <- infos_data_sessao %>%
    stringr::str_extract("(?<= - )\\d{1,3}") %>%
    stringr::str_pad(width = 2, side = "left", pad = 0)

  # pegar os dados do rodapé

  # horário da abertura
  abertura <- xml2::read_html(arquivo, encoding = "UTF-8") %>%
    xml2::xml_find_all("//div[@class = 'anotacaoAberturaStyle']/span") %>%
    xml2::xml_text() %>%
    data.frame() %>%
    dplyr::filter(stringr::str_detect(., pattern = "^\\(Iniciada")) %>%
    as.character() %>%
    stringr::str_extract("(?<=Iniciada..s) .*") %>%
    stringr::str_extract_all("\\d+.*,") %>%
    stringr::str_extract_all("\\d+", simplify = T) %>%
    stringr::str_c(collapse = ":")

  # colocar em formato date
  abertura_completa <- paste(data, abertura) %>%
    lubridate::dmy_hm()

  # dados do encerramento
  encerramento <- xml2::read_html(arquivo, encoding = "UTF-8") %>%
    xml2::xml_find_all("//div[@class = 'anotacaoAberturaStyle']/span") %>%
    xml2::xml_text() %>%
    stringr::str_c(collapse = "") %>%
    stringr::str_extract("(?<=encerrada..s) .*") %>%
    stringr::str_extract_all("\\d+", simplify = T) %>%
    stringr::str_c(collapse = ":")

  # colocar no formato date
  encerramento_completo <- paste(data, encerramento) %>%
    lubridate::dmy_hm()

  # gerar saída
  tibble::tibble_row(
    numero_sessao = sessao,
    data_sessao = lubridate::dmy(data),
    horario_abertura_sessao = abertura_completa,
    horario_encerramento_sessao = encerramento_completo
  )

}


# função limpar a sessão --------------------------------------------------

limpar_discursos_sessao <- function(arquivo) {

  # ler o html do arquivo, com encoding correto
  html <- xml2::read_html(arquivo, encoding = "UTF-8")

}











discursos <- xml2::read_html(arquivo, encoding = "UTF-8") %>%
  xml2::xml_find_all("//table[@id = 'tabelaQuartos']") %>%
  rvest::html_table() %>%
  purrr::pluck(1) %>%
  janitor::clean_names() %>%
  dplyr::rename(texto = texto_com_revisao)

discursos_limpo <- discursos %>%
  tidyr::separate_rows(texto, sep = "(O|A) (SR|SRA)\\.") %>%
  dplyr::filter(texto != "") %>%
  dplyr::mutate(
    texto = stringr::str_trim(texto),
    data = data,
    sessao = sessao,
    revisado = dplyr::if_else(stringr::str_detect(horario, "  R$"),
                              TRUE, FALSE,NA),
    horario = stringr::str_extract(horario, "\\d{2,2}\\:\\d{2,2}")
  )

discursos_limpo <- discursos_limpo %>%
  dplyr::mutate(
    falante = stringr::str_extract(texto, pattern = "^[[:upper:]]{2,}.* – "),
    texto = stringr::str_remove(texto, "^[[:upper:]]{2,}.* – ")
  ) %>%
  tidyr::fill(falante)

discursos_limpo %>%
  dplyr::mutate(
    texto = stringr::str_remove_all(texto, falante)
  ) %>% View()

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
  tidyr::separate_rows(texto, sep = "(O|A) (SR|SRA)\\.") %>%
  tidyr::separate(col = texto, into = c("orador", "texto_2"),
                  sep = " - ", remove = FALSE) %>% View()

#######
purrr::map_dfr(
  lista_html,
  ler_infos_sessao
) %>%
  dplyr::arrange(numero_sessao)

