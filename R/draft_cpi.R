# 2441
# CPIPANDEMIA

# https://legis.senado.leg.br/dadosabertos/comissao/CPIPANDEMIA
library(magrittr)


# progress ----------------------------------------------------------------
progressr::handlers(list(
  progressr::handler_progress(
    format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
    width    = 60,
    complete = "+"
  )))


# url ---------------------------------------------------------------------


url_base <- "https://legis.senado.leg.br/dadosabertos/"


# funções -----------------------------------------------------------------


baixar_reunioes_cpi <- function(mes,
                                query = list("colegiado" = "CPIPANDEMIA")) {

  mes <- as.character(mes)
  mes <- stringr::str_pad(string = mes, width = "2", pad = "0")

   end_agenda <- "agendareuniao/"

  datas <- glue::glue("2021{mes}01/2021{mes}31")

  # requisição

  httr::GET(
    url = glue::glue("{url_base}{end_agenda}{datas}"),
    query = query,
    httr::accept_xml(),
    httr::write_disk(
      path = glue::glue("data-raw/reunioes_{mes}.xml"),
      overwrite = TRUE
    ),
    httr::progress()
  )


}

# ler códigos -------------------------------------------------------------

# baixar os códigos de reunião das CPI
ler_codigos_cpi <- function(arquivo) {

xml2::read_xml(arquivo) %>%
  xml2::xml_find_all("//reuniao/codigo") %>%
  xml2::xml_text()
}


# baixar discursos --------------------------------------------------------

# baixar os discursos
baixar_discursos <- function(reuniao, prog, force = FALSE) {

  destino <- glue::glue("data-raw/discursos_{reuniao}.html")

  if (file.exists(destino)) {

    tamanho <- fs::file_info(destino)$size %>%
      as.numeric()

    if (tamanho > 1000 & force == TRUE) {

      return()
    }
  }

  # barra de progresso, se houver
  if (!missing(prog)) {
    prog()
  }

  # criar o o link
  end_discursos <- "reuniaocomissao/notas/"

  url_final <- glue::glue("{url_base}{end_discursos}{reuniao}")

  # requisição intermediária para obter a url da página com as notas
    r_intermediaria <- httr::GET(url = url_final,
                               httr::accept_xml(),
                               httr::progress())

  # obter o link
  link_notas <- r_intermediaria %>%
    httr::content() %>%
    xml2::xml_child("UrlNotasTaquigraficas") %>%
    xml2::xml_text()

  # com o link, salvar a página com as notas em si
  link_notas %>%
    httr::GET(
      httr::accept_xml(),
      httr::write_disk(
        path = arquivo_destino,
        overwrite = TRUE
      ),
      httr::progress()
    )

Sys.sleep(1)

}


# iterar ------------------------------------------------------------------

# baixar lista de reuniões
purrr::walk(.x = 4:7,
            .f = baixar_reunioes_cpi)

# lista com arquivos .xml das reuniões
lista_xml <- fs::dir_info("data-raw/", regexp = ".xml$") %>%
  purrr::pluck("path")

# baixar os discursos

lista_discursos_para_baixar <- purrr::map(
  .x = lista_xml,
  .f = ler_codigos_cpi) %>%
  purrr::flatten_chr()

progressr::with_progress({
  p <- progressr::progressor(length(lista_discursos_para_baixar))

lista_discursos_para_baixar %>%
  purrr::walk(
    purrr::possibly(baixar_discursos, otherwise = NULL),
    prog = p)

})

# teste
lista_html <- fs::dir_info("data-raw", regex = "\\.html$") %>%
  dplyr::select(path, size) %>%
  dplyr::arrange(dplyr::desc(size)) %>%
  dplyr::pull(path)


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
