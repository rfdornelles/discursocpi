## Rodrigo Dornelles - Fri Jul 02 17:11:13 2021
##
## Objetivo: Analisar discursos na CPI da Pandemia


# Pacotes -----------------------------------------------------------

library(magrittr)
library(tibble)

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


# baixar_reunioes ---------------------------------------------------------
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

# # teste
# lista_html <- fs::dir_info("data-raw", regex = "\\.html$") %>%
#   dplyr::select(path, size) %>%
#   dplyr::arrange(dplyr::desc(size)) %>%
#   dplyr::pull(path)


