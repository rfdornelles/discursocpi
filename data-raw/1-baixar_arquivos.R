## Rodrigo Dornelles - Fri Jul 02 17:11:13 2021
##
## Objetivo: Analisar discursos na CPI da Pandemia


# Pacotes -----------------------------------------------------------

library(magrittr)
library(tibble)

# define a progress bsr ----------------------------------------------------
progressr::handlers(list(
  progressr::handler_progress(
    format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
    width    = 60,
    complete = "+"
  )))


# url ---------------------------------------------------------------------


url_base <- "https://legis.senado.leg.br/dadosabertos/"


# criar pastas-------------------------------------------------------------

usethis::use_directory("data-raw/xml")
usethis::use_directory("data-raw/html")

# baixar_reunioes ---------------------------------------------------------
baixar_reunioes_cpi <- function(mes,
                                query = list("colegiado" = "CPIPANDEMIA")) {

  # define o mês como string de 2 dígitos
  mes <- as.character(mes)
  mes <- stringr::str_pad(string = mes, width = "2", pad = "0")

  # endpoint da API para as reuniões de uma comissão
  end_agenda <- "agendareuniao/"

  # intervalo de tempo, do dia 01 ao 31 (fevereiro que lute)
  datas <- glue::glue("2021{mes}01/2021{mes}31")

  # arquivo destino
  destino <- glue::glue("data-raw/xml/reunioes_{mes}.xml")

  # tempfile para receber o arquivo
  tmp <- tempfile(fileext = ".xml")

  # requisição

  req <- httr::GET(
    url = glue::glue("{url_base}{end_agenda}{datas}"), # junta tudo
    query = query, # será normalmente o padrão: colegiado da CPIPANDEMIA
    httr::accept_xml(), # pedir .xml
    httr::write_disk(
      path = tmp, # salvar no arquivo temporário
      overwrite = TRUE # sobrescrever, embora na prática vai ser raro
    )
  )

  # checar se o arquivo não falhou ao baixar, se não veio em branco ou outro erro
  if (!is.null(purrr::safely(xml2::read_xml)(tmp)$result) & req$status_code == 200) {
 # se deu certo, salvar em disco
    fs::file_copy(path = tmp, new_path = destino, overwrite = TRUE)

    print(glue::glue("Baixadas as sessões do mês {mes}."))

# se der errado avisa
  } else {

    print(glue::glue("Erro ao baixar sessões do mês {mes}."))
  }


}

# ler códigos -------------------------------------------------------------

# função auxiliar
# baixar os códigos de reunião das CPI
ler_codigos_cpi <- function(arquivo) {

  print(paste("Lendo arquivo", arquivo))

xml2::read_xml(arquivo) %>%
  xml2::xml_find_all("//reuniao/codigo") %>%
  xml2::xml_text()
}


# baixar discursos --------------------------------------------------------

# baixar os discursos
baixar_discursos <- function(reuniao, prog, force = FALSE) {

  arquivo_destino <- glue::glue("data-raw/html/discursos_{reuniao}.html")

  # checar se o arquivo existe
  if (file.exists(arquivo_destino)) {
   # se sim, verificar o tamanho
    tamanho <- fs::file_info(arquivo_destino)$size %>%
      as.numeric()

    # se for muito pequano provavelmente é vazio
    # se force não for TRUE, não sobrescrever e pular para o próximo
    if (tamanho > 1000 & force != TRUE) {

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

  print(paste("Lendo reunião", reuniao))

  # requisição intermediária para obter a url da página com as notas
    r_intermediaria <- httr::GET(url = url_final,
                               httr::accept_xml())

  # obter o link
  link_notas <- r_intermediaria %>%
    httr::content() %>%
    xml2::xml_child("UrlNotasTaquigraficas") %>%
    xml2::xml_text()

  # com o link, salvar a página com as notas em si

  # criar arquivo temporario
  temporario <- tempfile(fileext = ".html")

  # tentar o download
  link_notas %>%
    httr::GET(
      httr::accept_xml(),
      httr::write_disk(
        path = temporario,
        overwrite = TRUE
      )
    )

  # checar se o arquivo não é vazio
  if (fs::file_size(temporario, fail = FALSE) > 10000) {

    fs::file_copy(temporario, arquivo_destino, overwrite = force)
  }


Sys.sleep(1)

}


# iterar ------------------------------------------------------------------

# baixar lista de reuniões
# do mês de abril (quando instalou) até o presente mês
purrr::walk(.x = 4:lubridate::month(Sys.Date()),
            .f = baixar_reunioes_cpi)

# lista com arquivos .xml das reuniões
lista_xml <- fs::dir_info("data-raw/xml/", regexp = ".xml$") %>%
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
    prog = p, force = FALSE)

})

# forçar baixar de novo os últimos discursos
lista_discursos_para_baixar %>%
  tail(5) %>%
  purrr::walk(
    purrr::possibly(baixar_discursos, otherwise = NULL),
    prog = p, force = TRUE)

