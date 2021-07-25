
source("data-raw/1-baixar_arquivos.R", encoding = "UTF-8")
source("data-raw/2-ler_arquivos.R", encoding = "UTF-8")
source("data-raw/3-limpar_base.R", encoding = "UTF-8")


# readr::with_edition(
#   1,
  readr::write_csv(discrusos_cpi, "data/discursos_cpi_pandemia_limpos.csv")
#)


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

source("data-raw/exporta_base_tokenizada.R", encoding = "UTF-8")
source("data-raw/exporta_fotos.R", encoding = "UTF-8")

