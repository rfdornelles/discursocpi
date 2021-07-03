library(magrittr)

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
