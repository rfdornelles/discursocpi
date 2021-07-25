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

# tokenizar ---------------------------------------------------------------

tokenizar_base <- function(base) {

  base_tokenizada <- base %>%
    tidytext::unnest_tokens(termo, texto) %>%
    # remover acentos
    dplyr::mutate(termo = abjutils::rm_accent(termo))

  # remover letras soltas
  base_tokenizada <- base_tokenizada %>%
    dplyr::mutate(tamanho = stringr::str_length(termo)) %>%
    dplyr::arrange(tamanho) %>%
    dplyr::filter(tamanho > 1) %>%
    dplyr::select(-tamanho)

  # stopwords
  stopwords <- c(tm::stopwords("pt"), "v", "exa", "sra", "sr", "ai", "sa",
                 "dr", "dra", "la", "aqui", "senhor", "senhora",
                 "entao", "neste", "nesta", "nesse", "nessa", "nisso",
                 "senador", "senadora", "excelencia", "senhoria",
                 "porque", "por", "que")

  # remover stopwords
  base_tokenizada <- base_tokenizada %>%
    dplyr::filter(
      !termo %in% abjutils::rm_accent(stopwords)
    )

  return(base_tokenizada)
}



