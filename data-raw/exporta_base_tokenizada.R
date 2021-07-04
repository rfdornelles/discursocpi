## Rodrigo Dornelles - Sun Jul 04 13:55:44 2021
##
## Objetivo: exportar base tokenizada, já limpa de stopwrds

## Referência:
## https://www.tidytextmining.com/twitter.html
# https://www.tidytextmining.com/tfidf.html

# Pacotes -----------------------------------------------------------

library(magrittr)
library(tibble)


# carregar base -----------------------------------------------------------
data("discursos_cpi")

base <- discursos_cpi %>%
  # selecionar colunas pertinentes
  dplyr::select(
    numero_sessao:texto, partido_sigla, genero, senado
  )


# limpeza  ----------------------------------------------------------------

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


# exportar ----------------------------------------------------------------

usethis::use_data(base_tokenizada, overwrite = TRUE, version = 3)
