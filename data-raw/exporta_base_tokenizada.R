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
    numero_sessao:data_sessao, partido_sigla, genero, senado, papel
  )

base_tokenizada <- tokenizar_base(base)

# exportar ----------------------------------------------------------------

usethis::use_data(base_tokenizada, overwrite = TRUE, version = 3)
