library(magrittr)


# requisicao --------------------------------------------------------------
url_parlamentares <- "https://legis.senado.leg.br/dadosabertos/senador/lista/atual"

base_senado <- httr::GET(url_parlamentares) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON(flatten = TRUE, simplifyDataFrame = TRUE) %>%
  tibble::as_tibble() %>%
  purrr::pluck("ListaParlamentarEmExercicio", "Parlamentares", "Parlamentar") %>%
  tibble::as_tibble() %>%
  janitor::clean_names()


# limpar ------------------------------------------------------------------

base_senado <- base_senado %>%
  dplyr::select(
    falante = identificacao_parlamentar_nome_parlamentar,
    #nome_completo = identificacao_parlamentar_nome_completo_parlamentar,
    url_foto = identificacao_parlamentar_url_foto_parlamentar
  ) %>%
  dplyr::mutate(
    falante = stringr::str_to_upper(falante)
  )


# lista de não senadores ------------------------------------------------------------
# discursos_cpi %>%
#   dplyr::filter(senado == FALSE) %>%
#   dplyr::distinct(falante) %>%
#   writexl::write_xlsx(path = "data-raw/xlsx/lista_nao_senadores_tmp.xlsx")


# base_auxiliar -----------------------------------------------------------
base_nao_senadores <- readxl::read_excel(
  "data-raw/xlsx/lista_nao_senadores_preenchida.xlsx"
  ) %>%
  janitor::clean_names()

if (janitor::compare_df_cols_same(base_senado, base_nao_senadores)) {

tabela_fotos <- dplyr::bind_rows(base_senado, base_nao_senadores)

usethis::use_data(tabela_fotos, version = 3, overwrite = TRUE)

}
