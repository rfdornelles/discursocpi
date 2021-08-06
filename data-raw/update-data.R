source("R/auxiliares-foto-token.R", encoding = "UTF-8")
source("data-raw/1-baixar_arquivos.R", encoding = "UTF-8")
source("data-raw/2-ler_arquivos.R", encoding = "UTF-8")
source("data-raw/3-limpar_base.R", encoding = "UTF-8")


# readr::with_edition(
#   1,
  readr::write_csv(discursos_cpi, "data/discursos_cpi_pandemia_limpos.csv")
# )



source("data-raw/exporta_base_tokenizada.R", encoding = "UTF-8")
source("data-raw/exporta_fotos.R", encoding = "UTF-8")

