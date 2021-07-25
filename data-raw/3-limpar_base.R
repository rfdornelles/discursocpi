## Rodrigo Dornelles - Fri Jul 02 22:45:40 2021
##
## Objetivo: limpeza dos discursos e nome dos oradores


# Pacotes -----------------------------------------------------------

library(magrittr)
library(tibble)


# carregar a base suja ----------------------------------------------------

base <- readr::read_rds("data-raw/rds/base_suja.rds")

# expressões a serem removidas --------------------------------------------
# do campo falante

remover <- c("Para questão de ordem\\.", "Pela ordem\\.",
             "Fora do microfone\\.", "Bloco Parlamentar.*\\/",
             "Bloco",
             "Para responder questão de ordem\\.", "Por videoconferência\\.",
             "Para interpelar\\.", "Para expor\\.", "PRESIDENTE ",
             "Para depor\\.", "Para breve comunicação\\.",
             "Fala da Presidência\\.", "Para comunicação inadiável",
             "Para explicação pessoal", "Como Relator",
             "Fazendo soar a campainha", "Para contraditar",
             "GEN\\. ",
             "Para discutir", "[[:upper:]]*.-.[[:upper:]]{2,2}",
             "[[:punct:]]*",
             "\\.") %>%
  # montar a regex
  stringr::str_c(collapse = "|")

base_limpa <- base %>%
  dplyr::mutate(
    # características da fala
    pela_ordem = stringr::str_detect(falante, "Pela ordem"),
    questao_ordem = stringr::str_detect(falante, "Para questão de ordem"),
    fora_microfone = stringr::str_detect(falante, "Fora do microfone."),
    responder_qordem = stringr::str_detect(falante, "Para responder questão de ordem."),
    por_videoconferencia = stringr::str_detect(falante, "Por videoconferência."),
    para_interpelar = stringr::str_detect(falante, "Para interpelar."),
    para_expor = stringr::str_detect(falante, "Para expor"),
    para_depor = stringr::str_detect(falante, "Para depor."),
    bloco_parlamentar = stringr::str_extract(falante, "Bloco Parlamentar.*\\/"),
    como_presidente = stringr::str_detect(falante, "PRESIDENTE "),
    # separar o partido
    partido = stringr::str_extract(falante, "[[:upper:]]*.-.[[:upper:]]{2,2}"),
    # remover as expressões da lista
    falante = stringr::str_remove_all(falante, remover),
    # padronizar o nome da pessoa que está falando
    falante = stringr::str_trim(falante),
    falante = stringr::str_to_upper(falante),
    # limpar bloco e o texto
    bloco_parlamentar = stringr::str_remove(bloco_parlamentar, "\\/"),
    texto = stringr::str_trim(texto)
  )


# separar sigla do partido da UF ------------------------------------------

base_limpa <- base_limpa %>%
  tidyr::separate(col = partido,
                  into = c("partido_sigla", "partido_uf"),
                  sep = " - ")


# corrigir duplicados -----------------------------------------------------

base_limpa <- base_limpa %>%
  dplyr::mutate(
    falante = dplyr::case_when(
    falante == "MARCELO ANTÔNIO CARTAXO QUEIROGA LOPES" ~ "MARCELO QUEIROGA",
    falante == "MARCELLUS JOSÉ BARROSO CAMPÊLO" ~ "MARCELLUS CAMPELO",
    falante == "FRANCIELI FONTANA SUTILE FANTINATO" ~ "FRANCIELI FONTANA SUTILE TARDETTI FANTINATO",
    TRUE ~ falante
    )
  )


# acrescentar dados -------------------------------------------------------

# acrescentar gênero
base_limpa <- base_limpa %>%
  dplyr::mutate(
    genero = genderBR::get_gender(abjutils::rm_accent(falante)),
    genero = dplyr::if_else(genero == "Female", "Feminino",
                            "Masculino",
                            # os nomes NA eram de homens
                            "Masculino")
  )

# se é ou não parlamentar do Senado
base_limpa <- base_limpa %>%
  dplyr::mutate(
    # padronizar Osmar Terra que aparece às vezes com partido
    partido_sigla = dplyr::if_else(falante %in% c("OSMAR TERRA",
                                                  "LUIS MIRANDA"),
                                   NA_character_,
                                   partido_sigla),
    senado = dplyr::if_else(is.na(partido_sigla), FALSE, TRUE, FALSE)
  )

# papel na Comissão

base_limpa <- base_limpa %>%
  dplyr::mutate(
    papel = dplyr::case_when(
      senado == FALSE ~ "Depoente/Convidado",
    #  como_presidente == TRUE ~ "Presidindo Sessão",
      senado == TRUE ~ "Senador/a",
      TRUE ~ NA_character_
    )
  )


# corrigir partidos -------------------------------------------------------

base_limpa <- base_limpa %>%
  dplyr::mutate(
    partido_sigla = dplyr::if_else(
      is.na(partido_sigla),
      "Sem partido/Não se aplica",
      partido_sigla
    )
  )

# exportar ----------------------------------------------------------------
# salvar com nome melhor
discursos_cpi <- base_limpa

usethis::use_data(discursos_cpi, overwrite = TRUE,
                  version = 3, compress = "gzip")


# exporta xlsx -------------------------------------------------------------

writexl::write_xlsx(discursos_cpi, "data/discursos_cpi_pandemia_limpos.xlsx")


# exporta csv -------------------------------------------------------------

readr::with_edition(
  1,
  readr::write_csv(discrusos_cpi, "data/discursos_cpi_pandemia_limpos.csv")
)
