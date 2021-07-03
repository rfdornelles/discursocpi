## Rodrigo Dornelles - Fri Jul 02 22:45:40 2021
##
## Objetivo:


# Pacotes -----------------------------------------------------------

library(magrittr)
library(tibble)


# montar a base -----------------------------------------------------------
base <- purrr::map_dfr(
  lista_html,
  limpar_discursos_sessao
) %>%
  dplyr::arrange(numero_sessao)


# expressões a serem removidas --------------------------------------------
# do campo falante

remover <- c("Para questão de ordem\\.", "Pela ordem\\.",
             "Fora do microfone\\.", "Bloco Parlamentar.*\\/",
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


