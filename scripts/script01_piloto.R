# Matéria:  ANÁLISE DE DADOS CATEGORIZADOS
# Objetivo: Encontrar uma base de dados para a realização do estudo


# base 1: datasus ---------------------------------------------------------

# https://www.scielo.br/j/csp/a/gdJXqcrW5PPDHX8rwPDYL7F/
# devtools::install_github("rfsaldanha/microdatasus")
# library(microdatasus)
# sinasc_sp_22 <- microdatasus::fetch_datasus(year_start = 2022, year_end = 2022,
#                                            information_system = "SINASC",
#                                            uf = "SP")
# write.csv(sinasc_sp_22, "sinasc_sp_22.csv")

base <- read.csv("bases/sinasc_sp_22.csv")


# base 2: -----------------------------------------------------------------

