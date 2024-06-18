# 21/05/2024
# Trabalho Final de Análise de Dados Categóricos
# Dados: Microdados educação
# Análise Descritiva

require(data.table)

dicionario <- readxl::read_xlsx(path =  "bases/microdados_censo_escolar_2023/Anexos/anexo1_dicionario/dicionario_dados.xlsx")

df1 <- data.table::fread(input = "bases/microdados_censo_escolar_2023/dados/microdados_ed_basica_2023.csv")

df2 <- data.table::fread(input = "bases/microdados_censo_escolar_2023/dados/suplemento_cursos_tecnicos_2023.csv")
