# 18/06/2024
# Encceja 2023 - Nacional Regular e Pessoas Privadas de Liberdade
# Leitura, seleção/filtro e tratamento


# Bases de dados:
# Regulares: 81 individuais
# PPL: 115 individuais do questionario socioeonomico
# Há 43 variaveis da base cadastral que Regulares e PPL tem em comum
# Questionários socios economicos são diferentes para Regulares e PPL
# Os questionarios socios economicos para PPL são seprados das informações
# cadastrais para preservar a anonimidade das PPL

# quais dessas variaveis serão utilizadas nas analises?

# objetivo é estimar a razão de prevalencia entre o efeito e a possivel causa
# tabelas dupla e tripla entrada
# modelo de regressão logistico utilizando covariaveis
# design 

# bibliotecas -------------------------------------------------------------
library(dplyr)

# dicionário encceja 2023 -------------------------------------------------

# caminho
path_dicionario <- "dado/bruto/microdados_encceja_2023/DICIONÁRIO/Dicionário_Microdados_ENCCEJA_2023.xlsx"

# sheets do arquivo xlsx
sheets_dicionario <- openxlsx::getSheetNames(path_dicionario); sheets_dicionario  

# dicionarios de cada base de dados
data_dicionario1 <- readxl::read_xlsx(path_dicionario,
                                     sheet = sheets_dicionario[1]) %>% # nacional regular
  janitor::clean_names() 

data_dicionario2 <- readxl::read_xlsx(path_dicionario,
                                     sheet = sheets_dicionario[2]) %>% # nacional ppl
  janitor::clean_names()

data_dicionario3 <- readxl::read_xlsx(path_dicionario,
                                     sheet = sheets_dicionario[3]) %>% # questionario nacional ppl
  janitor::clean_names()

# data_dicionario4 <- readxl::read_xlsx(path_dicionario,
#                                      sheet = sheets_dicionario[4]) %>% # itens prova
#   janitor::clean_names()

# nome das variaveis
colnames1 <- data_dicionario1$dicionario_de_variaveis_encceja_2023 %>% 
  na.omit() %>% 
  as.vector(); colnames1

colnames2 <- data_dicionario2$dicionario_de_variaveis_encceja_2023 %>% 
  na.omit() %>% 
  as.vector(); colnames2

colnames3 <- data_dicionario3$x2 %>% 
  na.omit() %>% 
  as.vector(); colnames3

# colnames4 <- data_dicionario4$itens_prova_encceja_2023 %>% 
#   na.omit() %>% 
#   as.vector(); colnames4

# dados encceja 2023 ------------------------------------------------------

# caminhos
caminho1 <- "dado/bruto/microdados_encceja_2023/DADOS/MICRODADOS_ENCCEJA_2023_REG_NAC.csv"
caminho2 <- "dado/bruto/microdados_encceja_2023/DADOS/MICRODADOS_ENCCEJA_2023_PPL_NAC.csv"
caminho3 <- "dado/bruto/microdados_encceja_2023/DADOS/MICRODADOS_ENCCEJA_2023_PPL_NAC_QSE.csv"
# caminho4 <- "dado/bruto/microdados_encceja_2023/DADOS/MICRODADOS_ENCCEJA_2023_ITENS_PROVA.csv"

# datas frames
data1 <- data.table::fread(caminho1,
                           nrows = 2000
                           ) # %>% janitor::clean_names()

data2 <- data.table::fread(caminho2,
                           nrows = 2000
                           ) # %>% janitor::clean_names()

data3 <- data.table::fread(caminho3,
                           nrows = 2000,
                           ) %>% 
  janitor::clean_names()

# data4 <- data.table::fread(caminho4)


# relacionando base Nacional Regular e Nacional PPL -----------------------

col_reg_ppl <- intersect(colnames(data1), colnames(data2)); col_reg_ppl
# 43 variáveis

data_dicionario_reg_ppl <- data_dicionario1 %>% 
  filter(dicionario_de_variaveis_encceja_2023 %in% col_reg_ppl)

data1$TP_CERTIFICACAO %>% table()
data1$TP_SEXO %>% table()

data_dicionario_reg
data_dicionario_ppl

colnames3


data1 %>% 
  select(-col_reg_ppl) %>% 
  ncol()
