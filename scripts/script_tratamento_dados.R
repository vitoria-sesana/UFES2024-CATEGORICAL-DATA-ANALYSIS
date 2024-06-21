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

# ENCCEJA NACIONAL REGULAR: cadastro, prova e questionário
dicRegular <- 
  readxl::read_xlsx(path_dicionario, sheet = sheets_dicionario[1]) %>% 
  janitor::clean_names() 

# # ENCCEJA NACIONAL PPL: cadastro e prova
# dicPPL <- 
#   readxl::read_xlsx(path_dicionario, sheet = sheets_dicionario[2]) %>% 
#   janitor::clean_names()
# 
# # ENCCEJA NACIONAL PPL: questionário socioeconômico
# dicPPLquestionario <- 
#   readxl::read_xlsx(path_dicionario, sheet = sheets_dicionario[3]) %>%
#   janitor::clean_names()
# 
# # Estrutura da prova
# dicProva <- 
#   readxl::read_xlsx(path_dicionario, sheet = sheets_dicionario[4]) %>% 
#    janitor::clean_names()


# Colunas em cada base ----------------------------------------------------

colRegular <- dicRegular$dicionario_de_variaveis_encceja_2023 %>% 
  na.omit() %>% 
  as.vector(); colRegular

# colPPL <- dicPPL$dicionario_de_variaveis_encceja_2023 %>% 
#   na.omit() %>% 
#   as.vector(); colPPL
# 
# colPPLquestionario <- dicPPLquestionario$x2 %>% 
#   na.omit() %>% 
#   as.vector(); colPPLquestionario
# 
# colProva <- dicProva$itens_prova_encceja_2023 %>% 
#   na.omit() %>% 
#   as.vector(); colProva

# Dados encceja 2023 ------------------------------------------------------

## Path das bases de dados
pathRegular <- "dado/bruto/microdados_encceja_2023/DADOS/MICRODADOS_ENCCEJA_2023_REG_NAC.csv"
# pathPPL <- "dado/bruto/microdados_encceja_2023/DADOS/MICRODADOS_ENCCEJA_2023_PPL_NAC.csv"
# pathPPLquestionario <- "dado/bruto/microdados_encceja_2023/DADOS/MICRODADOS_ENCCEJA_2023_PPL_NAC_QSE.csv"
# pathProva <- "dado/bruto/microdados_encceja_2023/DADOS/MICRODADOS_ENCCEJA_2023_ITENS_PROVA.csv"
 
## Bases de dados
dataRegular <- data.table::fread(pathRegular,
                           nrows = 200000
                           ) %>% 
  janitor::clean_names() %>% 
  mutate(modalidade_encceja = "Nacional Regular")
 
# dataPPL <- data.table::fread(pathPPL,
#                            nrows = 2000
#                            ) %>%
#   janitor::clean_names() %>% 
#   mutate(modalidade_encceja = "Nacional PPL")
# 
# dataPPLquestionario <- data.table::fread(pathPPLquestionario,
#                            nrows = 2000,
#                            ) %>% 
#   janitor::clean_names()
# 
# # relacionando base Nacional Regular e Nacional PPL -----------------------
# 
# ## divisoes da base de dados cadastrais e prova
# colPPL[!colPPL %in% colnames(dataPPL)]
# 
# ## Variáveis comuns entre Regulares e PPL
# colComunsRegularPPL <- 
#   intersect(x = colnames(dataPPL),
#             y = colnames(dataRegular))
# 
# ## juntando as bases
# 
# dataRegularPPL <- bind_rows(dataPPL, dataRegular) %>% 
#   select(c(colComunsRegularPPL))



# resultado certificação --------------------------------------------------

dataRegular %>% 
  filter(
    nu_nota_redacao >= 5 & in_aprovado_lc == 0
  ) %>% nrow()


dataRegular %>% 
  filter(
    in_prova_ch == 1 & in_aprovado_ch ==  1
  ) %>% nrow()

dataRegularTratado <- dataRegular %>% 
  mutate(
    in_aprovado_ch =
      data.table::fifelse(is.na(in_aprovado_ch), 0, in_aprovado_ch),
    in_aprovado_cn =
      data.table::fifelse(is.na(in_aprovado_cn), 0, in_aprovado_cn),
    in_aprovado_lc =
      data.table::fifelse(is.na(in_aprovado_lc), 0, in_aprovado_lc),
    in_aprovado_mt =
      data.table::fifelse(is.na(in_aprovado_mt), 0, in_aprovado_mt),
    
    soma_provas = 
      in_prova_ch +
      in_prova_lc +
      in_prova_lc +
      in_prova_mt
      ,
    soma_aprovacao = 
      in_aprovado_lc +
      in_aprovado_ch +
      in_aprovado_cn +
      in_aprovado_mt,
    
    nu_nota_redacao = 
      data.table::fifelse(is.na(nu_nota_redacao), 0, nu_nota_redacao),
    
    certificado = 
      data.table::fifelse((soma_provas == soma_aprovacao) & 
                            (nu_nota_redacao >= 5),
                          1, 0)
  )

