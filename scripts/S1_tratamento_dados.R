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
path_dicionario <- "dado/bruto/DICIONÁRIO/Dicionário_Microdados_ENCCEJA_2023.xlsx"

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
pathRegular <- "dado/bruto/DADOS/MICRODADOS_ENCCEJA_2023_REG_NAC.csv"
# pathPPL <- "dado/bruto/microdados_encceja_2023/DADOS/MICRODADOS_ENCCEJA_2023_PPL_NAC.csv"
# pathPPLquestionario <- "dado/bruto/microdados_encceja_2023/DADOS/MICRODADOS_ENCCEJA_2023_PPL_NAC_QSE.csv"
# pathProva <- "dado/bruto/microdados_encceja_2023/DADOS/MICRODADOS_ENCCEJA_2023_ITENS_PROVA.csv"
 
## Bases de dados
dataRegular <- data.table::fread(pathRegular) %>% 
  janitor::clean_names() %>% 
  mutate(modalidade_encceja = "Nacional Regular")

dataRegular %>% 
  group_by() %>% 
  
## Amostragem
 
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

# quantidade de pessoas que não fizeram a redação (NA)
dataRegular %>% 
  filter(
    is.na(nu_nota_redacao) 
  ) %>% nrow()

# quantidade de pessoas que fizeram a redação 
dataRegular %>% 
  filter(
    !is.na(nu_nota_redacao) 
  ) %>% nrow()

# quantidade de pessoas que não fizeram a redação (NA) e não solicitaram a prova
# de linguagens 
dataRegular %>% 
  filter(
    is.na(nu_nota_redacao) & in_prova_lc == 0
  ) %>% nrow()

# quantidade de pessoas que não fizeram a redação (NA) e solicitaram a prova
# de linguagens 
dataRegular %>% 
  filter(
    is.na(nu_nota_redacao) & in_prova_lc == 1
  ) %>% nrow()

# quantidade de pessoas que nao solicitaram linguagens e redacao
dataRegular %>% 
  filter(
    in_prova_lc == 0
  ) %>% nrow()

# quantidade de pessoas que solicitaram linguagens e redacao
dataRegular %>% 
  filter(
    in_prova_lc == 1
  ) %>% nrow()


# quantidade de aprovados em linguagens
dataRegular %>% 
  filter(
    nu_nota_lc >= 100
  ) %>% nrow()

# quantidade de aprovados em linguagens e redacao
dataRegular %>% 
  filter(
    in_aprovado_lc == 1
  ) %>% nrow()

# quantidade de aprovados em linguagens mas nao em redacao
dataRegular %>% 
  filter(
    nu_nota_lc >= 100 & nu_nota_redacao < 5
  ) %>% nrow()

# quantidade de não aprovados em redacao
dataRegular %>% 
  filter(
    nu_nota_redacao < 5
  ) %>% nrow()


# quantidade de aprovados em linguagens mas nao em redacao
dataRegular %>% 
  filter(
    nu_nota_lc >= 100 & nu_nota_redacao < 5
  ) %>% nrow()



# dado final --------------------------------------------------------------

dataRegularTratado <- dataRegular %>% 
  mutate(
    t_in_aprovado_ch = tidyr::replace_na(in_aprovado_ch, 0),
    t_in_aprovado_cn = tidyr::replace_na(in_aprovado_cn, 0),
    t_in_aprovado_lc = tidyr::replace_na(in_aprovado_lc, 0),
    t_in_aprovado_mt = tidyr::replace_na(in_aprovado_mt, 0),
    
    t_solicitacoes_totais = 
      in_prova_lc +
      in_prova_ch +
      in_prova_cn +
      in_prova_mt
      ,
    t_aprovacoes_totais = 
      t_in_aprovado_lc +
      t_in_aprovado_ch +
      t_in_aprovado_cn +
      t_in_aprovado_mt,
    
    t_resposta = 
      data.table::fifelse(
        t_solicitacoes_totais != t_aprovacoes_totais,
         0, 1)
    # t_aprovado_nu_lc = 
    # data.table::fifelse(nu_nota_lc < 100 | is.na(nu_nota_lc), 0, 1),
    # t_aprovado_nu_redacao = 
    #  data.table::fifelse(nu_nota_redacao < 5 | is.na(nu_nota_redacao), 0, 1),
  )

# aprovados e nao aprovados em redação e prova de linguagens
dataRegularTratado %>% 
  filter(in_prova_lc == 1) %>% 
  group_by(t_aprovado_nu_lc, t_aprovado_nu_redacao) %>% 
  summarise(qntd = n())


dataRegularTratado %>% 
  filter(in_prova_mt == 1) %>% 
  group_by(t_certificado) %>% 
  summarise(qntd = n()) 
#   
