# 18/06/2024
# Encceja 2023 - Nacional Regular e Pessoas Privadas de Liberdade
# Leitura, seleção/filtro e tratamento

# Bibliotecas -------------------------------------------------------------
library(dplyr)

# Dicionário --------------------------------------------------------------

# caminho
path_dicionario <- "dado/bruto/DICIONÁRIO/Dicionário_Microdados_ENCCEJA_2023.xlsx"

# sheets do arquivo xlsx
sheets_dicionario <- openxlsx::getSheetNames(path_dicionario); sheets_dicionario  

# dicionario
dicRegular <- 
  readxl::read_xlsx(path_dicionario, sheet = sheets_dicionario[1]) %>% 
  janitor::clean_names() 

# colunas
colRegular <- dicRegular$dicionario_de_variaveis_encceja_2023 %>% 
  na.omit() %>% 
  as.vector(); colRegular

# Dados -------------------------------------------------------------------

pathRegular <- "dado/bruto/DADOS/MICRODADOS_ENCCEJA_2023_REG_NAC.csv"

dataRegular <- data.table::fread(pathRegular) %>% 
  janitor::clean_names() %>% 
  mutate(modalidade_encceja = "Nacional Regular")

# Tratamento --------------------------------------------------------------

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
        t_solicitacoes_totais != t_aprovacoes_totais, 0, 1)
    )

dataRegularTratado <- dataRegularTratado %>% 
  mutate(t_resposta = factor(t_resposta, levels = c(0, 1)))
