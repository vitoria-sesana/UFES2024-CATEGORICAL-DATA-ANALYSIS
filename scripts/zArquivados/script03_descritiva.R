# 17/04/2024
# Trabalho Final de Análise de Dados Categóricos
# Dados: Conclusão da Formação de Estudantes nos Eua
# Análise Descritiva

require(tidyverse)
require(data.table)

# detalhes da instiuição --------------------------------------------------
b1 <- read.csv(file = "bases/cc_institution_details.csv") %>% 
  as.tibble() %>% 
  mutate(hbcu = fifelse(hbcu == "X", 1, 0),
         flagship = fifelse(flagship == "X", 1, 0))

y <- b1$chronname %>% 
  table() %>% 
  as.data.frame() %>% 
  filter(Freq > 1)

y
y2 <- b1 %>% 
  filter(chronname == y[3,1])

y2 # são de estados diferentes

# graduados da instituição  -----------------------------------------------=
b2 <- read.csv(file = "bases/cc_institution_grads.csv") %>% 
  as.tibble()


# detalhes do setor estadual ----------------------------------------------
b3 <- read.csv(file = "bases/cc_state_sector_details.csv") %>% 
  as.tibble()


# graduados do setor estadual ---------------------------------------------
b4 <- read.csv(file = "bases/cc_state_sector_grads.csv") %>% 
  as.tibble()

nomes_colunas <- c(
  colnames(b1),
  colnames(b2),
  colnames(b3),
  colnames(b4)) 

origem <- c(
  rep("cc_institution_details", ncol(b1)),
  rep("cc_institution_grads", ncol(b2)),
  rep("cc_state_sector_details", ncol(b3)),
  rep("cc_state_sector_grads", ncol(b4))
  )

origem2 <- c(
  rep("b1", ncol(b1)),
  rep("b2", ncol(b2)),
  rep("b3", ncol(b3)),
  rep("b4", ncol(b4))
)

dicionario <- cbind(origem2, origem, nomes_colunas)

write.csv2(dicionario, "bases/dicionario.csv")

x <- apply(b1, 2, table)
