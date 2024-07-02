# Encceja 2023 - Nacional Regular e Pessoas Privadas de Liberdade
# Análise de prevalencias

library(epiR)

dat.v01 <- c(13,2163,5,3349); dat.v01
matrix(dat.v01, nrow = 2, byrow = TRUE)
dat.v01

epi.2by2(dat = dat.v01, method = "cross.sectional", conf.level = 0.95, units = 100, 
         interpret = TRUE, outcome = "as.columns")



# filhos ------------------------------------------------------------------
# se possui filhos ou nao 
x_filhos <- dataRegularTratado %>% 
  mutate(t_q02 = data.table::fifelse(q02 == "D", 1, 0)) %>% 
  group_by(t_q02, t_resposta) %>% 
  summarise(n = n()) %>% 
  mutate(t_q02 = factor(t_q02, levels = c(0, 1)))


matrix(x_filhos$n, nrow = 2, byrow = TRUE)
epi.2by2(dat = x_filhos$n, method = "cross.sectional", conf.level = 0.95, units = 100, 
         interpret = TRUE, outcome = "as.columns")


# se já frequentou a escola -----------------------------------------------

x_freq_escola <- dataRegularTratado %>% 
  mutate(t_q10 = data.table::fifelse(q10 == "C", 0, 1)) %>% 
  group_by(t_q10, t_resposta) %>% 
  summarise(n = n()) %>% 
  mutate(t_q10 = factor(t_q10, levels = c(0, 1)))


matrix(x_freq_escola$n, nrow = 2, byrow = TRUE)
epi.2by2(dat = x_freq_escola$n, method = "cross.sectional", conf.level = 0.95, units = 100, 
         interpret = TRUE, outcome = "as.columns")



x_freq_escola <- dataRegularTratado %>% 
  mutate(t_q10 = data.table::fifelse(q10 == "C", 0, 1)) %>% 
  group_by(t_q10, t_resposta) %>% 
  summarise(n = n()) %>% 
  mutate(t_q10 = factor(t_q10, levels = c(0, 1)))


matrix(x_freq_escola$n, nrow = 2, byrow = TRUE)
epi.2by2(dat = x_freq_escola$n, method = "cross.sectional", conf.level = 0.95, units = 100, 
         interpret = TRUE, outcome = "as.columns")
