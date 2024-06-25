# Encceja 2023 - Nacional Regular e Pessoas Privadas de Liberdade
# Análise descritiva

library(ggplot2)


# tabelas -----------------------------------------------------------------

# library(modelsummary)
# datasummary(Species ~Sepal.Width*(Mean + SD), data=iris)



# qntd total --------------------------------------------------------------
dataRegularTratado %>% 
  nrow()


# tipo de certificação ----------------------------------------------------
dataRegularTratado %>% 
  group_by(tp_certificacao) %>% 
  summarise(qntd = n()) %>% 
  mutate(prop = qntd / sum(qntd))



# proporção homens e mulheres ---------------------------------------------
dataRegularTratado %>% 
  group_by(tp_sexo) %>% 
  summarise(qntd = n()) %>% 
  mutate(prop = qntd / sum(qntd))


# idade -------------------------------------------------------------------
d_idade <- dataRegularTratado %>% 
  group_by(t_fx_etaria) %>% 
  summarise(qntd = n()) %>% 
  mutate(prop = qntd / sum(qntd))


ggplot(d_idade, aes(t_fx_etaria, qntd)) +
  geom_col(position = "dodge")


hist(dataRegularTratado$tp_faixa_etaria)

# proporção certificados homens e mulheres --------------------------------
x <- dataRegularTratado %>% 
  group_by(t_certificado, tp_sexo) %>% 
  summarise(qntd = n()) %>% 
  mutate(prop = qntd/ sum(qntd))


ggplot(x, aes(t_certificado, prop, fill = tp_sexo)) +
  geom_col(position = "stack")


# histogramas notas dos participantes -------------------------------------
hist(dataRegularTratado$nu_nota_ch)
abline(v = 100, col = "red")

hist(dataRegularTratado$nu_nota_cn)
abline(v = 100, col = "red")

hist(dataRegularTratado$nu_nota_lc)
abline(v = 100, col = "red")

hist(dataRegularTratado$nu_nota_mt)
abline(v = 100, col = "red")

hist(dataRegularTratado$nu_nota_redacao)
abline(v = 5, col = "red")


# associações -------------------------------------------------------------

dataRegularTratado %>% 
  # filter(in_prova_mt == 1) %>% 
  group_by(in_prova_mt, t_certificado) %>% 
  summarise(qntd = n()) %>% 
  mutate(prop = qntd / sum(qntd))


yq12 <- dataRegularTratado %>% 
  filter(q12 != "") %>% 
  group_by(t_certificado, q12) %>% 
  summarise(qntd = n()) %>% 
  mutate(prop = qntd / sum(qntd)) 

xtabs(qntd ~ q12 + t_certificado, yq12, sparse = TRUE)
xtabs(prop ~ q12 + t_certificado, yq12, sparse = TRUE)


