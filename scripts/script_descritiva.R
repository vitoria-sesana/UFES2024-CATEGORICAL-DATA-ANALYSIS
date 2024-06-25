# Encceja 2023 - Nacional Regular e Pessoas Privadas de Liberdade
# Análise descritiva

library(ggplot2)


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

