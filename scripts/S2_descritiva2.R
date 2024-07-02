# Encceja 2023 - Nacional Regular e Pessoas Privadas de Liberdade
# Análise descritiva

options(scipen = 999)
library(ggplot2)
library(geobr)
library(sf)
library(DescTools)


# mapa qntd de parcipantes por uf -----------------------------------------

dataQntd <- dataRegularTratado %>% 
  group_by(co_uf_prova) %>% 
  summarise(nu_participantes = n()) %>% 
  rename(code_state = co_uf_prova) %>% 
  mutate(pct = nu_participantes / sum(nu_participantes))

dataGeobr <- geobr::read_state(code_state = "all")

dataGeobr <- left_join(dataGeobr, dataQntd, by = "code_state")

ggplot() +
  geom_sf(data = dataGeobr, aes(fill = nu_participantes), color=grDevices::grey(.2)) +
  scale_fill_fermenter(palette = "Blues",
                       direction = 1,
                       name='Qntd de participantes \n',
                       limits = c(0, 200000)
                       ) +
  theme_minimal() 


# grafico 2 ---------------------------------------------------------------

dataPiramide <- dataRegularTratado %>% 
  group_by(tp_sexo, tp_faixa_etaria) %>% 
  summarise(qntd = n()) %>% 
  mutate(pct = qntd/sum(qntd))

m <- dataPiramide %>% 
  filter(tp_sexo == "M") 

m.pop <- m$pct 

f <- dataPiramide %>% 
  filter(tp_sexo == "F")

f.pop <- f$pct

age <- c("-17","17","18","19","20",
         "21","22","23","24","25",
         "26-30", "31-35","36-40","41-45","46-50",
         "51-55","56-60","61-65", "66-70", "+70")

PlotPyramid(m.pop, 
            f.pop,
            ylab = age, space = 0, col = c("cornflowerblue", "indianred"),
            # main="Age distribution at baseline of HELP study",
            lxlab="Masculino", rxlab="Feminino", xlim = c(-max(dataPiramide$pct), max(dataPiramide$pct)) )

# grafico 3 ---------------------------------------------------------------

ch <- dataRegularTratado %>% 
  group_by(in_aprovado_ch) %>% 
  summarise(qntd_aprovados = n()) %>% 
  mutate(pct = qntd_aprovados/ sum(qntd_aprovados),
         area = "ch") %>% 
  rename(aprovado = in_aprovado_ch)
  
cn <- dataRegularTratado %>% 
  group_by(in_aprovado_cn) %>% 
  summarise(qntd_aprovados = n()) %>% 
  mutate(pct = qntd_aprovados/ sum(qntd_aprovados),
         area = "cn") %>% 
  rename(aprovado = in_aprovado_cn)

lc <- dataRegularTratado %>% 
  group_by(in_aprovado_lc) %>% 
  summarise(qntd_aprovados = n()) %>% 
  mutate(pct = qntd_aprovados/ sum(qntd_aprovados),
         area = "lc") %>% 
  rename(aprovado = in_aprovado_lc)

mt <- dataRegularTratado %>% 
  group_by(in_aprovado_mt) %>% 
  summarise(qntd_aprovados = n()) %>% 
  mutate(pct = qntd_aprovados/ sum(qntd_aprovados),
         area = "mt") %>% 
  rename(aprovado = in_aprovado_mt)

dataAprovados <- rbind(cn, ch, lc, mt) %>% 
  mutate(aprovado = as.character(aprovado),
         aprovado = tidyr::replace_na(aprovado, "Não solicitado"),
         aprovado = data.table::fifelse(aprovado == 1, "Aprovado", aprovado),
         aprovado = data.table::fifelse(aprovado == 0, "Não aprovado", aprovado),
         aprovado = factor(aprovado, levels= c("Não solicitado", "Aprovado", "Não aprovado"))
         )

# dataAprovados <- rbind(cn, ch, lc, mt) %>% 
#   mutate(aprovado = as.character(aprovado),
#          aprovado = tidyr::replace_na(aprovado, "Não Solicitado"),
#          aprovado = data.table::fifelse(aprovado == 1, "Aprovado", aprovado),
#          aprovado = data.table::fifelse(aprovado == 0, "Não Aprovado", aprovado),
#          aprovado = factor(aprovado, levels= c("Não Solicitado", "Aprovado", "Não Aprovado"))
#   )


ggplot(dataAprovados, aes(area, pct, fill = aprovado)) +
  geom_col(position = "fill") +
  xlab("Áreas do conhecimento") +
  ylab("Porcentagem dos participantes") +
  scale_fill_manual("", values = c("gray",  "#1492C9", "#DF5538")) + 
  scale_y_continuous(labels = scales::percent,breaks = seq(0, 1, .1)) +
  scale_x_discrete(    labels = c(
    "ch" = "Ciências Humanas",
    "cn" = "Ciências Naturais",
    "lc" = "Linguagens",
    "mt" = "Matemática"
  )) +
  theme(legend.position = "bottom")
  

# grafico aprovacao todas as areas ----------------------------------------

dataAprovacaoTotal <- dataRegularTratado %>% 
  group_by(t_resposta) %>% 
  summarise(qntd = n()) %>% 
  mutate(pct = qntd/ sum(qntd)) %>% 
  mutate(t_resposta = data.table::fifelse(t_resposta == 1, "Aprovação em todas\nas áreas solicitadas", 
                                          "Não aprovação em pelo menos\numa das áreas solicitadas")) %>% 
  mutate(xlabel = "Resultado")

ggplot(dataAprovacaoTotal, aes(xlabel, pct, fill = t_resposta)) +
  geom_col(position = "fill") +
  xlab("") +
  ylab("Porcentagem dos participantes") +
  scale_fill_manual("", values = c( "#0C940D", "#DF5538")) + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, .1)) +
  theme(legend.position = "bottom")


