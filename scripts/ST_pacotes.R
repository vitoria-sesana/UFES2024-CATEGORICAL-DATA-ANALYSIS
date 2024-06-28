# 18/06/2024
# instalação de pacotes necessarios para analises de dados categorizados


# prLogistic --------------------------------------------------------------
# pacote para estimar razão de prevalencia/prevalence ratio/PR usando regressão logistica
# devtools::install_github("Raydonal/prLogistic")
library(prLogistic)
prLogistic::prLogisticDelta()

prLogisticDelta(survived~ sex + pclass + embarked, 
                data = titanic, pattern="marginal")

titanic$survived %>% table()

# ex2 
data("Thailand", package = "prLogistic")
prLogisticDelta(rgi~  sex + pped + (1|schoolid), data = Thailand, cluster=TRUE)

Thailand$rgi %>% table()

# ex3

data("Thailand", package = "prLogistic")
prLogisticDelta(rgi~  sex + pped + (1|schoolid),
data = Thailand, cluster=TRUE)

# epiR --------------------------------------------------------------------
require(epiR)
epiR::epi.ssxsectn()


dat.v01 <- c(13,2163,5,3349); dat.v01

matrix(dat.v01, nrow = 2, byrow = TRUE)

epiR::epi.2by2(dat = dat.v01, method = "cross.sectional", conf.level = 0.95, units = 100, 
         interpret = TRUE, outcome = "as.columns")


