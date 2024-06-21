require(epiR)
require('vcdExtra')

data(Alligator)
allitable <- xtabs(count~lake+sex+size+food, data=Alligator)
structable(food~lake+sex+size, allitable)

Alligator

require(nnet)
mod <- multinom(food ~ lake+size, data=Alligator, weights=count)
summary(mod)


# ordinal -----------------------------------------------------------------

dados <- read.csv("dado/queijos.csv")
ftable(xtabs(~ Queijo + Nota, data = dados))

require(MASS)
m <- polr(factor(Nota) ~ Queijo, data = dados, Hess=TRUE)
m


ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

(ci <- confint(m))


exp(cbind(OR = coef(m), ci))



# -------------------------------------------------------------------------


epi.2by2(dat = dat.v01, method = "cross.sectional", conf.level = 0.95, units = 100, 
         interpret = FALSE, outcome = "as.columns")
