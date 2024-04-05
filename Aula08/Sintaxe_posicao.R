base <- read.csv2("Questionario_modif.csv", dec=".")

# ANALISE DESCRITIVA
summary(base$IMC)
summary(base$Dinheiro)

# ANALISE DESCRITIVA POR GRUPO
tapply(base$IMC, base$Sexo, summary)

# PERCENTIS
quantile(base$IMC,probs=c(0.10,0.30,0.70,0.90))
by(base$IMC,base$Sexo,quantile,probs=c(0.10,0.30,0.70,0.90))

# ANALISE DESCRITIVA - PSYCH
library(psych)
describeBy(IMC ~ Sexo, data=base, mat=T)

library(ggplot2)
ggplot(base, aes(Sexo, IMC)) +
  geom_bar(position='dodge', fill="white", color="blue", stat='summary', fun='mean')

