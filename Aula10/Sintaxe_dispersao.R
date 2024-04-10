base = read.csv2("Questionario_modif.csv", dec=".")

# ANALISE COM O PACOTE FSA
library(FSA)
result1 <- Summarize(IMC ~ Sexo, data=base)
result1$cv = (result1$sd/result1$mean)*100
result1


# ANALISE COM O PACOTE PSYCH
library(psych)
result2 <- describeBy(IMC~Sexo, data=base, mat=T)
result2 <- result2[,c(2,4,5,6)]
result2
# incluir o cálculo do coeficiente de variação na tabela result2


# ANALISE COM O PACOTE DOBY
library(doBy)
result3 <- summaryBy(IMC ~ Sexo, data = base,
                     FUN = function(x) { c(n = length(x), m = mean(x), s = sd(x), cv = sd(x)/mean(x)*100) } )
result3
# refazer os resultados "result3" incluindo mais uma variavel qualitativa "Cursinho"
# coloque o nome de "result4"


# GRAFICO COM MEDIDA DE VARIACAO
library(ggplot2)
ggplot(base, aes(x = Sexo, y = IMC, color = Sexo)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0)) +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange')


# GRAFICO DE DENSIDADE
library(sm)
comp = sm.density.compare(base$IMC, base$Sexo, xlab="Peso")
title(main="Distribuição do Peso")
legend("topright", comp$levels, col = comp$col, lty = comp$lty, lwd = comp$lwd)

