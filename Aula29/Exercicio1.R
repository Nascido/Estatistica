# Leitura da base de dados
base = read.csv2("Exercicio1.csv")

# Sintaxe para o teste de hipótese para média de um grupo
#t.test(x,alternative = c("two.sided", "less", "greater"), mu = 0)
t.test(base, alternative = "less", mu = 15)

# Sintaxe quando se tem somente a média e o desvio padrão, mas não tem a base de dados
library(BSDA)
tsum.test(mean.x=14.5, s.x=3.655, n.x=30, alternative = "less", mu=15)
