#0 Leitura da base de dados
dados <- read.csv2("dados_salarios.csv", dec=".")

#1  Amostra aleatoria simples de 300 observacoes na base de dados
install.packages("dplyr")
library("dplyr")

set.seed("12345")

tam_amostra = 300
amostra = sample_n(dados, tam_amostra)

#2  Media, mediana, percentil5, percentil 25, percentil 75,percentil 95, minimo e maximo (Variavel salario_USD)
salario = amostra$salario_USD
mean(salario)
median(salario)
quantile(salario, c(0.05, 0.25, 0.75, 0.95))
min(salario)
max(salario)

summary(salario)

#3  Por Categoria (Variavel trab_remoto)
by(amostra$salario_USD, amostra$trab_remoto, mean)
by(amostra$salario_USD, amostra$trab_remoto, median)
by(amostra$salario_USD, amostra$trab_remoto, quantile, probs=c(0.05, 0.25, 0.75, 0.95))
by(amostra$salario_USD, amostra$trab_remoto, min)
by(amostra$salario_USD, amostra$trab_remoto, max)

by(amostra$salario_USD, amostra$trab_remoto, summary)

#4  Categoria (tam_empresa)
by(amostra$salario_USD, amostra$tam_empresa, mean)
by(amostra$salario_USD, amostra$tam_empresa, median)
by(amostra$salario_USD, amostra$tam_empresa, quantile, probs=c(0.05, 0.25, 0.75, 0.95))
by(amostra$salario_USD, amostra$tam_empresa, min)
by(amostra$salario_USD, amostra$tam_empresa, max)

by(amostra$salario_USD, amostra$tam_empresa, summary)

#5  Grafico dos resultados: item 4

