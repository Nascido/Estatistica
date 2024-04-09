#0 Leitura da base de dados
dados <- read.csv2("dados_salarios.csv", dec=".")

#1  Amostra aleatoria simples de 300 observacoes na base de dados
tam_amostra = 300
salarios = dados$salario_USD

amostra_salarios = sample(salarios, tam_amostra)

#2  Media, mediana, percentil5, percentil 25, percentil 75,percentil 95, minimo e maximo (Variavel salario_USD)
mean(amostra_salarios)
median(amostra_salarios)
quantile(amostra_salarios, c(0.05, 0.25, 0.75, 0.95))
min(amostra_salarios)
max(amostra_salarios)

#3  Por Categoria (Variavel trab_remoto)


#4  Categoria (tam_empresa)

#5  Grafico dos resultados: item 4

