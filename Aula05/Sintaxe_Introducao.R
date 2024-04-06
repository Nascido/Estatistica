# LEITURA DA BASE
dados = read.csv2("Questionario.csv",dec=".")
names(dados)
str(dados)

# CALCULAR A ALTURA MEDIA


# RECLASSIFICAR VARIAVEL
library(dplyr)
dados$Sexo = case_match(dados$Sexo, 1 ~ "Fem", 2 ~ "Masc") 

# CRIAR SUBCONJUNTO DE DADOS
dados1 = dados[dados$Idade<=30,]
dados2 = dados[dados$Sexo=="Fem",]

# CALCULAR O IMC
dados$IMC = dados$Peso/dados$Altura^2

# AGRUPAMENTO DE VARIAVEL QUANTITATIVA
library(psych)
dados$IMC_Cat[dados$IMC <20] = "Faixa1"
dados$IMC_Cat[dados$IMC >=20 & dados$IMC <25] = "Faixa2"
dados$IMC_Cat[dados$IMC >=25] = "Faixa3"

# RETIRAR UMA AMOSTRA SIMPLES AO ACASO
ASA = dados[sample(nrow(dados), size=10), ]

# SALVAR DATAFRAME EM ARQUIVO CSV
write.table(dados, "Questionario_modif.csv", sep=";", row.names=FALSE)

# APAGAR LINHA E COLUNA DA BASE DE DADOS

