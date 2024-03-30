#######################################################
# CARREGAR BASE DE DADOS
dados = read.csv2("Questionario_modif.csv", dec=".")
names(dados)

#######################################################
# TABELA PARA UMA VARIAVEL QUALITATIVA
ftabela = table(dados$Gosta_Est, useNA = "ifany") 
ptabela = round(prop.table(ftabela)*100,1)
tabela1 = data.frame(ftabela,ptabela)
tabela1 = tabela1[,-3]
colnames(tabela1) <- c("Gosta_Est","Frequencia","Porcentagem")
tabela1
write.table(tabela1,"Tabela.csv", sep=";", dec=",", row.names=TRUE)


#######################################################
# GRAFICO PARA UMA VARIAVEL QUALITATIVA
library(ggplot2)
ggplot(dados, aes(x=Cidade )) +
  geom_bar(color="blue", fill=rgb(0.1,0.6,0.7,0.7) ) +
  labs(title="Gráfico para Cidade",
       x ="Cidade", y = "Frequência")


#######################################################
# TABELA PARA DUAS VARIAVEIS QUALITATIVAS
ftabela2 = table(dados$Sexo, dados$Gosta_Est, useNA = "ifany") 
ptabela2 = round(prop.table(ftabela2,1)*100,1)
tabela2 = data.frame(ftabela2,ptabela2)
tabela2 = tabela2[,-c(4,5)]
colnames(tabela2) <- c("Sexo","Gosta_Est","Frequencia","Porcentagem")
tabela2
write.table(tabela1,"Tabela2.csv", sep=";", dec=",", row.names=TRUE)


#######################################################
# GRAFICO PARA DUAS VARIAVEIS QUALITATIVAS
library(ggplot2)
ggplot(dados, aes(x=Sexo, fill=Gosta_Est)) + 
  geom_bar(position="fill") +
  ylab("Porcentagem")


#######################################################
# GRAFICO DE DISPERSAO
library(ggplot2)
ggplot(dados, aes(x=Altura, y=Peso)) +
  geom_point() 

ggplot(dados, aes(x=Altura, y=Peso, color=Sexo)) +
  geom_point() 


#######################################################
# GRAFICO PARA UMA VARIAVEL QUANTITATIVA
# Histograma
library(ggplot2)
ggplot(dados, aes(x=IMC)) + 
  geom_histogram(bins=7)

ggplot(dados, aes(x=IMC)) + 
  geom_histogram(bins = 7, fill = 'blue', color = 'white')


#######################################################
# TABELA PARA UMA VARIAVEL QUALITATIVA E UMA QUANTITATIVA
tabela.medias <- aggregate(dados$IMC, by=list(dados$Sexo), FUN="mean")
colnames(tabela.medias) <- c("IMC","Media")
tabela.medias


#######################################################
# SALVAR ARQUIVO EM FORMATO DE RELATORIO
# File -> Compile Report...

