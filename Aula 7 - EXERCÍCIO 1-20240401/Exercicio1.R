dados = read.csv2("dados_salarios.csv", dec=".")

# 2
library(dplyr)
dados$trab_remoto = case_match(dados$trab_remoto, 0 ~ "Não", 50 ~ "Parcial", 100 ~ "Sim")

# 3
tabela_experiencia = table(dados$experiencia, useNA = "ifany")
tabela_media_experiencia = round(prop.table(tabela_experiencia)*100,1)
tabela_freq_media_experiencia = data.frame(tabela_experiencia,tabela_media_experiencia)
tabela_freq_media_experiencia = tabela_freq_media_experiencia[,-3]
colnames(tabela_freq_media_experiencia) <- c("Experiência","Frequência","Porcentagem")
tabela_freq_media_experiencia
write.table(tabela_freq_media_experiencia,"tabela_media_freq_experiencia.csv", sep=";", dec=",", row.names=FALSE)

# 4

# 5
tabela_experiencia_salario <- aggregate(dados$salario_USD, by=list(dados$experiencia), FUN="mean")
colnames(tabela_experiencia_salario) <- c("Salário","Média")
tabela_experiencia_salario
write.table(tabela_experiencia_salario,"tabela_experiencia_salario.csv", sep=";", dec=",", row.names=FALSE)

#6
