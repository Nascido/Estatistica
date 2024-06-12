load("dados.RData")
mean(dados$salario_USD)

# AMOSTRA SIMPLES AO ACASO (n=10)
asa10 = dados[sample(nrow(dados), size=10), ]
mean(asa10$salario_USD)

# AMOSTRA SIMPLES AO ACASO (n=100)
asa100 = dados[sample(nrow(dados), size=100), ]
mean(asa100$salario_USD)

# SIMULAÇÃO DO TEOREMA CENTRAL DO LIMITE
# Dados originais
dados_sim = runif(n=1000, min=50000, max=140000)
hist(dados_sim, col='steelblue', main='Histograma do Salario',xlim=c(50000,140000))
# Retirar 10000 vezes uma amostra de tamanho 5
amostra5 = c()
n = 10000
for (i in 1:n){
  amostra5[i] = mean(sample(dados_sim, 5, replace=TRUE))
}
hist(amostra5, col='steelblue', main='Histograma Amostra=5',xlim=c(50000,140000))
# Retirar 10000 vezes uma amostra de tamanho 30
amostra30 = c()
n = 10000
for (i in 1:n){
  amostra30[i] = mean(sample(dados_sim, 30, replace=TRUE))
}
hist(amostra30, col='steelblue', main='Histograma Amostra=30',xlim=c(50000,140000))

mean(dados_sim) 
sd(dados_sim)
mean(amostra5)
sd(amostra5)
mean(amostra30)
sd(amostra30)

# TIRANDO UMA AMOSTRA DE TAMANHO 100
Amostra100 = sample(dados_sim, 100, replace=TRUE)

# INTERVALO DE CONFIANÇA 95%
media = mean(Amostra100)
n = length(Amostra100)
desvio_P = sd(dados_sim)
IC_LI = media-1.96*desvio_P/sqrt(n)
IC_LS = media+1.96*desvio_P/sqrt(n)


